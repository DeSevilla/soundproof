// #![allow(unused)]
use fundsp::hacker32::*;

use clap::*;
use lambdapi::*;
use soundproof::*;
use soundproof::select::*;
use music::*;
use types::*;
use translate::*;
use ast::*;

/// The "proof" side. Implementation of a simple dependently-typed lambda calculus, 
/// translated from Löh, McBride, and Swierstra's [LambdaPi](https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf)
/// and in particular [Ilya Klyuchnikov's implementation](https://github.com/ilya-klyuchnikov/lambdapi/).
/// See the AST submodule page for most of the operative pieces.
pub mod lambdapi;
/// The "sound" side. Synths and utilities for generating audio.
pub mod music;
/// Translation from LambdaPi to music.
pub mod soundproof;

/// Modes for structuring the translation from LambdaPi term to SoundTree.
#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum Structure {
    /// Assign melodies according to the structure of terms themselves
    Term,
    /// Assign melodies mostly according to the types of terms
    Type,
    /// Run through a series of terms designed to test the different melodies. Overrides --value.
    Test,
    /// TODO allow multiple values instead of this
    Buildup,
}

/// Determines how time is broken down between sequential segments.
#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum Scaling {
    /// At each node, just splits time evenly among sequential children. Outer terms get much more time relative to deeper subtrees.
    Linear,
    /// Splits time according to the size of the subtree, but adjusted to give a bit more weight to outer terms.
    Weight,
    // SizeAligned,  //goal here was to align to a rhythm but this has conceptual/structural issues
    /// Splits time according to the size of the subtree in terms of pure number of nodes, no increased weight of outer terms.
    Size,
}

impl Scaling {
    fn exponent() -> f64 {
        0.77
    }

    fn child_scale(&self, child: &SoundTree) -> f64 {
        match self {
            Scaling::Linear => 1.0,
            Scaling::Weight => child.subtree_weight(Self::exponent()),
            Scaling::Size => child.size() as f64,
        }
    }

    fn parent_scale(&self, parent: &SoundTree) -> f64 {
        match self {
            Scaling::Linear => match parent {
                SoundTree::Simul(vec, _) => vec.len() as f64,
                SoundTree::Seq(vec, _) => vec.len() as f64,
                SoundTree::Sound(_, _) => 1.0,
            },
            Scaling::Weight => parent.weight(Self::exponent()),
            Scaling::Size => parent.size() as f64,
        }
    }
}

/// Predefined terms of the dependently typed lambda calculus. Options are drawn from terms used to construct Girard's Paradox.
/// See [Hurkens et al.](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf) for details of the paradox.
#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum NamedTerm {
    /// The type of types.
    Star,
    SetsOf,
    // SetsOfNat, //these two commented out are not supported by the current preliminary melody functions
    // ExFalso,
    /// The "universe" type U used to derive Girard's Paradox: `forall (X :: *) . (P(P(X)) -> X) -> P(P(X))`.
    U,
    /// A term of type P(P(U)) -> U.
    Tau,
    /// A term of type U -> P(P(U)).
    Sigma,
    /// A term of type U.
    Omega,
    Lem0,
    Lem2,
    Lem3,
    /// Girard's Paradox, full term according to the Hurkens approach.
    Girard,
    /// Girard's Paradox, reduced as far as possible without nontermination.
    GirardReduced
}

impl NamedTerm {
    fn term(&self) -> ITerm {
        use NamedTerm::*;
        match self {
            Star => ITerm::Star,
            SetsOf => sets_of(),
            // SetsOfNat => sets_of_nat(),
            // ExFalso => exfalso(),
            U => u(),
            Tau => tau(),
            Sigma => sigma(),
            Omega => omega(),
            Lem0 => lem0(),
            Lem2 => ireduce(lem2()).unwrap(),
            Lem3 => ireduce(lem3()).unwrap(),
            Girard => girard(),
            GirardReduced => girard_reduced(),
        }
    }
}



/// Command-line options to determine which set of melodies to use when generating melodies for a term.
#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum AudioSelectorOptions {
     /// First, highly arbitary melody suite. See [imelody1] and [cmelody2] (there is no cmelody1).
    A,
    /// Melodies based on B, C, E, and G with arbitrarily-chosen instruments. See [imelody2] and [cmelody2].
    B,
    /// More intentionally-chosen melodies with still-arbitrary instruments. See [imelody3] and [cmelody3].
    C,
    /// Same melodies as C but with cleaner-sounding instruments. See [imelody4] and [cmelody4].
    D,
    /// Melody suite with some hints of dissonance. See [imelody5] and [cmelody5].
    E,
    /// Like E, but switched around and more textured
    F,
    /// Same melodies as B and C but exclusively as sines. See [imelody_oneinstr] and [cmelody_oneinstr].
    PureSine,
    /// Pulled from audio files, short names
    NamesShort,
    NamesLong,
    Stratified,
    Effects,
    Mixed,
    Loop,
    Rhythmized,
    Strat2,
}

// impl From<AudioSelectorOptions> for MelodySelector {
//     fn from(value: AudioSelectorOptions) -> Self {
//         use AudioSelectorOptions::*;
//         match value {
//             A => Self::A,
//             B => Self::B,
//             C => Self::C,
//             D => Self::D,
//             E => Self::E, 
//             F => Self::F,
//             PureSine => Self::PureSine,
//             // NamesShort => Self::Concrete(ClipSelector::names()),
//             // NamesLong => Self::Concrete(ClipSelector::names_long())
//             _ => unimplemented!()
//         }
//     }
// }

#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum FilterOptions {
    ClipLowpass,
    None,
}

/// A system which converts dependently-typed lambda calculus into music, with a focus on Girard's Paradox.
#[derive(Parser, Debug)]
#[command(version, about, long_about=None)]
pub struct Args {
    /// Determines how time is broken down between sequential segments.
    #[arg(short, long, default_value="weight")]
    scaling: Scaling,
    /// In seconds. If unset, scales with size of tree.
    #[arg(short, long)]
    time: Option<f64>,
    /// Predefined terms of the dependently typed lambda calculus.
    #[arg(short, long, default_value="girard-reduced")]
    value: NamedTerm,
    /// Determines which set of melodies to use.
    #[arg(short, long, default_value="strat2")]
    melody: AudioSelectorOptions,
    /// How to assign sound-tree structure to a term.
    #[arg(short('S'), long, default_value="type")]
    structure: Structure,
    /// Filters to apply to output
    #[arg(short, long, default_value="clip-lowpass")]
    filters: FilterOptions,
}

pub fn main() {
    // let wave = Wave::load("output/malhombrechords.wav").unwrap();
    // let wave = retime_wave(wave, 6.0);
    // wave.save_wav32("output/malhombreslow.wav").unwrap();
    let args = Args::parse();
    let term = args.value.term();
    //validation just makes sure it typechecks; we can't evaluate the paradox or it'll run forever.
    validate(&format!("Term: {:?}", args.value), &term, false);
    use std::time::Instant;
    println!("Translating...");
    let now = Instant::now();
    // use MelodySelector::*;
    fn structure_func(selector: impl Selector, args: &Args) -> SoundTree {
        match args.structure {
            Structure::Term => term_translate(args.value.term(), selector),
            Structure::Type => type_translate(args.value.term(), selector),
            Structure::Test => test_tree(selector),
            Structure::Buildup => buildup([u(), tau(), sigma(), omega(), lem0(), ireduce(lem2()).unwrap(), ireduce(lem3()).unwrap(), girard_reduced()], selector)
        }
    }
    use AudioSelectorOptions::*;
    let tree = match args.melody {
        A => structure_func(MelodySelector::A.deepen(), &args),
        B => structure_func(MelodySelector::B.deepen(), &args),
        C => structure_func(MelodySelector::C.deepen(), &args),
        D => structure_func(MelodySelector::D.deepen(), &args),
        E => structure_func(MelodySelector::E.deepen(), &args),
        F => structure_func(MelodySelector::F.deepen(), &args),
        PureSine => structure_func(MelodySelector::PureSine.deepen(), &args),
        NamesShort => structure_func(ClipSelector::names(), &args),
        NamesLong => structure_func(ClipSelector::names_long(), &args),
        Stratified => structure_func(StratifiedInfo::default(), &args),
        Effects => structure_func(Effector::new(), &args),
        Mixed => structure_func(MixedOutput::new(), &args),
        Loop => structure_func(Looper::new(Rhythmizer::new()), &args),
        Rhythmized => structure_func(Rhythmizer::new(), &args),
        Strat2 => structure_func(Stratifier2::new(), &args)
    };
    println!("...done in {:?}", now.elapsed());
    let time = args.time.unwrap_or(std::cmp::min(tree.size(), 10000) as f64);
    let mut seq = Box::new(Sequencer::new(false, 2));
    println!("Sequencing over {time} seconds...");
    let now = Instant::now();
    tree.generate_with(&mut seq, 0.0, time, args.scaling, 0.0);
    println!("...done in {:?}", now.elapsed());
    let mut output = unit::<U0, U2>(seq);
    match args.filters {
        FilterOptions::None => save(&mut output, time),
        FilterOptions::ClipLowpass => save(
            &mut (output >> 
                stacki::<U2, _, _>(|_| 
                    shape(Adaptive::new(0.1, Tanh(0.5))) >> 
                    lowpass_hz(2500.0, 1.0) >>
                    mul(0.05)
                    // >> mul(10.0)
                )), 
            time
        ),
    }
    println!("Done.");
}
