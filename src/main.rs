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
        0.85
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
    /// Girard's Paradox, full term according to the Hurkens approach.
    Girard,
    /// Girard's Paradox, reduced as far as possible without nontermination.
    GirardReduced
}

impl NamedTerm {
    fn term(&self) -> ITerm {
        match self {
            NamedTerm::Star => ITerm::Star,
            // NamedTerm::SetsOfNat => sets_of_nat(),
            // NamedTerm::ExFalso => exfalso(),
            NamedTerm::U => u(),
            NamedTerm::Tau => tau(),
            NamedTerm::Sigma => sigma(),
            NamedTerm::Omega => omega(),
            NamedTerm::Girard => girard(),
            NamedTerm::GirardReduced => girard_reduced(),
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
    Effector,
    Mixed,
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
    #[arg(short, long, default_value="f")]
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
    // let selector = args.melody.into();
    use std::time::Instant;
    print!("Translating...");
    let now = Instant::now();
    use MelodySelector::*;
    fn structure_func(selector: impl Selector, args: &Args) -> SoundTree {
        match args.structure {
            Structure::Term => term_translate(args.value.term(), &F),
            // Structure::Type => type_translate(term, &selector),
            Structure::Type => strat_translate(args.value.term(), selector),
            Structure::Test => test_tree(&F),
        }
    }
    let tree = match args.melody {
        AudioSelectorOptions::A => structure_func(A.deepen(), &args),
        AudioSelectorOptions::B => structure_func(B.deepen(), &args),
        AudioSelectorOptions::C => structure_func(C.deepen(), &args),
        AudioSelectorOptions::D => structure_func(D.deepen(), &args),
        AudioSelectorOptions::E => structure_func(E.deepen(), &args),
        AudioSelectorOptions::F => structure_func(F.deepen(), &args),
        AudioSelectorOptions::PureSine => structure_func(PureSine.deepen(), &args),
        AudioSelectorOptions::NamesShort => structure_func(ClipSelector::names(), &args),
        AudioSelectorOptions::NamesLong => structure_func(ClipSelector::names_long(), &args),
        AudioSelectorOptions::Stratified => structure_func(StratifiedInfo::default(), &args),
        AudioSelectorOptions::Effector => structure_func(Effector::new(), &args),
        AudioSelectorOptions::Mixed => structure_func(MixedOutput::new(), &args),
    };
    println!("...done in {:?}", now.elapsed());
    // let txt = tree.metadata().name;
    // println!("{txt}");
    let time = args.time.unwrap_or(std::cmp::min(tree.size(), 10000) as f64);
    let mut seq = Sequencer::new(false, 2);
    print!("Sequencing over {time} seconds...");
    let now = Instant::now();
    tree.generate_with(&mut seq, 0.0, time, args.scaling, 0.0);
    println!("...done in {:?}", now.elapsed());
    let mut output = unit::<U0, U2>(Box::new(seq));
    match args.filters {
        FilterOptions::None => save(&mut output, time),
        FilterOptions::ClipLowpass => save(
            &mut (output >> 
                stacki::<U2, _, _>(|_| 
                    shape(Adaptive::new(0.1, Tanh(0.5))) >> 
                    lowpass_hz(2500.0, 1.0) >>
                    mul(0.05)
                )), 
            time
        ),
    }
    println!("Done");
}
