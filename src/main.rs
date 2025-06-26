use std::fs;

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

pub mod draw;

/// Modes for structuring the translation from LambdaPi term to SoundTree.
#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum Structure {
    /// Assign melodies according to the structure of terms themselves
    Term,
    /// Assign melodies mostly according to the types of terms
    Type,
    /// Run through a series of terms designed to test the different melodies. Overrides `--value`.
    Test,
    // TODO allow multiple values instead of this
    // Buildup,
}

/// Determines how time is broken down between sequential segments.
#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum Scaling {
    /// At each node, just splits time evenly among sequential children. Outer terms get much more time relative to deeper subtrees.
    Even,
    /// Splits time according to the size of the subtree, but adjusted to give a bit more weight to outer terms.
    Weight,
    // SizeAligned,  //goal here was to align to a rhythm but this has conceptual/structural issues
    /// Splits time according to the size of the subtree in terms of pure number of nodes, no increased weight of outer terms.
    Size,
}

impl Scaling {
    fn exponent() -> f64 {
        0.87
    }

    fn child_scale(&self, child: &SoundTree) -> f64 {
        match self {
            Scaling::Even => 1.0,
            Scaling::Weight => child.subtree_weight(Self::exponent()),
            Scaling::Size => child.size() as f64,
        }
    }

    fn parent_scale(&self, parent: &SoundTree) -> f64 {
        match self {
            Scaling::Even => match parent {
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
    /// A term of type U, tau of the set of 'inductive' elements of U.
    Omega,
    /// A proof that Omega is well-founded.
    Lem0,
    /// A proof that tau(sigma(Omega)) is not a predecessor of Omega.
    Lem2,
    /// A proof that tau(sigma(Omega)) is a predecessor of Omega.
    Lem3,
    /// Girard's Paradox, full term according to the Hurkens approach.
    Girard,
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
            Lem2 => lem2(),
            Lem3 => lem3(),
            Girard => girard(),
        }
    }
}



/// Command-line options to determine which set of melodies to use when generating melodies for a term.
#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum AudioSelectorOptions {
    /// Canonical selector: melody is determined by node; instrument, rhythm, and effect are determined by parent node.
    StratFull,
     /// First, highly arbitary melody suite.
    A,
    /// Melodies based on B, C, E, and G with arbitrarily-chosen instruments.
    B,
    /// More intentionally-chosen melodies with still-arbitrary instruments.
    C,
    /// Same melodies as C but with cleaner-sounding instruments.
    D,
    /// Melody suite with some hints of dissonance.
    E,
    /// Like E, but switched around and more textured. See
    F,
    /// Same melodies as B and C but exclusively as sines.
    PureSine,
    /// Pulled from audio files, short names of the AST variants
    NamesShort,
    /// Pulled from audio files, extended names of the AST variants
    NamesLong,
    /// Melody is determined by node, instrument by parent node.
    StratInstr,
    /// Melody & instrument determined by node, an additional effect is determined by parent node.
    Effects,
    /// A mix of melodies, concrete audio clips, and a stochastic texture
    Mixed,
    /// Melodies loop instead of being stretched out over the duration.
    Loop,
    /// Melody & instrument are determined by node, rhythm is determined by parent node. 
    Rhythmized,
    /// Just plays sine tones, no melody.
    Bare,
}

/// Additional filters added after audio generation.
#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum FilterOptions {
    /// Clip amplitude and put it through a low-pass filter.
    ClipLowpass,
    /// Reduce volume significantly.
    Quiet,
    /// No additional filter.
    None,
}

/// A system which converts dependently-typed lambda calculus into music, with a focus on Girard's Paradox.
/// Generates audio, a spectrograph, an image representation of the "sound tree" structure, and
/// animation frames matching the visualization with the sound.
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
    #[arg(short, long, default_value="girard")]
    value: NamedTerm,
    /// When set, evaluate the term as far as possible before being presented.
    #[arg(short, long, action)]
    eval: bool,
    /// When set, only generate visualization (including animation frames), not music.
    #[arg(short, long, action)]
    draw_only: bool,
    /// When set, do not generate animation frames.
    #[arg(short, long, action)]
    noanimate: bool,
    /// Determines which set of melodies to use.
    #[arg(short, long, default_value="strat-full")]
    melody: AudioSelectorOptions,
    /// How to assign sound-tree structure to a term.
    #[arg(short('S'), long, default_value="type")]
    structure: Structure,
    /// Additional filters added after audio generation.
    #[arg(short, long, default_value="clip-lowpass")]
    filters: FilterOptions,
}

impl Args {
    pub fn term(&self) -> ITerm {
        if self.eval {
            match self.value {
                NamedTerm::Girard => girard_reduced(),
                name => ireduce(name.term()).unwrap()
            }
        } else {
            self.value.term()
        }
    }
}

pub fn main() {
    // let wave = Wave::load("output/malhombrechords.wav").unwrap();
    // let wave = retime_wave(wave, 6.0);
    // wave.save_wav32("output/malhombreslow.wav").unwrap();
    let args = Args::parse();
    //validation just makes sure it typechecks; we can't evaluate the paradox or it'll run forever.
    validate(&format!("Term: {:?}", args.value), &args.term(), false);
    use std::time::Instant;
    println!("Translating...");
    let now = Instant::now();
    // use MelodySelector::*;
    fn structure_func(selector: impl Selector, args: &Args) -> SoundTree {
        match args.structure {
            Structure::Term => term_translate(args.term(), selector),
            Structure::Type => type_translate(args.term(), selector),
            Structure::Test => test_tree(selector),
            // Structure::Buildup => buildup([sets_of(), u(), tau(), sigma(), omega()], selector),//, lem0(), ireduce(lem2()).unwrap(), ireduce(lem3()).unwrap(), girard_reduced()], selector)
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
        StratInstr => structure_func(StratifyInstrument::default(), &args),
        Effects => structure_func(Effector::new(), &args),
        Mixed => structure_func(MixedOutput::new(), &args),
        Loop => structure_func(Looper::new(Rhythmizer::new()), &args),
        Rhythmized => structure_func(Rhythmizer::new(), &args),
        StratFull => structure_func(FullStratifier::new(), &args),
        Bare => structure_func(Plain::new(), &args),
    };
    println!("...done in {:?}", now.elapsed());
    println!("Drawing...");
    let now = Instant::now();
    draw::draw(&tree, args.scaling, format!("output/images/{:?}{}-viz.png", args.value, if args.eval { "-reduced" } else { "" }));
    println!("One image: {:?}", now.elapsed());
    draw::draw(&tree, args.scaling, "output/visualization.png");
    let time = args.time.unwrap_or(tree.size() as f64);
    if !args.noanimate {
        let frames_path = "output/images/frames";
        fs::remove_dir_all(frames_path).unwrap();
        fs::create_dir(frames_path).unwrap();
        let frames = (30.0 * time).floor() as usize;
        println!("Drawing {frames} frames...");
        draw::draw_anim(&tree, args.scaling, frames_path, frames);
        println!("All frames: {:?}", now.elapsed());
    }
    if args.draw_only {
        return;
    }
    let mut seq = Box::new(Sequencer::new(false, 2));
    // let backend = seq.backend();
    println!("Sequencing over {time} seconds...");
    let now = Instant::now();
    tree.generate_with(&mut seq, 0.0, time, args.scaling, 0.0);
    println!("...done in {:?}", now.elapsed());
    let output: &mut Sequencer = &mut seq;
    match args.filters {
        FilterOptions::None => save(output, time),
        FilterOptions::ClipLowpass => save(
            &mut (unit::<U0, U2>(seq) >>
                stacki::<U2, _, _>(|_|
                    shape(Adaptive::new(0.1, Tanh(0.5))) >> 
                    lowpass_hz(2500.0, 1.0) >>
                    mul(0.1)
                    // >> mul(10.0)
                )),
            time
        ),
        FilterOptions::Quiet => save(&mut (unit::<U0, U2>(seq) >> (mul(0.05) | mul(0.05))), time),
    }
    println!("Done.");
}
