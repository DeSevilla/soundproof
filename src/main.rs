use std::fs;
use fundsp::hacker32::*;
use clap::*;
use read_input::prelude::input;
use read_input::InputBuild;
use std::time::Instant;

use soundproof::*;
use soundproof::select::*;
use music::*;
use types::*;
use translate::*;
use lambdapi::*;
use lambdapi::ast::*;

use crate::parse::{statement, Statement};

// use parse::test_iterm_replicate;


/// The "proof" side. Implementation of a simple dependently-typed lambda calculus, 
/// translated from Löh, McBride, and Swierstra's [LambdaPi](https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf)
/// and in particular [Ilya Klyuchnikov's implementation](https://github.com/ilya-klyuchnikov/lambdapi/).
/// See the AST submodule page for most of the operative pieces.
pub mod lambdapi;
/// The "sound" side. Synths and utilities for generating audio.
pub mod music;
/// Translation from LambdaPi to music.
pub mod soundproof;

pub mod parse;
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
pub enum DivisionMethod {
    /// At each node, just splits time evenly among sequential children. Outer terms get much more time relative to deeper subtrees.
    Even,
    /// Splits time according to the size of the subtree, but adjusted to give a bit more weight to outer terms.
    Weight,
    // SizeAligned,  //goal here was to align to a rhythm but this has conceptual/structural issues
    /// Splits time according to the size of the subtree in terms of pure number of nodes, no increased weight of outer terms.
    Size,
}

impl DivisionMethod {
    fn exponent() -> f64 {
        0.87
    }

    fn child_scale(&self, child: &SoundTree) -> f64 {
        match self {
            DivisionMethod::Even => 1.0,
            DivisionMethod::Weight => child.subtree_weight(Self::exponent()),
            DivisionMethod::Size => child.size() as f64,
        }
    }

    fn parent_scale(&self, parent: &SoundTree) -> f64 {
        match self {
            DivisionMethod::Even => match parent {
                SoundTree::Simul(vec, _) => vec.len() as f64,
                SoundTree::Seq(vec, _) => vec.len() as f64,
                SoundTree::Sound(_, _) => 1.0,
            },
            DivisionMethod::Weight => parent.weight(Self::exponent()),
            DivisionMethod::Size => parent.size() as f64,
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

    fn term_reduced(&self) -> ITerm {
        match self {
            NamedTerm::Girard => girard_reduced(),
            _ => ireduce(self.term()).unwrap()  // can't fail because it's a named term
        }
    }
}



/// Command-line options to determine which set of melodies to use when generating melodies for a term.
#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum AudioSelectorOptions {
    /// Canonical selector: melody is determined by node; instrument, rhythm, and effect are determined by parent node.
    FullStratified,
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

// impl AudioSelectorOptions {
//     pub fn apply()
// }

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
    /// Predefined terms of the dependently typed lambda calculus.
    #[arg(short, long, default_value="girard")]
    value: NamedTerm,
    /// When set, normalize the term as far as possible before being presented.
    #[arg(short, long, action)]
    reduce: bool,
    /// In seconds. If unset, scales with size of tree.
    #[arg(short, long)]
    time: Option<f64>,
    /// Determines how time is broken down between sequential segments.
    #[arg(short, long, default_value="weight")]
    division: DivisionMethod,
    /// Determines which audio selector to use, determining melodies, rhythm, timbre, and so on.
    #[arg(short, long, default_value="full-stratified")]
    content: AudioSelectorOptions,
    /// How to assign sound-tree structure to a term.
    #[arg(short, long, default_value="type")]
    structure: Structure,
    /// Additional filters added after audio generation.
    #[arg(short, long, default_value="clip-lowpass")]
    filters: FilterOptions,
    /// When set, only generate visualization (potentially including animation frames), not music.
    #[arg(short('D'), long, action)]
    draw_only: bool,
    /// When set, generate animation frames.
    #[arg(short, long, action)]
    animate: bool,
    /// Run live instead of saving to file
    #[arg(short, long, action)]
    live: bool,
    /// Name of the output file
    #[arg(short, long, default_value="output.wav")]
    output: String
}

impl Args {
    pub fn term(&self) -> ITerm {
        if self.reduce {
            self.value.term_reduced()
            // for value in self.values {
            // match self.value {
            //     NamedTerm::Girard => girard_reduced(),
            //     name => ireduce(name.term()).unwrap()
            // }
        } else {
            self.value.term()
        }
    }
}

// pub struct ReplState {
//     value: ITerm,
//     division: DivisionMethod,
//     structure: Structure,
//     content: AudioSelectorOptions,
// }

// impl TreeMaker {
//     pub fn term(&self) -> &ITerm {
//         &self.value
//     }
// }

// impl From<&Args> for TreeMaker {
//     fn from(value: &Args) -> Self {
//         Self {
//             value: value.term(),
//             division: value.division,
//             structure: value.structure,
//             content: value.content
//         }
//     }
// }

pub fn make_tree(structure: Structure, content: AudioSelectorOptions, term: &ITerm) -> SoundTree {
    // validate(&format!("Term: {:?}", args.value), &args.term(), false);
    println!("Translating...");
    let now = Instant::now();
    // use MelodySelector::*;
    fn structure_func(selector: impl Selector, structure: Structure, term: &ITerm) -> SoundTree {
        match structure {
            Structure::Term => term_translate(term, selector),
            Structure::Type => type_translate(term, selector),
            Structure::Test => test_tree(selector),
            // Structure::Buildup => buildup([sets_of(), u(), tau(), sigma(), omega()], selector),//, lem0(), ireduce(lem2()).unwrap(), ireduce(lem3()).unwrap(), girard_reduced()], selector)
        }
    }
    use AudioSelectorOptions::*;
    let tree = match content {
        A => structure_func(MelodySelector::A.deepen(), structure, term),
        B => structure_func(MelodySelector::B.deepen(), structure, term),
        C => structure_func(MelodySelector::C.deepen(), structure, term),
        D => structure_func(MelodySelector::D.deepen(), structure, term),
        E => structure_func(MelodySelector::E.deepen(), structure, term),
        F => structure_func(MelodySelector::F.deepen(), structure, term),
        PureSine => structure_func(MelodySelector::PureSine.deepen(), structure, term),
        NamesShort => structure_func(ClipSelector::names(), structure, term),
        NamesLong => structure_func(ClipSelector::names_long(), structure, term),
        StratInstr => structure_func(StratifyInstrument::default(), structure, term),
        Effects => structure_func(Effector::new(), structure, term),
        Mixed => structure_func(MixedOutput::new(), structure, term),
        Loop => structure_func(Looper::new(Rhythmizer::new()), structure, term),
        Rhythmized => structure_func(Rhythmizer::new(), structure, term),
        FullStratified => structure_func(FullStratifier::new(), structure, term),
        Bare => structure_func(Plain::new(), structure, term),
    };
    println!("...done in {:?}", now.elapsed());
    tree
}

pub fn draw_tree(tree: &SoundTree, args: &Args) {
    println!("Drawing...");
    let now = Instant::now();
    draw::draw(&tree, args.division, format!("output/images/{:?}{}-viz.png", args.value, if args.reduce { "-reduced" } else { "" }));
    println!("One image: {:?}", now.elapsed());
    draw::draw(&tree, args.division, "output/visualization.png");
    if args.animate {
        let frames_path = "output/images/frames";
        fs::remove_dir_all(frames_path).unwrap();
        fs::create_dir(frames_path).unwrap();
        let time = args.time.unwrap_or(tree.size() as f64);
        // let frames = (30.0 * time).floor() as usize;
        // println!("Drawing {frames} frames...");
        draw::draw_anim(&tree, args.division, time, 30);
        // println!("All frames: {:?}", now.elapsed());
    }
}

pub fn make_output(sound: Box<impl AudioUnit + 'static>, args: &Args) -> Box<dyn AudioUnit> {
    match args.filters {
        FilterOptions::None => sound,
        FilterOptions::ClipLowpass => Box::new(unit::<U0, U2>(sound) >>
            stacki::<U2, _, _>(|_|
                shape(Adaptive::new(0.1, Tanh(0.5))) >> 
                lowpass_hz(2500.0, 1.0) >>
                mul(0.1)
                // >> mul(10.0)
            )
        ),
        FilterOptions::Quiet => Box::new(unit::<U0, U2>(sound) >> (mul(0.05) | mul(0.05))),
    }
}

pub fn main_to_file(args: &Args) {
    assert!(!args.live);
    let tree = make_tree(args.structure, args.content, &args.term());

    let time = args.time.unwrap_or(tree.size() as f64);
    draw_tree(&tree, args);
    if args.draw_only {
        return;
    }
    let mut seq = Sequencer::new(false, 2);
    let backend = Box::new(seq.backend());
    // let mut output: Box<dyn AudioUnit> = match args.filters {
    //     FilterOptions::None => backend,
    //     FilterOptions::ClipLowpass => Box::new(unit::<U0, U2>(backend) >>
    //         stacki::<U2, _, _>(|_|
    //             shape(Adaptive::new(0.1, Tanh(0.5))) >> 
    //             lowpass_hz(2500.0, 1.0) >>
    //             mul(0.1)
    //             // >> mul(10.0)
    //         )
    //     ),
    //     FilterOptions::Quiet => Box::new(unit::<U0, U2>(backend) >> (mul(0.05) | mul(0.05))),
    // };
    println!("Sequencing over {time} seconds...");
    let now = Instant::now();
    tree.generate_with(&mut ConfigSequencer::new(seq, args.live), 0.0, time, args.division, 0.0);
    println!("...done in {:?}", now.elapsed());
    let mut output = make_output(backend, &args);
    save(&mut *output, time);
    println!("Done.");
}

pub fn main_live(args: &mut Args) {
    let mut seq = Sequencer::new(false, 2);
    let backend = Box::new(seq.backend());
    let output = make_output(backend, &args);
    run_live(output);
    let mut cfg_seq = ConfigSequencer::new(seq, true);
    // let mut repl_state: TreeMaker = args.into();
    let mut term = args.term();
    loop {
        println!("{args:?}");
        let tree = make_tree(args.structure, args.content, &term);
        let time = args.time.unwrap_or(tree.size() as f64);
        
        println!("Sequencing live...");
        tree.generate_with(&mut cfg_seq, 0.0, time, args.division, 0.0);
        println!("Done");
        draw_tree(&tree, &args);
        // if args.draw_only {
        //     return;
        // }
        // run_live(Box::new(sine_hz(300.0) * 0.1));
        let cmd = input::<String>().msg("(press enter to exit)...\n").get();
        if cmd.is_empty() {
            break
        }
        let statement = statement(vec![], &cmd).map(|(_, r)| r).unwrap_or(Statement::Command("uh oh".to_owned()));
        
        match statement {
            Statement::Let(_, iterm) => { term = iterm; },
            Statement::Assume(_) => todo!(),
            Statement::Eval(_) => todo!(),
            Statement::PutStrLn(_) => todo!(),
            Statement::Out(_) => todo!(),
            Statement::Command(cmd) => {
                let terms = ["soundproof.exe"].into_iter().chain(cmd.split_whitespace());
                // let (term, time) = cmd.split_once(' ').unwrap_or(("omega", "20"));
                // println!("{cmd}");
                // let res = args.try_update_from(["soundproof.exe", "--value", term, "-t", time]);
                let res = args.try_update_from(terms);
                println!("parsed as: {res:?} {args:?}");
            }
        }
    }
    println!("Closing connection");
}

pub fn main() {
    // test_iterm_replicate();
    let mut args = Args::parse();
    if args.live {
        main_live(&mut args);
    }
    else {
        main_to_file(&args);
    }
}
