use clap::*;
use fundsp::prelude32::*;

use std::fs;
use std::time::Instant;

use lambdapi::ast::*;
use lambdapi::*;
use music::*;
use sound_generators::*;
use soundproof::select::*;
use soundproof::*;
use translate::*;
use types::*;
use performance::animate::*;

use crate::lambdapi::step::Stepper;
// use crate::lambdapi::eval2::*;
// use crate::lambdapi::term::std_env;

/// The "proof" side. Implementation of a simple dependently-typed lambda calculus,
/// translated from Löh, McBride, and Swierstra's [LambdaPi](https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf)
/// and in particular [Ilya Klyuchnikov's implementation](https://github.com/ilya-klyuchnikov/lambdapi/).
/// See the AST submodule page for most of the operative pieces.
pub mod lambdapi;
/// The "sound" side. Synths and utilities for generating audio.
pub mod music;
/// The live performance
pub mod performance;
/// Translation from LambdaPi to music.
pub mod soundproof;

pub mod draw;
pub mod parse;

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
            _ => ireduce(self.term()).expect("Named term can't fail to reduce"),
        }
    }
}

/// Command-line options to determine which set of melodies to use when generating melodies for a term.
#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum AudioSelectorOptions {
    /// Canonical selector: melody is determined by node; instrument, rhythm, and effect are determined by parent node.
    FullStratified,
    /// Like FullStratified, but uses Arcs to update it on the fly
    AsyncStratified,
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
    /// Melody determined by node, rhythm determined by parent node, all instruments are sines.
    SineRhythm,
    /// Just plays sine tones, no melody.
    Bare,
    ToneMake,
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

/// Which process to run
#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum RunMode {
    /// Generates a file for a single proof term
    Single,
    /// Generates a file for a proof term's evolution as it reduces
    Steps,
}

// impl RunMode {
//     fn live(&self) -> bool {
//         match self {
//             RunMode::Term => false,
//             RunMode::Steps => false,
//             RunMode::LiveSteps => true,
//             #[cfg(feature = "bevy")]
//             RunMode::Live => true,
//         }
//     }
// }

/// A system which converts dependently-typed lambda calculus into music, with a focus on Girard's Paradox.
/// Generates audio, a spectrograph, an image representation of the "sound tree" structure, and
/// animation frames matching the visualization with the sound.
#[derive(Parser, Debug, Clone)]
#[command(version, about, long_about=None)]
pub struct SoundproofArgs {
    /// Whether to run the single-term or step-based translation.
    #[arg(short, long, default_value = "single")]
    mode: RunMode,
    /// Whether to render to file or run it live. Single-term live runs are currently unavailable on this branch.
    #[arg(short, long, action)]
    live: bool,
    /// Predefined terms of the dependently typed lambda calculus.
    #[arg(short, long, default_value = "sigma")]
    value: NamedTerm,
    /// When set, normalize the term as far as possible before being presented.
    #[arg(short, long, action)]
    reduce: bool,
    /// In seconds. If unset, scales with size of tree. In step mode, determines time of one frame.
    #[arg(short, long)]
    time: Option<f64>,
    /// Determines how time is broken down between sequential segments.
    #[arg(short, long, default_value = "weight")]
    division: DivisionMethod,
    /// Determines which audio selector to use, determining melodies, rhythm, timbre, and so on.
    #[arg(short, long, default_value = "full-stratified")]
    content: AudioSelectorOptions,
    /// How to assign sound-tree structure to a term.
    #[arg(short, long, default_value = "type")]
    structure: Structure,
    /// Low end of frequency range in step mode. Does nothing unless mode=steps
    #[arg(long, default_value="60")]
    freq_min: f32,
    /// High end of frequency range in step mode. Does nothing unless mode=steps
    #[arg(long, default_value="2500")]
    freq_max: f32,
    /// Maximum number of steps before quitting in step mode. Does nothing unless mode=steps
    #[arg(short('S'), long)]
    step_count: Option<usize>,
    /// Additional filters added after audio generation.
    #[arg(short, long, default_value = "clip-lowpass")]
    filters: FilterOptions,
    /// A file from which to load multiple configurations for stepping.
    #[arg(long, requires="mode")]
    step_file: Option<String>,
    /// Whether to take MIDI input for steps. Only works live.
    #[arg(long, action, requires="file", requires="live")]
    midi: bool,
    // /// When set, only generate visualization (potentially including animation frames), not music.
    // #[arg(short('D'), long, action)]
    // draw_only: bool,
    // /// When set, generate animation frames.
    // #[arg(short, long, action)]
    // animate: bool,
    /// Name of the output file
    #[arg(short, long, default_value = "output.wav")]
    output: String,
}

impl SoundproofArgs {
    pub fn term(&self) -> ITerm {
        if self.reduce {
            self.value.term_reduced()
        } else {
            self.value.term()
        }
    }
}

pub fn async_tree(content: AsyncStratifier, term: &ITerm, ctx: Context) -> SoundTree {
    // type_translate(term, content)
    term.infer_translate(&ctx, content, false).unwrap().2
}

pub fn make_tree(structure: Structure, content: AudioSelectorOptions, term: &ITerm) -> SoundTree {
    validate("term", term, None);
    println!("Translating...");
    let now = Instant::now();
    // use MelodySelector::*;
    fn structure_func(selector: impl Selector, structure: Structure, term: &ITerm) -> SoundTree {
        match structure {
            Structure::Term => term_translate(term, selector),
            Structure::Type => type_translate(term, selector).unwrap(),
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
        SineRhythm => structure_func(SineRhythmizer::new(), structure, term),
        FullStratified => structure_func(FullStratifier::new(), structure, term),
        AsyncStratified => structure_func(AsyncStratifier::new(), structure, term),
        Bare => structure_func(Plain::new(), structure, term),
        ToneMake => structure_func(ToneMaker::new(0.0, 10.0), structure, term),
    };
    println!("...done in {:?}", now.elapsed());
    tree
}

pub fn make_output(
    sound: Box<impl AudioUnit + 'static>,
    filters: FilterOptions,
) -> Box<dyn AudioUnit> {
    match filters {
        FilterOptions::None => sound,
        FilterOptions::ClipLowpass => Box::new(
            unit::<U0, U2>(sound)
                >> stacki::<U2, _, _>(
                    |_| {
                        shape(Adaptive::new(0.1, Tanh(0.25))) >> lowpass_hz(2500.0, 1.0) >> mul(0.5)
                    }, // >> mul(10.0)
                ),
        ),
        FilterOptions::Quiet => Box::new(unit::<U0, U2>(sound) >> (mul(0.05) | mul(0.05))),
    }
}

pub fn main_to_file(args: &SoundproofArgs) {
    let tree = make_tree(args.structure, args.content, &args.term());
    let time = args.time.unwrap_or(tree.size() as f64);

    println!("Sequencing over {time} seconds...");
    let seq = Sequencer::new(0, 2, ReplayMode::None);
    let mut cfg_seq = ConfigSequencer::new(seq, false);
    // let backend = Box::new(seq.backend());
    let now = Instant::now();
    tree.generate_with(&mut cfg_seq, time, args.division);
    println!("...done in {:?}", now.elapsed());
    let mut output = make_output(Box::new(cfg_seq.seq), args.filters);
    save(&mut *output, time);
    println!("Done.")
}

#[allow(unused)]
fn run_steps(term: ITerm, limit: usize) {
    let ctx = Context::new(term::std_env());
    let mut prev = 0;
    for (ii, tm) in step::Stepper::step_over(term, ctx.clone()).enumerate() {
        // println!();
        if ii > limit {
            break;
        }
        if let ITerm::Ann(ref new, ref ty) = tm {
            println!(
                "looped {ty}! after {ii} ({}) steps as: {:?} {}\n",
                ii - prev,
                new.tag(),
                format!("{tm}").len()
            );
            prev = ii;
        }
    }
}

pub fn main_steps_live(args: SoundproofArgs) {
    if args.midi {
        animate_term_midi(args);
    }
    else {
        animate_term_steps(args);
    }
}

pub fn main_steps(mut args: SoundproofArgs) {
    use crate::term::std_env;
    let all_start = Instant::now();
    println!("Starting tone generation");
    let base_dur = args.time.unwrap_or(5.0);
    let mut tones = ToneMaker::new(0.0, base_dur);
    let selector = Silence::new();

    let seq = Sequencer::new(0, 2, ReplayMode::None);
    let mut cfg_seq = ConfigSequencer::new(seq, args.live);
    let mut base_size = SetOnce::new();
    let sequence = match &args.step_file {
        Some(path) => {
            let contents = fs::read_to_string(path).expect("Could not open config file");
            contents.split("\n").map(|s| s.to_owned()).collect()
        },
        None => vec![],
    };
    let max_steps = args.step_count.unwrap_or(100);
    for (ii, term) in args.term().step_over(Context::new(std_env())).enumerate() {
        if ii < sequence.len() {
            println!("Loading from file: {}", sequence[ii]);
            args = SoundproofArgs::parse_from(sequence[ii].split(' '))
        }
        println!("Translating for number {ii}...");
        let step_start = Instant::now();
        let dur = base_dur;
        tones.duration = dur;

        let tree = type_translate(&term, selector).unwrap();
        base_size.get(tree.size());
        println!("\t{:?}, output size {}", step_start.elapsed(), tree.size());

        println!("Sequencing over frequency range {}-{} for duration {dur}...", args.freq_min, args.freq_max);
        let seq_start = Instant::now();
        const NUM_BUCKETS: usize = 512;
        
        let buckets: Buckets<NUM_BUCKETS> =
            Buckets::from_tree(&tree, args.freq_min, args.freq_max, args.division);
        buckets
            .reverse()
            .sequence(&mut cfg_seq, tones.start_time, tones.duration, 0.0);

        tones.increment();
        println!(
            "\t{:?}, adding up to {:?}",
            seq_start.elapsed(),
            step_start.elapsed()
        );
        println!("\t{:?} with stepping", step_start.elapsed());
        if ii >= max_steps {
            println!("Hit {max_steps} steps");
            break;
        }
    }
    println!("Translated in {:?}", all_start.elapsed());
    let mut output = make_output(Box::new(cfg_seq.seq), args.filters);
    save(&mut *output, tones.start_time);
    println!("Done. Total time: {:?}", all_start.elapsed());
}

pub fn main() {
    let args = SoundproofArgs::parse();
    // println!("{:?}", args);
    // println!("Running in mode: {:?}", args.mode);
    match (args.mode, args.live) {
        // #[cfg(feature = "bevy")]
        // (RunMode::Single, true) => performance::main_live(),
        (RunMode::Single, true) => println!("Cannot run single-term live mode on this branch due to FunDSP incompatibilities. Switch to branch 'bevy'."),
        (RunMode::Single, false) => main_to_file(&args),
        (RunMode::Steps, true) => main_steps_live(args),
        (RunMode::Steps, false) => main_steps(args),
    }
}
