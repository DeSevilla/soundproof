use clap::*;
use fundsp::hacker32::*;
// use read_input::prelude::input;
// use read_input::InputBuild;
use bevy::prelude::*;

use std::{f32::consts::PI, sync::atomic::Ordering};

#[cfg(not(target_arch = "wasm32"))]
use bevy::pbr::wireframe::{WireframeConfig, WireframePlugin};
use bevy::{
    color::palettes::basic::SILVER,
    // prelude::*,
    render::{
        render_asset::RenderAssetUsages,
        render_resource::{Extent3d, TextureDimension, TextureFormat},
    },
};

use std::sync::{Arc, Mutex};
// use std::thread;
use std::fs;
use std::time::{Duration, Instant};
// use tokio;

use lambdapi::ast::*;
use lambdapi::*;
use music::*;
// use music::notes::*;
use soundproof::select::*;
use soundproof::*;
use translate::*;
use types::*;

// use crate::music::notes::A;
// use parse::{statement, Statement};

use crate::lambdapi::term::std_env;

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
            _ => ireduce(self.term()).unwrap(), // can't fail because it's a named term
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
    #[arg(short, long, default_value = "girard")]
    value: NamedTerm,
    /// When set, normalize the term as far as possible before being presented.
    #[arg(short, long, action)]
    reduce: bool,
    /// In seconds. If unset, scales with size of tree.
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
    /// Additional filters added after audio generation.
    #[arg(short, long, default_value = "clip-lowpass")]
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
    #[arg(short, long, default_value = "output.wav")]
    output: String,
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

pub fn async_tree(content: AsyncStratifier, term: &ITerm, ctx: Context) -> SoundTree {
    // type_translate(term, content)
    itype_translate(ctx, term, content).unwrap().1
}

pub fn make_tree(structure: Structure, content: AudioSelectorOptions, term: &ITerm) -> SoundTree {
    validate("term", term, None);
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
        AsyncStratified => structure_func(AsyncStratifier::new(), structure, term),
        Bare => structure_func(Plain::new(), structure, term),
    };
    println!("...done in {:?}", now.elapsed());
    tree
}

pub fn draw_tree(tree: &SoundTree, args: &Args) {
    println!("Drawing...");
    let now = Instant::now();
    draw::draw(
        tree,
        args.division,
        format!(
            "output/images/{:?}{}-viz.png",
            args.value,
            if args.reduce { "-reduced" } else { "" }
        ),
    );
    println!("One image: {:?}", now.elapsed());
    draw::draw(tree, args.division, "output/visualization.png");
    if args.animate {
        let frames_path = "output/images/frames";
        fs::remove_dir_all(frames_path).unwrap();
        fs::create_dir(frames_path).unwrap();
        let time = args.time.unwrap_or(tree.size() as f64);
        // let frames = (30.0 * time).floor() as usize;
        // println!("Drawing {frames} frames...");
        draw::draw_anim(tree, args.division, time, 30);
        // println!("All frames: {:?}", now.elapsed());
    }
}

pub fn make_output(sound: Box<impl AudioUnit + 'static>, args: &Args) -> Box<dyn AudioUnit> {
    match args.filters {
        FilterOptions::None => sound,
        FilterOptions::ClipLowpass => Box::new(
            unit::<U0, U2>(sound)
                >> stacki::<U2, _, _>(
                    |_| shape(Adaptive::new(0.1, Tanh(0.5))) >> lowpass_hz(2500.0, 1.0) >> mul(0.1), // >> mul(10.0)
                ),
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
    tree.generate_with(
        &mut ConfigSequencer::new(seq, args.live),
        0.0,
        time,
        args.division,
        0.0,
    );
    println!("...done in {:?}", now.elapsed());
    let mut output = make_output(backend, args);
    save(&mut *output, time);
    println!("Done.");
}

pub fn sequence_times_live(seq: Arc<Mutex<ConfigSequencer>>, times: Vec<(f64, Arc<dyn SoundGenerator + Send + Sync>, f64, f32)>) {
    let mut last_time = 0.0;
    for (start_time, sound, duration, lean) in times {
        let time_gap = start_time - last_time;
        if time_gap > 0.01 {
            // println!("Sleeping for {time_gap} seconds {:?}", Instant::now());
            let gap = Duration::from_secs_f64(time_gap);
            std::thread::sleep(gap);
            last_time = start_time; 
        }
        // else {
        //     println!("nowhere to go");
        // }
        let mut seq = seq.lock().unwrap();
        sound.sequence(&mut seq, start_time, duration, lean);
    }
}

// pub fn main_live(args: &mut Args) {
//     let mut seq = Sequencer::new(false, 2);
//     let backend = Box::new(seq.backend());
//     let output = make_output(backend, args);
//     let seq = Arc::new(Mutex::new(ConfigSequencer::new(seq, true)));
//     run_live(output);
//     // let mut cfg_seq = ConfigSequencer::new(seq, true);
//     // let mut repl_state: TreeMaker = args.into();
//     let mut term = args.term();
//     let mut new_term = true;
//     let selector = AsyncStratifier::new();
//     let mut ctx = Context::new(std_env());
//     loop {
//         if new_term {
//             let seq = seq.clone();
//             println!("{args:?}");
//             let tree = async_tree(selector.clone(), &term, ctx.clone());
//             args.time = Some(60.0.min(tree.size() as f64));
//             // let tree = make_tree(args.structure, selector, &term);
//             let time = args.time.unwrap_or(tree.size() as f64);
//             println!("Sequencing live...");
//             let mut sound_times = tree.sound_times(0.0, time, args.division, 0.0);
//             sound_times.sort_by(|a, b| a.0.total_cmp(&b.0));
            
//             thread::spawn(move || sequence_times_live(seq, sound_times));
//             // tree.generate_with(&mut cfg_seq, 0.0, time, args.division, 0.0);
//             println!("Done");
//             draw_tree(&tree, args);
 
//         }
//        // if args.draw_only {
//         //     return;
//         // }
//         // run_live(Box::new(sine_hz(300.0) * 0.1));
//         let cmd = input::<String>().msg("(press enter to exit)...\n").get();
//         if cmd.is_empty() {
//             break;
//         }
//         let statement = match statement(vec![], &cmd) {
//             Ok((rest, val)) => if rest.is_empty() { val } else { Statement::Command("incomplete parse: ".to_owned() + rest) },
//             Err(_) => Statement::Command("parse failure".to_owned()),
//         };
//         new_term = false;
//         match statement {
//             Statement::Let(name, iterm) => {
//                 let res = ctx.add_free_iterm(name, iterm.clone());
//                 if res.is_err() {
//                     println!("Got error on term {iterm}: {res:?}");
//                     continue;
//                 }
//                 term = iterm;
//                 new_term = true;
//             }
//             Statement::Assume(assumptions) => {
//                 for (name, prop) in assumptions {
//                     ctx.assume_cterm(name, prop);
//                 }
//             },
//             // Statement::Eval(_) => todo!(),
//             // Statement::PutStrLn(_) => todo!(),
//             Statement::Set(tag, notes) => {
//                 let point = selector.get(&tag);
//                 *point.notes.lock().unwrap() = notes;
//                 println!("Set notes for {tag:?} to {notes:?}");
//             }, //TODO this isn't it
//             Statement::Command(cmd) => {
//                 let terms = ["soundproof.exe"].into_iter().chain(cmd.split_whitespace());
//                 // let (term, time) = cmd.split_once(' ').unwrap_or(("omega", "20"));
//                 // println!("{cmd}");
//                 // let res = args.try_update_from(["soundproof.exe", "--value", term, "-t", time]);
//                 let res = args.try_update_from(terms);
//                 println!("parsed as: {res:?} {args:?}");
//             },
//             _ => todo!()
//         }
//     }
//     println!("Closing connection");
// }

// pub fn main_old() {
//     // test_iterm_replicate();
//     let mut args= Args::parse();
//     if args.live {
//         main_live(&mut args);
//     } else {
//         main_to_file(&args);
//     }
// }


const MAX_TIME: f64 = 60.0;
const SEG_LENGTH: f32 = 1.0;

#[derive(Clone, Copy)]
pub struct HelloPlugin;

impl Plugin for HelloPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(TreeTimer(bevy::prelude::Timer::from_seconds(MAX_TIME as f32 + 2.0, TimerMode::Repeating)));
        app.add_systems(Startup, (setup, add_tree));
        app.add_systems(Update, rotate);
        app.add_systems(Update, time_visibility);
        // app.add_systems(Update, (rotate, time_visibility));
    }
}

#[derive(Bundle, Clone)]
pub struct TreeSegment {
    timings: Timings,
    meta: TreeMetadata,
    mesh: Mesh3d,
    material: MeshMaterial3d<StandardMaterial>,
    transform: Transform,
    visiblity: Visibility,
    shape: Shape,
}

fn make_segment(
    tree: &SoundTree,
    duration: f64,
    elapsed: f64,
    lean: f32,
    angle: f32,
    images: &mut ResMut<Assets<Image>>,
    materials: &mut ResMut<Assets<StandardMaterial>>, 
    meshes: &mut ResMut<Assets<Mesh>>
) -> TreeSegment {
    assert!(match tree { SoundTree::Sound(_, _) => true, _ => false });
    // println!("duration: {duration}");
    let timings = Timings {
        duration,
        start: elapsed,
        lean,
    };
    let meta = tree.metadata().clone();
    // let scaler = width;
    let scaler = (2.0_f32.powf(timings.duration as f32) / (4. * MAX_TIME as f32)).min(0.25);
    // let scaler = (timings.duration as f32 / 10.0).min(1.0);
    // let scaler = timings.duration as f32 / 2.0;
    let shape = meshes.add(Cylinder::new(scaler, SEG_LENGTH));
    // let shape = meshes.add(Sphere::new(scaler));
    // let shape = meshes.add(Sphere::new(timings.duration as f32 / 10.0));
    // let shape = meshes.add(Extrusion { base_shape: Rectangle::from_length(scaler), half_depth: 0.2});
    // let shape = meshes.add(Capsule3d { radius: scaler, ..default() });
    // let transform = Transform::from_xyz((timings.start - timings.duration * 0.5) as f32 % 2.0, 1.0, timings.lean)
    let mut transform = Transform::from_xyz(0.0, SEG_LENGTH, 0.0);
    transform.rotate_around(Vec3::new(0.0, SEG_LENGTH * 0.5, 0.0), Quat::from_rotation_z(angle));
    transform.rotate(Quat::from_rotation_y(PI / 10.));
        // .with_rotation(Quat::from_rotation_z(angle));
    // let transform = if has_parent {
    //     Transform::from_xyz(0.0, 1.0, 0.0)
    //         .with_rotation(Quat::from_rotation_x(PI * (prior + width / 2.) / 10.));
    // }
    // else {
    //     Transform::from_xyz(0.0, 1.0, 0.0)
    // // let transform = Transform::from_xyz(1.0, 2.0, timings.lean * 2.0)
    //         .with_rotation(Quat::from_rotation_x(PI / 20.));
    //     // .with_rotation(Quat::from_rotation_x(PI / 10.0));
    // }
    let (r, g, b, _) = tree.metadata().base_color.as_rgba8();
    let debug_material = materials.add(StandardMaterial {
        base_color_texture: Some(images.add(uv_debug_texture(r, g, b))),
        ..default()
    });
    TreeSegment {
        timings, 
        meta,
        mesh: Mesh3d(shape),
        material: MeshMaterial3d(debug_material),
        transform,
        visiblity: Visibility::default(),
        shape: Shape,
    }
}

fn add_tree_rec(
    tree: &SoundTree,
    parent: Option<Entity>,
    duration: f64,
    elapsed: f64,
    lean: f32,
    angle: f32,
    commands: &mut Commands,
    meshes: &mut ResMut<Assets<Mesh>>,
    images: &mut ResMut<Assets<Image>>,
    materials: &mut ResMut<Assets<StandardMaterial>>
) {
    let size = tree.size();
    match tree {
        SoundTree::Simul(children, meta) => {
            let val = SIGN.fetch_add(1, Ordering::Relaxed);
            let dir = if val % 2 == 0 { 1.0 } else { -1.0 };
            // let scale = vec.len();
            let scale = (size - 1) as f32;
            let base_lean = dir * scale / 2.0;
            // for (ii, elem) in vec.iter().enumerate() {
            // let mut output = Vec::new();
            // let mut parent = parent;
            // commands.spawn(bundle)
            // if children.len() != 2 {
            //     // println!();
            //     panic!("uh oh: {}, {meta:?}", children.len());
            // }
            let Some((head, tail)) = children.split_first() else { return; };
            
            let head_segment = make_segment(head, duration, elapsed, lean, angle, images, materials, meshes);
            
            let head_ref = commands.spawn(head_segment); //.with_children(|parent| {
            let head_obj = head_ref.id();
            if let Some(p) = parent {
                commands.entity(p).add_child(head_obj);
            }
            // parent.unwrap().add_child(head_ref);
            let parent = Some(head_obj);
            // let mut prior_ratio = 0.0;
            // assert!(tail.len() == 1);
            for child in tail {
                // match subtree {
                    // SoundTree::Simul(_, _) => panic!("Got a simul in a simul"),
                    // SoundTree::Seq(children, _) => {
                    //     for child in children {
                // let ratio = DivisionMethod::Weight.child_scale(&child) / DivisionMethod::Weight.parent_scale(&tree);
                let local_lean = (base_lean - dir * child.size() as f32) * 0.8 / scale; // can't divide by 0 bc if scale is 0 vec is empty
                    //         add_tree_rec(child, parent, duration * ratio, elapsed, lean + local_lean, commands, meshes, images, materials);
                    //         // let child_segment = make_segment(child, duration, elapsed, lean + local_lean, images, materials, meshes);
                    //         // let child_ref = parent.spawn(child_segment);
                    //         time_elapsed += new_time;
                    //     }
                    // },
                add_tree_rec(child, parent, duration, elapsed, lean + local_lean, 0.0, commands, meshes, images, materials);
                // }
            }
            // });
            // parent = Some(add_tree_rec(head.clone(), parent, duration, elapsed, lean, commands, meshes, images, materials));
        },
        SoundTree::Seq(children, _) => {
            let mut ratio_elapsed = 0.0;
            for child in children {
                let ratio = DivisionMethod::Weight.child_scale(&child) / DivisionMethod::Weight.parent_scale(&tree);
                // let ratio = match scaling {
                //     Scaling::Linear => 1.0 / vec.len() as f64,
                //     Scaling::Weight => child.subtree_weight(Scaling::exponent()) / self.weight(Scaling::exponent()),
                //     // we do want scaling exponent to be an argument but for now...
                //     // Scaling::SizeAligned => round_by(child.size_factor() / self.size_adjusted(), segment),
                //     Scaling::Size => child.size() as f64 / self.size() as f64,
                // };
                let new_time = duration * ratio;
                let time_elapsed = duration * ratio_elapsed;
                let angle = PI * 0.5 * (ratio_elapsed + ratio / 2. - 0.5) as f32;
                add_tree_rec(child, parent, new_time, elapsed + time_elapsed, lean, angle, commands, meshes, images, materials);
                // output.append(&mut child.sound_times(sound_time + time_elapsed, new_time, scaling, lean));
                ratio_elapsed += ratio;
            }
        },
        SoundTree::Sound(_, _) => {
            // let (r, g, b, _) = meta.base_color.as_rgba8();
            // let debug_material = materials.add(StandardMaterial {
            //     base_color_texture: Some(images.add(uv_debug_texture(r, g, b))),
            //     ..default()
            // });
            let segment = make_segment(tree, duration, elapsed, lean, angle, images, materials, meshes);
            // if parent.is_some() {
            //     segment.transform = Transform::from_xyz(0.0, 1.0, 0.0)
            //         .with_rotation(Quat::from_rotation_x(PI * (prior + width / 2.) / 10.));
            // }
            let ent_ref = commands.spawn(segment);
            let ent = ent_ref.id();
            if let Some(p) = parent {
                commands.entity(p).add_child(ent);
            }
        },
    }
}

// fn do_parents(layers: Vec<Vec<TreeSegment>>, layer: usize, seg: Segment) {
//     for segment in layers[layer] {
        
//     }
// }

fn add_tree(mut commands: Commands, mut meshes: ResMut<Assets<Mesh>>, mut images: ResMut<Assets<Image>>, mut materials: ResMut<Assets<StandardMaterial>>) {
    
    
    let strat = AsyncStratifier::new();
    let (_, tree) = itype_translate(Context::new(std_env()), &lem0(), strat).unwrap();
    // let tree = itype_translate(Context::new(std_env()), &tau(), AsyncStratifier).unwrap()
    add_tree_rec(&tree, None, MAX_TIME, 0.0, 0.0, 0.0, &mut commands, &mut meshes, &mut images, &mut materials);
    // let mut material_map = HashMap::new();
    // let mut depth_map = Vec::new();
    // // let mut children_map = HashMap::new();
    // for (_, timings, meta) in tree.sound_times(0.0, MAX_TIME, DivisionMethod::Weight, 0.0) {
    //     let shape = meshes.add(Extrusion { base_shape: Rectangle::from_length(timings.duration as f32 / 2.0), half_depth: 0.5});
    //     // let shape = meshes.add(Cylinder { radius: (timings.duration as f32 / 4.0).min(2.0), ..default() });
    //     let transform = Transform::from_xyz((timings.start + timings.duration / 2. - MAX_TIME * 0.5) as f32, meta.max_depth as f32 + 0.5, timings.lean * 2.0)
    //         .with_rotation(Quat::from_rotation_x(PI / 2.));
    //     let (r, g, b, _) = meta.base_color.as_rgba8();
    //     let debug_material = match material_map.get(&(r, g, b)) {
    //         None => {
    //             let new_mat = materials.add(StandardMaterial {
    //                 base_color_texture: Some(images.add(uv_debug_texture(r, g, b))),
    //                 ..default()
    //             });
    //             material_map.insert((r, g, b), new_mat.clone());
    //             new_mat
    //         },
    //         Some(mat) => mat.clone(),
    //     };
    //     let depth = meta.max_depth;
    //     // println!("{} {}", meta.name, meta.max_depth);
    //     let segment = TreeSegment {
    //         timings,
    //         meta,
    //         mesh: Mesh3d(shape),
    //         material: MeshMaterial3d(debug_material),
    //         transform,
    //         shape: Shape
    //     };
    //     match depth_map.get_mut(depth) {
    //         None => {
    //             let v = vec![segment];
    //             depth_map.insert(depth, v);
    //         },
    //         Some(v) => {
    //             // let old_time = v.last().unwrap().timings;
    //             // println!("extra time {}", segment.timings.start - old_time.start + old_time.duration);
    //             // TODO we assume this is sorted but don't do anything to ensure it really
    //             v.push(segment);
    //         }
    //     };
    // }
    // // let mut results = Vec::new();
    // for (depth, layer) in depth_map.iter().enumerate() {
    //     for segment in layer {
    //         let x = commands.spawn(segment.clone());
    //         // results.push(x);
    //         // commands.spawn(segment.clone())
    //         //     .with_children(|parent| {
    //         //         if depth >= layer.len() {
    //         //             return;
    //         //         }
    //         //         for child in depth_map[depth + 1].iter() {
    //         //             if child.timings.start < segment.timings.start || child.timings.start > segment.timings.start + segment.timings.duration {
    //         //                 continue;
    //         //             }
    //         //             parent.spawn(child.clone());
    //         //         }
    //         //     });
    //     }
    // }


    // commands.spawn((Tag::Pi, strat.for_pair(Tag::Pi, Tag::FreeVar), strat.imeta(&sets_of())));
    // commands.spawn((Tag::Application, strat.for_pair(Tag::Application, Tag::Lambda), strat.imeta(&tau())));
    // commands.spawn((Tag::Lambda, strat.for_pair(Tag::Application, Tag::Nat), strat.imeta(&sigma())));
    // commands.spawn((MelodyAsync::))
}

// const SHAPES_X_EXTENT: f32 = 14.0;
// const EXTRUSION_X_EXTENT: f32 = 16.0;
// const Z_EXTENT: f32 = 5.0;

#[derive(Component, Clone, Copy)]
struct Shape;

/// Creates a colorful test pattern
fn uv_debug_texture(r: u8, g: u8, b: u8) -> Image {
    const TEXTURE_SIZE: usize = 1;

    let mut palette: [u8; 4] = [
        r, g, b, 255
    ];
    // let mut palette: [u8; 32] = [
    //     255, 102, 159, 255, 255, 159, 102, 255, 236, 255, 102, 255, 121, 255, 102, 255, 102, 255,
    //     198, 255, 102, 198, 255, 255, 121, 102, 255, 255, 236, 102, 255, 255,
    // ];

    let mut texture_data = [0; TEXTURE_SIZE * TEXTURE_SIZE * 4];
    for y in 0..TEXTURE_SIZE {
        let offset = TEXTURE_SIZE * y * 4;
        texture_data[offset..(offset + TEXTURE_SIZE * 4)].copy_from_slice(&palette);
        palette.rotate_right(4);
    }

    Image::new_fill(
        Extent3d {
            width: TEXTURE_SIZE as u32,
            height: TEXTURE_SIZE as u32,
            depth_or_array_layers: 1,
        },
        TextureDimension::D2,
        &texture_data,
        TextureFormat::Rgba8UnormSrgb,
        RenderAssetUsages::RENDER_WORLD,
    )
}

fn setup(
    mut commands: Commands,
    // mut meshes: ResMut<Assets<Mesh>>,
    // mut images: ResMut<Assets<Image>>,
    // mut materials: ResMut<Assets<StandardMaterial>>,
) {
    commands.spawn((
        PointLight {
            shadows_enabled: true,
            intensity: 10_000_000.,
            range: 100.0,
            shadow_depth_bias: 0.2,
            ..default()
        },
        Transform::from_xyz(8.0, 16.0, 8.0),
    ));

    // ground plane
    // commands.spawn((
    //     Mesh3d(meshes.add(Plane3d::default().mesh().size(50.0, 50.0).subdivisions(10))),
    //     MeshMaterial3d(materials.add(Color::from(SILVER))),
    // ));

    commands.spawn((
        Camera3d::default(),
        Transform::from_xyz(0.0, 15., 20.0).looking_at(Vec3::new(0., 7., 0.), Vec3::Y),
    ));

    #[cfg(not(target_arch = "wasm32"))]
    commands.spawn((
        Text::new("this is my next step towards a new tree visualization!!"),
        Node {
            position_type: PositionType::Absolute,
            top: Val::Px(12.0),
            left: Val::Px(12.0),
            ..default()
        },
    ));
}

#[derive(Resource)]
struct TreeTimer(bevy::prelude::Timer);

fn rotate(mut query: Query<&mut Transform, With<Shape>>, time: Res<Time>) {
    for mut transform in &mut query {
        // transform.rotate_y(time.delta_secs() / 2.0);
        transform.rotate_around(Vec3::new(0.0, SEG_LENGTH / 2., 0.0), Quat::from_rotation_y(time.delta_secs() / 2.0));
    }
}

fn time_visibility(mut query: Query<(&mut Visibility, &Timings, &mut MeshMaterial3d<StandardMaterial>), With<Shape>>, time: Res<Time>, mut timer: ResMut<TreeTimer>) {
    const WINDOW: f64 = 1.0;
    timer.0.tick(time.delta());
    let moment = timer.0.elapsed_secs_f64();
    // println!("moment: {moment}");
    for (mut vis, timings, mut material) in &mut query {
        if timings.start - moment <= WINDOW / 2. && moment - (timings.start + timings.duration) <= WINDOW / 2. {
            if timings.start < moment && moment < timings.start + timings.duration {
                let x = &mut material.0;
            }
            *vis = Visibility::Visible;
        }
        else {
            *vis = Visibility::Hidden;
        }
    }
}



pub fn main() {
    // println!("Hello world!");
    App::new()
        .add_plugins((
            DefaultPlugins.set(ImagePlugin::default_nearest()),
            #[cfg(not(target_arch = "wasm32"))]
            WireframePlugin::default(),
        ))
        .add_plugins(HelloPlugin)
        .run();
}