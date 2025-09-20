use bevy_procedural_audio::{prelude::{DspGraph, DspManager, DspSource, SourceType}, DspAppExt, DspPlugin};
use clap::*;
// use cpal::InputStreamTimestamp;
use fundsp::{hacker32::*, realseq::SequencerBackend};
// use read_input::prelude::input;
// use read_input::InputBuild;
use bevy::{core_pipeline::bloom::Bloom, input::keyboard::{Key, KeyboardInput}, prelude::*};

use std::{f32::consts::PI, sync::atomic::Ordering};

// #[cfg(not(target_arch = "wasm32"))]
// // use bevy::pbr::wireframe::{WireframeConfig, WireframePlugin};
// use bevy::{
//     color::palettes::basic::SILVER,
//     // prelude::*,
//     render::{
//         // render_asset::RenderAssetUsages,
//         // render_resource::{Extent3d, TextureDimension, TextureFormat},
//     },
// };

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

// use crate::{lambdapi::term::std_env, music::notes::E};

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

pub fn make_output(sound: Box<impl AudioUnit + 'static>, filters: FilterOptions) -> Box<dyn AudioUnit> {
    match filters {
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
    let mut output = make_output(backend, args.filters);
    save(&mut *output, time);
    println!("Done.");
}

pub fn sequence_times_live(seq: Arc<Mutex<ConfigSequencer>>, times: Vec<(Arc<dyn SoundGenerator + Send + Sync>, Timings, TreeMetadata)>) {
    let mut last_time = 0.0;
    for (sound, timings, _) in times {
        let time_gap = timings.start - last_time;
        if time_gap > 0.01 {
            // println!("Sleeping for {time_gap} seconds {:?}", Instant::now());
            let gap = Duration::from_secs_f64(time_gap);
            std::thread::sleep(gap);
            last_time = timings.start;
        }
        // else {
        //     println!("nowhere to go");
        // }
        let mut seq = seq.lock().unwrap();
        sound.sequence(&mut seq, timings.start, timings.duration, timings.lean);
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

#[derive(Clone, Component)]
pub struct AudioInfo {
    sound: Arc<dyn SoundGenerator + Send + Sync>,
    done: bool,
}

impl AudioInfo {
    pub fn new(sound: Arc<dyn SoundGenerator + Send + Sync>) -> Self {
        Self {
            sound,
            done: false,
        }
    }
}


#[derive(Bundle, Clone)]
pub struct TreeSegment {
    timings: Timings,
    audio_info: AudioInfo,
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
    _images: &mut ResMut<Assets<Image>>,
    materials: &mut ResMut<Assets<StandardMaterial>>, 
    meshes: &mut ResMut<Assets<Mesh>>,
) -> TreeSegment {
    
    let SoundTree::Sound(sound, _) = tree else { panic!("unsound!") };
    // println!("duration: {duration}");
    let timings = Timings {
        duration,
        start: elapsed,
        lean,
    };
    let meta = tree.metadata().clone();
    // let scaler = width;
    let scaler = (2.0_f32.powf(timings.duration as f32) / (4. * MAX_TIME) + 0.02).min(0.25);
    // let scaler = (2.0 * timings.duration / MAX_TIME).min(1.0) as f32;
    // let scaler = (timings.duration as f32 / 10.0).min(1.0);
    // let scaler = timings.duration as f32 / 2.0;
    // let shape = meshes.add(Cylinder::new(scaler, SEG_LENGTH));
    let shape = meshes.add(Cone::new(scaler, SEG_LENGTH));
    // let shape = meshes.add(Sphere::new(scaler));
    // let shape = meshes.add(Extrusion { base_shape: Rectangle::from_length(scaler), half_depth: 0.2});
    // let shape = meshes.add(Capsule3d::new(scaler, SEG_LENGTH));
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
        base_color: Color::linear_rgba(r as f32 / 255., g as f32 / 255., b as f32 / 255., 1.),
        // base_color_texture: Some(images.add(uv_debug_texture(r, g, b))),
        ..default()
    });
    TreeSegment {
        timings, 
        audio_info: AudioInfo::new(sound.clone()),
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
    materials: &mut ResMut<Assets<StandardMaterial>>,
    index: Option<Index>,
) {
    let size = tree.size();
    match tree {
        SoundTree::Simul(children, _) => {
            let val = SIGN.fetch_add(1, Ordering::Relaxed);
            let dir = if val % 2 == 0 { 1.0 } else { -1.0 };
            let scale = (size - 1) as f32;
            let base_lean = dir * scale / 2.0;
            let Some((head, tail)) = children.split_first() else { return; };
            
            let mut head_segment = make_segment(head, duration, elapsed, lean, angle, images, materials, meshes);
            if let Some(idx) = &index {
                let sign = (-1_i32).pow(idx.0 as u32) as f32;
                // println!("sign: {sign} {} ", index.0);
                head_segment.transform = head_segment.transform.with_translation(Vec3::new(idx.0 as f32 * 2. * sign, 0., 0.))
            }
            let mut head_ref = commands.spawn(head_segment);
            if let Some(idx) = index {
                head_ref.insert(idx);
            }
            let head_obj = head_ref.id();
            if let Some(p) = parent {
                commands.entity(p).add_child(head_obj);
            }
            let parent = Some(head_obj);
            for child in tail {
                let local_lean = (base_lean - dir * child.size() as f32) * 0.8 / scale; // can't divide by 0 bc if scale is 0 tail is empty
                add_tree_rec(child, parent, duration, elapsed, lean + local_lean, 0.0, commands, meshes, images, materials, None);
            }
        },
        SoundTree::Seq(children, _) => {
            let mut ratio_elapsed = 0.0;
            for child in children {
                let ratio = DivisionMethod::Weight.child_scale(&child) / DivisionMethod::Weight.parent_scale(&tree);
                let new_time = duration * ratio;
                let time_elapsed = duration * ratio_elapsed;
                let angle = PI * 0.6 * (ratio_elapsed + ratio / 2. - 0.5) as f32;
                add_tree_rec(child, parent, new_time, elapsed + time_elapsed, lean, angle, commands, meshes, images, materials, None);
                ratio_elapsed += ratio;
            }
        },
        SoundTree::Sound(_, _) => {
            let segment = make_segment(tree, duration, elapsed, lean, angle, images, materials, meshes);
            let ent_ref = commands.spawn(segment);
            let ent = ent_ref.id();
            if let Some(p) = parent {
                commands.entity(p).add_child(ent);
            }
        },
    }
}

fn add_tree(
    tree: &SoundTree,
    commands: &mut Commands,
    meshes: &mut ResMut<Assets<Mesh>>,
    images: &mut ResMut<Assets<Image>>,
    materials: &mut ResMut<Assets<StandardMaterial>>,
    time: &Res<Time>,
    counter: &mut ResMut<TreeCounter>,
) {
    let current_time = time.elapsed_secs_f64();
    let index = counter.insert();
    add_tree_rec(&tree, None, MAX_TIME as f64, current_time + 0.1, 0.0, 0.0, 
        commands, meshes, images, materials, Some(index));
}

#[derive(Component, Clone, Copy)]
struct Shape;

/// Creates a colorful test pattern
// fn uv_debug_texture(r: u8, g: u8, b: u8) -> Image {
//     const TEXTURE_SIZE: usize = 1;

//     let mut palette: [u8; 4] = [
//         r, g, b, 255
//     ];
//     // let mut palette: [u8; 32] = [
//     //     255, 102, 159, 255, 255, 159, 102, 255, 236, 255, 102, 255, 121, 255, 102, 255, 102, 255,
//     //     198, 255, 102, 198, 255, 255, 121, 102, 255, 255, 236, 102, 255, 255,
//     // ];

//     let mut texture_data = [0; TEXTURE_SIZE * TEXTURE_SIZE * 4];
//     for y in 0..TEXTURE_SIZE {
//         let offset = TEXTURE_SIZE * y * 4;
//         texture_data[offset..(offset + TEXTURE_SIZE * 4)].copy_from_slice(&palette);
//         palette.rotate_right(4);
//     }

//     Image::new_fill(
//         Extent3d {
//             width: TEXTURE_SIZE as u32,
//             height: TEXTURE_SIZE as u32,
//             depth_or_array_layers: 1,
//         },
//         TextureDimension::D2,
//         &texture_data,
//         TextureFormat::Rgba8UnormSrgb,
//         RenderAssetUsages::RENDER_WORLD,
//     )
// }

fn cleanup(
    mut commands: Commands,
    query: Query<(&Timings, &Index, Entity), (With<Shape>, Without<ChildOf>)>,
    mut counter: ResMut<TreeCounter>,
    time: Res<Time>
) {
    let moment = time.elapsed_secs_f64();
    for (timings, idx, entity) in query {
        if moment > timings.start + timings.duration + WINDOW {
            counter.remove(idx);
            commands.entity(entity).despawn();
        }
    }
}

#[derive(Component)]
pub struct InputTextBuffer;

fn setup(
    mut commands: Commands,
    dsp_manager: Res<DspManager>,
    mut dsp_sources: ResMut<Assets<DspSource>>,
    seq_id: Res<SeqId>,
    // mut meshes: ResMut<Assets<Mesh>>,
    // mut images: ResMut<Assets<Image>>,
    // mut materials: ResMut<Assets<StandardMaterial>>,
) {
    commands.spawn((
        PointLight {
            shadows_enabled: true,
            intensity: 15_00_000.,
            range: 100.0,
            shadow_depth_bias: 0.2,
            ..default()
        },
        Transform::from_xyz(8.0, 16.0, 8.0),
    ));

    commands.spawn((
        Camera3d::default(),
        Camera {
            hdr: true, // 1. HDR is required for bloom
            clear_color: ClearColorConfig::Custom(Color::BLACK),
            ..default()
        },
        Bloom::NATURAL,
        Transform::from_xyz(0.0, 20., 30.0).looking_at(Vec3::new(0., 7., 0.), Vec3::Y),
    ));

    #[cfg(not(target_arch = "wasm32"))]
    commands.spawn((
        Text::new(""),
        Node {
            position_type: PositionType::Absolute,
            top: Val::Px(12.0),
            left: Val::Px(12.0),
            ..default()
        },
        InputTextBuffer,
    ));

    let seq = dsp_sources.add(dsp_manager.get_graph_by_id(&seq_id.0).unwrap());

    commands.spawn((AudioPlayer(seq), PlaybackSettings { paused: false, ..Default::default()}));

    commands.spawn((
        Center,
        Transform::from_xyz(0., 0., 0.)
    ));
}

#[derive(Component)]
pub struct Center;

#[derive(Event)]
pub struct TextCmd(String);

fn run_command(
    mut commands: Commands,
    mut text_events: EventReader<TextCmd>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    time: Res<Time>,
    mut counter: ResMut<TreeCounter>,
) {
    for text in text_events.read() {
        let cmd = match parse::statement(vec![], &text.0) {
            Ok((rest, stmts)) => {
                println!("Rest: {rest}");
                stmts
            },
            Err(e) => {
                println!("uhoh error {e}");
                continue;
            }
        };
        match cmd {
            parse::Statement::Let(_, iterm) => {
                println!("got let statement");
                let tree = match type_translate(&iterm, AsyncStratifier::new()) {
                    Ok(t) => t,
                    Err(e) => { println!("{e}"); continue }
                };
                add_tree(
                    &tree,
                    &mut commands, &mut meshes, &mut images, &mut materials, 
                    &time, &mut counter,
                );
            },
            parse::Statement::Assume(_items) => todo!(),
            parse::Statement::Eval(_iterm) => todo!(),
            parse::Statement::PutStrLn(_) => todo!(),
            parse::Statement::Out(_) => todo!(),
            parse::Statement::Set(_tag, _notes) => todo!(),
            parse::Statement::Command(_) => todo!(),
        }
    }
}

fn handle_typing(
    mut char_input_events: EventReader<KeyboardInput>,
    mut text_cmd_events: EventWriter<TextCmd>,
    query: Single<&mut Text, With<InputTextBuffer>>
) {
    let mut buf = query.into_inner();
    for event in char_input_events.read() {
        // Only check for characters when the key is pressed.
        if !event.state.is_pressed() {
            continue;
        }
        match (&event.logical_key, &event.text) {
            (Key::Enter, _) => {
                println!("Sending command; {buf:?}");
                text_cmd_events.write(TextCmd(buf.to_string()));
                buf.clear();
            },
            (Key::Backspace, _) => {
                buf.pop();
            },
            (_, Some(text)) => {
                buf.push_str(text);
            },
            _ => continue,
            // info!("{:?}: '{}'", event, character);
        }
    }
}


fn rotate_root(mut query: Query<(&mut Transform, &Timings), (With<Shape>, Without<ChildOf>)>, time: Res<Time>) {
    for (mut transform, timings) in &mut query {
        let factor = 5. + (1. / timings.duration as f32) % 4.;
        transform.rotate_y(time.delta_secs() / factor);
    }
}

fn rotate_child(mut query: Query<(&mut Transform, &Timings), (With<Shape>, With<ChildOf>)>, time: Res<Time>) {
    for (mut transform, timings) in &mut query {
        let factor = 5. + (1. / timings.duration as f32) % 4.;
        transform.rotate_around(Vec3::new(0.0, SEG_LENGTH / 2., 0.0), Quat::from_rotation_y(time.delta_secs() / factor));
    }
}

fn color_change(
    mut query: Query<(&Timings, &MeshMaterial3d<StandardMaterial>), With<Shape>>, 
    mut materials: ResMut<Assets<StandardMaterial>>,
    timer: Res<Time>
) {
    let moment = timer.elapsed_secs_f64();
    for (timings, material) in &mut query {
        if let Some(mat) = materials.get_mut(&material.0) {
            if timings.start + timings.duration + 0.01 < moment {
                // mat.base_color = Color::BLACK;
                mat.emissive = Color::BLACK.to_linear();
            }
            else if timings.start - 0.002 <= moment && moment <= timings.start + timings.duration + 0.002 {
                mat.emissive = mat.base_color.to_linear() * 5.0;
                // mat.metallic = 1.0;
                // mat.reflectance = 1.0;
                // mat.perceptual_roughness = 0.1;
            }
            else {
                mat.emissive = Color::BLACK.to_linear();
                // mat.reflectance = 0.5;
                // mat.metallic = 0.0;
                // mat.perceptual_roughness = 0.5;
            }
        }
    }
}

fn time_visibility(mut query: Query<(&mut Visibility, &Timings), With<Shape>>, timer: Res<Time>) {
    let moment = timer.elapsed_secs_f64();
    // println!("moment: {moment}");
    for (mut vis, timings) in &mut query {
        if timings.start - moment <= WINDOW / 2. && moment - (timings.start + timings.duration) <= WINDOW / 2. {
            *vis = Visibility::Visible;
        } // TODO actually despawn stuff, mb in another system
        // else if moment > timings.start + timings.duration as f64 {
        //     *vis = Visibility::Visible;
        // }
        else {
            *vis = Visibility::Hidden;
        }
    }
}

fn play_sound(query: Query<(&Timings, &mut AudioInfo)>, mut seq: ResMut<CfgSeq>, timer: Res<Time>) {
    let moment = timer.elapsed_secs_f64();
    for (timings, mut info) in query {
        if !info.done && timings.start - moment < 0.1 {
            info.sound.sequence(&mut seq.0, timings.start - moment, timings.duration, timings.lean);
            info.done = true;
        }
    }
}

const MAX_TIME: f32 = 45.0;
const WINDOW: f64 = MAX_TIME as f64 / 12.;
// const WINDOW: f64 = 2.0;
const SEG_LENGTH: f32 = 1.0;

#[derive(Clone, Copy)]
pub struct HelloPlugin;

#[derive(Clone)]
pub struct SharedBackend(Arc<Mutex<SequencerBackend>>);

impl SharedBackend {
    fn new(seq: SequencerBackend) -> Self {
        SharedBackend(Arc::new(Mutex::new(seq)))
    }

    fn lock(&self) -> std::sync::MutexGuard<SequencerBackend> {
        self.0.lock().unwrap()
    }
}

impl AudioUnit for SharedBackend {
    fn tick(&mut self, input: &[f32], output: &mut [f32]) {
        self.lock().tick(input, output);
    }

    fn process(&mut self, size: usize, input: &BufferRef, output: &mut BufferMut) {
        self.lock().process(size, input, output);
    }

    fn inputs(&self) -> usize {
        self.lock().inputs()
    }

    fn outputs(&self) -> usize {
        self.lock().outputs()
    }

    fn route(&mut self, input: &SignalFrame, frequency: f64) -> SignalFrame {
        self.lock().route(input, frequency)
    }

    fn get_id(&self) -> u64 {
        self.lock().get_id()
    }

    fn footprint(&self) -> usize {
        self.lock().footprint()
    }
}

impl Plugin for HelloPlugin {
    fn build(&self, app: &mut App) {
        let mut seq = Sequencer::new(false, 2);
        let backend = SharedBackend::new(seq.backend());
        let thing = move || unit::<U0, U2>(make_output(Box::new(backend.clone()), FilterOptions::ClipLowpass));
        let id = thing.id();
        app.insert_resource(SeqId(id)); 
        app.add_dsp_source(thing, SourceType::Dynamic);
        let cfg_seq = CfgSeq(ConfigSequencer::new(seq, true));
        app.insert_resource(cfg_seq);
        app.insert_resource(TreeCounter::new());
        app.add_event::<TextCmd>();
        app.add_systems(Startup, setup);
        app.add_systems(Update, (rotate_child, rotate_root));
        app.add_systems(Update, (time_visibility, cleanup));
        app.add_systems(Update, play_sound);
        app.add_systems(Update, color_change);
        app.add_systems(Update, (handle_typing, run_command));
    }
}

#[derive(Resource)]
pub struct TreeCounter(Vec<bool>);

#[derive(Component)]
pub struct Index(usize);

impl TreeCounter {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn insert(&mut self) -> Index {
        for (i, b) in self.0.iter_mut().enumerate() {
            if !*b {
                *b = true;
                // println!("inserting {i}");
                return Index(i)
            }
        }
        let res = self.0.len();
        self.0.push(true);
        // println!("inserting {res}");
        Index(res)
    }

    pub fn remove(&mut self, idx: &Index) {
        // this can leak in a sense but who cares
        // length is like 10 here max
        // can't panic as long as all Indexes are generated by the single TreeCounter
        // println!("deleting from {}", idx.0);
        self.0[idx.0] = false;
    }
}

#[derive(Resource)]
pub struct SeqId(uuid::Uuid);

#[derive(Resource)]
pub struct CfgSeq(ConfigSequencer);

pub fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(ImagePlugin::default_nearest()),
            DspPlugin::default(),
        ))
        .add_plugins(HelloPlugin)
        .run();
}