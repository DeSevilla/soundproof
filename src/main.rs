use bevy_procedural_audio::{prelude::{DspGraph, DspManager, DspSource, SourceType}, DspAppExt, DspPlugin};
use clap::*;
// use cpal::InputStreamTimestamp;
use fundsp::{hacker32::*, realseq::SequencerBackend};
// use read_input::prelude::input;
// use read_input::InputBuild;
use bevy::{core_pipeline::bloom::Bloom, input::keyboard::{Key, KeyboardInput}, prelude::*, window::WindowMode};
use rand::Rng;
// use bevy::render::camera::Viewport;

use std::{f32::consts::PI, sync::atomic::Ordering};

use std::sync::{Arc, Mutex};
use std::fs;
use std::time::{Duration, Instant};

use lambdapi::ast::*;
use lambdapi::*;
use music::*;
// use music::notes::*;
use soundproof::select::*;
use soundproof::*;
use translate::*;
use types::*;

use crate::lambdapi::term::{full_env, iann, quote0, std_env};

// use crate::lambdapi::term::std_env;

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
                    |_| shape(Adaptive::new(0.1, Tanh(0.5))) >> lowpass_hz(2500.0, 1.0) >> mul(0.4), // >> mul(10.0)
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

fn scale_width(duration: f64) -> f32 {
    // let scaler = width;
    // let scaler = (2.0_f32.powf(5. * timings.duration as f32 / MAX_TIME) / 4. + 0.02).min(0.25);
    // let scaler = (timings.duration as f32 / 10.0).min(1.0);
    // let scaler = timings.duration as f32 / 2.0;
    (2.0 * duration as f32 / MAX_TIME + 0.02).min(0.25)
}

fn scale_length(duration: f64) -> f32 {
    // SEG_LENGTH;
    (scale_width(duration) * 6. * SEG_LENGTH).max(SEG_LENGTH * 0.7).min(SEG_LENGTH * 1.4)
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
    
    let SoundTree::Sound(sound, meta) = tree else { panic!("unsound!") };
    // println!("duration: {duration}");
    let timings = Timings {
        duration,
        start: elapsed,
        lean,
    };
    // let meta =.clone();
    let scaler = scale_width(timings.duration);
    let seg_scaler = scale_length(timings.duration);
    let shape = meshes.add(Cone::new(scaler, seg_scaler));
    // let shape = meshes.add(Cylinder::new(scaler, SEG_LENGTH));
    // let shape = meshes.add(Sphere::new(scaler));
    // let shape = meshes.add(Extrusion { base_shape: Rectangle::from_length(scaler), half_depth: 0.2});
    // let shape = meshes.add(Capsule3d::new(scaler, SEG_LENGTH));
    // let transform = Transform::from_xyz((timings.start - timings.duration * 0.5) as f32 % 2.0, 1.0, timings.lean)
    let mut transform = Transform::from_xyz(0.0, seg_scaler, 0.0);
    transform.rotate_around(Vec3::new(0.0, seg_scaler * 0.5, 0.0), Quat::from_rotation_z(angle));
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
    let (r, g, b, _) = meta.base_color.as_rgba();
    let segment_material = materials.add(StandardMaterial {
        base_color: Color::srgb(r as f32, g as f32, b as f32),
        // base_color_texture: Some(images.add(uv_debug_texture(r, g, b))),
        ..default()
    });
    TreeSegment {
        timings, 
        audio_info: AudioInfo::new(sound.clone()),
        meta: meta.clone(),
        mesh: Mesh3d(shape),
        material: MeshMaterial3d(segment_material),
        transform,
        visiblity: DEFAULT_VIS,
        shape: Shape,
    }
}

fn spawn_segment<'a>(mut segment: TreeSegment, commands: &mut Commands, font: &Handle<Font>, parent: Option<Entity>, index: Index) -> Entity {
    if parent.is_none() {
        segment.transform = segment.transform.with_translation(Vec3::new(index.x_pos(), 0., 0.));
    }
    let (r, g, b, _) = segment.meta.base_color.as_rgba();
    commands.spawn((
        Text::new(&segment.meta.name),
        TextColor(Color::srgb(r as f32, g as f32, b as f32)),
        TextFont::from_font(font.clone()).with_font_size(SIDE_FONT_SIZE),
        segment.timings,
        Node {
            // position_type: PositionType::Relative,
            // display: Display::Grid,
            position_type: PositionType::Absolute,
            top: Val::Px(20. * (segment.meta.max_depth + 1) as f32),
            right: index.text_pos(),
            ..default()
        },
        Visibility::Hidden,
        VisibleWhenActive,
    ));
    let mut ent_ref = commands.spawn(segment);
    // ent_ref.add_child(name_text);
    let ent_id = ent_ref.id();
    
    if let Some(p) = parent {
        commands.entity(p).add_child(ent_id);
    }
    else {
        ent_ref.insert(index);
    }
    ent_id
}

fn add_tree_rec(
    tree: &SoundTree,
    parent: Option<Entity>,
    duration: f64,
    elapsed: f64,
    lean: f32,
    angle: f32,
    commands: &mut Commands,
    font: &Handle<Font>,
    meshes: &mut ResMut<Assets<Mesh>>,
    images: &mut ResMut<Assets<Image>>,
    materials: &mut ResMut<Assets<StandardMaterial>>,
    index: Index,
) {
    let size = tree.size();
    match tree {
        SoundTree::Simul(children, _) => {
            let val = SIGN.fetch_xor(1, Ordering::Relaxed);
            // let val = SIGN.fetch_add(1, Ordering::Relaxed);
            // let dir = if val % 2 == 0 { 1.0 } else { -1.0 };
            let dir = 1. - val as f32 * 2.;
            let scale = (size - 1) as f32;
            let base_lean = dir * scale / 2.0;
            let Some((head, tail)) = children.split_first() else { return; };
            
            let head_segment = make_segment(head, duration, elapsed, lean, angle, images, materials, meshes);
            // if parent.is_none() {
            //     head_segment.transform = head_segment.transform.with_translation(Vec3::new(index.x_pos(), 0., 0.))
            // }
            // let mut head_ref = commands.spawn(head_segment);
            // if parent.is_none() {
            //     head_ref.insert(index);
            // }
            let head_obj = spawn_segment(head_segment, commands, font, parent, index);
            // let head_obj = head_ref.id();
            // if let Some(p) = parent {
            //     commands.entity(p).add_child(head_obj);
            // }
            // let parent = Some(head_obj);
            for child in tail {
                let local_lean = (base_lean - dir * child.size() as f32) * 0.8 / scale; // can't divide by 0 bc if scale is 0 tail is empty
                add_tree_rec(child, Some(head_obj), duration, elapsed, lean + local_lean, 0.0, commands, font, meshes, images, materials, index);
            }
        },
        SoundTree::Seq(children, _) => {
            let mut ratio_elapsed = 0.0;
            for child in children {
                let ratio = DivisionMethod::Weight.child_scale(&child) / DivisionMethod::Weight.parent_scale(&tree);
                let new_time = duration * ratio;
                let time_elapsed = duration * ratio_elapsed;
                let angle = PI * 0.6 * (ratio_elapsed + ratio / 2. - 0.5) as f32;
                add_tree_rec(child, parent, new_time, elapsed + time_elapsed, lean, angle, commands, font, meshes, images, materials, index);
                ratio_elapsed += ratio;
            }
        },
        SoundTree::Sound(_, _) => {
            let segment = make_segment(tree, duration, elapsed, lean, angle, images, materials, meshes);
            spawn_segment(segment, commands, font, parent, index);
            
        },
    }
}

fn add_tree(
    tree: &SoundTree,
    name: &str,
    commands: &mut Commands,
    font: &Handle<Font>,
    meshes: &mut ResMut<Assets<Mesh>>,
    images: &mut ResMut<Assets<Image>>,
    materials: &mut ResMut<Assets<StandardMaterial>>,
    time: &Res<Time>,
    counter: &mut ResMut<TreeCounter>,
) {
    let current_time = time.elapsed_secs_f64();
    let index = counter.insert();
    let start = current_time + 0.1;
    let duration = if name == "girard" {
        600. - current_time
    } else {
        (tree.size() as f64 * 0.5 + 10.).min(MAX_TIME as f64)
    };
    let timings = Timings {
        start,
        duration,
        lean: 0.,
    };
    commands.spawn((
        Text::new(name),
        TextFont::from_font(font.clone()).with_font_size(SIDE_FONT_SIZE),
        // TextFont::from_font_size(SIDE_FONT_SIZE),
        timings, 
        Node {
            position_type: PositionType::Absolute,
            top: Val::Px(0.),
            right: index.text_pos(),
            ..default()
        },
        Visibility::Hidden,
        VisibleWhenActive,
    ));
    commands.spawn((
        // TextLayout::new_with_justify(),
        timings,
        Node {
            position_type: PositionType::Absolute,
            justify_content: JustifyContent::Center,
            top: Val::Percent(82.6),
            overflow: Overflow::visible(),
            max_width: Val::Px(0.0),
            // right: Val::Px(717. - 30. * index.x_pos()),
            right: Val::Percent(50.24 - 2.52 * index.x_pos()),
            ..default()
        },
        Visibility::Hidden,
        VisibleWhenActive,
    )).with_child((
        Text::new(name),
        TextFont::from_font(font.clone()).with_font_size(SIDE_FONT_SIZE),
        // TextFont::from_font_size(SIDE_FONT_SIZE),
    ));
    add_tree_rec(&tree, None, duration, start, 0.0, 0.0, 
        commands, font, meshes, images, materials, index);
}

// struct 


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

fn setup(
    mut commands: Commands,
    assets: Res<AssetServer>,
    dsp_manager: Res<DspManager>,
    mut dsp_sources: ResMut<Assets<DspSource>>,
    seq_id: Res<SeqId>,
    mut meshes: ResMut<Assets<Mesh>>,
    // mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    // window: Single<&Window>,
) {
    commands.spawn((
        PointLight {
            shadows_enabled: true,
            intensity: 50_00_000.,
            range: 100.0,
            shadow_depth_bias: 0.2,
            ..default()
        },
        Transform::from_xyz(8.0, 40.0, 30.0),
    ));

    let mut rng = rand::rng();
    let shape = meshes.add(Sphere::new(0.035));
    let material = materials.add(StandardMaterial {
        base_color: Color::WHITE,
        emissive: Color::WHITE.into(),
        ..default()
    });
    for _ in 0..1000 {
        let p = vec3(rng.random_range(-STAR_RANGE..STAR_RANGE), rng.random_range(-STAR_RANGE..STAR_RANGE), -30.0);
        commands.spawn((
            Mesh3d(shape.clone()),
            MeshMaterial3d(material.clone()),
            // Mesh3d
            // Sprite {
            //     color: Color::WHITE,
            //     custom_size: Some(vec2(0.1, 0.1)),
            //     ..default()
            // },
            Transform::from_translation(p),
            BgStar,
        ));
    }

    // let window_size = window.resolution.physical_size().as_vec2();

    commands.spawn((
        Camera3d::default(),
        Camera {
            hdr: true, // 1. HDR is required for bloom
            clear_color: ClearColorConfig::Custom(Color::BLACK),
            // viewport: Some(Viewport {
            //     physical_position: UVec2::new(0, 0),
            //     physical_size: (window_size * 0.75).as_uvec2(),
            //     ..default()
            // }),
            ..default()
        },
        Bloom::NATURAL,
        Transform::from_xyz(0.0, 20., 23.0).looking_at(Vec3::new(0., 8., 0.), Vec3::Y),
    ));

    commands.spawn((
        Node {
            display: Display::Flex,
            justify_content: JustifyContent::Center,
            position_type: PositionType::Absolute,
            top: Val::Percent(89.),
            left: Val::Percent(50.),
            max_width: Val::Px(0.0),
            overflow: Overflow::visible(),
            ..default()
        },
    )).with_child((
        InputTextBuffer,
        TextFont::from_font(assets.load(TEXT_FONT)).with_font_size(COMMAND_FONT_SIZE),
        Text::new(""),
        TextLayout::new_with_justify(JustifyText::Center).with_no_wrap(),
    ));

    let seq = dsp_sources.add(dsp_manager.get_graph_by_id(&seq_id.0).unwrap());

    commands.spawn((AudioPlayer(seq), PlaybackSettings { paused: false, ..Default::default()}));
    println!("{:?}", quote0(sigma().eval(Context::new(full_env()))));
    commands.spawn((
        Center,
        Transform::from_xyz(0., 0., 0.)
    ));
}

fn subst_std_env(mut iterm: ITerm) -> ITerm {
    for (name, ty, val) in std_env() {
        if let Some(v) = val {
            let body = quote0(v);
            let replacement = match body {
                CTerm::Inf(it) => *it,
                _ => iann(body, quote0(ty))
            };
            iterm = iterm.subst_free(&name, &replacement);
        }
    }
    iterm
}

fn run_command(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut text_events: EventReader<TextCmd>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    time: Res<Time>,
    mut counter: ResMut<TreeCounter>,
    selector: ResMut<TheSelector>,
) {
    for text in text_events.read() {
        let cmd = match parse::statement(vec![], &text.0) {
            Ok((rest, stmts)) => {
                if rest.len() > 0 {
                    println!("Rest: {rest}");
                }
                stmts
            },
            Err(e) => {
                println!("uhoh error {e}");
                continue;
            }
        };
        match cmd {
            parse::Statement::Let(name, iterm) => {
                println!("got let statement");
                let iterm = match iterm {
                    ITerm::Free(ref name) => {
                        if let Some((ty, Some(val))) = Context::new(full_env()).find_free(name) {
                            iann(quote0(val), quote0(ty))
                        }
                        else {
                            iterm
                        }
                    },
                    _ => subst_std_env(iterm), // might want to substitute out all the std_env variables, since they're just direct LP constructs?
                };
                let tree = match type_translate(&iterm, selector.0.clone()) {
                    Ok(t) => t,
                    Err(e) => { println!("{e}"); continue }
                };
                let font = asset_server.load(TEXT_FONT);
                add_tree(
                    &tree, &name,
                    &mut commands, &font, &mut meshes, &mut images, &mut materials, 
                    &time, &mut counter,
                );
            },
            // parse::Statement::Assume(_items) => todo!(),
            // parse::Statement::Eval(_iterm) => todo!(),
            // parse::Statement::PutStrLn(_) => todo!(),
            // parse::Statement::Out(_) => todo!(),
            parse::Statement::Set(tag, notes) => {
                let point = selector.0.get(&tag);
                *point.notes.lock().unwrap() = notes;
                println!("Set notes for {tag:?} to {notes:?}");
            },
            // parse::Statement::Clear => {
            //     println!("clearing by advancing time from {}", time.elapsed_secs_f64());
            //     // this doesn't work because we're using system time, we'd need our own timer
            //     time.advance_by(Duration::from_secs(MAX_TIME.ceil() as u64 * 5));
            //     println!("{}", time.elapsed_secs_f64());
            // }
            _ => todo!()
            // parse::Statement::Command(_) => todo!(),
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
            (Key::Escape, _) => {
                buf.clear();
            }
            (_, Some(text)) => {
                // println!("Adding {text} at {} bytes", text.bytes().len());
                buf.push_str(text);
                // println!("Buffer now at {}", buf.bytes().len());
            },
            _ => continue,
            // info!("{:?}: '{}'", event, character);
        }
    }
}

fn move_stars(query: Query<&mut Transform, With<BgStar>>, time: Res<Time>) {
    for mut transform in query {
        let elapsed = time.delta_secs();
        if transform.translation.x > STAR_RANGE {
            transform.translation.x -= 2. * STAR_RANGE;
        }
        if transform.translation.y > STAR_RANGE {
            transform.translation.y -= 2. * STAR_RANGE;
        }
        
        transform.translation += Vec3::new(elapsed * 0.5, elapsed * 0.3, 0.);
    }
}

fn rotate_root(mut query: Query<(&mut Transform, &Timings), (With<Shape>, Without<ChildOf>)>, time: Res<Time>) {
    for (mut transform, timings) in &mut query {
        let factor = 11. - (MAX_TIME / timings.duration as f32) % 6.;
        transform.rotate_y(time.delta_secs() / factor);
    }
}

fn rotate_child(mut query: Query<(&mut Transform, &Timings), (With<Shape>, With<ChildOf>)>, time: Res<Time>) {
    for (mut transform, timings) in &mut query {
        let factor = 11. - (MAX_TIME / timings.duration as f32) % 6.;
        transform.rotate_around(Vec3::new(0.0, scale_length(timings.duration) / 2., 0.0), Quat::from_rotation_y(time.delta_secs() / factor));
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
            let delta = timer.delta_secs_f64();
            if timings.start + timings.duration + delta < moment {
                // mat.base_color = Color::BLACK;
                mat.emissive = Color::BLACK.to_linear();
            }
            else if timings.start - delta / 2. <= moment && moment <= timings.start + timings.duration + delta / 2. {
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


fn visibility_active(mut query: Query<(&mut Visibility, &Timings), With<VisibleWhenActive>>, timer: Res<Time>) {
    let moment = timer.elapsed_secs_f64();
    // println!("moment: {moment}");
    for (mut vis, timings) in &mut query {
        if timings.start <= moment && moment <= (timings.start + timings.duration) {
            *vis = Visibility::Visible;
        }
        else {
            *vis = Visibility::Hidden;
        }
    }
}

fn visibility_window(mut query: Query<(&mut Visibility, &Timings), With<Shape>>, timer: Res<Time>) {
    let moment = timer.elapsed_secs_f64();
    // println!("moment: {moment}");
    for (mut vis, timings) in &mut query {
        if timings.start - moment <= WINDOW / 2. && moment - (timings.start + timings.duration) <= WINDOW / 2. {
            *vis = Visibility::Visible;
        }
        else {
            *vis = Visibility::Hidden;
        }
    }
}

fn play_sound(query: Query<(&Timings, &mut AudioInfo)>, mut seq: ResMut<CfgSeq>, timer: Res<Time>) {
    let moment = timer.elapsed_secs_f64();
    for (timings, mut info) in query {
        if !info.done && timings.start - moment < 0.1 {
            // let loc = transform.translation();
            info.sound.sequence(&mut seq.0, timings.start - moment, timings.duration, timings.lean);
            info.done = true;
        }
    }
}

const TEXT_FONT: &'static str = "fonts/lambda/JetBrainsMonoLamb-Regular.ttf";
const SIDE_FONT_SIZE: f32 = 15.;
const COMMAND_FONT_SIZE: f32 = 20.;
const DEFAULT_VIS: Visibility = Visibility::Hidden;
const TIME_MULT: f32 = 4.;
const MAX_TIME: f32 = 60.0 * TIME_MULT;
const WINDOW: f64 = 2.5 * TIME_MULT as f64;
const STAR_RANGE: f32 = 50.0;
// const WINDOW: f64 = MAX_TIME as f64 / 12.;
const SEG_LENGTH: f32 = 1.0;

#[derive(Component, Clone, Copy)]
struct Shape;

#[derive(Component)]
pub struct InputTextBuffer;

#[derive(Component)]
pub struct BgStar;

#[derive(Component)]
pub struct Center;

#[derive(Event)]
pub struct TextCmd(String);

#[derive(Component)]
struct VisibleWhenActive;
#[derive(Resource)]
pub struct TheSelector(AsyncStratifier);

#[derive(Resource)]
pub struct SeqId(uuid::Uuid);

#[derive(Resource)]
pub struct CfgSeq(ConfigSequencer);

#[derive(Resource)]
pub struct TreeCounter(Vec<bool>);

#[derive(Component, Clone, Copy)]
pub struct Index(usize);

impl Index {
    pub fn x_pos(&self) -> f32 {
        let sign = (-1_i32).pow(self.0 as u32) as f32;
        // println!("sign: {sign} {} ", index.0);
        sign * (self.0 as f32 + 0.5) * 3.
    }

    pub fn text_pos(&self) -> Val {
        let sign = (-1_i32).pow(self.0 as u32) as f32;
        let start = (0.5 - 0.5 * sign) * (1500. - 35. - 3. * SIDE_FONT_SIZE) + 35.;
        let calc = start + sign * (self.0 as f32 / 2.).floor() * 6.5 * SIDE_FONT_SIZE;
        // println!("{} {start} -> {calc}", self.0);
        Val::Px(calc)
        // Val::Px(self.0 as f32 * 6.5 * SIDE_FONT_SIZE)
    }
}

// fn print_window_size(query: Single<&Window>) {
//     let window = query.into_inner();
//     println!("{} : {} : [{}, {}]", window.size(), window.physical_size(), window.width(), window.height());
// }

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
        // can panic but w/e
        // if we disabled copy/clone for Index it wouldn't be able to
        // but i think we do want a child to know what its index is...
        // println!("deleting from {}", idx.0);
        self.0[idx.0] = false;
    }
}

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

#[derive(Clone, Copy)]
pub struct HelloPlugin;

impl Plugin for HelloPlugin {
    fn build(&self, app: &mut App) {
        let mut seq = Sequencer::new(false, 2);
        let backend = SharedBackend::new(seq.backend());
        let thing = move || unit::<U0, U2>(make_output(Box::new(backend.clone()), FilterOptions::ClipLowpass));
        let id = thing.id();
        app.add_dsp_source(thing, SourceType::Dynamic);
        app.insert_resource(SeqId(id)); 
        let cfg_seq = CfgSeq(ConfigSequencer::new(seq, true));
        app.insert_resource(cfg_seq);
        app.insert_resource(TheSelector(AsyncStratifier::new()));
        app.insert_resource(TreeCounter::new());
        app.add_event::<TextCmd>();
        app.add_systems(Startup, setup);
        // app.add_systems(Startup, print_window_size);
        app.add_systems(FixedUpdate, move_stars);
        // app.add_systems(Update, (visibility_active, rotate_root));
        app.add_systems(FixedUpdate, (rotate_child, rotate_root));
        app.add_systems(FixedUpdate, (visibility_active, visibility_window, cleanup));
        app.add_systems(FixedUpdate, color_change);
        app.add_systems(Update, play_sound);
        app.add_systems(Update, (handle_typing, run_command));
    }
}


pub fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins
                .set(ImagePlugin::default_nearest())
                .set(WindowPlugin {
                    primary_window: Some(Window {
                        mode: WindowMode::BorderlessFullscreen(MonitorSelection::Primary),
                        ..default()
                    }),
                    ..default()
                }),
            DspPlugin::default(),
        ))
        .add_plugins(HelloPlugin)
        .run();
}