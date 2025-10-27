use bevy_procedural_audio::{prelude::{DspGraph, DspManager, DspSource, SourceType}, DspAppExt};
use fundsp::{hacker32::*, realseq::SequencerBackend};
use bevy::{core_pipeline::bloom::Bloom, input::keyboard::{Key, KeyboardInput}, prelude::*};
use rand::Rng;

use std::{f32::consts::PI, sync::atomic::Ordering};
use std::sync::{Arc, Mutex};

use crate::*;
use crate::lambdapi::term::{full_env, iann, quote0, std_env};

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
    (2.0 * duration as f32 / MAX_TIME + 0.037).min(0.23)
}

fn scale_length(duration: f64) -> f32 {
    // SEG_LENGTH;
    (scale_width(duration) * 6. * SEG_LENGTH).clamp(SEG_LENGTH * 0.7, SEG_LENGTH * 1.4)
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

fn spawn_segment(mut segment: TreeSegment, commands: &mut Commands, font: &Handle<Font>, parent: Option<Entity>, index: Index) -> Entity {
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
    // let size = tree.size();
    match tree {
        SoundTree::Simul(children, _) => {
            let val = SIGN.fetch_xor(1, Ordering::Relaxed);
            // let val = SIGN.fetch_add(1, Ordering::Relaxed);
            // let dir = if val % 2 == 0 { 1.0 } else { -1.0 };
            let dir = 1. - val as f32 * 2.;
            // let scale = (size - 1) as f32;
            // let base_lean = dir * scale / 2.0;
            let base_lean = dir;
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
                let local_lean = base_lean;
                // let local_lean = (base_lean - dir * child.size() as f32) * 0.8 / scale; // can't divide by 0 bc if scale is 0 tail is empty
                add_tree_rec(child, Some(head_obj), duration, elapsed, local_lean, 0.0, commands, font, meshes, images, materials, index);
            }
        },
        SoundTree::Seq(children, _) => {
            let mut ratio_elapsed = 0.0;
            for child in children {
                let ratio = DivisionMethod::Weight.child_scale(child) / DivisionMethod::Weight.parent_scale(tree);
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
        630. - current_time
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
    add_tree_rec(tree, None, duration, start, 0.0, 0.0, 
        commands, font, meshes, images, materials, index);
}

// #[derive(Resource)]
// struct TheTimer(Timer);

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
            intensity: 25_000_000.,
            range: 100.0,
            shadow_depth_bias: 0.2,
            ..default()
        },
        Transform::from_xyz(8.0, 40.0, 30.0),
    ));

    let mut rng = rand::rng();
    let shape = meshes.add(Sphere::new(0.07));
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
                if !rest.is_empty() {
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
            // if (timings.lean.abs() != 1. && timings.lean.abs() != 0.) {
            //     println!("{}", timings.lean);
            //     panic!();
            // }
            info.sound.sequence(&mut seq.0, timings.start - moment, timings.duration, timings.lean); //timings.lean);
            info.done = true;
        }
    }
}

const TEXT_FONT: &str = "fonts/lambda/JetBrainsMonoLamb-Regular.ttf";
const SIDE_FONT_SIZE: f32 = 20.;
const COMMAND_FONT_SIZE: f32 = 20.;
const DEFAULT_VIS: Visibility = Visibility::Hidden;
const TIME_MULT: f32 = 4.;
const MAX_TIME: f32 = 60.0 * TIME_MULT;
const WINDOW: f64 = 2.5 * TIME_MULT as f64;
const STAR_RANGE: f32 = 50.0;
// const WINDOW: f64 = MAX_TIME as f64 / 12.;
const SEG_LENGTH: f32 = 1.0;

/// Marker for visible shapes within the Bevy scene, e.g. tree components.
#[derive(Component, Clone, Copy)]
struct Shape;

/// Marker for the text entity that stores the input commands.
#[derive(Component)]
pub struct InputTextBuffer;

/// Marker for the stars in the background, so they can be selected to move.
#[derive(Component)]
pub struct BgStar;

/// Marker for the entity spawned at the center. I don't think this is actually used?
#[derive(Component)]
pub struct Center;

#[derive(Event)]
pub struct TextCmd(String);

/// Marker for entities that have [Timings] and are visible during that time.
#[derive(Component)]
struct VisibleWhenActive;

/// The [Selector] used to translate terms of LambdaPi into music.
/// We need access to this within the Bevy app so we can edit note sequences on the fly.
#[derive(Resource)]
pub struct TheSelector(AsyncStratifier);


/// ID of the shared [Sequencer] as a DSP audio node in Bevy because the fundsp crate is weird.
#[derive(Resource)]
pub struct SeqId(uuid::Uuid);

/// Wrapper struct for a [Sequencer] frontend + config to let it work live within Bevy ([ConfigSequencer]).
#[derive(Resource)]
pub struct CfgSeq(ConfigSequencer);

/// Tracks which positions have trees in them; works in concert with [Index].
#[derive(Resource)]
pub struct TreeCounter(Vec<bool>);

/// An index of a currently-running tree.
/// Should only be created by [TreeCounter::insert] and destroyed by [TreeCounter::remove].
/// If this didn't have Clone/Copy we'd be guaranteed to only remove existing Indices,
/// but it became necessary to pass the information down to children & didn't have time to refactor.
#[derive(Component, Clone, Copy)]
pub struct Index(usize);

impl Index {
    /// The in-scene x-axis position for the tree at this index.
    pub fn x_pos(&self) -> f32 {
        let sign = (-1_i32).pow(self.0 as u32) as f32;
        // println!("sign: {sign} {} ", index.0);
        sign * (self.0 as f32 + 0.5) * 3.
    }

    /// The UI x-axis position for the text column corresponding to this index.
    pub fn text_pos(&self) -> Val {
        let sign = (-1_i32).pow(self.0 as u32) as f32;
        let start = (0.5 - 0.5 * sign) * (1500. - 50. - 3. * SIDE_FONT_SIZE) + 35.;
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
    /// Initialize an empty TreeCounter.
    pub fn new() -> Self {
        Self(vec![])
    }

    /// Get an index for a new tree.
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

    /// Free up an index that no longer contains an active tree.
    pub fn remove(&mut self, idx: &Index) {
        // can panic but w/e
        // if we disabled copy/clone for Index it wouldn't be able to
        // but i think we do want a child to know what its index is...
        // println!("deleting from {}", idx.0);
        self.0[idx.0] = false;
    }
}

/// Wrapper to simplify handling the one FunDSP [SequencerBackend] we're using for audio output.
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

/// Bevy plugin that runs the performance environment -- text input and audiovisual output
#[derive(Clone, Copy)]
pub struct PerformancePlugin;

impl Plugin for PerformancePlugin {
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

