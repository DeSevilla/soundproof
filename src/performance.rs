pub mod animate;

// #[cfg(feature = "bevy")]
// pub mod soundproof_bevy;
// #[cfg(feature = "bevy")]
// use bevy::prelude::*;
// #[cfg(feature = "bevy")]
// use bevy::window::WindowMode;
// #[cfg(feature = "bevy")]
// use bevy_procedural_audio::prelude::DspPlugin;

// #[cfg(feature = "bevy")]
// pub fn main_live() {
//     App::new()
//         .add_plugins((
//             DefaultPlugins
//                 .set(ImagePlugin::default_nearest())
//                 .set(WindowPlugin {
//                     primary_window: Some(Window {
//                         mode: WindowMode::BorderlessFullscreen(MonitorSelection::Primary),
//                         ..default()
//                     }),
//                     ..default()
//                 }),
//             DspPlugin::default(),
//         ))
//         .add_plugins(soundproof_bevy::PerformancePlugin)
//         .run();
// }
