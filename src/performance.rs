pub mod soundproof_bevy;
use bevy::prelude::*;
use bevy_procedural_audio::prelude::DspPlugin;
use bevy::window::WindowMode;


pub fn main_live() {
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
        .add_plugins(soundproof_bevy::PerformancePlugin)
        .run();
}