
use crate::draw::LiveDrawContext;
use crate::soundproof::types::ConfigSequencer;
use crate::step::*;
use crate::type_translate;
use std::sync::atomic::{AtomicBool, AtomicU8, Ordering};
use std::sync::Arc;
use std::thread::sleep;
use std::time::{Duration, Instant};
use std::fs;

use clap::Parser;
// use fundsp::sequencer::ReplayMode;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{self, StreamConfig};
use fundsp::prelude::Sequencer;
use fundsp::sequencer::ReplayMode;
use midi_msg::{MidiMsg, ChannelVoiceMsg};
// use midi_msg::{ChannelVoiceMsg, MidiMsg};
use midir::{Ignore, MidiInput};

use crate::{SoundproofArgs, FilterOptions};
use crate::lambdapi::ast::*;
use crate::lambdapi::term::*;
use crate::music::write_data;
use crate::soundproof::select::Silence;
use crate::soundproof::sound_generators::{Buckets, SoundGenerator};

// TODO: could we like, pregenerate animation frames for a tree somehow? idk

pub fn animate_term_midi(mut args: SoundproofArgs) {
    // Initialize FunDSP controls for audio output
    let mut seq = Sequencer::new(0, 2, ReplayMode::None);
    let backend = crate::make_output(Box::new(seq.backend()), FilterOptions::ClipLowpass);
    let mut cfg_seq = ConfigSequencer::new(seq, true);

    // Initialize audio device output
    let host = cpal::default_host();
    let audio_device = host
        .default_output_device()
        .expect("failed to find a default output device");
    let config: StreamConfig = audio_device.default_output_config().unwrap().into();
    let channels = config.channels as usize;
    std::thread::spawn(move || {
        let sample_rate = config.sample_rate.0 as f64;
        let mut sound = backend;
        sound.set_sample_rate(sample_rate);

        let mut next_value = move || sound.get_stereo();
        let err_fn = |err| eprintln!("an error occurred on stream: {err}");
        let stream = audio_device
            .build_output_stream(
                &config,
                move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                    write_data(data, channels, &mut next_value)
                },
                err_fn,
                None,
            )
            .unwrap();

        stream.play().unwrap();
        loop {
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
    });

    // Channels to transfer MIDI input to main loop
    let updated = Arc::new(AtomicBool::new(false));
    let current_note = Arc::new(AtomicU8::new(0));
    let current_velocity = Arc::new(AtomicU8::new(0));
    let updated_close = Arc::clone(&updated);
    let note_close = Arc::clone(&current_note);
    let vel_close = Arc::clone(&current_velocity);

    // Initialize MIDI input
    let mut midi_in = MidiInput::new("midir reading input").expect("Should have loaded midi");
    midi_in.ignore(Ignore::None);
    let in_ports = midi_in.ports();
    println!("got {} ports", in_ports.len());
    let in_port = if in_ports.is_empty() {
        println!("Could not load MIDI!");
        return;
    }
    else {
        in_ports[1].clone()
    };
    let in_port_name = midi_in.port_name(&in_port).unwrap();
    let _conn_in = midi_in.connect(&in_port, "midir-read-input", move |_stamp, message, _| {
                println!("got something");
                let (msg, _len) = MidiMsg::from_midi(message).unwrap();
                if let MidiMsg::ChannelVoice { channel: _, msg } = msg {
                    println!("Received {msg:?}");
                    match msg {
                        ChannelVoiceMsg::NoteOn { note, velocity } => {
                            println!("Got note! {note} {velocity}");
                            note_close.store(note, Ordering::Relaxed);
                            vel_close.store(velocity, Ordering::Relaxed);
                            updated_close.store(true, Ordering::Relaxed);
                        }
                        ChannelVoiceMsg::NoteOff { note: _, velocity: _ } => {
                            println!("Ending note");
                            note_close.store(0, Ordering::Relaxed);
                            vel_close.store(0, Ordering::Relaxed);
                            updated_close.store(true, Ordering::Relaxed);
                        }
                        _ => {}
                    }
                }
            },
            (),
        )
        .unwrap();
    println!("Connection open, reading input from '{in_port_name}'");

    // Set up loop parameters
    let meta = Silence::new();
    let ctx = Context::new(std_env());
    let term = args.term();
    let limit = args.step_count.unwrap_or(100);
    let sequence = match &args.step_file {
        Some(path) => {
            let contents = fs::read_to_string(path).expect("Could not open config file");
            contents.split("\n").map(|s| s.to_owned()).collect()
        },
        None => vec![],
    };
    let mut draw_ctx = LiveDrawContext::new();
    for (ii, tm) in term.step_over(ctx.clone()).enumerate() {
        if !draw_ctx.window.is_open() {
            println!("Window closed; quitting");
            return;
        }
        if ii > limit {
            break;
        }

        while !(updated.load(Ordering::Relaxed)) {
            sleep(Duration::from_millis(1));
        }
        let note = current_note.load(Ordering::Relaxed);
        let velocity = current_velocity.load(Ordering::Relaxed);
        updated.store(false, Ordering::Relaxed);
        println!("Loaded note: {note} at vel {velocity}");
        match note {
            48..=73 => {
                let ii = (note - 48) as usize;
                println!("Running config {ii}");
                if ii >= sequence.len() {
                    println!("Selected config file only supports notes 48-{}", 47 + sequence.len())
                }
                args = SoundproofArgs::parse_from(sequence[ii].split(' '));
            },
            _ => {
                println!("Got unexpected note: {note}");
                continue;
            }
        }

        let tree = type_translate(&tm, meta).unwrap();
        let buckets: Buckets<64> = Buckets::from_tree(&tree, args.freq_min, args.freq_max, args.division)
            .reverse();
        let sound_duration = args.time.unwrap_or(1.0); // TODO can we tie duration to keypress length?
        buckets.sequence(&mut cfg_seq, 0.0, sound_duration, 0.0);
        draw_ctx.draw_tree(&tree, args.division);
       
        // wait for note to end before proceeding
        // TODO do we need this part?
        while !(updated.load(Ordering::Relaxed)) {
            sleep(Duration::from_millis(1));
        }
        updated.store(false, Ordering::Relaxed);
    }
    println!("Closing connection");
}

pub fn animate_term_steps(mut args: SoundproofArgs) {
    // Initialize FunDSP controls for audio output
    let mut seq = Sequencer::new(0, 2, ReplayMode::None);
    let backend = crate::make_output(Box::new(seq.backend()), FilterOptions::ClipLowpass);
    let mut cfg_seq = ConfigSequencer::new(seq, true);

    // Initialize audio device output
    let host = cpal::default_host();
    let audio_device = host
        .default_output_device()
        .expect("failed to find a default output device");
    let config: StreamConfig = audio_device.default_output_config().unwrap().into();
    let channels = config.channels as usize;
    std::thread::spawn(move || {
        let sample_rate = config.sample_rate.0 as f64;
        let mut sound = backend;
        sound.set_sample_rate(sample_rate);

        let mut next_value = move || sound.get_stereo();
        let err_fn = |err| eprintln!("an error occurred on stream: {err}");
        let stream = audio_device
            .build_output_stream(
                &config,
                move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                    write_data(data, channels, &mut next_value)
                },
                err_fn,
                None,
            )
            .unwrap();

        stream.play().unwrap();
        loop {
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
    });

    // Set up loop parameters
    let meta = Silence::new();
    let ctx = Context::new(std_env());
    let term = args.term();
    let limit = args.step_count.unwrap_or(100);
    let sequence = match &args.step_file {
        Some(path) => {
            let contents = fs::read_to_string(path).expect("Could not open config file");
            contents.split("\n").map(|s| s.to_owned()).collect()
        },
        None => vec![],
    };
    let mut draw_ctx = LiveDrawContext::new();

    for (ii, tm) in term.step_over(ctx.clone()).enumerate() {
        let frame_start = Instant::now();
        if !draw_ctx.window.is_open() {
            println!("Window closed; quitting");
            return;
        }
        if ii > limit {
            break;
        }

        if ii < sequence.len() {
            // we could do this with a mod & loop situation
            println!("Loading from file: {}", sequence[ii]);
            args = SoundproofArgs::parse_from(sequence[ii].split(' '))
        }

        let frame_secs = args.time.unwrap_or(1.0);
        let frame_time = Duration::new(0, (1e9 * frame_secs) as u32);
        let tree = type_translate(&tm, meta).unwrap();
        let buckets: Buckets<64> = Buckets::from_tree(&tree, args.freq_min, args.freq_max, args.division)
            .reverse();
        buckets.sequence(&mut cfg_seq, 0.0, frame_secs, 0.0);
        draw_ctx.draw_tree(&tree, args.division);
        
        let frame_end = Instant::now();
        let render_dur = frame_end - frame_start;
        if render_dur < frame_time {
            sleep(frame_time - render_dur)
        }
    }
    sleep(Duration::from_secs(2))
}
