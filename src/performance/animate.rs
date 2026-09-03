use std::fs;
use std::sync::mpsc::{Receiver, SyncSender, sync_channel};
use std::thread::sleep;
use std::time::{Duration, Instant};

use clap::Parser;
// use fundsp::sequencer::ReplayMode;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{self, StreamConfig};
use fundsp::prelude::Sequencer;
use fundsp::prelude64::AudioUnit;
use fundsp::sequencer::ReplayMode;
use midi_msg::{ChannelVoiceMsg, MidiMsg};
// use midi_msg::{ChannelVoiceMsg, MidiMsg};
use midir::{Ignore, MidiInput};

use crate::draw::LiveDrawContext;
use crate::music::write_data;
use crate::soundproof::select::Silence;
use crate::soundproof::sound_generators::{Buckets, SoundGenerator};
use crate::soundproof::types::{ConfigSequencer, SetOnce};
use crate::step::*;
use crate::type_translate;
use crate::{FilterOptions, SoundproofArgs};

// TODO: could we like, pregenerate animation frames for a tree somehow? idk

// TODO: implement the architecture for these structs to replace the delegate function
// the way I'm envisioning it is: instead of "delegate" the idea is "multithreaded pipeline"
// (I'm not changing the names until I get it working)
// given a processing function, it creates input and output ends of the pipeline
// the input end can be used like a SyncSender, output can be used like a Receiver
// then instead of spawning the intermediate threads you just use it in place of the sync_channel
// the only threads spawned are the processing threads which the sender and receiver coordinate between
// which doesn't require communication bc it's just cyclic
// bc we need sequential order, there's little reason to do any more complex strategy
// 

// pub struct DelegateSender<T> {
//     channel_size: usize,
//     thread_count: usize,
//     // input: Receiver<T>,
//     input_delegates: Vec<SyncSender<T>>
// }

// pub struct DelegateReceiver<T> {
//     thread_count: usize,
//     channel_size: usize,
//     // output: SyncSender<T>,
//     output_delegates: Vec<Receiver<T>>
// }

pub fn delegate<A, B, F>(
    thread_count: usize,
    channel_size: usize,
    input: Receiver<A>,
    output: SyncSender<B>,
    closure: F,
) where
    A: Send + 'static,
    B: Send + 'static,
    F: Fn(A) -> B + Clone + Send + 'static,
{
    let mut input_delegates = Vec::new();
    let mut output_delegates = Vec::new();
    for _ in 0..thread_count {
        let f = closure.clone();
        let (send_r, receive_r) = sync_channel(channel_size);
        let (send_s, receive_s) = sync_channel(channel_size);
        std::thread::spawn(move || {
            for entry in receive_r {
                let result = f(entry);
                send_s
                    .send(result)
                    .expect("Channel should not close until program is done");
            }
        });
        input_delegates.push(send_r);
        output_delegates.push(receive_s);
    }
    std::thread::spawn(move || {
        for (ii, entry) in input.iter().enumerate() {
            let index = ii % input_delegates.len();
            let send_r = &input_delegates[index];
            send_r.send(entry).expect("Channel should not be closed");
        }
    });
    std::thread::spawn(move || {
        let mut ii = 0;
        loop {
            let index = ii % output_delegates.len();
            let receive_s = &output_delegates[index];
            let result = receive_s.recv().expect("Channel should not be closed");
            println!("Received delegated result {ii}");
            output.send(result).expect("Channel should not be closed");
            ii += 1;
        }
    });
}

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

    // Initialize MIDI input
    let mut midi_in = MidiInput::new("midir reading input").expect("Should have loaded midi");
    midi_in.ignore(Ignore::None);
    let in_ports = midi_in.ports();
    println!("got {} ports", in_ports.len());
    let in_port = if in_ports.is_empty() {
        println!("Could not load MIDI!");
        return;
    } else {
        in_ports[1].clone()
    };
    let in_port_name = midi_in.port_name(&in_port).unwrap();
    let (note_sender, note_receiver) = sync_channel(30);
    // let (end_sender, end_receiver) = sync_channel(30);
    let _conn_in = midi_in
        .connect(
            &in_port,
            "midir-read-input",
            move |_stamp, message, _| {
                // println!("got something");
                let (msg, _len) = MidiMsg::from_midi(message).unwrap();
                if let MidiMsg::ChannelVoice { channel: _, msg } = msg {
                    println!("Received {msg:?}");
                    match msg {
                        ChannelVoiceMsg::NoteOn { note, velocity } => {
                            println!("Got note! {note} {velocity}");
                            note_sender.send((note, velocity)).unwrap();
                        }
                        ChannelVoiceMsg::NoteOff {
                            note: _,
                            velocity: _,
                        } => {
                            println!("Ending note");
                            // tx2.send(()).unwrap();
                        }
                        _ => {
                            println!("what kind of message is this? {msg:?}")
                        }
                    }
                }
            },
            (),
        )
        .unwrap();
    println!("Connection open, reading input from '{in_port_name}'");

    // Set up loop parameters
    let meta = Silence::new();
    // let ctx = Context::default();
    // let term = args.term();
    let limit = args.step_count.unwrap_or(100);
    let channel_size = 500.min(limit);
    let sequence = match &args.step_file {
        Some(path) => {
            let contents = fs::read_to_string(path).expect("Could not open config file");
            contents.split("\n").map(|s| s.to_owned()).collect()
        }
        None => vec![],
    };

    let (term_sender, term_receiver) = sync_channel(channel_size);
    let (tree_sender, tree_receiver) = sync_channel(channel_size);
    let args1 = args.clone();
    let mut base_size = SetOnce::new();
    std::thread::spawn(move || {
        for (ii, (tm, change)) in args1.term().step_with_change(args1.ctx()).enumerate() {
            if ii >= limit {
                break;
            }
            let modifier = if let Some(diff) = change {
                let diff_size = diff.size();
                let modifier =
                    (diff_size as f64 / base_size.get(tm.size()) as f64 * 5.0).powf(0.7) + 0.05;
                modifier
            } else {
                1.0
            };
            term_sender
                .send((tm, modifier))
                .expect("receiver will not disconnect");
        }
    });
    delegate(
        5,
        channel_size,
        term_receiver,
        tree_sender,
        move |(tm, modifier)| {
            let tree = type_translate(&tm, meta)
                .expect("Can only animate a term that's passed typechecking");
            (tree, modifier)
        },
    );
    // std::thread::spawn(move || {
    //     for (ii, (tm, modifier)) in term_receiver.iter().enumerate() {
    //         let tree = type_translate(&tm, meta)
    //             .expect("Can only animate a term that's passed typechecking");
    //         println!("Finished translating tree {ii}");
    //         tree_sender.send((tree, modifier)).expect("receiver will not disconnect")
    //     }
    // });
    let mut draw_ctx = LiveDrawContext::new();
    let mut ii = 0;
    for (tree, modifier) in tree_receiver.iter() {
        let frame_start = Instant::now();
        if !draw_ctx.window.is_open() {
            println!("Window closed; quitting");
            return;
        }
        if ii > limit {
            println!("hit limit");
            break;
        }

        // while !(new_note.load(Ordering::Relaxed)) {
        //     sleep(Duration::from_millis(1));
        // }
        // println!("flipping new_note");
        // let note = current_note.load(Ordering::Relaxed);
        // let velocity = current_velocity.load(Ordering::Relaxed);
        // new_note.store(false, Ordering::Relaxed);
        let maybe_note = if args.diff_time {
            note_receiver.try_recv().ok()
        } else {
            note_receiver.recv().ok()
        };
        cfg_seq.seq.reset();
        sleep(Duration::from_millis(20));
        args = match maybe_note {
            Some((note, velocity)) => match note {
                48..=73 => {
                    println!("Loaded note {note} at vel {velocity}");
                    let ii = (note - 48) as usize;
                    println!("Running config {ii}: {}", sequence[ii]);
                    if ii >= sequence.len() {
                        println!(
                            "Selected config file only supports notes 48-{}",
                            47 + sequence.len()
                        );
                        continue;
                    }
                    SoundproofArgs::parse_from(sequence[ii].split(' '))
                }
                _ => {
                    println!("Got unexpected note: {note}");
                    continue;
                }
            },
            None => args,
        };

        println!("sequencing tree of size {}", tree.size());
        let buckets: Buckets<64> =
            Buckets::from_tree(&tree, args.freq_low, args.freq_high, args.division).reverse();
        let frame_secs = if args.diff_time {
            let base_secs = args.time.unwrap_or(1.0);
            let total = base_secs * modifier;
            total
        } else {
            f64::INFINITY
        };

        buckets.sequence(&mut cfg_seq, 0.0, frame_secs, 0.0);
        draw_ctx.draw_tree(&tree, args.division);
        println!("sequenced over {frame_secs} secs");

        // wait for note to end before proceeding
        // end_receiver.recv().unwrap();
        ii += 1;
        if args.diff_time {
            let render_dur = Instant::now() - frame_start;
            let frame_dur = Duration::from_secs_f64(frame_secs);
            if render_dur < frame_dur {
                sleep(frame_dur - render_dur);
            }
        }
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
    let limit = args.step_count.unwrap_or(100);
    let sequence = match &args.step_file {
        Some(path) => {
            let contents = fs::read_to_string(path).expect("Could not open config file");
            contents.split("\n").map(|s| s.to_owned()).collect()
        }
        None => vec![],
    };
    let mut draw_ctx = LiveDrawContext::new();

    let mut base_size = SetOnce::new();
    let channel_size = 300.min(limit);
    let (term_sender, term_receiver) = sync_channel(channel_size);
    let (tree_sender, tree_receiver) = sync_channel(channel_size);

    let args2 = args.clone();
    std::thread::spawn(move || {
        for (ii, (tm, change)) in args.term().step_with_change(args.ctx()).enumerate() {
            if ii >= limit {
                break;
            }
            // term_sender.send((tm, change))
            //     .expect("receiver will not disconnect");
            // println!("Stepped {ii} in {:?}", Instant::now() - start);
            // let start = Instant::now();
            let next_args = if ii < sequence.len() {
                // we could do this with a mod & loop situation
                // in which case we'd need to change the condition to "if sequence.len() > 0"
                println!("Parsing args loaded from file: {}", sequence[ii]);
                Some(SoundproofArgs::parse_from(sequence[ii].split(' ')))
            } else {
                None
            };
            let base_dur = args2.time.unwrap_or(1.0); // this should be next_args but it's annoying TODO
            let frame_secs = if args2.diff_time
                && let Some(diff) = change
            {
                //same w this one TODO
                // let diff_size = type_translate(&diff, meta).expect("Changes are guaranteed valid by stepper").size();
                let diff_size = diff.size();
                let modifier =
                    (diff_size as f64 / base_size.get(tm.size()) as f64 * 5.0).powf(0.7) + 0.05;
                base_dur * modifier
            } else {
                base_dur
            };
            term_sender.send((tm, frame_secs, next_args)).expect("Channel should be open until program ends");
        }
    });
    delegate(
        5,
        channel_size,
        term_receiver,
        tree_sender,
        move |(tm, frame_secs, next_args)| {    
            let tree = type_translate(&tm, meta)
                .expect("Can only animate a term that's passed typechecking");
            (tree, frame_secs, next_args)
    });
    args = args2;
    let mut count = 0;
    for (tree, frame_secs, next_args) in tree_receiver.iter() {
        let frame_start = Instant::now();
        if !draw_ctx.window.is_open() {
            println!("Window closed; quitting");
            return;
        }
        println!("Consuming tree {count}");
        count += 1;
        args = next_args.unwrap_or(args);
        let frame_time = Duration::from_secs_f64(frame_secs);

        let buckets: Buckets<64> =
            Buckets::from_tree(&tree, args.freq_low, args.freq_high, args.division).reverse();
        buckets.sequence(&mut cfg_seq, 0.0, frame_secs, 0.0);

        draw_ctx.draw_tree(&tree, args.division);

        let frame_end = Instant::now();
        let render_dur = frame_end - frame_start;
        if render_dur < frame_time {
            println!("Sleeping {frame_time:?} - {render_dur:?}");
            sleep(frame_time - render_dur)
        }
    }
    sleep(Duration::from_secs(2))
}
