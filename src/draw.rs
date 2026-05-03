use std::path::Path;
use std::thread::sleep;
use std::time::{Duration, Instant};

// use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
// use fundsp::hacker::{AudioUnit, Sequencer};
use minifb::{Window, WindowOptions};
use piet_common::*; //{Color, Device, DwriteFactory, FontFamily, ImageFormat, PietText, PietTextLayout, RenderContext, Text, TextLayoutBuilder};
use piet_common::kurbo::{Circle, Line, Rect};
// use cpal::{self, FromSample, SizedSample, StreamConfig};

use crate::lambdapi::ast::*;
use crate::lambdapi::term::*;
// use crate::Selector;
use crate::soundproof::select::ToneMaker;
use crate::type_translate;
use crate::step::*;
// use crate::soundproof::types::{ConfigSequencer, SetOnce, SoundTree};
use crate::soundproof::types::SoundTree;
use crate::DivisionMethod;

// const WIDTH_PX: usize = 377 * 3;
// const WIDTH_PX: usize = 1920;
const WIDTH_PX: usize = 3000;
// const HEIGHT_PX: usize = 120 * 3;
// const HEIGHT_PX: usize = 1080;
const HEIGHT_PX: usize = 1800;
const DPI: f64 = 96.;
const WIDTH_IN: f64 = WIDTH_PX as f64 / DPI;
const HEIGHT_IN: f64 = HEIGHT_PX as f64 / DPI;

pub fn draw(tree: &SoundTree, scaling: DivisionMethod, path: impl AsRef<Path>) {
    let mut device = Device::new().unwrap();
    let mut bitmap = device.bitmap_target(WIDTH_PX, HEIGHT_PX, DPI).unwrap();
    let mut rc = bitmap.render_context();
    let rect = Rect::new(0.0, 0.0, WIDTH_IN, HEIGHT_IN);
    // rc.fill(rect, &Color::rgb8(0xCE, 0xCE, 0xCE));
    rc.fill(rect, &Color::BLACK);
    // rc.fill(rect, &Color::WHITE);
    let args = FixedDrawArgs::new(tree.metadata().max_depth, None, scaling);
    drawtree(tree, &mut rc, args);
    rc.finish().unwrap();
    std::mem::drop(rc);
    bitmap.save_to_file(path).expect("should save file successfully");
}

// TODO: maybe like, pregenerate animation frames for a tree somehow? idk

// fn write_data<T>(output: &mut [T], channels: usize, next_sample: &mut dyn FnMut() -> (f32, f32))
// where
//     T: SizedSample + FromSample<f32>,
// {
//     for frame in output.chunks_mut(channels) {
//         let sample = next_sample();
//         let left = T::from_sample(sample.0);
//         let right: T = T::from_sample(sample.1);

//         for (channel, sample) in frame.iter_mut().enumerate() {
//             if channel & 1 == 0 {
//                 *sample = left;
//             } else {
//                 *sample = right;
//             }
//         }
//     }
// }

pub fn animate_term_steps(term: ITerm, mut meta: ToneMaker, scaling: DivisionMethod, limit: usize, fps: f64) {
    let mut visual_device = Device::new().unwrap();
    let window_options = WindowOptions { borderless: true, ..Default::default() };
    // window_options.borderless = true;
    let mut window = Window::new("Hi", WIDTH_PX, HEIGHT_PX, window_options).unwrap();
    // let mut elapsed = 0.0;
    let base_time = Duration::new(0, (1e9 / fps) as u32);
    // let mut base_size = SetOnce::new();
    let frame_time = base_time;

    println!("{frame_time:?}");
    // let frames = (duration * fps as f64).ceil() as usize;
    let ctx = Context::new(std_env());
    // let mut seq = Sequencer::new(false, 2);
    // let backend = Box::new(seq.backend());
    // let mut cfg_seq = ConfigSequencer::new(seq, true);
    // let host = cpal::default_host();
    // let audio_device = host
    //     .default_output_device()
    //     .expect("failed to find a default output device");
    // let config: StreamConfig = audio_device.default_output_config().unwrap().into();
    // let channels = config.channels as usize;
    // let mut current = Step::Cont(start_term);
    // std::thread::spawn(move || {
    //     let sample_rate = config.sample_rate.0 as f64;
    //     // let mut sound = create_sound(pitch, volume, pitch_bend, control);
    //     let mut sound = backend;
    //     sound.set_sample_rate(sample_rate);

    //     let mut next_value = move || sound.get_stereo();
    //     let err_fn = |err| eprintln!("an error occurred on stream: {err}");
    //     let stream = audio_device
    //         .build_output_stream(
    //             &config,
    //             move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
    //                 write_data(data, channels, &mut next_value)
    //             },
    //             err_fn,
    //             None,
    //         )
    //         .unwrap();

    //     stream.play().unwrap();
    //     loop {
    //         std::thread::sleep(std::time::Duration::from_millis(1));
    //     }
    // });
    meta.increment();
    for (ii, tm) in term.step_over(ctx.clone()).enumerate() {
        let frame_start = Instant::now();
        if ii > limit {
            break;
        }
        meta.increment();
        let tree = type_translate(&tm, meta.clone()).unwrap();
        let size = tree.size();
        match tm {
            ITerm::Ann(_, _) => println!("looped! after {ii} steps size {size}"),
            _ => {}
        }
        // let ratio = size as f64 / base_size.get(size) as f64;
        // frame_time = base_time * size as u32 / base_size.get(size) as u32;
        // println!("Frame time: {frame_time:?}");
        // tree.generate_with(&mut cfg_seq, 0.0, 2000.0, DivisionMethod::Weight, 0.0);
        if !window.is_open() {
            println!("Window closed; quitting");
            break;
        }
        let mut bitmap = visual_device.bitmap_target(WIDTH_PX, HEIGHT_PX, DPI).unwrap();
        let mut rc = bitmap.render_context();
        let rect = Rect::new(0.0, 0.0, WIDTH_IN, HEIGHT_IN);
        rc.fill(rect, &Color::BLACK);
        let args = FixedDrawArgs::new(tree.metadata().max_depth, None, scaling);
        drawtree(&tree, &mut rc, args);
        rc.finish().unwrap();
        std::mem::drop(rc);
        let a = bitmap.to_image_buf(ImageFormat::RgbaPremul).unwrap();
        let buf: Vec<u32> = a.raw_pixels()
            .chunks_exact(4)
            .map(|s| s.try_into().unwrap())
            .map(|[r, g, b, _a]: [u8; 4]| ((r as u32) << 16) | ((g as u32) << 8) | b as u32)
            .collect();
        window.update_with_buffer(&buf, WIDTH_PX, HEIGHT_PX).unwrap();
        // let digits = frames.ilog10() as usize + 1;
        // bitmap.save_to_file(path.as_ref().join(format!("{:0digits$}.png", ii))).expect("should save file successfully");
        // if ii % log_margin == 0 {
        //     println!("Completed {ii}/{frames} frames in {:?}", Instant::now() - start);
        // }
        // elapsed += frame_time;
        let frame_end = Instant::now();
        let render_dur = frame_end - frame_start;
        if render_dur < frame_time {
            sleep(frame_time - render_dur)
        }
    }
    sleep(Duration::from_secs(10))
}

pub fn draw_anim(tree: &SoundTree, scaling: DivisionMethod, duration: f64, fps: usize) {
    let mut device = Device::new().unwrap();
    let window_options = WindowOptions { borderless: true, ..Default::default() };
    // window_options.borderless = true;
    let mut window = Window::new("Hi", WIDTH_PX, HEIGHT_PX, window_options).unwrap();
    // let mut elapsed = 0.0;
    let frame_time = Duration::new(0, (1e9 / fps as f64) as u32);
    let frames = (duration * fps as f64).ceil() as usize;
    // let log_margin = frames / 10;
    // let start = Instant::now();
    for ii in 0..frames {
        let frame_start = Instant::now();
        let fraction = ii as f64 / frames as f64;
        if !window.is_open() {
            println!("Window closed; quitting");
            break;
        }
        let mut bitmap = device.bitmap_target(WIDTH_PX, HEIGHT_PX, DPI).unwrap();
        let mut rc = bitmap.render_context();
        let rect = Rect::new(0.0, 0.0, WIDTH_IN, HEIGHT_IN);
        rc.fill(rect, &Color::BLACK);
        let args = FixedDrawArgs::new(tree.metadata().max_depth, Some(fraction), scaling);
        let cursor_loc = ((WIDTH_IN - args.text_bar) * fraction, HEIGHT_IN * 0.05);
        let dot = Circle::new(cursor_loc, 0.05);
        rc.fill(dot, &Color::WHITE);
        rc.stroke(dot, &Color::BLUE, 0.03);
        let line = Line::new(cursor_loc, (cursor_loc.0, HEIGHT_IN));
        rc.stroke(line, &Color::GRAY, 0.01);
        drawtree(tree, &mut rc, args);
        rc.finish().unwrap();
        std::mem::drop(rc);
        let a = bitmap.to_image_buf(ImageFormat::RgbaPremul).unwrap();
        let buf: Vec<u32> = a.raw_pixels()
            .chunks_exact(4)
            .map(|s| s.try_into().unwrap())
            .map(|[r, g, b, _a]: [u8; 4]| ((r as u32) << 16) | ((g as u32) << 8) | b as u32)
            .collect();
        window.update_with_buffer(&buf, WIDTH_PX, HEIGHT_PX).unwrap();
        // let digits = frames.ilog10() as usize + 1;
        // bitmap.save_to_file(path.as_ref().join(format!("{:0digits$}.png", ii))).expect("should save file successfully");
        // if ii % log_margin == 0 {
        //     println!("Completed {ii}/{frames} frames in {:?}", Instant::now() - start);
        // }
        // elapsed += frame_time;
        let frame_end = Instant::now();
        let render_dur = frame_end - frame_start;
        if render_dur < frame_time {
            sleep(frame_time - render_dur)
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
struct FixedDrawArgs {
    max_depth: usize,
    depth_height: f64,
    text_bar: f64,
    current: Option<f64>,
    scaling: DivisionMethod,
}

impl FixedDrawArgs {
    pub fn new(max_depth: usize, current: Option<f64>, scaling: DivisionMethod) -> Self {
        // let depth_height = if max_depth != 0 { HEIGHT_IN * 0.9 / max_depth as f64 } else { 0.05 };
        let depth_height = HEIGHT_IN / 40.;
        let text_bar = depth_height * 4.0;
        Self {
            max_depth,
            depth_height,
            text_bar,
            current,
            scaling
        }
    }
}

fn drawtree(
    tree: &SoundTree,
    ctx: &mut impl RenderContext<TextLayout = PietTextLayout>,
    args: FixedDrawArgs,
) {
    tree.distribute(
        0.0, 1.0, args.scaling, 0.0,
        &mut |_, meta, start, frac, _| {
            if frac * WIDTH_IN <  0.1 / DPI {
                // println!("Too short: {duration} {:?}", meta.color);
                return;
            }

            let bottom = HEIGHT_IN - meta.max_depth as f64 * args.depth_height;  // height is negative
            let top = bottom - args.depth_height;

            let main_width = WIDTH_IN - if args.current.is_some() { args.text_bar } else { 0.0 };
            let left = start * main_width;
            let right = left + frac * main_width;

            let rect = Rect::new(
                left,
                bottom,
                right,
                top,
            );
            if args.current.is_some_and(|t| start <= t && t <= start + frac) {
                // let mut text_manager = PietText::new_with_shared_fonts(DwriteFactory::new().unwrap(), None);
                let mut text_manager = PietText::new();
                let text = meta.name.to_owned();
                let text_layout = text_manager.new_text_layout(text)
                    .max_width(args.text_bar)
                    .font(FontFamily::SANS_SERIF, args.depth_height * 0.6)
                    .text_color(meta.alt_color)
                    .build().unwrap();
                ctx.draw_text(&text_layout, (main_width + 0.05 * args.text_bar, top));
                ctx.fill(rect, &meta.alt_color);
            }
            else {
                ctx.fill(rect, &meta.base_color);
            };
            let border_width = ((right - left).min(args.depth_height) * 0.5).min(0.1);
            if border_width > 1.0 / DPI {
                ctx.stroke(rect, &Color::BLACK, border_width);
            }
        },
        &mut |_, _, _, _, _| {},
        &mut |_, _, _, _, _| {},
    )
}