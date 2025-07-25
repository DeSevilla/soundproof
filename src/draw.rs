use std::path::Path;
use std::time::Instant;

use piet_common::{Color, Device, DwriteFactory, FontFamily, PietText, PietTextLayout, RenderContext, Text, TextLayoutBuilder};
use piet_common::kurbo::{Circle, Line, Rect};

use crate::soundproof::types::SoundTree;
use crate::DivisionMethod;

const WIDTH_PX: usize = 1920;
const HEIGHT_PX: usize = 1080;
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
    let args = FixedDrawArgs::new(tree.metadata().max_depth, None, scaling);
    drawtree(&tree, &mut rc, args, 0.0, 1.0, 0);
    rc.finish().unwrap();
    std::mem::drop(rc);
    bitmap.save_to_file(path).expect("should save file successfully");
}

pub fn draw_anim(tree: &SoundTree, scaling: DivisionMethod, path: impl AsRef<Path>, frames: usize) {
    let mut device = Device::new().unwrap();
    let mut elapsed = 0.0;
    let frame_time = 1.0 / frames as f64;
    let log_margin = frames / 10;
    let start = Instant::now();
    for ii in 0..frames {
        let mut bitmap = device.bitmap_target(WIDTH_PX, HEIGHT_PX, DPI).unwrap();
        let mut rc = bitmap.render_context();
        let rect = Rect::new(0.0, 0.0, WIDTH_IN, HEIGHT_IN);
        rc.fill(rect, &Color::BLACK);
        let args = FixedDrawArgs::new(tree.metadata().max_depth, Some(elapsed), scaling);
        let cursor_loc = ((WIDTH_IN - args.text_bar) * elapsed, HEIGHT_IN * 0.05);
        let dot = Circle::new(cursor_loc, 0.05);
        rc.fill(dot, &Color::WHITE);
        rc.stroke(dot, &Color::BLUE, 0.03);
        let line = Line::new(cursor_loc, (cursor_loc.0, HEIGHT_IN));
        rc.stroke(line, &Color::GRAY, 0.01);
        drawtree(&tree, &mut rc, args, 0.0, 1.0, 0);
        rc.finish().unwrap();
        std::mem::drop(rc);
        let digits = frames.ilog10() as usize + 1;
        bitmap.save_to_file(path.as_ref().join(format!("{:0digits$}.png", ii))).expect("should save file successfully");
        if ii % log_margin == 0 {
            println!("Completed {ii}/{frames} frames in {:?}", Instant::now() - start);
        }
        elapsed += frame_time;
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
        let depth_height = if max_depth != 0 { HEIGHT_IN * 0.9 / max_depth as f64 } else { 0.2 };
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
    start_time: f64,
    duration: f64,
    depth: usize
) {
    match tree {
        SoundTree::Simul(vec, _) => {
            for (ii, elem) in vec.iter().enumerate() {
                drawtree(elem, ctx, args, start_time, duration, depth + ii);
            }
        },
        SoundTree::Seq(vec, _) => {
            let mut time_elapsed = 0.0;
            for child in vec {
                let ratio = args.scaling.child_scale(child) / args.scaling.parent_scale(tree);
                let new_time = duration * ratio;
                drawtree(child, ctx, args, start_time + time_elapsed, new_time, depth);
                time_elapsed += new_time;
            }
        },
        SoundTree::Sound(_, meta) => {
            if duration * WIDTH_IN <  0.1 / DPI {
                // println!("Too short: {duration} {:?}", meta.color);
                return;
            }


            let bottom = HEIGHT_IN - depth as f64 * args.depth_height;  // height is negative
            let top = bottom - args.depth_height;

            let main_width = WIDTH_IN - if args.current.is_some() { args.text_bar } else { 0.0 };
            let left = start_time * main_width;
            let right = left + duration * main_width;

            let rect = Rect::new(
                left,
                bottom,
                right,
                top,
            );
            if args.current.map_or(false, |t| start_time <= t && t <= start_time + duration) {
                let mut text_manager = PietText::new_with_shared_fonts(DwriteFactory::new().unwrap(), None);
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
    }
}