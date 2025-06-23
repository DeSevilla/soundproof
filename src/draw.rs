use std::path::Path;

use piet_common::{Color, Device, DwriteFactory, FontFamily, PietText, PietTextLayout, RenderContext, Text, TextLayoutBuilder};
use piet_common::kurbo::{Circle, Rect};

use crate::soundproof::types::SoundTree;
use crate::Scaling;

const WIDTH_PX: usize = 1920;
const HEIGHT_PX: usize = 1080;
const DPI: f64 = 96.;
const WIDTH_IN: f64 = WIDTH_PX as f64 / DPI;
const HEIGHT_IN: f64 = HEIGHT_PX as f64 / DPI;

pub fn draw(tree: &SoundTree, scaling: Scaling, path: impl AsRef<Path>) {
    let mut device = Device::new().unwrap();
    let mut bitmap = device.bitmap_target(WIDTH_PX, HEIGHT_PX, DPI).unwrap();
    let mut rc = bitmap.render_context();
    let rect = Rect::new(0.0, 0.0, WIDTH_IN, HEIGHT_IN);
    // rc.fill(rect, &Color::rgb8(0xCE, 0xCE, 0xCE));
    rc.fill(rect, &Color::BLACK);
    drawtree(&tree, &mut rc, tree.metadata().max_depth, None, 0.0, 1.0, scaling, 0);
    rc.finish().unwrap();
    std::mem::drop(rc);
    bitmap.save_to_file(path).expect("should save file successfully");
}

pub fn draw_anim(tree: &SoundTree, scaling: Scaling, path: impl AsRef<Path>, frames: usize) {
    let mut device = Device::new().unwrap();
    let mut elapsed = 0.0;
    let frame_time = 1.0 / frames as f64;
    let log_margin = frames / 10;
    for ii in 0..frames {
        let mut bitmap = device.bitmap_target(WIDTH_PX, HEIGHT_PX, DPI).unwrap();
        let mut rc = bitmap.render_context();
        let rect = Rect::new(0.0, 0.0, WIDTH_IN, HEIGHT_IN);
        rc.fill(rect, &Color::BLACK);
        drawtree(&tree, &mut rc, tree.metadata().max_depth, Some(elapsed), 0.0, 1.0, scaling, 0);
        rc.finish().unwrap();
        std::mem::drop(rc);
        bitmap.save_to_file(path.as_ref().join(format!("{:05}.png", ii))).expect("should save file successfully");
        if ii % log_margin == 0 {
            println!("Completed {ii}/{frames} frames");
        }
        elapsed += frame_time;
    }
}

fn drawtree(
    tree: &SoundTree,
    ctx: &mut impl RenderContext<TextLayout = PietTextLayout>,
    max_depth: usize,
    current: Option<f64>,
    start_time: f64,
    duration: f64,
    scaling: Scaling,
    depth: usize
) {
    match tree {
        SoundTree::Simul(vec, _) => {
            for (ii, elem) in vec.iter().enumerate() {
                drawtree(elem, ctx, max_depth, current, start_time, duration, scaling, depth + ii);
            }
        },
        SoundTree::Seq(vec, _) => {
            let mut time_elapsed = 0.0;
            for child in vec {
                let ratio = scaling.child_scale(child) / scaling.parent_scale(tree);
                let new_time = duration * ratio;
                drawtree(child, ctx, max_depth, current, start_time + time_elapsed, new_time, scaling, depth);
                time_elapsed += new_time;
            }
        },
        SoundTree::Sound(_, meta) => {
            // if duration * WIDTH_IN <  0.25 / DPI {
            //     // println!("Too short: {duration} {:?}", meta.color);
            //     return;
            // }

            let depth_height = HEIGHT_IN * 0.9 / max_depth as f64;
            let text_bar = depth_height * 4.0;

            let bottom = HEIGHT_IN - depth as f64 * depth_height;  // height is negative
            let top = bottom - depth_height;

            let main_width = WIDTH_IN - if current.is_some() { text_bar } else { 0.0 };
            let left = start_time * main_width;
            let right = left + duration * main_width;

            let rect = Rect::new(
                left,
                bottom,
                right,
                top,
            );
            if current.map_or(false, |t| start_time <= t && t <= start_time + duration) {
                let dot = Circle::new((WIDTH_IN * duration, HEIGHT_IN * 0.05), 0.1);
                ctx.fill(dot, &Color::WHITE);
                let mut text_manager = PietText::new_with_shared_fonts(DwriteFactory::new().unwrap(), None);
                let text = meta.name.to_owned();
                let text_layout = text_manager.new_text_layout(text)
                    .max_width(text_bar)
                    .font(FontFamily::SANS_SERIF, depth_height * 0.6)
                    .text_color(meta.alt_color)
                    .build().unwrap();
                ctx.draw_text(&text_layout, (main_width + 0.05 * text_bar, top));
                ctx.fill(rect, &meta.alt_color);
            }
            else {
                ctx.fill(rect, &meta.base_color);
            };
        },
    }
}