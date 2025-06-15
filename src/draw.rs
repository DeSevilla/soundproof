use std::path::Path;

use piet_common::{Color, RenderContext, Device};
use piet_common::kurbo::{Rect};

use crate::soundproof::types::SoundTree;
use crate::Scaling;

const WIDTH: usize = 1920 * 4;
const HEIGHT: usize = 1080 * 4;
const DPI: f64 = 96. * 4.0;
const DEPTH_HEIGHT: f64 = 0.25;
const MIN_SIZE: f64 = 1.0 / DPI;

pub fn draw(tree: &SoundTree, scaling: Scaling, path: impl AsRef<Path>) {
    let mut device = Device::new().unwrap();
    let mut bitmap = device.bitmap_target(WIDTH, HEIGHT, DPI).unwrap();
    let mut rc = bitmap.render_context();
    let rect = Rect::new(0.0, 0.0, 20., 11.25);
    rc.fill(rect, &Color::BLACK);
    drawtree(&tree, &mut rc, 0.0, 1.0, scaling, 0);
    rc.finish().unwrap();
    std::mem::drop(rc);
    bitmap.save_to_file(path).expect("should save file successfully");
}

fn drawtree(tree: &SoundTree, ctx: &mut impl RenderContext, start_time: f64, duration: f64, scaling: Scaling, depth: usize) {
    match tree {
        SoundTree::Simul(vec, _) => {
            for (ii, elem) in vec.iter().enumerate() {
                drawtree(elem, ctx, start_time, duration, scaling, depth + ii);
            }
        },
        SoundTree::Seq(vec, _) => {
            let mut time_elapsed = 0.0;
            for child in vec {
                let ratio = scaling.child_scale(child) / scaling.parent_scale(tree);
                let new_time = duration * ratio;
                drawtree(child, ctx, start_time + time_elapsed, new_time, scaling, depth);
                time_elapsed += new_time;
            }
        },
        SoundTree::Sound(_, meta) => {
            if duration * WIDTH as f64 / DPI < MIN_SIZE * 0.25 {
                // println!("Too short: {duration} {:?}", meta.color);
                return;
            }
            let rect = Rect::new(
                start_time * WIDTH as f64 / DPI,
                HEIGHT as f64 / DPI - depth as f64 * DEPTH_HEIGHT,
                (start_time + duration) * WIDTH as f64 / DPI, 
                HEIGHT as f64 / DPI - (depth + 1) as f64 * DEPTH_HEIGHT
            );
            ctx.fill(rect, &meta.color);
        },
    }
}