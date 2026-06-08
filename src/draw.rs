use crate::DivisionMethod;
use crate::soundproof::types::{Highlight, SoundTree};
use minifb::{Window, WindowOptions};
use piet_common::kurbo::Rect;
use piet_common::*;
use std::path::Path;

// TODO we should take these as options
// const WIDTH_PX: usize = 377 * 3;
const WIDTH_PX: usize = 1920;
// const WIDTH_PX: usize = 3000;
// const HEIGHT_PX: usize = 120 * 3;
const HEIGHT_PX: usize = 1080;
// const HEIGHT_PX: usize = 1800;
const DPI: f64 = 96.;
const WIDTH_IN: f64 = WIDTH_PX as f64 / DPI;
const HEIGHT_IN: f64 = HEIGHT_PX as f64 / DPI;

// we should restructure a bit here, the live/static split is wonky

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
    bitmap
        .save_to_file(path)
        .expect("should save file successfully");
}

pub fn make_soundproof_window() -> Window {
    let window_options = WindowOptions {
        borderless: true,
        ..Default::default()
    };
    Window::new("Soundproof Output", WIDTH_PX, HEIGHT_PX, window_options).expect("")
}

pub fn draw_tree_canvas(
    tree: &SoundTree,
    scaling: DivisionMethod,
    device: &mut Device,
) -> Vec<u32> {
    let mut bitmap = device.bitmap_target(WIDTH_PX, HEIGHT_PX, DPI).unwrap();
    let mut rc = bitmap.render_context();
    let rect = Rect::new(0.0, 0.0, WIDTH_IN, HEIGHT_IN);
    rc.fill(rect, &Color::BLACK);
    let draw_args = FixedDrawArgs::new(tree.metadata().max_depth, None, scaling);
    drawtree(&tree, &mut rc, draw_args);
    rc.finish().unwrap();
    std::mem::drop(rc);
    let buf = bitmap.to_image_buf(ImageFormat::RgbaPremul).unwrap();
    buf.raw_pixels()
        .chunks_exact(4)
        .map(|s| s.try_into().unwrap())
        .map(|[r, g, b, _a]: [u8; 4]| ((r as u32) << 16) | ((g as u32) << 8) | b as u32)
        .collect()
}

pub struct LiveDrawContext {
    pub(crate) window: Window,
    pub(crate) device: Device,
}

impl LiveDrawContext {
    pub fn new() -> Self {
        Self {
            window: make_soundproof_window(),
            device: Device::new().unwrap(),
        }
    }

    pub fn draw_tree(&mut self, tree: &SoundTree, scaling: DivisionMethod) {
        let buf = draw_tree_canvas(&tree, scaling, &mut self.device);

        self.window
            .update_with_buffer(&buf, WIDTH_PX, HEIGHT_PX)
            .unwrap();
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
        let depth_height = HEIGHT_IN / 45.;
        let text_bar = depth_height * 4.0;
        Self {
            max_depth,
            depth_height,
            text_bar,
            current,
            scaling,
        }
    }
}

fn drawtree(
    tree: &SoundTree,
    ctx: &mut impl RenderContext<TextLayout = PietTextLayout>,
    args: FixedDrawArgs,
) {
    tree.distribute(
        0.0,
        1.0,
        args.scaling,
        0.0,
        &mut |_, meta, start, frac, _| {
            // if frac * WIDTH_IN < 0.001 / DPI {
            //     // println!("Too short: {duration} {:?}", meta.color);
            //     return;
            // }

            let bottom = HEIGHT_IN - meta.max_depth as f64 * args.depth_height; // height is negative
            let top = bottom - args.depth_height;

            let main_width = WIDTH_IN
                - if args.current.is_some() {
                    args.text_bar
                } else {
                    0.0
                };
            let left = start * main_width;
            let right = left + frac * main_width;
            let border_width = ((right - left).min(args.depth_height) * 0.5).min(0.1);
            let rect_base = Rect::new(left, bottom, right, top);
            let rect = if border_width > 1.0 / DPI {
                let adjust = border_width * 0.5;
                Rect::new(left + adjust, bottom - adjust, right - adjust, top + adjust)
            } else {
                rect_base
            };

            if args
                .current
                .is_some_and(|t| start <= t && t <= start + frac)
            {
                // let mut text_manager = PietText::new_with_shared_fonts(DwriteFactory::new().unwrap(), None);
                let mut text_manager = PietText::new();
                let text = meta.name.to_owned();
                let text_layout = text_manager
                    .new_text_layout(text)
                    .max_width(args.text_bar)
                    .font(FontFamily::SANS_SERIF, args.depth_height * 0.6)
                    .text_color(meta.alt_color)
                    .build()
                    .unwrap();
                ctx.draw_text(&text_layout, (main_width + 0.05 * args.text_bar, top));
                ctx.fill(rect, &meta.alt_color);
            } else {
                ctx.fill(rect, &meta.base_color);
                // if meta.will_step {
                //     ctx.fill(rect, &meta.base_color);
                // }
                // else {
                //     ctx.fill(rect, &meta.alt_color);
                // }
            };
            // let adjust = border_width * 0.5;
            if let Some(w) = meta.will_step
                && border_width > 1.0 / DPI
            {
                let border_color = match w {
                    Highlight::One => Color::WHITE,
                    Highlight::Two => Color::RED,
                    Highlight::Three => Color::BLUE,
                };
                ctx.stroke(rect_base, &border_color, border_width);
            }
        },
        &mut |_, _, _, _, _| {},
        &mut |_, _, _, _, _| {},
    )
}
