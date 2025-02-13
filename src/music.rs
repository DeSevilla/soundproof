use std::path::Path;
use fundsp::hacker32::*;
use sonogram::{ColourGradient, FrequencyScale, RGBAColour, SpecOptionsBuilder};

pub mod instruments;
pub mod sequences;
pub mod notes;

const SAMPLE_RATE: f32 = 44100.0;

pub fn save(au: &mut dyn AudioUnit, dur: f64) {
    println!("Rendering .wav file ({dur} seconds)");
    let mut wave1 = Wave::render(SAMPLE_RATE as f64, dur, au);
    wave1.normalize();
    let filename = Path::new("output/output.wav");
    println!("Saving .wav file ({dur} seconds)");
    wave1.save_wav16(filename).expect("Could not save wave.");
    println!("File saved. Computing spectrograph...");
    let mut spectrograph = SpecOptionsBuilder::new(2.pow(12))
        .load_data_from_file(filename).unwrap()
        .build().unwrap();
    let mut spectrograph = spectrograph.compute();
    let mut gradient = ColourGradient::new();
    gradient.add_colour(RGBAColour::new(0, 0, 0, 255)); // Black
    gradient.add_colour(RGBAColour::new(0, 0, 0, 255)); // Black
    gradient.add_colour(RGBAColour::new(0, 0, 0, 255)); // Black
    gradient.add_colour(RGBAColour::new(0, 0, 0, 255)); // Black
    gradient.add_colour(RGBAColour::new(0, 0, 0, 255)); // Black
    gradient.add_colour(RGBAColour::new(0, 0, 0, 255)); // Black
    gradient.add_colour(RGBAColour::new(0, 0, 0, 255)); // Black
    gradient.add_colour(RGBAColour::new(0, 0, 0, 255)); // Black
    gradient.add_colour(RGBAColour::new(0, 0, 25, 255)); // Black
    gradient.add_colour(RGBAColour::new(0, 0, 50, 255)); // Blue
    gradient.add_colour(RGBAColour::new(25, 0, 75, 255)); // Blue
    gradient.add_colour(RGBAColour::new(50, 0, 100, 255)); // Blue
    gradient.add_colour(RGBAColour::new(140, 40, 130, 255)); // Pink
    gradient.add_colour(RGBAColour::new(227, 61, 215, 255)); // Pink
    gradient.add_colour(RGBAColour::new(246, 55, 55, 255)); // Red
    gradient.add_colour(RGBAColour::new(246, 115, 55, 255)); // Red
    gradient.add_colour(RGBAColour::new(225, 255, 100, 255)); // White
    gradient.add_colour(RGBAColour::new(225, 255, 200, 255)); // White
    gradient.add_colour(RGBAColour::new(225, 225, 200, 255)); // White
    gradient.add_colour(RGBAColour::new(255, 255, 255, 255)); // White
    gradient.add_colour(RGBAColour::new(255, 255, 255, 255)); // White
    let png_file = Path::new("output/output.png");
    println!("Saving image...");
    spectrograph.to_png(png_file,
        FrequencyScale::Log,
        &mut gradient, 
        4096, 2048
    ).expect("Could not save as PNG");
    println!("Done saving.");
}
