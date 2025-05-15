use std::path::Path;
use fundsp::hacker32::*;
use sonogram::{ColourGradient, FrequencyScale, RGBAColour, SpecOptionsBuilder};

/// Synth instruments. Many of these are unused currently, but kept around for tinkering.
pub mod instruments;
/// Synth envelopes for controlling key presses. Many of these are unused currently, but kept around for tinkering.
pub mod sequences;
/// Constants and functions for twelve-tone equal temperament notes and converting them to frequencies.
pub mod notes;
/// Code for stretching audio clips. May be revised as needed
pub mod stretch;

const SAMPLE_RATE: f32 = 44100.0;

/// Render and save a spectrograph for a WAV file.
pub fn make_spectrograph(input_wav: &Path, output_png: &Path) {
    println!("Computing spectrograph...");
    let mut spectrograph = SpecOptionsBuilder::new(2.pow(12))
        .load_data_from_file(input_wav).unwrap()
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
    println!("Saving image...");
    spectrograph.to_png(output_png,
        FrequencyScale::Log,
        &mut gradient, 
        4096, 2048
    ).expect("Could not save as PNG");

}

/// Render an [AudioUnit] (in practice this is a [Sequencer]) to a WAV file and generate a spectrograph for it.
pub fn save(au: &mut dyn AudioUnit, dur: f64) {
    println!("Rendering .wav file ({dur} seconds)");
    let mut wave1 = Wave::render(SAMPLE_RATE as f64, dur, au);
    if wave1.amplitude() > 1.0 {
        wave1.normalize();
    }
    let filename = Path::new("output/output.wav");
    println!("Saving .wav file ({dur} seconds)");
    wave1.save_wav16(filename).expect("Could not save wave.");
    println!("File saved.");
    let png_file = Path::new("output/output.png");
    make_spectrograph(filename, png_file);
    println!("Done saving.");
}
