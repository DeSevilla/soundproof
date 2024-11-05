// #![allow(unused)]
use fundsp::hacker32::*;
use lambda::{itranslate, Sequenceable, SoundTreeScaling, Term};
use music::notes::BASE_HZ;
use music::sequences::*;
use music::instruments::*;
use sonogram::{ColourGradient, FrequencyScale, RGBAColour, SpecOptionsBuilder};
use std::path::Path;
use std::fs;
use clap::*;
use lambdapi::*;

mod lambdapi;
mod music;
mod lambda;

#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum Scaling {
    Linear,
    Size,
    SizeAligned,
    SizeRaw,
}

#[derive(Parser, Debug)]
#[command(version, about, long_about=None)]
struct Args {
    scaling: Option<Scaling>,
    time: Option<f64>,
}

fn main() {
    let args = Args::parse();
    let scaling = args.scaling.unwrap_or(Scaling::SizeAligned);
    let time = args.time.unwrap_or(600.0);
    // let term = girard();
    // let term = ast::ITerm::Star;
    // let term = u();
    // let term = tau();
    let term = girard_reduced();
    // validate("term", &term);
    println!("{term}");
    let mut seq = Sequencer::new(false, 1);
    println!("sequencing....");
    let tree = itranslate(term, 0);
    println!("size: {}", tree.size());
    SoundTreeScaling(tree, scaling).sequence(&mut seq, 0.0, time);
    let mut output = unit::<U0, U1>(Box::new(seq)) >> shape(Adaptive::new(0.1, Tanh(0.5)));
    save(&mut output, time);
    println!("Done");
}

fn standard(filename: Option<String>) -> (impl AudioUnit, f32) {
    let repeats = 2;
    let incr = 1.2;
    let (envel1, time) = sequence_envelope(incr, filename);
    let output = (envel1 | press_sequences(incr)) >> harmonizer(base_instrument(), incr) >> effect();
    (output, time * repeats as f32)
}

fn sequence_envelope(incr: f32, file: Option<String>) -> (An<impl AudioNode<Inputs=U0, Outputs=U1>>, f32) {
    let octave = -2;
    let notes = get_notes(file);
    let count = notes.len();
    // let envel1 = delay_envelope(notes, octave, incr);
    let envel1 = notes_envelope(notes, octave, incr); //* octarp(incr / 8.0, incr);
    // let envel1 = envel1 * tremolo(50.0, 2.0, 0.0);
    // let envel1 = envel1 * tremolo(50.0, 2.0, 1.2);
    (envel1, incr * count as f32)
}

fn double_sequence(incr: f32, file: Option<String>) -> (An<impl AudioNode<Inputs = U0, Outputs = U2>>, f32) {
    let octave = -1;
    let notes = get_notes(file);
    let count = notes.len();
    let envel1 = delay_envelope(notes.clone(), octave, incr);
    let envel2 = notes_envelope(notes, octave, incr);
    (envel1 | envel2, incr * count as f32)
}

fn get_notes(file: Option<String>) -> Vec<i8> {
    match file {
        Some(filename) => read_notes(&filename),
        None => vec![57, 60, 59, 64, 57, 62, 59, 67, 69, 64, 67, 60, 57, 64, 59]
    }
    // vec![B, D, F, D, E, A, C, E]
    // vec![G - 12, F, G, F, DSHARP, D, C, ASHARP, G - 12, GSHARP - 12, DSHARP, DSHARP, FSHARP - 12, G - 12, D, D]
    // vec![A, G, A + 12, G, F, E, D, C, A, ASHARP, F, F, A, ASHARP, GSHARP - 12, A, E, E]
    // vec![
    //     E, C, B, C,
    //     F, B, E, A
    // ]
}

fn base_instrument() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    // let instrument = karplus();
    let instrument = violinish();
    // let instrument = sine();
    // let instrument = fm_pi();
    // let instrument = fm_epi();
    // let instrument = fm_basic();
    instrument
}

fn press_sequences(incr: f32) -> An<impl AudioNode<Inputs=U0, Outputs=U1>> {
    onepress(incr / 1.5, incr)
    // octarp(incr / 6.0, incr)
}

fn effect()  -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    let effect = pass();
    // let effect = split() >> !fbd(0.2, -2.0) >> !fbd(0.3, -3.0) >> fbd(0.1, -4.0);
    // let effect = effect >> highpass_hz(BASE_HZ, 0.5);
    // let effect = effect >> split() >> (pass() | (sink() >> constant(BASE_HZ)) | constant(1.0)) >> lowpass();
    let effect = effect >> shape(Clip(1.0));
    // let effect = effect >> flanger_default();
    let effect = effect >> lowpass_hz(BASE_HZ, 1.0);
    effect
}

fn harmonizer(instrument: An<impl AudioNode<Inputs=U1, Outputs=U1> + 'static>, incr: f32) -> An<impl AudioNode<Inputs=U2, Outputs=U1>> {
    // instrument * amp_envelope(incr)
    // additive_third(instrument, incr)
    // additive_decaying(instrument, incr)
    // additive_increasing(instrument, incr)
    // attack_descend(instrument, incr)
    // square_additive_one(instrument, incr)
    // additive_switchup(instrument, incr)
    additive_another(instrument, incr)
}

fn amp_envelope(incr: f32) -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    // sink() >> constant(1.0)
    adsr_live(0.2 * incr, 0.25 * incr, 0.5, 0.3 * incr)           //violinish
    // adsr_live(0.1, 0.25, 0.25, 0.5)          // intermediate
    // adsr_live(0.1, 0.25, 0.25, 0.25)         // intermediate
    // adsr_live(0.1, 0.25, 0.0, 0.0)           // intermediate
    // adsr_live(0.00, 0.15, 0.1, 0.05)             // short
    // adsr_live(0.01, 0.05, 0.01, 0.05)        // pingy
}

fn read_notes(name: &str) -> Vec<i8> {
    let contents = fs::read_to_string(format!("input/{name}")).expect(&format!("Failed to read {name}"));
    // let major = [A, B, C, D, E, F, G];
    let mut notes = Vec::new();
    for c in contents.chars().filter(|x| !x.is_whitespace()) {
        // let noteish = c as usize % 16;
        // let index = noteish % 7;
        // let oct_mod = noteish / 7;
        // let note = major[index] + 12 * oct_mod as i8;
        let note = (c as usize % 64) as i8 - 10;
        // let note = *major.choose(&mut thread_rng()).expect("No notes in the major scale");
        print!("{note} ");
        notes.push(note);
    }
    notes
}

fn lacri_spectra() -> (impl AudioUnit, f32) {
    let mut output = Sequencer::new(false, 1);
    output.push_duration(0.0, 0.5, Fade::Smooth, 0.0, 0.0, Box::new(read_spectrum("spectrum3.txt", sine())));
    output.push_duration(0.5, 0.5, Fade::Smooth, 0.0, 0.0, Box::new(read_spectrum("spectrum4.txt", sine())));
    output.push_duration(1.0, 3.0, Fade::Smooth, 0.0, 0.0, Box::new(read_spectrum("spectrum2.txt", sine()) * tremolo_smooth(4.0, 2.5, 1.0, 0.0, 0.02) * 0.5));
    output.push_duration(1.0, 3.0, Fade::Smooth, 0.0, 0.0, Box::new(read_spectrum("spectrum.txt", sine()) * tremolo_smooth(4.0, 2.5, 0.0, 1.0, 0.02) * 0.5));
    output.push_duration(4.0, 0.5, Fade::Smooth, 0.0, 0.0, Box::new(read_spectrum("spectrum3.txt", sine())));
    output.push_duration(4.5, 0.5, Fade::Smooth, 0.0, 0.0, Box::new(read_spectrum("spectrum4.txt", sine())));
    output.push_duration(5.0, 3.0, Fade::Smooth, 0.0, 0.0, Box::new(read_spectrum("spectrum5.txt", sine()) * tremolo_smooth(4.0, 4.0, 1.0, 0.0, 0.02) * 0.5));
    output.push_duration(5.0, 3.0, Fade::Smooth, 0.0, 0.0, Box::new(read_spectrum("spectrum6.txt", sine()) * tremolo_smooth(4.0, 4.0, 0.0, 1.0, 0.02) * 0.5));
    (output, 8.0)
}

const SAMPLE_RATE: f32 = 44100.0;

fn save(au: &mut dyn AudioUnit, dur: f64) {
    println!("Saving .wav file...");
    let mut wave1 = Wave::render(SAMPLE_RATE as f64, dur, au);
    // wave1 = wave1.filter(dur, &mut ((pass() | lfo(|t| (xerp11(110.0, 880.0, spline_noise(0, t * 5.0)), 1.0))) >> bandpass()));
    wave1.normalize();
    let filename = Path::new("output/output.wav");
    wave1.save_wav16(&filename).expect("Could not save wave.");
    println!("File saved. Computing spectrograph...");
    let mut spectrograph = SpecOptionsBuilder::new(2.pow(12))
        .load_data_from_file(&filename).unwrap()
        // .set_step_size(2.pow(10))
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
    // let buf = spectrograph.to_rgba_in_memory(
    //     FrequencyScale::Linear,
    //     &mut gradient, 
    //     4096, 2048
    // ); //.expect("Could not save as PNG");
    // let mut count = 0;
    // let mut amounts = (0, 0, 0, 0);
    // for column in buf.chunks(4 * 2048) {
    //     for color in column.chunks(4) {
    //         let [r, g, b, a] = color else { println!("Wrong color format"); return };
    //         amounts = match amounts {
    //             (or, og, ob, oa) => (or + r, og + g, ob + b, oa + a)
    //         }
    //     } 
    //     if count % 2.pow(12 - 5) == 0 {
    //         println!("max for now: {amounts:?}");
    //         amounts = (0, 0, 0, 0);
    //     }
    //     count += 1;
    // }
    spectrograph.to_png(png_file,
        FrequencyScale::Log,
        &mut gradient, 
        4096, 2048
    ).expect("Could not save as PNG");
    println!("Done saving.");
}
