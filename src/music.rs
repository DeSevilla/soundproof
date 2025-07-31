use std::path::Path;
use cpal::{traits::{DeviceTrait, HostTrait, StreamTrait}, Device, FromSample, SampleFormat, SizedSample, StreamConfig};
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

// pub const SAMPLE_RATE: f32 = 44100.0;
pub const SAMPLE_RATE: f32 = 48000.0; //this is the default for my computer

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
    use std::time::Instant;
    println!("Rendering .wav file ({dur}s)...");
    let now = Instant::now();
    let mut wave1 = Wave::render(SAMPLE_RATE as f64, dur + 1.0.min(dur * 0.2), au);
    println!("...done in {:?}", now.elapsed());
    println!("Normalizing wave...");
    let now = Instant::now();
    if wave1.amplitude() > 1.0 {
        wave1.normalize();
    }
    println!("...done in {:?}", now.elapsed());
    let filename = Path::new("output/output.wav");
    println!("Saving .wav file ({dur}s)...");
    let now = Instant::now();
    wave1.save_wav16(filename).expect("Could not save wave.");
    println!("...done in {:?}", now.elapsed());
    let png_file = Path::new("output/output.png");
    make_spectrograph(filename, png_file);
    println!("Saved spectrograph.");
}

pub fn run_live(sound: Box<dyn AudioUnit>) {
    run_output(sound);
    // let _ = input::<String>().msg("(press enter to exit)...\n").get();
    // println!("Closing connection");
}


/// This function figures out the sample format and calls `run_synth()` accordingly.
fn run_output(sound: Box<dyn AudioUnit>) {
    let host = cpal::default_host();
    let device = host
        .default_output_device()
        .expect("failed to find a default output device");
    let config = device.default_output_config().unwrap();
    match config.sample_format() {
        SampleFormat::F32 => {
            run_synth::<f32>(sound, device, config.into())
        }
        SampleFormat::I16 => {
            run_synth::<i16>(sound, device, config.into())
        }
        SampleFormat::U16 => {
            run_synth::<u16>(sound, device, config.into())
        }
        _ => panic!("Unsupported format"),
    }
}


/// This function is where the sound is created and played. Once the sound is playing, it loops
/// infinitely, allowing the `shared()` objects to shape the sound in response to MIDI events.
fn run_synth<T: SizedSample + FromSample<f64>>(
    mut sound: Box<dyn AudioUnit>,
    device: Device,
    config: StreamConfig,
) {
    println!("Running synth with config {:?}", config);
    std::thread::spawn(move || {
        let sample_rate = config.sample_rate.0 as f64;
        // let mut sound = create_sound(pitch, volume, pitch_bend, control);
        sound.set_sample_rate(sample_rate);

        let mut next_value = move || sound.get_stereo();
        let channels = config.channels as usize;
        let err_fn = |err| eprintln!("an error occurred on stream: {err}");
        let stream = device
            .build_output_stream(
                &config,
                move |data: &mut [T], _: &cpal::OutputCallbackInfo| {
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
}

/// Callback function to send the current sample to the speakers.
fn write_data<T: SizedSample + FromSample<f64>>(
    output: &mut [T],
    channels: usize,
    next_sample: &mut dyn FnMut() -> (f32, f32),
) {
    for frame in output.chunks_mut(channels) {
        let sample = next_sample();
        let left: T = T::from_sample(sample.0 as f64);
        let right: T = T::from_sample(sample.1 as f64);

        for (channel, sample) in frame.iter_mut().enumerate() {
            *sample = if channel & 1 == 0 { left } else { right };
        }
    }
}
