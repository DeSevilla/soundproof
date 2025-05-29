use signalsmith_stretch::Stretch;
use fundsp::wave::Wave;

pub struct Interleave<'a> {
    wave: &'a Wave,
    position: usize,
    channel: usize,
}

impl<'a> Interleave<'a> {
    pub fn new(wave: &'a Wave) -> Interleave<'a> {
        // let positions = vec![0; wave.channels()];
        Interleave { wave, position: 0, channel: 0 }
    }
}

impl Iterator for Interleave<'_> {
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        let result = *self.wave.channel(self.channel).get(self.position)?;
        self.position += 1;
        self.channel = (self.channel + 1) % self.wave.channels();
        Some(result)
    }
}

fn deinterleave(input: impl AsRef<[f32]>, channels: usize, sample_rate: f64) -> Wave {
    let mut wave = Wave::new(0, sample_rate);
    for channel in 0..channels {
        let input_channel: Vec<f32> = input.as_ref()
            .iter()
            .skip(channel)
            .copied()
            .step_by(channels)
            .collect();
        wave.push_channel(&input_channel);
    }
    wave
}

pub fn stretch_wave(mut stretch: Stretch, wave: &Wave, time: f64) -> Wave {
    let channel_count = wave.channels();
    let sample_rate = wave.sample_rate();
    let mut output = vec![0.0; channel_count * (sample_rate * time).round() as usize];

    stretch.process(Interleave::new(wave).collect::<Vec<f32>>(), &mut output);
    deinterleave(output, channel_count, sample_rate)
}

pub fn retime_wave(wave: &Wave, time: f64) -> Wave {
    // should we calculate block length, interval & transpose factor from size? how do they vary?
    retime_pitch_wave(wave, time, 0.5) 
}

pub fn retime_pitch_wave(wave: &Wave, time: f64, pitch_factor: f32) -> Wave {
    let mut stretch = Stretch::new(wave.channels() as u32, 2056, 64);
    stretch.set_transpose_factor(pitch_factor, None);
    stretch_wave(stretch, wave, time)
}