use fundsp::hacker32::*;
use crate::music::notes::*;
use crate::Scaling;

pub trait Sequenceable {
    fn sequence(&self, seq: &mut Sequencer, start_time: f64, duration: f64) {
        self.sequence_full(seq, start_time, duration, duration);
    }
    fn sequence_full(&self, seq: &mut Sequencer, start_time: f64, loop_duration: f64, total_duration: f64);
}


// TODO I want to support audio clips
// #[derive(Clone)]
// pub struct Clip {
//     audio: Wave,
//     duration: f64,
// }

// impl Sequenceable for Clip {
//     fn sequence_full(&self, seq: &mut Sequencer, start_time: f64, loop_duration: f64, total_duration: f64) {
//         todo!()
//     }
// }

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Note {
    note: i8,
    time: f64,
    volume: f64,
    attack: f32,
    decay: f32,
    sustain: f32,
    release: f32,
}

impl Note {
    pub fn mul_duration(self, factor: f64) -> Self {
        Note {
            time: self.time * factor,
            attack: self.attack * factor as f32,
            decay: self.decay * factor as f32,
            release: self.release * factor as f32,
            ..self
        }
    }
}

#[derive(Clone)]
pub struct Melody {
    instrument: An<Unit<U1, U1>>,
    notes: Vec<(Note, f64)>,
    note_adjust: i8,
}

impl Melody {
    pub fn new_even(instrument: impl AudioUnit + 'static, notes: &[i8]) -> Self {
        Melody {
            instrument: unit(Box::new(instrument)),
            notes: notes.into_iter().map(|x| (Note { note: *x, time: 0.75, volume: 1.0, attack: 0.25, decay: 0.25, sustain: 0.5, release: 0.1 }, 1.0)).collect(),
            note_adjust: 0
        }
    }

    pub fn duration(&self) -> f64 {
        self.notes.iter().map(|(_, x)| x).sum()
    }

    // pub fn set_duration(&mut self, time: f64) -> () {
    //     let ratio = time / self.duration();
    //     // let se = self.clone();
    //     let ratio32 = ratio as f32;
    //     self.notes = self.notes.iter_mut().map(|(x, t)| { x.length *= ratio; x.attack *= ratio32; x.decay *= ratio32; x.release *= ratio32; (*x, *t * ratio)} ).collect();
    // }

    pub fn set_octave(&mut self, octave: i8) -> () {
        self.note_adjust = 12 * octave;
    }

    pub fn map_notes(&mut self, mut f: impl FnMut(&Note) -> Note) {
        self.notes = self.notes.iter_mut().map(|(x, d)| (f(x), *d)).collect();
    }

    pub fn append(&mut self, other: Self) {
        self.notes.append(&mut other.notes.into_iter().map(|(note, dur)| (Note { note: note.note + 12 * other.note_adjust, ..note}, dur)).collect());
    }

    pub fn adjust_depth(&mut self, depth: usize) {
        self.set_octave((depth as f64 / 2.5).sqrt().ceil() as i8 + 1);
        self.map_notes(|n| Note {
            time: n.time * lerp(0.25, 0.95, 1.0 / depth as f32) as f64,
            attack: 0.2 / (depth as f32 + 0.1),
            sustain: lerp(0.25, 0.5, 1.0 / depth as f32),
            ..*n
        });
    }
}
impl Sequenceable for Melody {
    fn sequence_full(&self, seq: &mut Sequencer, start_time: f64, loop_duration: f64, total_duration: f64) {
        if self.duration() <= 0.0 {
            return
        }
        let instr = self.instrument.clone();
        let mut elapsed = 0.0;
        let ratio = loop_duration / self.duration();
        loop {
            for (note, dur) in self.notes.iter() {
                let dur = if elapsed + dur * ratio <= total_duration { dur * ratio } else { total_duration - elapsed };
                let note = note.mul_duration(ratio);
                let len = note.time;
                let hz = get_hz(note.note + self.note_adjust);
                let instr = constant(hz) >> instr.clone() * 
                    (envelope(move |t| if t < len as f32 { 1.0 } else { 0.0 }) >> adsr_live(note.attack, note.decay, note.sustain, note.release))
                    >> shape(Clip(1.0));
                if elapsed + dur >= total_duration {
                    return;
                }
                seq.push_duration(start_time + elapsed, dur, Fade::Power, 0.0, 0.0, Box::new(instr));
                elapsed += dur;
                
            }
            if elapsed <= 0.0 {
                panic!("Melody {:?} cannot be played", self.notes);
            }
        }
    }

}

pub struct SoundTree {
    melody: Box<dyn Sequenceable>,
    children: Vec<SoundTree>
}

fn round_by(x: f64, by: f64) -> f64 {
    (x / by).round() * by
}


impl SoundTree {
    pub fn new(melody: impl Sequenceable + 'static, children: Vec<SoundTree>) -> SoundTree {
        SoundTree {
            melody: Box::new(melody),
            children
        }
    }

    pub fn size(&self) -> usize {
        1 + self.children.iter().map(|x| x.size()).sum::<usize>()
    }

    fn size_adjusted(&self) -> f64 {
        if self.children.len() > 0 {
            self.children.iter().map(|x| x.size_factor()).sum::<f64>()
        }
        else {
            1.0
        }
    }

    fn size_factor(&self) -> f64 {
        self.size_adjusted().powf(0.85)
    }

    fn generate_with(&self, start_time: f64, duration: f64, align_to: f64, seq: &mut Sequencer, scaling: Scaling) {
        println!("Generating {start_time} {duration}");
        if align_to > 0.0 {
            self.melody.sequence_full(seq, start_time, align_to, duration);
        }
        else {
            self.melody.sequence(seq, start_time, duration);
        }
        let child_count = self.children.len();
        let segment = 0.5f64.powi(child_count as i32);
        let mut time_elapsed = 0.0;
        for child in self.children.iter() {
            let ratio = match scaling {
                Scaling::Linear => 1.0 / child_count as f64,
                Scaling::Size => child.size_factor() / self.size_adjusted(),
                Scaling::SizeAligned => round_by(child.size_factor() / self.size_adjusted(), segment),
                Scaling::SizeRaw => child.size() as f64 / self.size() as f64,
            };
            let new_time = duration * ratio;
            let mut align_to = align_to;
            while align_to > new_time * 1.0000001 { align_to *= 0.5 };
            child.generate_with(start_time + time_elapsed, new_time, align_to, seq, scaling);
            time_elapsed += new_time;
        }
    }
}

impl Sequenceable for SoundTree {
    fn sequence_full(&self, seq: &mut Sequencer, start_time: f64, loop_duration: f64, total_duration: f64) {
        self.generate_with(start_time, total_duration, loop_duration, seq, Scaling::Size);
    }
}

pub struct SoundTreeScaling(pub SoundTree, pub Scaling);

impl Sequenceable for SoundTreeScaling {
    fn sequence_full(&self, seq: &mut Sequencer, start_time: f64, loop_duration: f64, total_duration: f64) {
        self.0.generate_with(start_time, total_duration, loop_duration, seq, self.1);
    }
}

pub struct SoundTreeExpanding {
    melody: Box<dyn Sequenceable>,
    children: Vec<SoundTreeExpanding>,
}

impl From<SoundTree> for SoundTreeExpanding {
    fn from(value: SoundTree) -> Self {
        SoundTreeExpanding {
            melody: value.melody,
            children: value.children.into_iter().map(|x| x.into()).collect(),
        }
    }
}

impl Sequenceable for SoundTreeExpanding {
    fn sequence_full(&self, seq: &mut Sequencer, start_time: f64, loop_duration: f64, total_duration: f64) {
        let switch_time = total_duration * 0.2;
        let rem_time = total_duration - switch_time;
        let loop_time = switch_time * loop_duration / total_duration;
        let rem_loop = loop_duration - loop_time;
        self.melody.sequence_full(seq, start_time, loop_time, switch_time);
        for (ii, child) in self.children.iter().enumerate() {
            let extra_time = ii as f64 * 0.0001 * total_duration;
            child.sequence_full(seq, start_time + switch_time + extra_time, rem_loop, rem_time - extra_time);
        }
    }
}

// #[derive(Clone)]
// pub struct SoundContext(Vec<(Name, SoundTree2, Type)>);

// impl SoundContext {
//     pub fn push(&mut self, name: Name, mel: impl Sequenceable + 'static, ty: Type) {
//         self.0.push((name, SoundTree2::Sound(Box::new(mel)), ty))
//     }
// }

// impl From<SoundContext> for Context {
//     fn from(value: SoundContext) -> Self {
//         value.0.into_iter().map(|(n, m, t)| (n, t)).collect()
//     }
// }

// #[derive(Clone)]
pub enum SoundTree2 {
    Simul(Vec<SoundTree2>),
    Seq(Vec<SoundTree2>),
    Sound(Box<dyn Sequenceable>)
}

impl SoundTree2 {
    pub fn new_sound(sound: impl Sequenceable + 'static) -> Self {
        Self::Sound(Box::new(sound))
    }

    pub fn size(&self) -> usize {
        match self {
            SoundTree2::Simul(vec) => vec.iter().map(|x| x.size()).max().unwrap_or(0),
            SoundTree2::Seq(vec) => vec.iter().map(|x| x.size()).sum::<usize>(),
            SoundTree2::Sound(_) => 1,
        }
    }

    pub fn size_adjusted(&self) -> f64 {
        match self {
            SoundTree2::Simul(vec) => vec.iter().map(|x| x.size_factor()).reduce(f64::max).unwrap_or(0.0),
            SoundTree2::Seq(vec) => vec.iter().map(|x| x.size_factor()).sum::<f64>(),
            SoundTree2::Sound(_) => 1.0,
        }
    }

    pub fn size_factor(&self) -> f64 {
        self.size_adjusted().powf(0.85)
    }

    pub fn generate_with(&self, seq: &mut Sequencer, start_time: f64, duration: f64, scaling: Scaling) {
        match self {
            SoundTree2::Simul(vec) => {
                for elem in vec {
                    elem.generate_with(seq, start_time, duration, scaling);
                }
            },
            SoundTree2::Seq(vec) => {
                let child_count = vec.len();
                let mut time_elapsed = 0.0;
                for child in vec {
                    let ratio = match scaling {
                        Scaling::Linear => 1.0 / child_count as f64,
                        Scaling::Size => child.size_factor() / self.size_adjusted(),
                        // Scaling::SizeAligned => round_by(child.size_factor() / self.0.size_adjusted(), segment),
                        Scaling::SizeRaw => child.size() as f64 / self.size() as f64,
                        _ => panic!("SizeAligned not yet implemented for SoundTree2")
                    };
                    let new_time = duration * ratio;
                    child.generate_with(seq, start_time + time_elapsed, new_time, scaling);
                    time_elapsed += new_time;
                }
            },
            SoundTree2::Sound(sound) => sound.sequence(seq, start_time, duration),
        }
    }
}

pub struct SoundTree2Scaling(pub SoundTree2, pub Scaling);

impl Sequenceable for SoundTree2Scaling {
    fn sequence_full(&self, seq: &mut Sequencer, start_time: f64, loop_duration: f64, total_duration: f64) {
        //TODO ignoring loop duration we may need more work on this api tbh
        //like maybe instead of building it in to Sequenceable we add a loop construct for any Sequenceable
        //this also lets us potentially modify behavior at call sites individually
        self.0.generate_with(seq, start_time, total_duration, self.1);
    }
}

