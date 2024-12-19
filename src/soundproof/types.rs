use std::rc::Rc;

use fundsp::hacker32::*;
use crate::music::notes::*;
use crate::Scaling;

pub trait Sequenceable {
    fn sequence(&self, seq: &mut Sequencer, start_time: f64, duration: f64);

    // fn show(&self) -> String;

    // TODO replicate the looping functionality, probably with some sort of special construct?
    // hard part will be nesting probably.
    // we may need to modify generate_with for SoundTree.
    // {
    //     self.sequence_full(seq, start_time, duration, duration);
    // }
    // fn sequence_full(&self, seq: &mut Sequencer, start_time: f64, loop_duration: f64, total_duration: f64);
}


// TODO I want to support audio clips
// #[derive(Clone)]
// pub struct Clip {
//     audio: Wave,
//     duration: f64,
// }

// impl Sequenceable for Clip {
//     todo!()
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
            notes: notes.iter().map(|x| (Note { note: *x, time: 0.75, volume: 1.0, attack: 0.25, decay: 0.25, sustain: 0.5, release: 0.1 }, 1.0)).collect(),
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

    pub fn set_octave(&mut self, octave: i8) {
        self.note_adjust = 12 * octave;
    }

    pub fn map_notes(&mut self, mut f: impl FnMut(&Note) -> Note) {
        self.notes = self.notes.iter_mut().map(|(x, d)| (f(x), *d)).collect();
    }

    // pub fn append(&mut self, other: Self) {
    //     self.notes.append(&mut other.notes.into_iter().map(|(note, dur)| (Note { note: note.note + 12 * other.note_adjust, ..note}, dur)).collect());
    // }

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
    fn sequence(&self, seq: &mut Sequencer, start_time: f64, duration: f64) {
        if self.duration() <= 0.0 {
            return
        }
        let mut elapsed = 0.0;
        let ratio = duration / self.duration();
        for (note, dur) in self.notes.iter() {
            let dur = dur * ratio;
            let note = note.mul_duration(ratio);
            let hz = get_hz(note.note + self.note_adjust);
            let instr = constant(hz) >> (self.instrument.clone() * (
                envelope(move |t| if t < note.time as f32 { 1.0 } else { 0.0 }) >> 
                adsr_live(note.attack, note.decay, note.sustain, note.release)
            )) >> shape(Clip(1.0));
            seq.push_duration(start_time + elapsed, dur, Fade::Power, 0.0, 0.0, Box::new(instr));
            elapsed += dur;
        }
        assert!(elapsed > 0.0, "Melody must cause time to pass!")
    }
}

    // fn show(&self) -> String {
    //     self.notes.iter().map(|(n, _)| match n.note % 12 {
    //         A => 'A',
    //         B => 'B',
    //         C => 'C',
    //         D => 'D',
    //         E => 'E',
    //         F => 'F',
    //         G => 'G',
    //         _ => '*'
    //     }).collect()
    // }

// fn round_by(x: f64, by: f64) -> f64 {
//     (x / by).round() * by
// }

// #[derive(Clone)]
// pub enum TypedTree {
//     Simul(Vec<TypedTree>),
//     Seq(Vec<TypedTree>),
//     Sound(Type, Rc<dyn Sequenceable>)
// }

#[derive(Clone)]
pub enum SoundTree {
    Simul(Vec<SoundTree>),
    Seq(Vec<SoundTree>),
    Sound(Rc<dyn Sequenceable>)
}

impl SoundTree {
    pub fn sound(sound: impl Sequenceable + 'static) -> Self {
        Self::Sound(Rc::new(sound))
    }

    pub fn seq(values: &[SoundTree]) -> Self {
        Self::Seq(values.to_vec())
    }

    pub fn simul(values: &[SoundTree]) -> Self {
        Self::Simul(values.to_vec())
    }

    // pub fn new_simulseq(param: &[[Self]]) -> Self {
    //     for row in param {
    //         for elem in row {
    //             println!("")
    //         }
    //     }
    //     Self::Seq(vec![])
    // }

    pub fn size(&self) -> usize {
        match self {
            SoundTree::Simul(vec) => vec.iter().map(|x| x.size()).max().unwrap_or(0),
            SoundTree::Seq(vec) => vec.iter().map(|x| x.size()).sum::<usize>(),
            SoundTree::Sound(_) => 1,
        }
    }

    pub fn size_adjusted(&self) -> f64 {
        match self {
            SoundTree::Simul(vec) => vec.iter().map(|x| x.size_factor()).reduce(f64::max).unwrap_or(0.0),
            SoundTree::Seq(vec) => vec.iter().map(|x| x.size_factor()).sum::<f64>(),
            SoundTree::Sound(_) => 1.0,
        }
    }

    pub fn size_factor(&self) -> f64 {
        self.size_adjusted().powf(0.85)
    }

    pub fn generate_with(&self, seq: &mut Sequencer, start_time: f64, duration: f64, scaling: Scaling) {
        match self {
            SoundTree::Simul(vec) => {
                for elem in vec {
                    elem.generate_with(seq, start_time, duration, scaling);
                }
            },
            SoundTree::Seq(vec) => {
                let child_count = vec.len();
                let mut time_elapsed = 0.0;
                for child in vec {
                    let ratio = match scaling {
                        Scaling::Linear => 1.0 / child_count as f64,
                        Scaling::Size => child.size_factor() / self.size_adjusted(),
                        // Scaling::SizeAligned => round_by(child.size_factor() / self.size_adjusted(), segment),
                        Scaling::SizeRaw => child.size() as f64 / self.size() as f64,
                        // _ => panic!("SizeAligned not yet implemented for SoundTree2")
                    };
                    let new_time = duration * ratio;
                    child.generate_with(seq, start_time + time_elapsed, new_time, scaling);
                    time_elapsed += new_time;
                }
            },
            SoundTree::Sound(sound) => sound.sequence(seq, start_time, duration),
        }
    }

    // pub fn print_sizes(&self, depth: usize) {
    //     let tabs = "\t".repeat(depth);
    //     println!("{tabs}{}", self.size());
    //     match self {
    //         SoundTree::Simul(vec) => for child in vec { child.print_sizes(depth + 1); },
    //         SoundTree::Seq(vec) => for child in vec { child.print_sizes(depth + 1); },
    //         SoundTree::Sound(_) => (),
    //     }
    // }

    // pub fn show(&self, depth: usize) -> String {
    //     // let tabs = " ".repeat(depth);
    //     match self {
    //         SoundTree2::Simul(vec) => "{".to_owned() + &vec.iter().map(|x| x.show(depth + 1)).collect::<Vec<String>>().join("\n") + "}\n",
    //         SoundTree2::Seq(vec) => "[".to_owned() + &vec.iter().map(|x| x.show(depth + 1)).collect::<Vec<String>>().join(", ") + "]",
    //         SoundTree2::Sound(seqable) => seqable.show(),
    //     }
    // }
}
