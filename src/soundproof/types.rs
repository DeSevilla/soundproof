use std::rc::Rc;

use fundsp::hacker32::*;
use crate::music::notes::*;
use crate::Scaling;

/// Objects that can be used to generate audio output through a FunDSP [Sequencer].
pub trait Sequenceable {
    /// Generate audio into the sequencer for a duration starting at the selected time.
    fn sequence(&self, seq: &mut Sequencer, start_time: f64, duration: f64);
}

/// A single note within a [Melody], containing pitch, ADSR, and volume info.
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Note {
    /// Pitch of the note as an integer in 12-tone equal temperament. See [get_hz] for the translation to frequency.
    note: i8,
    /// Duration of the "key press" for the note. In a [Melody], will be scaled according to the melody's duration when output.
    time: f64,
    /// Volume of the note.
    volume: f64,
    /// Time in seconds from the start of "key press" to when the note reaches max volume.
    attack: f32,
    /// Time in seconds between max volume and sustain volume.
    decay: f32,
    /// Fraction of max volume for a held note.
    sustain: f32,
    /// Time between releasing the note and volume reaching 0. 
    release: f32,
}

impl Note {
    /// Scales the duration of the note, modifying ADSR along with it.
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

/// A sequence of notes played on a particular instrument.
#[derive(Clone)]
pub struct Melody {
    /// A FunDSP [AudioUnit] which is used to play the notes.
    instrument: An<Unit<U1, U1>>,
    /// [Note]s and how much time they're allowed to use.
    notes: Vec<(Note, f64)>,
    /// Adjustment to the pitch of all [Note]s in semitones, stored here so it can be easily tweaked.
    note_adjust: i8,
}

impl Melody {
    /// Create a melody from a sequence of notes as integers in 12-tone equal temperament.
    /// See [get_hz] for the relationship between these integers and frequencies.
    /// 
    /// # Examples:
    /// ```
    /// use music::notes::{A, D, F};
    /// use fundsp::hacker32::sine;
    /// 
    /// let mel = Melody::new_even(sine(), &[A, D, F, A]);
    /// ```
    pub fn new_even(instrument: impl AudioUnit + 'static, notes: &[i8]) -> Self {
        if notes.len() < 3 {
            println!("wtf is with the notes even: {:?}", notes);
        }
        Melody {
            instrument: unit(Box::new(instrument)),
            notes: notes.iter().map(|x| (Note { note: *x, time: 0.75, volume: 1.0, attack: 0.25, decay: 0.25, sustain: 0.5, release: 0.1 }, 1.0)).collect(),
            note_adjust: 0
        }
    }

    /// Create a melody from a sequence of notes as integers in 12-tone equal temperament, with specified durations.
    /// See [get_hz] for the relationship between these integers and frequencies.
    /// 
    /// # Examples:
    /// ```
    /// use music::notes::{B, C, E};
    /// use fundsp::hacker32::sine;
    /// 
    /// let mel = Melody::new_timed(sine(), &[(B, 0.5), (C, 0.5), (E, 3.0)])
    /// ```
    pub fn new_timed(instrument: impl AudioUnit + 'static, notes: &[(i8, f64)]) -> Self {
        Melody {
            instrument: unit(Box::new(instrument)),
            notes: notes.iter().map(|(x, t)| (Note { note: *x, time: 0.85 * t, volume: 1.0, attack: 0.25, decay: 0.25, sustain: 0.5, release: 0.1 }, *t)).collect(),
            note_adjust: 0
        }
    }

    /// Create a melody from a sequence of [Note] objects with specified durations.
    pub fn new_notes(instrument: impl AudioUnit + 'static, notes: &[(Note, f64)]) -> Self {
        Melody {
            instrument: unit(Box::new(instrument)),
            notes: notes.to_vec(),
            note_adjust: 0
        }
    }

    /// Duration of the melody if played at its default speed. 
    /// Used as a basis for sequencing at arbitrary durations.
    /// 
    /// # Examples:
    /// 
    /// ```
    /// use music::notes::{B, C, E};
    /// use fundsp::hacker32::sine;
    /// 
    /// let mel = Melody::new_timed(sine(), &[(B, 0.5), (C, 0.5), (E, 3.0)]);
    /// let dur = mel.duration();
    /// assert_eq!(dur, 4.0);
    /// ```
    pub fn duration(&self) -> f64 {
        self.notes.iter().map(|(_, x)| x).sum()
    }

    /// Adjust the octave of all notes.
    pub fn set_octave(&mut self, octave: i8) {
        self.note_adjust = 12 * octave;
    }

    /// Takes a closure and applies it to each [Note] in the melody.
    pub fn map_notes(&mut self, mut f: impl FnMut(&Note) -> Note) {
        self.notes = self.notes.iter_mut().map(|(x, d)| (f(x), *d)).collect();
    }

    /// Concatenate two melodies, preserving pitch.
    pub fn append(&mut self, other: Self) {
        self.notes.append(&mut other.notes.into_iter().map(|(note, dur)| (Note { note: note.note + 12 * (other.note_adjust - self.note_adjust), ..note}, dur)).collect());
    }

    /// Applies some adjustments to melodies according to their depth in a [SoundTree].
    /// Melodies deeper into the tree will be higher-pitched and have shorter notes,
    /// for a more twinkly effect.
    pub fn adjust_depth(&mut self, depth: usize) {
        self.set_octave((depth as f64 / 2.5).powf(0.5).ceil() as i8 + 1);
        self.map_notes(|&n| Note {
            time: n.time * lerp(0.45, 0.95, 1.0 / depth as f32) as f64,
            attack: 0.2 / (depth as f32 + 0.1),
            sustain: lerp(0.25, 0.5, 1.0 / depth as f32),
            ..n
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

/// Tree of simultaneous and/or sequential sounds. Lambda calculus terms are translated into this structure.
/// The purpose of the design is to allow layers of sounds at different paces which each progress in time
/// as fits their place in the structure.
#[derive(Clone)]
pub enum SoundTree {
    /// Subtrees will play simultaneously.
    Simul(Vec<SoundTree>),
    /// Subtrees will play sequentially.
    Seq(Vec<SoundTree>),
    /// Plays a predefined sound-pattern.
    Sound(Rc<dyn Sequenceable>)
}

impl SoundTree {
    /// Constructs a SoundTree containing a single sound-pattern.
    pub fn sound(sound: impl Sequenceable + 'static) -> Self {
        Self::Sound(Rc::new(sound))
    }

    /// Constructs a SoundTree which plays its subtrees one after another.
    pub fn seq(subtrees: &[SoundTree]) -> Self {
        // avoids nested Seqs; this can affect duration assignments. 
        // subject to change but I think I like it
        let mut result = Vec::new();
        for val in subtrees {
            match val.clone() {
                SoundTree::Seq(mut trees) => result.append(&mut trees),
                other => result.push(other),
            }
        }
        Self::Seq(result)
    }

    /// Constructs a SoundTree which plays its subtrees simultaneously.
    pub fn simul(subtrees: &[SoundTree]) -> Self {
        // avoids redundant nested Simuls; this cannot affect the resulting audio
        let mut result = Vec::new();
        for val in subtrees {
            match val.clone() {
                SoundTree::Simul(mut trees) => result.append(&mut trees),
                other => result.push(other)
            }
        }
        Self::Simul(result)
    }

    /// The total number of nodes in the tree.
    pub fn size(&self) -> usize {
        match self {
            SoundTree::Simul(vec) => vec.iter().map(|x| x.size()).max().unwrap_or(0),
            SoundTree::Seq(vec) => vec.iter().map(|x| x.size()).sum::<usize>(),
            SoundTree::Sound(_) => 1,
        }
    }

    /// The weight of the tree for duration scaling.
    pub fn weight(&self) -> f64 {
        match self {
            SoundTree::Simul(vec) => vec.iter().map(|x| x.subtree_weight()).reduce(f64::max).unwrap_or(0.0),
            SoundTree::Seq(vec) => vec.iter().map(|x| x.subtree_weight()).sum::<f64>(),
            SoundTree::Sound(_) => 1.0,
        }
    }

    /// The weight of the tree, considered as a subtree, for duration scaling.
    pub fn subtree_weight(&self) -> f64 {
        self.weight().powf(0.85)
    }

    /// Generate audio into a [Sequencer] for the tree, distributing subtree durations by the selected [scaling](Scaling).
    pub fn generate_with(&self, seq: &mut Sequencer, start_time: f64, duration: f64, scaling: Scaling) {
        // this function is mostly like Sequenceable.sequence but carries scaling info around
        // we could refactor to use the trait but then we'd have to carry scaling on every individual tree node instead
        // although that could allow within-tree variation.... but we're not there yet.
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
                        Scaling::Weight => child.subtree_weight() / self.weight(),
                        // Scaling::SizeAligned => round_by(child.size_factor() / self.size_adjusted(), segment),
                        Scaling::Size => child.size() as f64 / self.size() as f64,
                    };
                    let new_time = duration * ratio;
                    child.generate_with(seq, start_time + time_elapsed, new_time, scaling);
                    time_elapsed += new_time;
                }
            },
            SoundTree::Sound(sound) => sound.sequence(seq, start_time, duration),
        }
    }
}
