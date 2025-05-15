use std::rc::Rc;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;

use fundsp::hacker32::*;
use crate::lambdapi::ast::CTerm;
use crate::music::notes::*;
use crate::music::stretch::retime_wave;
use crate::Scaling;

/// Objects that can be used to generate audio output through a FunDSP [Sequencer].
pub trait Sequenceable {
    /// Generate audio into the sequencer for a duration starting at the selected time.
    fn sequence(&self, seq: &mut Sequencer, start_time: f64, duration: f64, lean: f32);

    /// Duration of the Sequenceable prior to stretching
    fn base_duration(&self) -> f64;
}

// /// An audio clip, to be stretched etc as needed
// /// this is commented out until we decide to store additional metadata over just a wave
// pub struct Clip {
//     wave: Wave
// }

impl<T> Sequenceable for &T where T: Sequenceable {
    fn sequence(&self, seq: &mut Sequencer, start_time: f64, duration: f64, lean: f32) {
        (*self).sequence(seq, start_time, duration, lean);
    }

    fn base_duration(&self) -> f64 {
        (*self).base_duration()
    }
}

impl<T> Sequenceable for Rc<T> where T: Sequenceable {
    fn sequence(&self, seq: &mut Sequencer, start_time: f64, duration: f64, lean: f32) {
        Rc::as_ref(self).sequence(seq, start_time, duration, lean);
    }

    fn base_duration(&self) -> f64 {
        Rc::as_ref(self).base_duration()
    }
}

impl Sequenceable for Wave {
    fn sequence(&self, seq: &mut Sequencer, start_time: f64, duration: f64, lean: f32) {
        let scaled = retime_wave((*self).clone(), duration);
        let wave_arc = Arc::new(scaled);
        // TODO incorporate lean
        let instr = wavech(&wave_arc, 0, None)
            >> split()
            >> (mul(2.0_f32.powf(lean)) | mul(2.0_f32.powf(-lean)));
        seq.push_duration(start_time, duration, Fade::Smooth, 0.0, 0.0, Box::new(instr));
    }

    fn base_duration(&self) -> f64 {
        self.duration()
    }
}

pub struct Loop<T: Sequenceable> {
    body: T,
    loop_duration: f64,
}

impl<T: Sequenceable> Loop<T> {
    pub fn new(body: T) -> Self {
        Loop { loop_duration: body.base_duration(), body }
    }

    pub fn new_dur(body: T, duration: f64) -> Self {
        Loop { body, loop_duration: duration }
    }
}

impl<T: Sequenceable> Sequenceable for Loop<T> {
    fn sequence(&self, seq: &mut Sequencer, start_time: f64, duration: f64, lean: f32) {
        let mut cur_time = start_time;
        while cur_time + self.loop_duration < duration {
            self.body.sequence(seq, cur_time, self.loop_duration, lean);
            cur_time += self.loop_duration;
        }
    }

    fn base_duration(&self) -> f64 {
        self.loop_duration
        // self.body.base_duration()
    }
}

/// A single note within a [Melody], containing pitch, ADSR, and volume info.
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Note {
    /// Pitch of the note as an integer in 12-tone equal temperament. See [get_hz] for the translation to frequency.
    note: i8,
    /// Duration of the "key press" for the note. In a [Melody], will be scaled according to the melody's duration when output.
    time: f64,
    /// Volume of the note.
    volume: f32,
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

    pub fn replace_instrument(&mut self, other: &Self) {
        self.instrument = other.instrument.clone();
        // self.notes = other.notes.clone();
        // self.note_adjust = other.note_adjust;
    }

    /// Applies some adjustments to melodies according to their depth in a [SoundTree].
    /// Melodies deeper into the tree will be higher-pitched and have shorter notes,
    /// for a more twinkly effect.
    pub fn adjust_depth(&mut self, depth: usize) {
        let octave = (depth as f64 / 2.5).powf(0.5).ceil() as i8 + 1;
        self.set_octave(octave);
        // self.note_adjust = (depth as f64 / 1.5 + 1.0).powf(0.85).ceil() as i8 * 5;
        self.map_notes(|&n| Note {
            time: n.time * lerp(0.45, 0.95, 1.0 / depth as f32) as f64,
            attack: 0.2 / (depth as f32 + 0.1),
            sustain: lerp(0.25, 0.5, 1.0 / depth as f32),
            volume: /*0.000035*/ 0.5 * (1.0 + 0.07 * depth as f32),
            ..n
        });
    }
}

impl Sequenceable for Melody {
    fn sequence(&self, seq: &mut Sequencer, start_time: f64, duration: f64, lean: f32) {
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
                (adsr_live(note.attack, note.decay, note.sustain, note.release) * note.volume)
            )) >> shape(Clip(1.0));
            let instr = instr >> split() >> (mul(2.0_f32.powf(lean)) | (mul(2.0_f32.powf(-lean))));
            seq.push_duration(start_time + elapsed, dur, Fade::Power, 0.0, 0.0, Box::new(instr));
            elapsed += dur;
        }
        assert!(elapsed > 0.0, "Melody must cause time to pass!")
    }

    fn base_duration(&self) -> f64 {
        self.duration()
    }
}

/// Tree of simultaneous and/or sequential sounds. Lambda calculus terms are translated into this structure.
/// The purpose of the design is to allow layers of sounds at different paces which each progress in time
/// as fits their place in the structure.
#[derive(Clone)]
pub enum SoundTree {
    /// Subtrees will play simultaneously.
    Simul(Vec<SoundTree>, TreeMetadata),
    /// Subtrees will play sequentially.
    Seq(Vec<SoundTree>, TreeMetadata),
    /// Plays a predefined sound-pattern.
    Sound(Rc<dyn Sequenceable>, TreeMetadata)
}

static SIGN: AtomicU32 = AtomicU32::new(0);

#[derive(Clone, Debug, PartialEq)]
pub struct TreeMetadata {
    pub name: String,
    // pub lean: f32,
}

impl SoundTree {
    /// Constructs a SoundTree containing a single sound-pattern.
    pub fn sound(sound: impl Sequenceable + 'static, name: impl Into<CTerm>) -> Self {
        let term = name.into();
        // println!("{}", term.to_string());
        Self::Sound(Rc::new(sound), TreeMetadata { 
            name: term.to_string(),
            // name: "".to_owned(),
            // lean: 0.0,
        })
    }

    /// Constructs a SoundTree which plays its subtrees one after another.
    pub fn seq(subtrees: &[SoundTree]) -> Self {
        // avoids nested Seqs; this can affect duration assignments.
        // subject to change but I think I like it
        let mut result = Vec::new();
        let mut names = "[".to_owned();
        for val in subtrees {
            match val.clone() {
                SoundTree::Seq(mut trees, meta) => { 
                    names += &meta.name;
                    names += ";";
                    result.append(&mut trees);
                }
                other => {
                    names += &other.metadata().name;
                    names += ";";
                    result.push(other);
                },
            }
        }
        names += "]";
        Self::Seq(result, TreeMetadata { name: names })
    }

    /// Constructs a SoundTree which plays its subtrees simultaneously.
    pub fn simul(subtrees: &[SoundTree]) -> Self {
        // avoids redundant nested Simuls; this cannot affect the resulting audio
        let mut result = Vec::new();
        let mut names = "{".to_owned();
        for val in subtrees {
            match val.clone() {
                SoundTree::Simul(mut trees, meta) => {
                    names += &meta.name;
                    names += "||";
                    result.append(&mut trees);
                    
                }
                other => {
                    names += &other.metadata().name;
                    names += "||";
                    result.push(other)
                }
            }
        }
        names += "}";
        Self::Simul(result, TreeMetadata { name: names })
    }

    // pub fn set_leans(&mut self, lean: f32) {
    //     match self {
    //         SoundTree::Simul(subtrees, meta) => {
    //             let val = SIGN.fetch_add(1, Ordering::Relaxed);
    //             let dir = if val % 2 == 0 { 1.0 } else { -1.0 };
    //             // print!("{}", if dir > 0.0 { "l" } else { "r" });
    //             meta.lean = lean;
    //             let base_lean = (subtrees.len() as f32) * dir / 2.0;  // could also be based on size
    //             for (ii, tree) in subtrees.iter_mut().enumerate() {
    //                 tree.set_leans(lean + (base_lean + ii as f32) * 0.8_f32);
    //             }
    //         },
    //         SoundTree::Seq(subtrees, meta) => {
    //             meta.lean = lean;
    //             for tree in subtrees {
    //                 tree.set_leans(lean);
    //             }
    //         },
    //         SoundTree::Sound(_, meta) => {
    //             meta.lean = lean;
    //         },
    //     }
    // }

    pub fn metadata(&self) -> &TreeMetadata {
        match self {
            SoundTree::Simul(_, tree_metadata) => tree_metadata,
            SoundTree::Seq(_, tree_metadata) => tree_metadata,
            SoundTree::Sound(_, tree_metadata) => tree_metadata,
        }
    }

    pub fn metadata_mut(&mut self) -> &mut TreeMetadata {
        match self {
            SoundTree::Simul(_, tree_metadata) => tree_metadata,
            SoundTree::Seq(_, tree_metadata) => tree_metadata,
            SoundTree::Sound(_, tree_metadata) => tree_metadata,
        }
    }

    /// The total number of nodes in the tree.
    pub fn size(&self) -> usize {
        match self {
            SoundTree::Simul(vec, _) => vec.iter().map(|x| x.size()).max().unwrap_or(0),
            SoundTree::Seq(vec, _) => vec.iter().map(|x| x.size()).sum::<usize>(),
            SoundTree::Sound(_, _) => 1,
        }
    }

    /// The weight of the tree for duration scaling.
    pub fn weight(&self, exp: f64) -> f64 {
        match self {
            SoundTree::Simul(vec, _) => vec.iter().map(|x| x.subtree_weight(exp)).reduce(f64::max).unwrap_or(0.0),
            SoundTree::Seq(vec, _) => vec.iter().map(|x| x.subtree_weight(exp)).sum::<f64>(),
            SoundTree::Sound(_, _) => 1.0,
        }
    }

    /// The weight of the tree, considered as a subtree, for duration scaling.
    pub fn subtree_weight(&self, exp: f64) -> f64 {
        self.weight(exp).powf(exp)
    }

    /// Generate audio into a [Sequencer] for the tree, distributing subtree durations by the selected [scaling](Scaling).
    pub fn generate_with(&self, seq: &mut Sequencer, start_time: f64, duration: f64, scaling: Scaling, lean: f32) {
        // this function is mostly like Sequenceable.sequence but carries additional info around
        // we could refactor to use the trait but then we'd have to carry scaling/lean in TreeMetadata instead
        // setup would be tricky but could allow within-tree variation.... but we're not there yet.
        match self {
            SoundTree::Simul(vec, _) => {
                let val = SIGN.fetch_add(1, Ordering::Relaxed);
                let dir = if val % 2 == 0 { 1.0 } else { -1.0 };
                // let scale = vec.len();
                let scale = (self.size() - 1) as f32;
                let base_lean = dir * scale / 2.0;
                // for (ii, elem) in vec.iter().enumerate() {
                for elem in vec {
                    let local_lean = (base_lean - dir * elem.size() as f32) * 0.8 / scale; // can't divide by 0 bc if scale is 0 vec is empty
                    elem.generate_with(seq, start_time, duration, scaling, lean + local_lean);
                }
            },
            SoundTree::Seq(vec, _) => {
                // let child_count = vec.len();
                let mut time_elapsed = 0.0;
                for child in vec {
                    let ratio = scaling.child_scale(child) / scaling.parent_scale(self);
                    // let ratio = match scaling {
                    //     Scaling::Linear => 1.0 / vec.len() as f64,
                    //     Scaling::Weight => child.subtree_weight(Scaling::exponent()) / self.weight(Scaling::exponent()),
                    //     // we do want scaling exponent to be an argument but for now...
                    //     // Scaling::SizeAligned => round_by(child.size_factor() / self.size_adjusted(), segment),
                    //     Scaling::Size => child.size() as f64 / self.size() as f64,
                    // };
                    let new_time = duration * ratio;
                    child.generate_with(seq, start_time + time_elapsed, new_time, scaling, lean);
                    time_elapsed += new_time;
                }
            },
            SoundTree::Sound(sound, _) => sound.sequence(seq, start_time, duration, lean),
        }
    }
}
