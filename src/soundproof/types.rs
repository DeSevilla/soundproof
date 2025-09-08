use std::path::Path;
// use std::rc::Rc;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Instant;

use bevy::prelude::*;
use fundsp::hacker32::*;
use piet_common::Color;
use rand::seq::IndexedRandom;
use rand::rng;
use crate::music::notes::*;
use crate::music::stretch::{retime_pitch_wave, retime_wave};
use crate::soundproof::select::FS_MEL_SIZE;
use crate::DivisionMethod;

#[derive(Clone, Debug, PartialEq)]
pub enum SoundproofError {
    Mismatch
}

pub struct ConfigSequencer {
    seq: Sequencer,
    start_time: Option<Instant>,
    live: bool,
}

impl ConfigSequencer {
    pub fn new(seq: Sequencer, live: bool) -> Self {
        // should probably start this when we first push
        Self { seq, start_time: None, live }
    }

    pub fn push_duration(
        &mut self,
        start_time: f64,
        duration: f64,
        fade_ease: Fade,
        fade_in_time: f64,
        fade_out_time: f64,
        unit: Box<dyn AudioUnit>,
    ) -> EventId {
        if self.live {
            let now = Instant::now();
            let start_time = match self.start_time {
                None => {
                    self.start_time = Some(now);
                    start_time
                },
                Some(st) => {
                    start_time - (st - now).as_secs_f64()
                }
            };
            // let gap = ;
            self.seq.push_relative(start_time, start_time + duration, fade_ease, fade_in_time, fade_out_time, unit)
        }
        else {
            // note: relative isn't *bad* even for rendering to files but if we can do absolute let's do absolute
            // self.seq.push_relative(start_time, start_time + duration, fade_ease, fade_in_time, fade_out_time, unit)
            // self.seq.push(start_time, start_time + duration, fade_ease, fade_in_time, fade_out_time, unit)
            self.seq.push_duration(start_time, duration, fade_ease, fade_in_time, fade_out_time, unit)
        }
    }
}

/// Objects that can be used to generate audio output through a FunDSP [Sequencer].
pub trait SoundGenerator {
    /// Generate audio into the sequencer for a duration starting at the selected time.
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32);

    /// Duration of the Sequenceable prior to stretching
    fn base_duration(&self) -> f64;
}


impl<T: ?Sized> SoundGenerator for Arc<T> where T: SoundGenerator {
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
        self.as_ref().sequence(seq, start_time, duration, lean);
    }

    fn base_duration(&self) -> f64 {
        self.as_ref().base_duration()
    }
}

/// An audio clip, to be stretched etc as needed
/// this is commented out until we decide to store additional metadata over just a wave
#[derive(Clone)]
pub struct WaveClip {
    wave: Arc<Wave>,
    depth: usize,
}

impl WaveClip {
    pub fn from_file<P: AsRef<Path>>(path: P) -> Self {
        // TODO proper error handling
        WaveClip {
            wave: Arc::new(Wave::load(path).unwrap()),
            depth: 0,
        }
    }

    pub fn from_wave(wave: Wave) -> Self {
        WaveClip {
            wave: Arc::new(wave),
            depth: 0,
        }
    }

    pub fn depth_factor(&self) -> f32 {
        (self.depth as f32 / 5.0).powf(0.5).ceil() / 2.0
    }

    pub fn adjust_depth(&mut self, depth: usize) {
        self.depth = depth;
    }
}

impl SoundGenerator for WaveClip {
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
        let scaled = retime_pitch_wave(&self.wave, duration, self.depth_factor());
        let wave_arc = Arc::new(scaled);
        // TODO incorporate lean
        let instr = wavech(&wave_arc, 0, None)
            >> split()
            >> (mul(2.0_f32.powf(lean)) | mul(2.0_f32.powf(-lean)));
        seq.push_duration(start_time, duration, Fade::Smooth, 0.0, 0.0, Box::new(instr));
    }
    
    fn base_duration(&self) -> f64 {
        self.wave.duration()
    }
}

impl SoundGenerator for Wave {
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
        let scaled = retime_wave(self, duration);
        let wave_arc = Arc::new(scaled);
        let instr = wavech(&wave_arc, 0, None)
            >> split()
            >> (mul(2.0_f32.powf(lean)) | mul(2.0_f32.powf(-lean)));
        seq.push_duration(start_time, duration, Fade::Smooth, 0.0, 0.0, Box::new(instr));
    }

    fn base_duration(&self) -> f64 {
        self.duration()
    }
}

pub struct Loop<T: SoundGenerator> {
    body: T,
    loop_duration: f64,
}

impl<T: SoundGenerator> Loop<T> {
    pub fn new(body: T) -> Self {
        Loop { loop_duration: body.base_duration(), body }
    }

    pub fn new_dur(body: T, duration: f64) -> Self {
        Loop { body, loop_duration: duration }
    }

    pub fn set_duration(&mut self, duration: f64) {
        self.loop_duration = duration;
    }

    pub fn loop_duration(&self) -> f64 {
        self.loop_duration
    }
}

impl<T: SoundGenerator> SoundGenerator for Loop<T> {
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
        let mut loop_dur = self.loop_duration;
        while loop_dur > duration * 0.25 {
            loop_dur *= 0.5;
        }
        let mut cur_time = start_time;
        while cur_time + loop_dur < duration {
            self.body.sequence(seq, cur_time, loop_dur, lean);
            cur_time += loop_dur;
        }
        // self.body.sequence(seq, cur_time, duration - cur_time, lean); // finish up with a little fast one why not
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
    note: i32,
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
    pub fn mul_duration(self, factor: f64) -> Note {
        Note {
            time: self.time * factor,
            attack: self.attack * factor as f32,
            decay: self.decay * factor as f32,
            release: self.release * factor as f32,
            ..self
        }
    }

    pub fn adjust_pitch(self, tones: i32) -> Note {
        Note {
            note: self.note + tones,
            ..self
        }
    }

    pub fn with_instrument(self, instrument: An<impl AudioNode<Inputs=U1, Outputs=U1> + 'static>) -> An<impl AudioNode<Inputs=U0, Outputs=U1>> {
        let hz = constant(get_hz(self.note));
        let vol_control = envelope(move |t| if t < self.time as f32 { 1.0 } else { 0.0 }) >>
            (adsr_live(self.attack, self.decay, self.sustain, self.release) * self.volume);
        hz >> (instrument * vol_control) >> shape(Clip(1.0))
    }
}

/// A sequence of notes played on a particular instrument.
#[derive(Clone)]
pub struct Melody {
    /// A FunDSP [AudioUnit] which is used to play the notes.
    pub instrument: An<Unit<U1, U1>>,
    /// [Note]s and how much time they're allowed to use.
    pub notes: Vec<(Note, f64)>,
    /// Adjustment to the pitch of all [Note]s in semitones, stored here so it can be easily tweaked.
    pub note_adjust: i32,
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
    pub fn new_even(instrument: impl AudioUnit + 'static, notes: &[i32]) -> Self {
        Melody {
            instrument: unit(Box::new(instrument)),
            notes: notes.iter().map(|x| (Note { note: *x, time: 0.85, volume: 1.0, attack: 0.25, decay: 0.25, sustain: 0.5, release: 0.1 }, 1.0)).collect(),
            note_adjust: 0
        }
    }

    /// Create a melody from a sequence of notes as integers in a 12-tone scale, with specified durations.
    /// See [get_hz] for the relationship between these integers and frequencies.
    /// 
    /// # Examples:
    /// ```
    /// use music::notes::{B, C, E};
    /// use fundsp::hacker32::sine;
    /// 
    /// let mel = Melody::new_timed(sine(), &[(B, 0.5), (C, 0.5), (E, 3.0)])
    /// ```
    pub fn new_timed(instrument: impl AudioUnit + 'static, notes: &[(i32, f64)]) -> Self {
        Melody {
            instrument: unit(Box::new(instrument)),
            notes: notes.iter().map(|(x, t)| (Note {
                note: *x, 
                time: 0.85 * t,
                volume: 1.0,
                attack: 0.25,
                decay: 0.25,
                sustain: 0.5,
                release: 0.1
            }, *t)).collect(),
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
    pub fn set_octave(&mut self, octave: i32) {
        // self.note_adjust = 6 * octave;
        self.note_adjust = 12 * octave;
    }

    /// Takes a closure and applies it to each [Note] in the melody.
    pub fn map_notes(&mut self, mut f: impl FnMut(&(Note, f64)) -> (Note, f64)) {
        self.notes = self.notes.iter_mut().map(|pair| f(pair)).collect();
    }

    /// Takes a closure and applies it to each [Note] in the melody.
    pub fn map_indexed(&mut self, mut f: impl FnMut(usize, &(Note, f64)) -> (Note, f64)) {
        self.notes = self.notes.iter_mut().enumerate().map(|(i, pair)| f(i, pair)).collect();
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

    pub fn adjust_timings(&mut self, timings: &[f64]) -> Result<(), SoundproofError> {
        if self.notes.len() != timings.len() {
            return Err(SoundproofError::Mismatch)
        }
        self.map_indexed(|i, (n, _d)| (*n, timings[i]));
        Ok(())
    }

    /// Applies some adjustments to melodies according to their depth in a [SoundTree].
    /// Melodies deeper into the tree will be higher-pitched and have shorter notes,
    /// for a more twinkly effect.
    pub fn adjust_depth(&mut self, depth: usize) {
        let octave = (depth as f64 / 2.5).powf(0.5).ceil() as i32 + 1;
        self.set_octave(octave);
        // self.note_adjust = (depth as f64 / 1.5 + 1.0).powf(0.85).ceil() as i8 * 5;
        self.map_notes(|&(n, d)| (Note {
            time: n.time * lerp(0.45, 0.95, 1.0 / depth as f32) as f64,
            // time: n.time * lerp(0.95, 0.45, 1.0 / depth as f32) as f64,
            attack: 0.2 / (depth as f32 + 0.1),
            sustain: lerp(0.25, 0.5, 1.0 / depth as f32),
            volume: /*0.000035*/ 0.6 * (1.0 + 0.02 * depth as f32),
            ..n
        }, d));
    }
}

impl SoundGenerator for Melody {
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
        if self.duration() <= 0.0 {
            println!("No duration for melody! {:?}", self.notes);
            // println!("{}", std::backtrace::Backtrace::capture());
            return
        }
        // println!("lean: {lean}, {}, {}", 2.0_f32.powf(lean), 2.0_f32.powf(-lean));
        let mut elapsed = 0.0;
        let ratio = duration / self.duration();
        for (note, dur) in self.notes.iter() {
            let dur = dur * ratio;
            let note = note.mul_duration(ratio).adjust_pitch(self.note_adjust);
            let instr = note.with_instrument(self.instrument.clone());
            let instr = instr >> split() >> (mul(2.0_f32.powf(lean)) | (mul(2.0_f32.powf(-lean))));
            seq.push_duration(start_time + elapsed, dur, Fade::Power, 0.0, 0.0, Box::new(instr));
            // seq.push_relative(start_time + elapsed, start_time + elapsed + dur, Fade::Power, 0.0, 0.0, Box::new(instr));
            elapsed += dur;
        }
        assert!(elapsed > 0.0, "Melody must cause time to pass!")
    }

    fn base_duration(&self) -> f64 {
        self.duration()
    }
}

pub struct Texture {
    notes: Vec<Note>,
    instrument: An<Unit<U1, U1>>,
    time_gap: f64,
}

impl Texture {
    pub fn new_even(notes: impl IntoIterator<Item=i32>, instrument: impl AudioUnit + 'static, time_gap: f64) -> Self {
        let dur = (time_gap * 0.25) as f32;
        let full_notes = notes.into_iter().map(|n| Note {
            note: n,
            volume: 1.0,
            time: dur as f64,
            attack: 0.25 * dur,
            decay: 0.25 * dur,
            sustain: 0.5 * dur,
            release: 0.1 * dur
        });
        Self::new_notes(full_notes, instrument, time_gap)
    }

    pub fn new_notes(notes: impl IntoIterator<Item=Note>, instrument: impl AudioUnit + 'static, time_gap: f64) -> Self {
        Texture { notes: notes.into_iter().collect(), instrument: unit::<U1, U1>(Box::new(instrument)), time_gap }
    }

    pub fn adjust_depth(&mut self, depth: usize) {
        let octave = (depth as f64 / 2.5).powf(0.5).ceil() as i32 + 1;
        self.notes = self.notes.iter().map(|n| Note {note: n.note + 12 * octave, ..*n}).collect()
    }
}

impl SoundGenerator for Texture {
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
        let mut elapsed = 0.0;
        while elapsed < duration {
            let note = self.notes.choose(&mut rng()).unwrap();
            // let note = self.notes[random_range(0..self.notes.len())];
            let instr = note.with_instrument(self.instrument.clone());
            let instr = instr >> split() >> (mul(2.0_f32.powf(lean)) | (mul(2.0_f32.powf(-lean))));
            seq.push_duration(start_time + elapsed, note.time, Fade::Smooth, 0.0, 0.0, Box::new(instr));
            elapsed += self.time_gap;
        }
    }

    fn base_duration(&self) -> f64 {
        self.time_gap
    }
}

pub struct EffectMel<X: AudioNode<Inputs=U1, Outputs=U1> + 'static> {
    body: Melody,
    effect: An<X>
}

impl<X: AudioNode<Inputs=U1, Outputs=U1> + 'static> EffectMel<X> {
    pub fn new(body: Melody, effect: An<X>) -> Self {
        EffectMel { body, effect }
    }
}

impl<X: AudioNode<Inputs=U1, Outputs=U1> + 'static> SoundGenerator for EffectMel<X> {
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
        if self.body.duration() <= 0.0 {
            println!("No duration for melody! {:?}", self.body.notes);
            // println!("{}", std::backtrace::Backtrace::capture());
            return
        }
        let mut elapsed = 0.0;
        let ratio = duration / self.body.duration();
        for (note, dur) in self.body.notes.iter() {
            let dur = dur * ratio;
            let note = note.mul_duration(ratio).adjust_pitch(self.body.note_adjust);
            let instr = note.with_instrument(self.body.instrument.clone());
            let instr = instr >> split() >> (mul(2.0_f32.powf(lean)) | (mul(2.0_f32.powf(-lean))));
            let instr = instr >> (self.effect.clone() | self.effect.clone());
            seq.push_duration(start_time + elapsed, dur, Fade::Power, 0.0, 0.0, Box::new(instr));
            elapsed += dur;
        }
        assert!(elapsed > 0.0, "Melody must cause time to pass!")
    }

    fn base_duration(&self) -> f64 {
        self.body.base_duration()
    }
}

/// Applies an audio effect to any Sequenceable.
/// Unfortunately, given the genericity of Sequenceables this drastically harms performance.
/// [EffectMel] is a performant version when the Sequenceable is a Melody specifically.
pub struct EffectSeq<T, X>
    where
        T: SoundGenerator,
        X: AudioNode<Inputs=U1, Outputs=U1>
{
    body: T,
    effect: An<X>
}

impl<T, X> EffectSeq<T, X>
    where
        T: SoundGenerator,
        X: AudioNode<Inputs=U1, Outputs=U1>
{
    pub fn new(body: T, effect: An<X>) -> Self {
        EffectSeq {
            body,
            effect
        }
    }
}

impl<T, X> SoundGenerator for EffectSeq<T, X>
    where
        T: SoundGenerator,
        X: AudioNode<Inputs=U1, Outputs=U1> + 'static
{
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
        // self.body.sequence(seq, start_time, duration, lean);
        let mut new_seq = ConfigSequencer::new(Sequencer::new(false, 2), seq.live);
        self.body.sequence(&mut new_seq, 0.0, duration, lean);
        let effected = unit::<U0, U2>(Box::new(new_seq.seq)) >> (self.effect.clone() | self.effect.clone());
        let fade_out = 1.0.min(duration * 0.2);
        seq.push_duration(start_time, duration + fade_out, Fade::Smooth, 0.0, fade_out, Box::new(effected));
    }

    fn base_duration(&self) -> f64 {
        self.body.base_duration()
    }
}

#[derive(Component)]
pub struct MelodyAsync {
    pub notes: Arc<Mutex<[i32; FS_MEL_SIZE]>>,
    pub timings: Arc<Mutex<[f64; FS_MEL_SIZE]>>,
    pub instrument: Arc<Mutex<An<Unit<U1, U1>>>>,
    pub effect: Arc<Mutex<An<Unit<U1, U1>>>>,
    // pub body: Arc<Mutex<Melody>>,
    pub depth: usize,
}

// impl MelodyAsync {
    // pub fn new(body: Melody, depth: usize) -> Self {
    //     Self {
    //         body: Arc::new(Mutex::new(body)),
    //         depth
    //     }
    // }

    // fn new(notes: [i32; FS_MEL_SIZE], timings: [f64; FS_MEL_SIZE], instrument: impl AudioUnit + 'static, effect: impl AudioUnit + 'static) -> Self {
    //     Self {
    //         notes: Arc::new(Mutex::new(notes)),
    //         timings: Arc::new(Mutex::new(timings)),
    //         instrument: Arc::new(Mutex::new(unit(Box::new(instrument)))),
    //         effect: Arc::new(Mutex::new(unit(Box::new(effect)))),
    //     }
    //     // (notes, unit(Box::new(instrument)), unit(Box::new(effect)))
    // }

    // pub fn new_depth(body: Arc<Mutex<Melody>>, depth: usize) -> Self {
    //     Self { body, depth }
    // }
// }

impl SoundGenerator for MelodyAsync {
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
        // let x = self.body.lock().unwrap();
        // let mut mel = x.clone();
        // mel.adjust_depth(self.depth);
        // mel.sequence(seq, start_time, duration, lean)
        let notes = *self.notes.lock().unwrap();
        let timings = *self.timings.lock().unwrap();
        let instrument = self.instrument.lock().unwrap().clone() >> self.effect.lock().unwrap().clone();
        let mut mel = Melody::new_even(instrument, &notes);
        mel.adjust_timings(&timings).unwrap();
        mel.adjust_depth(self.depth);
        mel.sequence(seq, start_time, duration, lean);
    }

    fn base_duration(&self) -> f64 {
        FS_MEL_SIZE as f64
        // let x = self.body.lock().unwrap();
        // x.base_duration()
    }
}

// pub struct AsyncEffectMel<X: AudioNode<Inputs=U1, Outputs=U1> + 'static> {
//     mel: Rc<RefCell<EffectMel<X>>>
// }

// impl<X: AudioNode<Inputs=U1, Outputs=U1> + 'static> AsyncEffectMel<X> {
//     pub fn new(melody: EffectMel<X>) -> Self {
//         Self {
//             mel: Rc::new(RefCell::new(melody))
//         }
//     }

//     pub fn melody(&self) -> Melody {
//         self.mel.borrow().body.clone()
//     }

//     pub fn set_melody(&mut self, melody: Melody) {
//         self.mel.borrow_mut().body = melody;
//     }

//     pub fn with_effect<T>(&self, effect: T) -> AsyncEffectMel<T> 
//         where
//             T: AudioNode<Inputs=U1, Outputs=U1>
//     {
//         let old = self.mel.borrow();
//         let new = EffectMel::new(old.body.clone(), An(effect));
//         AsyncEffectMel::new(new)
//     }

//     // pub fn adjust_timings(&self, timings: &[f64]) -> Result<(), SoundproofError> {
//     //     self.mel.borrow_mut().adjust_timings(timings)
//     // }

//     // pub fn adjust_depth(&self, depth: usize) {
//     //     self.mel.borrow_mut().adjust_depth(depth);
//     // }
// }

// impl<X> SoundGenerator for AsyncEffectMel<X>
//     where
//         X: AudioNode<Inputs=U1, Outputs=U1>
// {
//     fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
//         self.mel.borrow().sequence(seq, start_time, duration, lean);
//     }

//     fn base_duration(&self) -> f64 {
//         self.mel.borrow().base_duration()
//     }
// }

#[derive(Debug, Clone, Copy, PartialEq, Component)]
pub struct Timings {
    pub start: f64,
    pub duration: f64,
    pub lean: f32,
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
    Sound(Arc<dyn SoundGenerator + Send + Sync>, TreeMetadata)
}

pub static SIGN: AtomicU32 = AtomicU32::new(0);

#[derive(Clone, Debug, PartialEq, Component)]
pub struct TreeMetadata {
    pub name: String,
    // pub parent: String,
    pub base_color: Color,
    pub alt_color: Color,
    pub max_depth: usize,
    // pub lean: f32,
}

impl SoundTree {
    /// Constructs a SoundTree containing a single sound-pattern.
    pub fn sound(sound: impl SoundGenerator + Send + Sync + 'static, meta: TreeMetadata) -> Self {
        Self::Sound(Arc::new(sound), meta)
    }

    /// Constructs a SoundTree which plays its subtrees one after another.
    pub fn seq(subtrees: impl IntoIterator<Item=SoundTree>) -> Self {
        // avoids nested Seqs; this can affect duration assignments.
        // subject to change but I think I like it
        let mut result = Vec::new();
        // let mut names = "".to_owned();
        // let mut names = "[".to_owned();
        let mut max_depth = 0;
        for val in subtrees {
            match val.clone() {
                SoundTree::Seq(mut trees, meta) => { 
                    // names += &meta.name;
                    // names += ";";
                    if meta.max_depth > max_depth {
                        max_depth = meta.max_depth
                    }
                    result.append(&mut trees);
                }
                other => {
                    // names += &other.metadata().name;
                    // names += ";";
                    if other.metadata().max_depth > max_depth {
                        max_depth = other.metadata().max_depth
                    }
                    result.push(other);
                },
            }
        }
        if result.len() == 1 {
            result.pop().unwrap()
        }
        else {
            Self::Seq(result, TreeMetadata { 
                name: "".to_owned(),
                // parent: "".to_owned(),
                base_color: Color::MAROON, 
                alt_color: Color::MAROON, 
                max_depth 
            })
        }
        // names += "]";
    }

    /// Constructs a SoundTree which plays its subtrees simultaneously.
    pub fn simul(subtrees: impl IntoIterator<Item=SoundTree>) -> Self {
        // avoids redundant nested Simuls; this cannot affect the resulting audio
        let mut result = Vec::new();
        // let mut names = "{".to_owned();
        let mut max_depth = 0;
        for val in subtrees {
            match val.clone() {
                SoundTree::Simul(mut trees, meta) => {
                    // names += &meta.name;
                    // names += "||";
                    if meta.max_depth > max_depth {
                        max_depth = meta.max_depth
                    }
                    result.append(&mut trees);
                    
                }
                other => {
                    // names += &other.metadata().name;
                    // names += "||";
                    if other.metadata().max_depth > max_depth {
                        max_depth = other.metadata().max_depth
                    }
                    result.push(other)
                }
            }
        }
        // names += "}";
        if result.len() == 1 {
            result.pop().unwrap()
        }
        else {
            Self::Simul(result, TreeMetadata { 
                name: "".to_owned(),
                // parent: "".to_owned(),
                base_color: Color::MAROON,
                alt_color: Color::MAROON,
                max_depth
            })
        }
    }

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
            SoundTree::Simul(vec, _) => 1 + vec.iter().map(|x| x.size()).max().unwrap_or(0),
            SoundTree::Seq(vec, _) => vec.iter().map(|x| x.size()).sum::<usize>(),
            SoundTree::Sound(_, _) => 1,
        }
    }

    // pub fn max_depth(&self) -> usize {
    //     match self {
    //         SoundTree::Simul(vec, _) => {
    //             let result = 1 + vec.iter().map(|x| x.max_depth()).max().unwrap_or(0);
    //             println!("{}{result} ({})", vec!["\t"; 5 - result].join(""), vec.len());
    //             result
    //         },
    //         SoundTree::Seq(vec, _) => vec.iter().map(|x| x.max_depth()).max().unwrap_or(0),
    //         SoundTree::Sound(_, _) => 1,
    //     }
    // }

    /// The weight of the tree for duration scaling.
    pub fn weight(&self, exp: f64) -> f64 {
        match self {
            SoundTree::Simul(vec, _) => 1.0 + vec.iter().map(|x| x.subtree_weight(exp)).reduce(f64::max).unwrap_or(0.0),
            SoundTree::Seq(vec, _) => vec.iter().map(|x| x.subtree_weight(exp)).sum::<f64>(),
            SoundTree::Sound(_, _) => 1.0,
        }
    }

    /// The weight of the tree, considered as a subtree, for duration scaling.
    pub fn subtree_weight(&self, exp: f64) -> f64 {
        self.weight(exp).powf(exp)
    }

    pub fn sound_times(&self, sound_time: f64, duration: f64, scaling: DivisionMethod, lean: f32) -> Vec<(Arc<dyn SoundGenerator + Send + Sync>, Timings, TreeMetadata)> {
        if duration <= 2.0 / crate::music::SAMPLE_RATE as f64 {
            // println!("Warning! No duration from start time {start_time}");
            return vec![];
        }
        // if duration > 10.0 {
        //     println!("{}", self.metadata().name.len())
        // }
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
                let mut output = Vec::new();
                for elem in vec {
                    let local_lean = (base_lean - dir * elem.size() as f32) * 0.8 / scale; // can't divide by 0 bc if scale is 0 vec is empty
                    // println!("local lean: {local_lean}, {base_lean}, {scale}: {}, {}", self.size(), vec.len());
                    // output.push((start_time, duration, scaling, lean + local_lean));
                    output.append(&mut elem.sound_times(sound_time, duration, scaling, lean + local_lean));
                }
                output
            },
            SoundTree::Seq(vec, _) => {
                // let child_count = vec.len();
                let mut output = Vec::new();
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
                    output.append(&mut child.sound_times(sound_time + time_elapsed, new_time, scaling, lean));
                    time_elapsed += new_time;
                }
                output
            },
            SoundTree::Sound(sound, meta) => {
                let timings = Timings {
                    start: sound_time,
                    duration,
                    lean,
                };
                vec![(Arc::clone(sound), timings, meta.clone())]
                // sound.sequence(seq, start_time, duration, lean),
            }
        }
    }

    /// Generate audio into a [Sequencer] for the tree, distributing subtree durations by the selected [scaling](Scaling).
    pub fn generate_with(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, scaling: DivisionMethod, lean: f32) {
        if duration <= 2.0 / crate::music::SAMPLE_RATE as f64 {
            // println!("Warning! No duration from start time {start_time}");
            return;
        }
        // if duration > 10.0 {
        //     println!("{}", self.metadata().name.len())
        // }
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
                    // println!("local lean: {local_lean}, {base_lean}, {scale}: {}, {}", self.size(), vec.len());
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
