use fundsp::prelude32::*;
use rand::rng;
use rand::seq::IndexedRandom;
use std::array;
use std::path::Path;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Mutex};

use crate::lambdapi::ast::Tag;
use crate::music::instruments::*;
use crate::music::notes::*;
use crate::music::stretch::{retime_pitch_wave, retime_wave};
use crate::soundproof::select::FS_MEL_SIZE;
use crate::{DivisionMethod, types::*};

pub static SIGN: AtomicU32 = AtomicU32::new(0);

/// Objects that can be used to generate audio output through a FunDSP [Sequencer].
pub trait SoundGenerator {
    /// Generate audio into the sequencer for a duration starting at the selected time.
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32);

    /// Duration of the SoundGenerator prior to stretching (relevant for audio clips & looping)
    fn base_duration(&self) -> f64;
}

impl<T: ?Sized> SoundGenerator for Arc<T>
where
    T: SoundGenerator,
{
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
        self.as_ref().sequence(seq, start_time, duration, lean);
    }

    fn base_duration(&self) -> f64 {
        self.as_ref().base_duration()
    }
}

// impl<T: ?Sized> SoundGenerator for Rc<T>
// where
//     T: SoundGenerator,
// {
//     fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
//         self.as_ref().sequence(seq, start_time, duration, lean);
//     }

//     fn base_duration(&self) -> f64 {
//         self.as_ref().base_duration()
//     }
// }

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
        let instr = playwave(&wave_arc, 0, None)
            >> split()
            >> (mul(2.0_f32.powf(lean)) | mul(2.0_f32.powf(-lean)));
        seq.push_duration(
            start_time,
            duration,
            Fade::Smooth,
            0.0,
            0.0,
            Box::new(instr),
        );
    }

    fn base_duration(&self) -> f64 {
        self.wave.duration()
    }
}

impl SoundGenerator for Wave {
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
        let scaled = retime_wave(self, duration);
        let wave_arc = Arc::new(scaled);
        let instr = playwave(&wave_arc, 0, None)
            >> split()
            >> (mul(2.0_f32.powf(lean)) | mul(2.0_f32.powf(-lean)));
        seq.push_duration(
            start_time,
            duration,
            Fade::Smooth,
            0.0,
            0.0,
            Box::new(instr),
        );
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
        Loop {
            loop_duration: body.base_duration(),
            body,
        }
    }

    pub fn new_dur(body: T, duration: f64) -> Self {
        Loop {
            body,
            loop_duration: duration,
        }
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

    pub fn with_instrument(
        self,
        instrument: An<impl AudioNode<Inputs = U1, Outputs = U1> + 'static>,
    ) -> An<impl AudioNode<Inputs = U0, Outputs = U1>> {
        let hz = constant(get_hz(self.note));
        let vol_control = envelope(move |t| if t < self.time as f32 { 1.0 } else { 0.0 })
            >> (adsr_live(self.attack, self.decay, self.sustain, self.release) * self.volume);
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
            notes: notes
                .iter()
                .map(|x| {
                    (
                        Note {
                            note: *x,
                            time: 0.85,
                            volume: 1.0,
                            attack: 0.25,
                            decay: 0.25,
                            sustain: 0.5,
                            release: 0.1,
                        },
                        1.0,
                    )
                })
                .collect(),
            note_adjust: 0,
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
            notes: notes
                .iter()
                .map(|(x, t)| {
                    (
                        Note {
                            note: *x,
                            time: 0.85 * t,
                            volume: 1.0,
                            attack: 0.25,
                            decay: 0.25,
                            sustain: 0.5,
                            release: 0.1,
                        },
                        *t,
                    )
                })
                .collect(),
            note_adjust: 0,
        }
    }

    /// Create a melody from a sequence of [Note] objects with specified durations.
    pub fn new_notes(instrument: impl AudioUnit + 'static, notes: &[(Note, f64)]) -> Self {
        Melody {
            instrument: unit(Box::new(instrument)),
            notes: notes.to_vec(),
            note_adjust: 0,
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
        self.notes = self
            .notes
            .iter_mut()
            .enumerate()
            .map(|(i, pair)| f(i, pair))
            .collect();
    }

    /// Concatenate two melodies, preserving pitch.
    pub fn append(&mut self, other: Self) {
        self.notes.append(
            &mut other
                .notes
                .into_iter()
                .map(|(note, dur)| {
                    (
                        Note {
                            note: note.note + 12 * (other.note_adjust - self.note_adjust),
                            ..note
                        },
                        dur,
                    )
                })
                .collect(),
        );
    }

    pub fn replace_instrument(&mut self, other: &Self) {
        self.instrument = other.instrument.clone();
        // self.notes = other.notes.clone();
        // self.note_adjust = other.note_adjust;
    }

    pub fn adjust_timings(&mut self, timings: &[f64]) -> Result<(), SoundproofError> {
        if self.notes.len() != timings.len() {
            return Err(SoundproofError::Mismatch);
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
        self.map_notes(|&(n, d)| {
            (
                Note {
            time: n.time * lerp(0.45, 0.95, 1.0 / depth as f32) as f64,
            // time: n.time * lerp(0.95, 0.45, 1.0 / depth as f32) as f64,
            attack: 0.2 / (depth as f32 + 0.1),
            sustain: lerp(0.25, 0.5, 1.0 / depth as f32),
            volume: /*0.000035*/ 0.6 * (1.0 + 0.02 * depth as f32),
            ..n
        },
                d,
            )
        });
    }
}

impl SoundGenerator for Melody {
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
        if self.duration() <= 0.0 {
            println!("No duration for melody! {:?}", self.notes);
            // println!("{}", std::backtrace::Backtrace::capture());
            return;
        }
        // println!("lean: {lean}, {}, {}", 2.0_f32.powf(lean), 2.0_f32.powf(-lean));
        let mut elapsed = 0.0;
        let ratio = duration / self.duration();
        for (note, dur) in self.notes.iter() {
            let dur = dur * ratio;
            let note = note.mul_duration(ratio).adjust_pitch(self.note_adjust);
            let instr = note.with_instrument(self.instrument.clone());
            let instr = instr >> split() >> (mul(2.0_f32.powf(lean)) | (mul(2.0_f32.powf(-lean))));
            seq.push_duration(
                start_time + elapsed,
                dur,
                Fade::Power,
                0.0,
                0.0,
                Box::new(instr),
            );
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
    pub fn new_even(
        notes: impl IntoIterator<Item = i32>,
        instrument: impl AudioUnit + 'static,
        time_gap: f64,
    ) -> Self {
        let dur = (time_gap * 0.25) as f32;
        let full_notes = notes.into_iter().map(|n| Note {
            note: n,
            volume: 1.0,
            time: dur as f64,
            attack: 0.25 * dur,
            decay: 0.25 * dur,
            sustain: 0.5 * dur,
            release: 0.1 * dur,
        });
        Self::new_notes(full_notes, instrument, time_gap)
    }

    pub fn new_notes(
        notes: impl IntoIterator<Item = Note>,
        instrument: impl AudioUnit + 'static,
        time_gap: f64,
    ) -> Self {
        Texture {
            notes: notes.into_iter().collect(),
            instrument: unit::<U1, U1>(Box::new(instrument)),
            time_gap,
        }
    }

    pub fn adjust_depth(&mut self, depth: usize) {
        let octave = (depth as f64 / 2.5).powf(0.5).ceil() as i32 + 1;
        self.notes = self
            .notes
            .iter()
            .map(|n| Note {
                note: n.note + 12 * octave,
                ..*n
            })
            .collect()
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
            seq.push_duration(
                start_time + elapsed,
                note.time,
                Fade::Smooth,
                0.0,
                0.0,
                Box::new(instr),
            );
            elapsed += self.time_gap;
        }
    }

    fn base_duration(&self) -> f64 {
        self.time_gap
    }
}

pub struct EffectMel<X: AudioNode<Inputs = U1, Outputs = U1> + 'static> {
    body: Melody,
    effect: An<X>,
}

impl<X: AudioNode<Inputs = U1, Outputs = U1> + 'static> EffectMel<X> {
    pub fn new(body: Melody, effect: An<X>) -> Self {
        EffectMel { body, effect }
    }
}

impl<X: AudioNode<Inputs = U1, Outputs = U1> + 'static> SoundGenerator for EffectMel<X> {
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
        if self.body.duration() <= 0.0 {
            println!("No duration for melody! {:?}", self.body.notes);
            // println!("{}", std::backtrace::Backtrace::capture());
            return;
        }
        let mut elapsed = 0.0;
        let ratio = duration / self.body.duration();
        for (note, dur) in self.body.notes.iter() {
            let dur = dur * ratio;
            let note = note.mul_duration(ratio).adjust_pitch(self.body.note_adjust);
            let instr = note.with_instrument(self.body.instrument.clone());
            let instr = instr >> split() >> (mul(2.0_f32.powf(lean)) | (mul(2.0_f32.powf(-lean))));
            let instr = instr >> (self.effect.clone() | self.effect.clone());
            seq.push_duration(
                start_time + elapsed,
                dur,
                Fade::Power,
                0.0,
                0.0,
                Box::new(instr),
            );
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
    X: AudioNode<Inputs = U1, Outputs = U1>,
{
    body: T,
    effect: An<X>,
}

impl<T, X> EffectSeq<T, X>
where
    T: SoundGenerator,
    X: AudioNode<Inputs = U1, Outputs = U1>,
{
    pub fn new(body: T, effect: An<X>) -> Self {
        EffectSeq { body, effect }
    }
}

impl<T, X> SoundGenerator for EffectSeq<T, X>
where
    T: SoundGenerator,
    X: AudioNode<Inputs = U1, Outputs = U1> + 'static,
{
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
        // self.body.sequence(seq, start_time, duration, lean);
        let mut new_seq = ConfigSequencer::new(Sequencer::new(0, 2, ReplayMode::None), seq.live);
        self.body.sequence(&mut new_seq, 0.0, duration, lean);
        let effected =
            unit::<U0, U2>(Box::new(new_seq.seq)) >> (self.effect.clone() | self.effect.clone());
        let fade_out = 1.0.min(duration * 0.2);
        seq.push_duration(
            start_time,
            duration + fade_out,
            Fade::Smooth,
            0.0,
            fade_out,
            Box::new(effected),
        );
    }

    fn base_duration(&self) -> f64 {
        self.body.base_duration()
    }
}

// #[derive(Component)]
pub struct MelodyAsync {
    pub notes: Arc<Mutex<[i32; FS_MEL_SIZE]>>,
    pub timings: Arc<Mutex<[f64; FS_MEL_SIZE]>>,
    pub instrument: Arc<Mutex<An<Unit<U1, U1>>>>,
    pub effect: Arc<Mutex<An<Unit<U1, U1>>>>,
    // pub body: Arc<Mutex<Melody>>,
    pub depth: usize,
}

impl SoundGenerator for MelodyAsync {
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
        // let x = self.body.lock().unwrap();
        // let mut mel = x.clone();
        // mel.adjust_depth(self.depth);
        // mel.sequence(seq, start_time, duration, lean)
        let notes = *self.notes.lock().unwrap();
        let timings = *self.timings.lock().unwrap();
        let instrument =
            self.instrument.lock().unwrap().clone() >> self.effect.lock().unwrap().clone();
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

#[derive(Clone)]
pub struct Toner {
    pub instrument: An<Unit<U1, U1>>,
    pub start_time: f64,
    pub duration: f64,
}

impl Toner {
    pub fn new(instr: impl AudioUnit + 'static) -> Self {
        Self {
            instrument: unit(Box::new(instr)),
            start_time: 0.0,
            duration: 10.0,
        }
    }
}

impl SoundGenerator for Toner {
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, _lean: f32) {
        SIGN.fetch_add(1, Ordering::Relaxed);
        let center = start_time + duration * 0.5;
        let freq = center as f32 * 1.5 + 100.;
        let factor = 8e-4;
        let modified_instrument =
            (constant(freq) >> self.instrument.clone()) >> split::<U2>() * factor;
        // if self.start_time > 11.0 {
        //     println!("{}", self.start_time)
        // }
        // let fade_duration = self.duration / 50.;
        let fade_duration = 0.;
        seq.push_duration(
            self.start_time,
            self.duration,
            Fade::Power,
            fade_duration,
            fade_duration,
            Box::new(modified_instrument),
        );
    }

    fn base_duration(&self) -> f64 {
        1.0
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Weights {
    body: [f32; Self::LENGTH],
}

impl Weights {
    const LENGTH: usize = 10;

    pub fn instrument(&self) -> An<impl AudioNode<Inputs = U2, Outputs = U2> + use<>> {
        let phase: f32 = self.body.into_iter().sum();
        let phase = phase.powi(5) / 5.;
        match self.body {
            [a, b, c, d, e, f, g, h, i, j] => {
                multisplit::<U2,U4>() >>
                    // a * sine()
                    // + b * saw()
                    // + c * square()
                    // + d * sinesaw()
                    // + e * sine()
                    // + f * saw()
                    // + g * square()
                    // + h * sinesaw()
                    // + i * sine()
                    // + j * square()
                    (pass()   + saw().phase(phase % 1.0)           >> (a + e + i) * sine())
                    + (pass() + square().phase((phase + 0.5) % 1.0) >> (b + f) * saw())
                    + (pass() + sine()                       >> (d + h) * sinesaw())
                    + (pass() + sine().phase((1.5 * phase) % 1.0)    >> (c + g + j) * square())
                    >> split()
            }
        }
    }

    pub fn insert(&mut self, tag: Tag) {
        match tag {
            Tag::Ann => self.body[0] += 1.,
            Tag::Type => self.body[1] += 1.,
            Tag::Pi => self.body[2] += 1.,
            Tag::App => self.body[3] += 1.,
            Tag::Bound => self.body[4] += 1.,
            Tag::Free => self.body[5] += 1.,
            Tag::Zero => self.body[6] += 1.,
            Tag::Nat => self.body[7] += 1.,
            Tag::Finite => self.body[8] += 1.,
            Tag::Lambda => self.body[9] += 1.,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SmallRatios {
    num: usize,
    denom: usize,
}

impl SmallRatios {
    pub fn new() -> Self {
        SmallRatios { num: 0, denom: 0 }
    }
}

impl Iterator for SmallRatios {
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.num == 0 {
            if self.denom < 4 {
                self.denom += 1;
                return Some(match self.denom {
                    1 => 3. / 2.,
                    2 => 4. / 5.,
                    3 => 2. / 3.,
                    4 => 5. / 4.,
                    _ => unreachable!(),
                });
            } else {
                self.num = 2;
                self.denom = 1;
                return Some(2.);
            }
        }
        let ratio = self.num as f32 / self.denom as f32;
        if self.num <= 1 {
            self.num = self.denom + 1;
            self.denom = 2;
        } else {
            self.num -= 1;
            self.denom += 1;
            if self.num == self.denom {
                self.next();
            }
        }
        Some(ratio)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Buckets<const N: usize> {
    buckets: [Weights; N],
    freqs: [f32; N],
    // min_freq: f32,
    // max_freq: f32,
}

impl<const N: usize> Buckets<N> {
    const MIN_FREQ: f32 = 60.;
    // const N_INV: f32 = 1. / N as f32;

    pub fn empty() -> Self {
        Buckets {
            buckets: [Weights {
                body: [0.0; Weights::LENGTH],
            }; N],
            freqs: [0.0; N], // min_freq: Self::MIN_FREQ,
                             // max_freq: Self::MIN_FREQ + range,
        }
    }

    pub fn from_tree(tree: &SoundTree, freq_min: f32, freq_max: f32, scaling: DivisionMethod) -> Self {
        let mut result = Buckets {
            buckets: [Weights {
                body: [0.0; Weights::LENGTH],
            }; N],
            freqs: array::from_fn(|i| {
                lerp(freq_min, freq_max, i as f32 / N as f32)
            }), // min_freq: Self::MIN_FREQ,
                // max_freq: Self::MIN_FREQ + range,
        };
        // need to distribute into buckets somehow
        // might not be able to be an array anymore but w/e
        tree.distribute(
            0.0,
            N as f64,
            scaling,
            0.0,
            &mut |_, meta, s, r, _| {
                let position = s + r / 2.0;
                let bucket = &mut result.buckets[position.floor() as usize];
                bucket.insert(meta.tag);
                if meta.will_step.is_some() {
                    bucket.insert(meta.tag);
                    // bucket.insert(meta.tag);
                    // bucket.insert(meta.tag);
                }
            },
            &mut |_, _, _, _, _| {},
            &mut |_, _, _, _, _| {},
        );
        result
    }

    pub fn just_fill(&mut self, tree: &SoundTree, center: f32, position: usize, width: f32) {
        // if position != 256 {
        // println!("Filling in {position} width {width} freq {center}");
        // }
        match tree {
            SoundTree::Simul(children, _) => {
                for child in children {
                    self.just_fill(child, center, position, width);
                }
            }
            SoundTree::Seq(children, _) => {
                let count = children.len();
                let mut ratios = Vec::new();
                for r in SmallRatios::new().take(count) {
                    ratios.push(r);
                }
                ratios.sort_by(f32::total_cmp);
                for (ii, (child, ratio)) in children.iter().zip(ratios.iter()).enumerate() {
                    // println!("pos {position} ii {ii} count {count} width {width}");
                    // let s1 = (ii as f32 - count as f32 * 0.5);
                    // println!("s1 {s1}");
                    // println!("s2 {}", s1 / count as f32 * width);
                    let shift = ((ii as f32 - count as f32 * 0.5) / count as f32 * width * 0.5)
                        .floor() as isize;
                    // if shift != 0 {
                    // println!("shift: {shift}");
                    // }
                    let new_pos = position as isize + shift;
                    if new_pos < 0 || new_pos > 512 {
                        println!("Bad position! {new_pos} from {position} + {shift}");
                    }
                    self.just_fill(child, center * ratio, max(new_pos, 0) as usize, width * 0.5)
                }
            }
            SoundTree::Sound(_, meta) => {
                // println!("setting {position}");
                self.buckets[position].insert(meta.tag);
                if self.freqs[position] == 0.0 {
                    // println!("Setting position {position} to frequency {center}");
                    self.freqs[position] = center
                }
            }
        }
    }

    pub fn reverse(mut self) -> Self {
        self.freqs.reverse();
        self
        // let tmp = self.max_freq;
        // self.max_freq = self.min_freq;
        // self.min_freq = tmp;
        // self
    }

    pub fn is_reversed(&self) -> bool {
        self.freqs[N - 1] < self.freqs[0] + self.freqs[N]
    }

    pub fn iter_buckets(&self) -> impl Iterator<Item = (f32, Weights)> {
        // let x = self.min_freq;
        // let y = self.max_freq;
        self.freqs.into_iter().zip(self.buckets.into_iter())
        // .enumerate()
        // .map(move |(ii, weights)| (2.0_f32.powf(lerp(x.log2(), y.log2(), ii as f32 / N as f32)), weights))
        // .map(move |(ii, weights)| (lerp(x.sqrt(), y.sqrt(), ii as f32 / N as f32).powi(2), weights))
        // .map(move |(ii, weights)| (lerp(x, y, ii as f32 / N as f32), weights))
    }
}

impl<const N: usize> SoundGenerator for Buckets<N> {
    fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, _lean: f32) {
        // SIGN.fetch_add(1, Ordering::Relaxed);
        // let center = start_time + duration * 0.5;
        // let freq = center as f32 * 1.5 + 100.;
        // let factor = 8e-4;
        // let modified_instrument = (constant(freq) >> self.instrument.clone()) >> split::<U2>() * factor;
        for (freq, weights) in self.iter_buckets() {
            let instr = (constant(freq) | constant(0.5)) >> weights.instrument();
            let fade_duration = 0.05;
            seq.push_duration(
                start_time,
                duration,
                Fade::Power,
                fade_duration,
                fade_duration,
                Box::new(instr),
            );
        }
    }

    fn base_duration(&self) -> f64 {
        1.0
    }
}

// #[test]
// pub fn test_small_ratios() {
//     let sm = SmallRatios::new();
//     let mut v: Vec<f32> = sm.take(3).collect();
//     v.sort_by(f32::total_cmp);
//     for n in v.iter() {
//         print!("{n} ")
//     }
//     println!();
// }
