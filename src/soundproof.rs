#![allow(unused)]
use crate::Scaling;
use crate::music::{instruments::*, notes::*};
use crate::lambdapi::ast::*;
use fundsp::hacker32::*;

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

}
impl Sequenceable for Melody {
    fn sequence_full(&self, seq: &mut Sequencer, start_time: f64, loop_duration: f64, total_duration: f64) {
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
                seq.push_duration(start_time + elapsed, dur, Fade::Power, 0.0, 0.0, Box::new(instr));
                elapsed += dur;
                if elapsed >= total_duration {
                    return;
                }
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

pub struct SoundTreeScaling(pub SoundTree, pub Scaling);

impl Sequenceable for SoundTreeScaling {
    fn sequence_full(&self, seq: &mut Sequencer, start_time: f64, loop_duration: f64, total_duration: f64) {
        self.0.generate_with(start_time, total_duration, loop_duration, seq, self.1);
    }
}

//this is a separate function outside of translate despite doing the same match
//so we can make multiple versions and switch them out easily
//without having to build infrastructure for config files
//can't parameterize without boxing all the instruments so we'll leave it at this
pub fn imelody1(term: &ITerm, depth: usize) -> Melody {
    let mut result = match term {
        ITerm::Ann(_, _) => Melody::new_even(violinish(), &[C, A, B, A]),
        ITerm::Star => Melody::new_even(wobbly_sine(), &[D, F, B, A]),
        ITerm::Pi(_, _) => Melody::new_even(sinesaw(), &[A, ASHARP, D, D]),
        ITerm::Bound(n) => Melody::new_even(sinesaw() >> split() >> fbd(*n as f32 / 10.0, -5.0), &[E, E, C, B]),
        ITerm::Free(_) => Melody::new_even(karplus(), &[E, G, A, B]),
        ITerm::App(_, _) => Melody::new_even(pink_sine(), &[B, C, D, E]),
        ITerm::Nat => Melody::new_even(sawfir(), &[C, B, B, A]),
        ITerm::Zero => Melody::new_even(fm_basic(), &[B, C, E, A]),
        ITerm::Succ(_) => Melody::new_even(violinish(), &[C, D, D, A]),
        ITerm::Fin(_) => Melody::new_even(violinish(), &[D, D, D, G]),
        ITerm::FZero(_) => Melody::new_even(pink_sine(), &[C, F, C, G]),
        ITerm::FSucc(_, _) => Melody::new_even(sinesaw(), &[F, B, B, B]),
        _ => panic!("{term} not implemented")
    };
    adjust_depth(&mut result, depth);
    result
}

pub fn imelody2(term: &ITerm, depth: usize) -> Melody {
    let mut result = match term {
        ITerm::Ann(_, _) => Melody::new_even(violinish(), &[C, E, B, E]),
        ITerm::Star => Melody::new_even(wobbly_sine(), &[G, E, C, B]),
        ITerm::Pi(_, _) => Melody::new_even(sinesaw(), &[E, C, G, E]),
        ITerm::Bound(n) => Melody::new_even(sinesaw() >> split() >> fbd(*n as f32 / 10.0, -5.0), &[E, E, C, B]),
        ITerm::App(_, _) => Melody::new_even(pink_sine(), &[B, C, G, E]),
        ITerm::Zero => Melody::new_even(fm_basic(), &[B, C, E, G]),
        ITerm::Fin(_) => Melody::new_even(violinish(), &[E, B, G, C]),
        _ => panic!("{term} not implemented")
    };
    adjust_depth(&mut result, depth);
    result
}

pub fn cmelody2(term: &CTerm, depth: usize) -> Melody {
    let mut result = match term {
        CTerm::Inf(_) => Melody::new_even(sine(), &[G, B, E, C]), //value will never be used
        CTerm::Lam(_) => Melody::new_even(fm_epi(), &[G, B, E, C]),
    };
    adjust_depth(&mut result, depth);
    result
}

pub fn imelody3(term: &ITerm, depth: usize) -> Melody {
    let mut result = match term {
        ITerm::Ann(_, _) => Melody::new_even(violinish(), &[A, B, C, B]),
        ITerm::Star => Melody::new_even(wobbly_sine(), &[A, C, A, B]),
        ITerm::Pi(_, _) => Melody::new_even(sinesaw(), &[C, C, B, A]),
        ITerm::Bound(n) => Melody::new_even(sinesaw() >> split() >> fbd(*n as f32 / 10.0, -5.0), &[E, E, C, B]),
        ITerm::App(_, _) => Melody::new_even(pink_sine(), &[B, C, G, E]),
        ITerm::Zero => Melody::new_even(fm_basic(), &[B, C, E, G]),
        ITerm::Fin(_) => Melody::new_even(violinish(), &[E, B, G, C]),
        _ => panic!("{term} not implemented")
    };
    adjust_depth(&mut result, depth);
    result
}

pub fn cmelody3(term: &CTerm, depth: usize) -> Melody {
    let mut result = match term {
        CTerm::Inf(_) => Melody::new_even(sine(), &[]), //value will never be used
        CTerm::Lam(_) => Melody::new_even(fm_epi(), &[G, B, E, C]),
    };
    adjust_depth(&mut result, depth);
    result
}

pub fn imelody_oneinstr(instrument: impl AudioUnit + 'static, term: &ITerm, depth: usize) -> Melody {
    let mut result = match term {
        ITerm::Ann(_, _) => Melody::new_even(instrument, &[C, E, B, E]),
        ITerm::Star => Melody::new_even(instrument, &[G, E, C, B]),
        ITerm::Pi(_, _) => Melody::new_even(instrument, &[E, C, G, E]),
        ITerm::Bound(_) => Melody::new_even(instrument, &[E, E, C, B]),
        ITerm::App(_, _) => Melody::new_even(instrument, &[B, C, G, E]),
        ITerm::Zero => Melody::new_even(instrument, &[B, C, E, G]),
        ITerm::Fin(_) => Melody::new_even(instrument, &[E, B, G, C]),
        _ => panic!("{term} not implemented")
    };
    adjust_depth(&mut result, depth);
    result
}

pub fn cmelody_oneinstr(instrument: impl AudioUnit + 'static, term: &CTerm, depth: usize) -> Melody {
    let mut result = match term {
        CTerm::Inf(_) => Melody::new_even(sine(), &[A, A, A, A]), //value will never be used
        CTerm::Lam(_) => Melody::new_even(instrument, &[G, B, E, C]),
    };
    adjust_depth(&mut result, depth);
    result
}

fn adjust_depth(mel: &mut Melody, depth: usize) {
    mel.set_octave((depth as f64 / 2.5).sqrt().ceil() as i8 + 1);
    mel.map_notes(|n| Note {
        time: n.time * lerp(0.25, 0.95, 1.0 / depth as f32) as f64,
        attack: 0.2 / (depth as f32 + 0.1),
        sustain: lerp(0.25, 0.5, 1.0 / depth as f32),
        ..*n
    });
}

pub fn itranslate(term: ITerm, depth: usize) -> SoundTree {
    //to customize the sound we edit this to change the function that produces the melody
    //if we wanted more customization I think we'd need to pass in files
    let mel = imelody_oneinstr((sinesaw() ^ pass() | constant(1.0)) >> lowpass(), &term, depth);
    match term {
        ITerm::Ann(cterm, cterm1) => SoundTree::new(mel, vec![ctranslate(cterm, depth + 1), ctranslate(cterm1, depth + 1)]),
        ITerm::Star => SoundTree::new(mel, vec![]),
        ITerm::Pi(cterm, cterm1) => SoundTree::new(mel, vec![ctranslate(cterm, depth + 1), ctranslate(cterm1, depth + 1)]),
        ITerm::Bound(_) => SoundTree::new(mel, vec![]),
        ITerm::Free(_) => SoundTree::new(mel, vec![]),
        ITerm::App(iterm, cterm) => SoundTree::new(mel, vec![itranslate(*iterm, depth + 1), ctranslate(cterm, depth + 1)]),
        ITerm::Nat => SoundTree::new(mel, vec![]),
        ITerm::Zero => SoundTree::new(mel, vec![]),
        ITerm::Succ(cterm) => SoundTree::new(mel, vec![ctranslate(cterm, depth + 1)]),
        ITerm::NatElim(motive, base, ind, k) => SoundTree::new(mel, vec![
            ctranslate(motive, depth + 1),
            ctranslate(base, depth + 1),
            ctranslate(ind, depth + 1),
            ctranslate(k, depth + 1),
        ]),
        ITerm::Fin(cterm) => SoundTree::new(mel, vec![ctranslate(cterm, depth + 1)]),
        ITerm::FinElim(motive, base, ind, n, f) => SoundTree::new(mel, vec![
            ctranslate(motive, depth + 1),
            ctranslate(base, depth + 1),
            ctranslate(ind, depth + 1),
            ctranslate(n, depth + 1),
            ctranslate(f, depth + 1),
        ]),
        ITerm::FZero(cterm) => SoundTree::new(mel, vec![ctranslate(cterm, depth + 1)]),
        ITerm::FSucc(cterm, cterm1) => SoundTree::new(mel, vec![ctranslate(cterm, depth + 1), ctranslate(cterm1, depth + 1)]),
    }
}

pub fn ctranslate(term: CTerm, depth: usize) -> SoundTree {
    //to customize the sound we edit this to change function that produces the melody
    let mel = cmelody_oneinstr(sinesaw(), &term, depth);
    match term {
        CTerm::Inf(iterm) => itranslate(*iterm, depth),
        CTerm::Lam(cterm) => SoundTree::new(mel, vec![ctranslate(*cterm, depth + 1)]),
    }
}

// fn depth_amp(incr: f32, depth: usize) -> An<impl AudioNode<Inputs=U0, Outputs=U1>> {
//     onepress(incr * lerp(0.25, 0.95, 1.0 / depth as f32), incr) >>
//         adsr_live(incr * 0.2 / (depth as f32 + 0.1), incr * 0.25, lerp(0.25, 0.5, 1.0 / depth as f32), incr * 0.1)
// }