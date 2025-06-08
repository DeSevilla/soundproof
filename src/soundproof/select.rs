use std::rc::Rc;

use fundsp::hacker32::*;
use crate::{
    ast::*,
    instruments::*,
    notes::*,
    types::*,
};

pub trait Selector: Clone {
    fn isound(&self, term: &ITerm) -> SoundTree;
    fn csound(&self, term: &CTerm) -> SoundTree;
    fn imerge(&self, term: &ITerm) -> Self;
    fn cmerge(&self, term: &CTerm) -> Self;
}

#[derive(Clone)]
pub struct Stratifier2 {
    instrument: An<Unit<U1, U1>>,
    effect: An<Unit<U1, U1>>,
    rhythm: Vec<f64>,
    depth: usize,
}

impl Stratifier2 {
    pub fn new() -> Self {
        let length = 4;
        Self {
            effect: unit(Box::new(pass())),
            instrument: unit(Box::new(sine())),
            rhythm: vec![1.0 / length as f64; length],
            depth: 1
        }
    }

    pub fn imelody(&self, term: &ITerm) -> Melody {
        let mut mel = match term {
            ITerm::Ann(_, _) => Melody::new_even(violinish(), &[A, D, F, A]),
            ITerm::Star => Melody::new_even(three_equivalents(wobbly_sine()) * 0.7, &[B, C, E, E]),
            ITerm::Pi(_, _) => Melody::new_even(sinesaw() >> split() >> fbd(0.25, -5.0), &[E, C, A, C]),
            ITerm::Bound(_) => Melody::new_even(sine() * 2.0, &[A, E, C, B]),
            ITerm::Free(_) => Melody::new_even(violinish() * 1.1, &[A, A, E, C]),
            ITerm::App(_, _) => Melody::new_even(sawfir() * 0.75, &[B, A, C, B]),
            ITerm::Zero => Melody::new_even(sinesaw(), &[E, F, C, A]),
            ITerm::Fin(_) => Melody::new_even(violinish() * 1.1, &[A, A + 12, E, F]),
            _ => panic!("{term} not implemented")
        };
        // let rhythm = self.rhythm.clone();
        mel.map_indexed(|i, (n, _d)| (*n, self.rhythm[i]));
        mel.adjust_depth(self.depth);
        mel
    }

    pub fn cmelody(&self, term: &CTerm) -> Melody {
        let mut mel = match term {
            CTerm::Lam(_) => Melody::new_even(fm_basic() * 0.28, &[D, F, E, C]),
            _ => unimplemented!()
            // _ => Melody::new_even(sine(), &[G, G, G, G]),  //value will never be used, but we have to call this for ownership reasons
        };
        mel.map_indexed(|i, (n, _d)| (*n, self.rhythm[i]));
        mel.adjust_depth(self.depth);
        mel
    }
}

impl Selector for Stratifier2 {
    fn isound(&self, term: &ITerm) -> SoundTree {
        let mut mel = self.imelody(term);
        mel.instrument = self.instrument.clone();
        let eff = EffectSeq::new(mel, self.effect.clone());
        SoundTree::sound(eff, term)
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        let mut mel = self.cmelody(term);
        mel.instrument = self.instrument.clone();
        let eff = EffectSeq::new(mel, self.effect.clone());
        SoundTree::sound(eff, term.clone())
    }

    fn imerge(&self, term: &ITerm) -> Self {
        let rhythm = match term {
            ITerm::Ann(_, _) => vec![1.0, 0.5, 1.5, 1.0],
            ITerm::Star => vec![0.0, 0.0, 0.0, -1.0], // can never occur as this has no children
            ITerm::Pi(_, _) => vec![0.5, 1.5, 0.5, 1.5],
            // ITerm::Bound(_) => vec![0.0, 0.0, -1.0, 0.0], // can never occur as this has no children
            ITerm::Free(_) => vec![3.0, 0.3, 0.3, 0.4],
            ITerm::App(_, _) => vec![2.5, 0.5, 0.5, 0.5],
            ITerm::Zero => vec![-1.0, 0.0, 0.0, 0.0], // can never occur as this has no children
            ITerm::Fin(_) => vec![0.7, 0.7, 2.1, 0.5],
            _ => unimplemented!()
        };
        let effect = match term {
            ITerm::Ann(_, _) => unit(Box::new((pass() | (800.0 + 100.0 * sine_hz(2.0)) | constant(1.0)) >> highpass())),
            ITerm::Star => unit(Box::new(shape(Clip(100.0)))),
            ITerm::Pi(_, _) => unit(Box::new(split() >> fbd(0.25, -1.5))),
            ITerm::Bound(_) => unit(Box::new(major_chord() >> join::<U3>())),
            ITerm::Free(_) => unit(Box::new(bell_hz(200.0, 1.0, 5.0))),
            ITerm::App(_, _) => unit(Box::new(reverb_distort())),
            ITerm::Zero => unit(Box::new(shape(Clip(100.0)))),
            ITerm::Fin(_) => unit(Box::new(shape(Clip(2.0)))),
            _ => panic!("{term} not implemented")
        };
        let instrument = self.imelody(term).instrument;
        Self { instrument, effect, rhythm: rhythm, depth: self.depth + 1 }
    }

    fn cmerge(&self, term: &CTerm) -> Self {
        let rhythm = match term {
            CTerm::Inf(_) => panic!("inf merge???"),
            CTerm::Lam(_) => vec![1.5, 0.5, 1., 0.5],
        };
        let effect = match term {
            CTerm::Inf(_) => unit(Box::new(pass())),
            CTerm::Lam(_) => unit(Box::new(reverb_highpass())),
        };
        let instrument = self.cmelody(term).instrument;
        Self { instrument, effect, rhythm: rhythm, depth: self.depth + 1 }
    }
}

#[derive(Clone)]
pub struct Rhythmizer {
    rhythm: Vec<f64>,
    depth: usize,
}

impl Rhythmizer {
    pub fn new() -> Self {
        let length = 4;
        Self {
            rhythm: vec![1.0 / length as f64; length],
            depth: 0
        }
    }

    pub fn imelody(&self, term: &ITerm) -> Melody {
        let mut mel = match term {
            ITerm::Ann(_, _) => Melody::new_even(violinish(), &[A, D, F, A]),
            ITerm::Star => Melody::new_even(three_equivalents(wobbly_sine()) * 0.7, &[B, C, E, E]),
            ITerm::Pi(_, _) => Melody::new_even(sinesaw() >> split() >> fbd(0.25, -5.0), &[E, C, A, C]),
            ITerm::Bound(_) => Melody::new_even(sine() * 2.0, &[A, E, C, B]),
            ITerm::Free(_) => Melody::new_even(violinish() * 1.1, &[A, A, E, C]),
            ITerm::App(_, _) => Melody::new_even(sawfir() * 0.75, &[B, A, C, B]),
            ITerm::Zero => Melody::new_even(sinesaw(), &[E, F, C, A]),
            ITerm::Fin(_) => Melody::new_even(violinish() * 1.1, &[A, A + 12, E, F]),
            _ => panic!("{term} not implemented")
        };
        // let rhythm = self.rhythm.clone();
        mel.map_indexed(|i, (n, _d)| (*n, self.rhythm[i]));
        mel.adjust_depth(self.depth);
        mel
    }

    pub fn cmelody(&self, term: &CTerm) -> Melody {
        let mut mel = match term {
            CTerm::Lam(_) => Melody::new_even(fm_basic() * 0.28, &[D, F, E, C]),
            _ => unimplemented!()
            // _ => Melody::new_even(sine(), &[G, G, G, G]),  //value will never be used, but we have to call this for ownership reasons
        };
        mel.map_indexed(|i, (n, _d)| (*n, self.rhythm[i]));
        mel.adjust_depth(self.depth);
        mel
    }
}

impl Selector for Rhythmizer {
    fn isound(&self, term: &ITerm) -> SoundTree {
        let mel = self.imelody(term);
        SoundTree::sound(mel, term)
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        let mel = self.cmelody(term);
        SoundTree::sound(mel, term.clone())
    }

    fn imerge(&self, term: &ITerm) -> Self {
        let rhythm = match term {
            ITerm::Ann(_, _) => vec![1.0, 0.5, 1.5, 1.0],
            ITerm::Star => vec![0.0, 0.0, 0.0, -1.0],
            ITerm::Pi(_, _) => vec![0.5, 1.5, 0.5, 1.5],
            ITerm::Bound(_) => vec![0.0, 0.0, -1.0, 0.0],
            ITerm::Free(_) => vec![3.0, 0.3, 0.3, 0.4],
            ITerm::App(_, _) => vec![2.5, 0.5, 0.5, 0.5],
            ITerm::Zero => vec![-1.0, 0.0, 0.0, 0.0],
            ITerm::Fin(_) => vec![0.7, 0.7, 2.1, 0.5],
            _ => unimplemented!()
        };
        Self { rhythm: rhythm, depth: self.depth + 1 }
    }

    fn cmerge(&self, term: &CTerm) -> Self {
        let rhythm = match term {
            CTerm::Inf(_) => panic!("inf merge???"),
            CTerm::Lam(_) => vec![1.5, 0.5, 1., 0.5],
        };
        Self { rhythm: rhythm, depth: self.depth + 1 }
    }
}

#[derive(Clone)]
pub struct Looper<T: Selector> {
    body: T,
    depth: usize,
}

impl<T: Selector> Looper<T> {
    pub fn new(body: T) -> Self {
        Self {
            body,
            depth: 0
        }
    }
}

impl<T: Selector> Selector for Looper<T> {
    fn isound(&self, term: &ITerm) -> SoundTree {
        let base_mel = match self.body.isound(term) {
            SoundTree::Sound(sequenceable, _) => sequenceable,
            _ => panic!("isound can only construct Sound")
        };
        let mut lp = Loop::new(base_mel);
        lp.set_duration(lp.loop_duration() * 8.0 * 0.5_f64.powi(self.depth.div_ceil(4) as i32));
        SoundTree::sound(lp, term)
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        let base_mel = match self.body.csound(term) {
            SoundTree::Sound(sequenceable, _) => sequenceable,
            _ => panic!("csound can only construct Sound")
        };
        let mut lp = Loop::new(base_mel);
        lp.set_duration(lp.loop_duration() * 32.0 * 0.5_f64.powi(self.depth.div_ceil(4) as i32));
        SoundTree::sound(lp, term.clone())
    }

    fn imerge(&self, term: &ITerm) -> Self {
        Self { body: self.body.imerge(term), depth: self.depth + 1 }
    }

    fn cmerge(&self, term: &CTerm) -> Self {
        Self { body: self.body.cmerge(term), depth: self.depth + 1 }
    }
}

pub enum Mixer {
    Melody(Melody),
    Clip(WaveClip),
    Texture(Texture),
}

#[derive(Clone)]
pub struct MixedOutput {
    clip1: WaveClip,
    clip2: WaveClip,
    depth: usize,
}

impl MixedOutput {
    pub fn new() -> Self {
        MixedOutput { 
            clip1: WaveClip::from_file("files/malhombrechords.wav"), 
            clip2: WaveClip::from_file("files/boundvariable.mp3"),
            depth: 1
        }
    }
}

impl Selector for MixedOutput {
    fn isound(&self, term: &ITerm) -> SoundTree {
        let result = match term {
            ITerm::Ann(_, _) => Mixer::Melody(Melody::new_timed(sinesaw(), &[(E, 1.0), (F, 0.5), (E, 0.5), (C, 1.0), (A, 1.0)])),
            ITerm::Star => Mixer::Melody(Melody::new_timed(three_equivalents(wobbly_sine()) * 0.7, &[
                (B, 0.5), (C, 0.5), (E, 3.0)
                // (E, 0.2), (E, 0.2), (E, 0.2), (E, 0.2), (E, 0.2),
                // (E, 0.2), (E, 0.2), (E, 0.2), (E, 0.2), (E, 0.2),
                // (E, 0.2), (E, 0.2), (E, 0.2), (E, 0.2), (E, 0.2)
                ])),
            ITerm::Pi(_, _) => Mixer::Clip(self.clip1.clone()),
            ITerm::Bound(_) => Mixer::Clip(self.clip2.clone()),
            // ITerm::Free(_) => Mixer::Clip(self.clip1.clone()),
            ITerm::Free(_) => Mixer::Melody(Melody::new_even(violinish() * 1.1, &[A, A, E, C])),
            ITerm::App(_, _) => Mixer::Melody(Melody::new_even(sinesaw() >> split() >> fbd(0.25, -5.0), &[E, C, A, C])),
            // ITerm::App(_, _) => Mixer::Melody(Melody::new_timed(sawfir() * 0.75, &[(B, 1.0), (A, 0.5), (E, 0.5), (C, 1.0), (B, 1.0)])),
            ITerm::Zero => Mixer::Melody(Melody::new_even(sine() * 2.0, &[A, E, C, B])),
            ITerm::Fin(_) => Mixer::Melody(Melody::new_even(violinish() * 1.1, &[A, A + 12, E, F])),
            _ => panic!("{term} not implemented")
        };
        match result {
            Mixer::Melody(mut mel) => {
                mel.adjust_depth(self.depth);
                SoundTree::sound(mel, term)
            },
            Mixer::Clip(mut wav) => {
                wav.adjust_depth(self.depth);
                SoundTree::sound(wav, term)
            },
            Mixer::Texture(mut tex) => {
                tex.adjust_depth(self.depth);
                SoundTree::sound(tex, term)
            }
        }
        // result.adjust_depth(depth);
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        let result = match term {
            CTerm::Inf(_) => Mixer::Clip(self.clip1.clone()),
            CTerm::Lam(_) => Mixer::Texture(Texture::new_even([A, E, C, F, B], sine(), 0.1)),
        };
        match result {
            Mixer::Melody(mut mel) => {
                mel.adjust_depth(self.depth);
                SoundTree::sound(mel, term.clone())
            },
            Mixer::Clip(mut wav) => {
                wav.adjust_depth(self.depth);
                SoundTree::sound(wav, term.clone())
            },
            Mixer::Texture(mut tex) => {
                tex.adjust_depth(self.depth);
                SoundTree::sound(tex, term.clone())
            }
        }
    }

    fn imerge(&self, _term: &ITerm) -> Self {
        Self { depth: self.depth + 1, ..self.clone() }
    }

    fn cmerge(&self, _term: &CTerm) -> Self {
        Self { depth: self.depth + 1, ..self.clone() }
    }
}

#[derive(Clone)]
pub struct Effector {
    effect: An<Unit<U1, U1>>,
    depth: usize,
}

impl Effector {
    pub fn new() -> Self {
        Self { effect: unit(Box::new(pass())), depth: 0 }
    }
}

impl Selector for Effector {
    fn isound(&self, term: &ITerm) -> SoundTree {
        let melody = MelodySelector::F.imelody(term, self.depth);
        let eff = EffectSeq::new(melody, self.effect.clone());
        // melody.instrument = unit(Box::new(melody.instrument >> self.effect.clone()));
        SoundTree::sound(eff, term)
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        let melody = MelodySelector::F.cmelody(term, self.depth);
        let eff = EffectSeq::new(melody, self.effect.clone());
        // melody.instrument = unit(Box::new(melody.instrument >> self.effect.clone()));
        SoundTree::sound(eff, term.clone())
    }

    fn imerge(&self, term: &ITerm) -> Self {
        let effect = match term {
            ITerm::Ann(_, _) => unit(Box::new((pass() | (800.0 + 100.0 * sine_hz(2.0)) | constant(1.0)) >> highpass())),
            ITerm::Star => unit(Box::new(shape(Clip(100.0)))),
            ITerm::Pi(_, _) => unit(Box::new(split() >> fbd(0.25, -1.5))),
            ITerm::Bound(_) => unit(Box::new(major_chord() >> join::<U3>())),
            ITerm::Free(_) => unit(Box::new(bell_hz(200.0, 1.0, 5.0))),
            ITerm::App(_, _) => unit(Box::new(reverb_distort())),
            ITerm::Zero => unit(Box::new(shape(Clip(100.0)))),
            ITerm::Fin(_) => unit(Box::new(shape(Clip(2.0)))),
            _ => panic!("{term} not implemented")
        };
        Self { effect, depth: self.depth + 1 }
    }

    fn cmerge(&self, term: &CTerm) -> Self {
        let effect = match term {
            CTerm::Inf(_) => unit(Box::new(pass())),
            CTerm::Lam(_) => unit(Box::new(reverb_highpass())),
        };
        Self { effect, depth: self.depth + 1 }
    }
}

#[derive(Clone)]
pub struct StratifiedInfo {
    pub mel: Melody,
    depth: usize,
}

impl Default for StratifiedInfo {
    fn default() -> Self {
        Self { mel: Melody::new_even(violinish(), &[A, D, F, A]), depth: 0 }
    }
}

impl Selector for StratifiedInfo {
    fn isound(&self, term: &ITerm) -> SoundTree {
        let mut mel = self.mel.clone();
        mel.adjust_depth(self.depth);
        SoundTree::sound(mel, term)
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        let mut mel = self.mel.clone();
        mel.adjust_depth(self.depth);
        SoundTree::sound(mel, term.clone())
    }

    fn imerge(&self, term: &ITerm) -> StratifiedInfo {
        let mel = match term {
            ITerm::Ann(_, _) => Melody::new_even(violinish(), &[A, D, F, A]),
            ITerm::Star => Melody::new_timed(three_equivalents(wobbly_sine()) * 0.7, &[(B, 0.5), (C, 0.5), (E, 3.0)]),
            ITerm::Pi(_, _) => Melody::new_even(sinesaw() >> split() >> fbd(0.25, -5.0), &[E, C, A, C]),
            ITerm::Bound(_) => Melody::new_even(sine() * 2.0, &[A, E, C, B]),
            ITerm::Free(_) => Melody::new_even(violinish() * 1.1, &[A, A, E, C]),
            ITerm::App(_, _) => Melody::new_timed(sawfir() * 0.75, &[(B, 1.0), (A, 0.5), (E, 0.5), (C, 1.0), (B, 1.0)]),
            ITerm::Zero => Melody::new_timed(sinesaw(), &[(E, 1.0), (F, 0.5), (E, 0.5), (C, 1.0), (A, 1.0)]),
            ITerm::Fin(_) => Melody::new_even(violinish() * 1.1, &[A, A + 12, E, F]),
            _ => panic!("{term} not implemented")
        };
        StratifiedInfo { mel, depth: self.depth + 1 }
    }

    fn cmerge(&self, term: &CTerm) -> StratifiedInfo {
        let mel = match term {
            CTerm::Lam(_) => Melody::new_even(fm_basic() * 0.28, &[D, F, E, C]),
            _ => Melody::new_even(sine(), &[G, G, G, G]),  //value will never be used, but we have to call this for ownership reasons
        };
        StratifiedInfo { mel, depth: self.depth + 1 }
    }
}

#[derive(Clone)]
pub struct ClipSelector {
    ann: Rc<Wave>,
    star: Rc<Wave>,
    pi: Rc<Wave>,
    bound: Rc<Wave>,
    free: Rc<Wave>,
    app: Rc<Wave>,
    zero: Rc<Wave>,
    fin: Rc<Wave>,
    lam: Rc<Wave>,
}

impl ClipSelector {
    pub fn names() -> Self {
        Self {
            ann: Rc::new(Wave::load("files/annotation.mp3").unwrap()), // type annotation
            star: Rc::new(Wave::load("files/star.mp3").unwrap()),  // universe type
            pi: Rc::new(Wave::load("files/forall.mp3").unwrap()),  // universal quantifier
            bound: Rc::new(Wave::load("files/bound.mp3").unwrap()),  // bound variable
            free: Rc::new(Wave::load("files/free.mp3").unwrap()),  // free variable
            app: Rc::new(Wave::load("files/application.mp3").unwrap()),  // function application
            zero: Rc::new(Wave::load("files/zero.mp3").unwrap()),  // natural number zero
            fin: Rc::new(Wave::load("files/finite.mp3").unwrap()),  // finite type
            // lam: Rc::new(Wave::load("files/malhombrechords.wav").unwrap()),  // lambda abstraction
            lam: Rc::new(Wave::load("files/lambdaabstraction.mp3").unwrap()),  // lambda abstraction
        }
    }

    pub fn names_long() -> Self {
        Self {
            ann: Rc::new(Wave::load("files/typeannotation.wav").unwrap()), // type annotation
            star: Rc::new(Wave::load("files/universe.mp3").unwrap()),  // universe type
            pi: Rc::new(Wave::load("files/universalquantifier.mp3").unwrap()),  // universal quantifier
            bound: Rc::new(Wave::load("files/boundvariable.mp3").unwrap()),  // bound variable
            free: Rc::new(Wave::load("files/freevariable.mp3").unwrap()),  // free variable
            app: Rc::new(Wave::load("files/functionapplication.mp3").unwrap()),  // function application
            zero: Rc::new(Wave::load("files/naturalnumberzero.mp3").unwrap()),  // natural number zero
            fin: Rc::new(Wave::load("files/finitetype.mp3").unwrap()),  // finite type
            lam: Rc::new(Wave::load("files/lambdaabstraction.mp3").unwrap()),  // lambda abstraction
        }
    }
}

impl Selector for ClipSelector {
    fn isound(&self, term: &ITerm) -> SoundTree {
        let result = match term {
            ITerm::Ann(_, _) => &self.ann,
            ITerm::Star => &self.star,
            ITerm::Pi(_, _) => &self.pi,
            ITerm::Bound(_) => &self.bound,
            ITerm::Free(_) => &self.free,
            ITerm::App(_, _) => &self.app,
            ITerm::Zero => &self.zero,
            ITerm::Fin(_) => &self.fin,
            _ => unimplemented!()
        };
        SoundTree::sound(Rc::clone(result), term)
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        let result = match term {
            CTerm::Inf(_) => &self.ann,
            CTerm::Lam(_) => &self.lam,
        };
        SoundTree::sound(Rc::clone(result), term.clone())
    }

    fn imerge(&self, _term: &ITerm) -> Self {
        self.clone()
    }

    fn cmerge(&self, _term: &CTerm) -> Self {
        self.clone()
    }
}

/// Determines which set of melodies to use when generating melodies for a term.
#[derive(Clone, Copy)]
pub enum MelodySelector {
    /// First, highly arbitary melody suite. See [imelody1] and [cmelody2] (there is no cmelody1).
    A,
    /// Melodies based on B, C, E, and G with arbitrarily-chosen instruments. See [imelody2] and [cmelody2].
    B,
    /// More intentionally-chosen melodies with still-arbitrary instruments. See [imelody3] and [cmelody3].
    C,
    /// Same melodies as C but with cleaner-sounding instruments. See [imelody4] and [cmelody4].
    D,
    /// Melody suite with some hints of dissonance. See [imelody5] and [cmelody5].
    E,
    /// Like E, but switched around and more textured
    F,
    /// Same melodies as B and C but exclusively as sines. See [imelody_oneinstr] and [cmelody_oneinstr].
    PureSine,
    // Concrete(ClipSelector),
}

impl MelodySelector {
    /// Get the appropriate melody for a certain type of [ITerm].
    pub fn imelody(&self, term: &ITerm, depth: usize) -> Melody {
        match self {
            MelodySelector::A => imelody1(term, depth),
            MelodySelector::B => imelody2(term, depth),
            MelodySelector::C => imelody3(term, depth),
            MelodySelector::D => imelody4(term, depth),
            MelodySelector::E => imelody5(term, depth),
            MelodySelector::F => imelody6(term, depth),
            MelodySelector::PureSine => imelody_oneinstr(sine(), term, depth),
            // MelodySelector::Concrete(clip_selector) => SoundTree::sound(clip_selector.imelody(term, depth), term),
            // _ => unimplemented!()
        }
    }

    /// Get the appropriate melody for a certain type of [CTerm].
    pub fn cmelody(&self, term: &CTerm, depth: usize) -> Melody {
        match self {
            MelodySelector::A => cmelody2(term, depth),
            MelodySelector::B => cmelody2(term, depth),
            MelodySelector::C => cmelody3(term, depth),
            MelodySelector::D => cmelody4(term, depth),
            MelodySelector::E => cmelody5(term, depth),
            MelodySelector::F => cmelody6(term, depth),
            MelodySelector::PureSine => cmelody_oneinstr(sine(), term, depth),
            // MelodySelector::Concrete(clip_selector) => SoundTree::sound(clip_selector.cmelody(term, depth), term.clone()),
            // _ => unimplemented!()
        }
    }

    pub fn deepen(self) -> MelodySelectorFull {
        MelodySelectorFull { mel: self, depth: 1 }
    }
}

#[derive(Clone, Copy)]
pub struct MelodySelectorFull {
    mel: MelodySelector,
    depth: usize,
}

// impl From<MelodySelector> for MelodySelectorFull {
//     fn from(value: MelodySelector) -> Self {
//         Self {
//             mel: value,
//             depth: 1
//         }
//     }
// }

impl Selector for MelodySelectorFull {
    fn isound(&self, term: &ITerm) -> SoundTree {
        SoundTree::sound(self.mel.imelody(term, self.depth), term)
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        SoundTree::sound(self.mel.cmelody(term, self.depth), term.clone())
    }

    fn imerge(&self, _term: &ITerm) -> Self {
        MelodySelectorFull { mel: self.mel, depth: self.depth + 1 }
    }

    fn cmerge(&self, _term: &CTerm) -> Self {
        MelodySelectorFull { mel: self.mel, depth: self.depth + 1 }
    }
}

// pub trait AudioSelector {
//     fn imelody(&self, term: &ITerm, depth: usize) -> impl Sequenceable + 'static;

//     fn cmelody(&self, term: &CTerm, depth: usize) -> impl Sequenceable + 'static;
// }

/// First, highly arbitrary melody suite
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
    result.adjust_depth(depth);
    result
}

/// Melodies based on C, E, B, and G with arbtrary instruments
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
    result.adjust_depth(depth);
    result
}

/// Melodies based on C, E, B, and G with arbtrary instruments
pub fn cmelody2(term: &CTerm, depth: usize) -> Melody {
    let mut result = match term {
        CTerm::Inf(_) => Melody::new_even(sine(), &[G, B, E, C]), //value will never be used
        CTerm::Lam(_) => Melody::new_even(fm_epi(), &[G, B, E, C]),
    };
    result.adjust_depth(depth);
    result
}

/// More intentionally-chosen melodies with arbitrary instruments
pub fn imelody3(term: &ITerm, depth: usize) -> Melody {
    let mut result = match term {
        ITerm::Ann(_, _) => Melody::new_even(violinish(), &[A, B, C, B]),
        ITerm::Star => Melody::new_even(wobbly_sine(), &[A, C, A, B]),
        ITerm::Pi(_, _) => Melody::new_even(sinesaw(), &[C, C, B, A]),
        ITerm::Bound(n) => Melody::new_even(sinesaw() >> split() >> fbd(*n as f32 / 10.0, -5.0), &[E, E, C, B]),
        ITerm::Free(_) => Melody::new_even(violinish(), &[A, G, F, D]),
        ITerm::App(_, _) => Melody::new_even(pink_sine(), &[B, C, G, E]),
        ITerm::Zero => Melody::new_even(fm_basic(), &[B, C, E, G]),
        ITerm::Fin(_) => Melody::new_even(violinish(), &[E, B, G, C]),
        _ => panic!("{term} not implemented")
    };
    result.adjust_depth(depth);
    result
}

/// More intentionally-chosen melodies with arbitrary instruments
pub fn cmelody3(term: &CTerm, depth: usize) -> Melody {
    let mut result = match term {
        CTerm::Inf(_) => Melody::new_even(sine(), &[]), //value will never be used
        CTerm::Lam(_) => Melody::new_even(fm_epi(), &[G, B, E, C]),
    };
    result.adjust_depth(depth);
    result
}

/// Same melodies as 3 but with cleaner-sounding instruments
pub fn imelody4(term: &ITerm, depth: usize) -> Melody {
    let mut result = match term {
        ITerm::Ann(_, _) => Melody::new_even(violinish(), &[A, B, C, B]),
        ITerm::Star => Melody::new_even(wobbly_sine(), &[A, C, A, B]),
        ITerm::Pi(_, _) => Melody::new_even(sine(), &[C, C, B, A]),
        ITerm::Bound(n) => Melody::new_even(sinesaw() >> split() >> fbd(*n as f32 / 10.0, -5.0), &[E, E, C, B]),
        ITerm::Free(_) => Melody::new_even(violinish(), &[A, G, F, D]),
        ITerm::App(_, _) => Melody::new_even(sawfir(), &[B, C, G, E]),
        ITerm::Zero => Melody::new_even(sinesaw(), &[B, C, E, G]),
        ITerm::Fin(_) => Melody::new_even(violinish(), &[E, B, G, C]),
        _ => panic!("{term} not implemented")
    };
    result.adjust_depth(depth);
    result
}

/// Same melodies as 3 but with cleaner-sounding instruments
pub fn cmelody4(term: &CTerm, depth: usize) -> Melody {
    let mut result = match term {
        CTerm::Inf(_) => Melody::new_even(sine(), &[]),
        CTerm::Lam(_) => Melody::new_even(wobbly_sine(), &[G, B, E, C]),
    };
    result.adjust_depth(depth);
    result
}

/// Melodies with some hints of dissonance
pub fn imelody5(term: &ITerm, depth: usize) -> Melody {
    let mut result = match term {
        ITerm::Ann(_, _) => Melody::new_even(violinish(), &[A, D, F, A]),
        ITerm::Star => Melody::new_even(wobbly_sine(), &[A, A + 12, E, F]),
        ITerm::Pi(_, _) => Melody::new_even(sine(), &[E, C, A, C]),
        ITerm::Bound(n) => Melody::new_even(sinesaw() >> split() >> fbd(*n as f32 / 10.0, -5.0), &[A, E, C, B]),
        ITerm::Free(_) => Melody::new_even(violinish(), &[A, A, E, C]),
        ITerm::App(_, _) => Melody::new_even(sawfir(), &[D, F, E, C]),
        ITerm::Zero => Melody::new_timed(sinesaw(), &[(E, 1.0), (F, 0.5), (E, 0.5), (C, 1.0), (A, 1.0)]),
        ITerm::Fin(_) => Melody::new_timed(violinish(), &[(B, 1.0), (A, 0.5), (E, 0.5), (C, 1.0), (B, 1.0)]),
        _ => panic!("{term} not implemented")
    };
    result.adjust_depth(depth);
    result
}

/// Melodies with some hints of dissonance
pub fn cmelody5(term: &CTerm, depth: usize) -> Melody {
    let mut result = match term {
        CTerm::Lam(_) => Melody::new_timed(violinish(), &[(B, 0.5), (C, 0.5), (E, 3.0)]),
        _ => Melody::new_even(sine(), &[G, G, G, G]),  //value will never be used, but we have to call this for ownership reasons
    };
    result.adjust_depth(depth);
    result
}

/// Melodies with some hints of dissonance & more texture
pub fn imelody6(term: &ITerm, depth: usize) -> Melody {
    let mut result = match term {
        ITerm::Ann(_, _) => Melody::new_even(violinish(), &[A, D, F, A]),
        ITerm::Star => Melody::new_timed(three_equivalents(wobbly_sine()) * 0.7, &[(B, 0.5), (C, 0.5), (E, 3.0)]),
        ITerm::Pi(_, _) => Melody::new_even(sinesaw() >> split() >> fbd(0.25, -5.0), &[E, C, A, C]),
        ITerm::Bound(_) => Melody::new_even(sine() * 2.0, &[A, E, C, B]),
        ITerm::Free(_) => Melody::new_even(violinish() * 1.1, &[A, A, E, C]),
        ITerm::App(_, _) => Melody::new_timed(sawfir() * 0.75, &[(B, 1.0), (A, 0.5), (E, 0.5), (C, 1.0), (B, 1.0)]),
        ITerm::Zero => Melody::new_timed(sinesaw(), &[(E, 1.0), (F, 0.5), (E, 0.5), (C, 1.0), (A, 1.0)]),
        ITerm::Fin(_) => Melody::new_even(violinish() * 1.1, &[A, A + 12, E, F]),
        _ => panic!("{term} not implemented")
    };
    result.adjust_depth(depth);
    result
}

/// Melodies with some hints of dissonance
pub fn cmelody6(term: &CTerm, depth: usize) -> Melody {
    let mut result = match term {
        CTerm::Lam(_) => Melody::new_even(fm_basic() * 0.28, &[D, F, E, C]),
        _ => Melody::new_even(sine(), &[G, G, G, G]),  //value will never be used, but we have to call this for ownership reasons
    };
    result.adjust_depth(depth);
    result
}

/// Melodies with B, C, E, and G, all on the same instrument
pub fn imelody_oneinstr(instrument: impl AudioUnit + 'static, term: &ITerm, depth: usize) -> Melody {
    let mut result = match term {
        ITerm::Ann(_, _) => Melody::new_even(instrument, &[C, E, B, E]),
        ITerm::Star => Melody::new_even(instrument, &[G, E, C, B]),
        ITerm::Pi(_, _) => Melody::new_even(instrument, &[E, C, G, E]),
        ITerm::Bound(_) => Melody::new_even(instrument, &[E, E, C, B]),
        ITerm::Free(_) => Melody::new_even(instrument, &[B, B, E, G]),
        ITerm::App(_, _) => Melody::new_even(instrument, &[B, C, G, E]),
        ITerm::Zero => Melody::new_even(instrument, &[B, C, E, G]),
        ITerm::Fin(_) => Melody::new_even(instrument, &[E, B, G, C]),
        _ => panic!("{term} not implemented")
    };
    result.adjust_depth(depth);
    result
}

/// Melodies with B, C, E, and G, all on the same instrument
pub fn cmelody_oneinstr(instrument: impl AudioUnit + 'static, term: &CTerm, depth: usize) -> Melody {
    let mut result = match term {
        CTerm::Lam(_) => Melody::new_even(instrument, &[G, B, E, C]),
        _ => Melody::new_even(sine(), &[A, A, A, A]), //value will never be used, but we have to call this for ownership reasons
    };
    result.adjust_depth(depth);
    result
}