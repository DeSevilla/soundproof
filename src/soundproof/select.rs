use std::collections::HashMap;
// use std::rc::Rc;
use std::sync::{Arc, Mutex};

use piet_common::Color;
use fundsp::hacker32::*;
use crate::{
    ast::*,
    instruments::*,
    notes::*,
    types::*,
};

const ORANGE: Color = Color::rgb8(0xCE, 0x5D, 0x00);
const PALEORANGE: Color = Color::rgb8(0xf5, 0xc1, 0x4b);
const LAVENDER: Color = Color::rgb8(0x96, 0x7e, 0xf7);
const DEEPBLUE: Color = Color::rgb8(0x27, 0x3a, 0x8c);
const DEEPPURPLE: Color = Color::rgb8(0x4D, 0x01, 0x86);
const PINK: Color = Color::rgb8(0xF8, 0x18, 0x94);
const TURQUOISE: Color = Color::rgb8(0x00, 0xDD, 0xD0);
const VIOLET: Color = Color::rgb8(0x90, 0x00, 0x90);

pub trait Selector: Clone {
    fn isound(&self, term: &ITerm) -> SoundTree;
    fn csound(&self, term: &CTerm) -> SoundTree;
    fn imerge(&self, term: &ITerm) -> Self;
    fn cmerge(&self, term: &CTerm) -> Self;
    fn imeta(&self, term: &ITerm) -> TreeMetadata {
        let color = match term {
            // ITerm::Ann(_, _) => Color::RED,
            ITerm::Ann(_, _) => Color::FUCHSIA,
            ITerm::Star => PALEORANGE,
            // ITerm::Pi(_, _) => Color::BLUE,
            ITerm::Pi(_, _) => LAVENDER,
            // ITerm::App(_, _) => ORANGE,
            ITerm::App(_, _) => DEEPBLUE,
            ITerm::Bound(_) => Color::grey(0.5),
            ITerm::Free(Name::Local(i)) => Color::rgb8(0x00, 0x28 + 0x12 * (*i as u8), 0x00),
            ITerm::Free(_) => Color::GREEN,
            ITerm::Zero => PINK,
            ITerm::Fin(_) => TURQUOISE,
            _ => unimplemented!()
        };
        TreeMetadata { name: term.to_string(), base_color: color, alt_color: color, max_depth: 0 }
    }
    fn cmeta(&self, term: &CTerm) -> TreeMetadata {
        match term {
            CTerm::Inf(it) => self.imeta(it),
            // CTerm::Lam(_) => TreeMetadata { name: term.to_string(), color: Color::YELLOW }
            CTerm::Lam(_) => TreeMetadata { name: term.to_string(), base_color: DEEPPURPLE, alt_color: VIOLET, max_depth: 0 }
        }
    }
}

#[derive(Clone)]
pub struct Plain {
    depth: usize,
}

impl Plain {
    pub fn new() -> Self {
        Plain { depth: 0 }
    }

    pub fn pitch_factor(&self) -> usize {
        (self.depth as f64 * 2.5).floor() as usize + 18
    }
}

impl Default for Plain {
    fn default() -> Self {
        Self::new()
    }
}

impl Selector for Plain {
    fn isound(&self, term: &ITerm) -> SoundTree {
        SoundTree::sound(Melody::new_even(sine(), &[self.pitch_factor().try_into().unwrap()]), self.imeta(term))
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        SoundTree::sound(Melody::new_even(sine(), &[self.pitch_factor().try_into().unwrap()]), self.cmeta(term))
    }

    fn imerge(&self, _term: &ITerm) -> Self {
        Plain {
            depth: self.depth + 1
        }
    }

    fn cmerge(&self, _term: &CTerm) -> Self {
        Plain {
            depth: self.depth + 1
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Term {
    I(ITerm),
    C(CTerm),
}

impl Term {
    pub fn tag(&self) -> Tag {
        match self {
            Term::I(iterm) => iterm.tag(),
            Term::C(cterm) => cterm.tag(),
        }
    }
}

#[derive(Clone)]
pub struct AsyncNodeInfo {
    pub notes: Arc<Mutex<[i32; FS_MEL_SIZE]>>,
    pub timings: Arc<Mutex<[f64; FS_MEL_SIZE]>>,
    pub instrument: Arc<Mutex<An<Unit<U1, U1>>>>,
    pub effect: Arc<Mutex<An<Unit<U1, U1>>>>,
}

// impl SoundGenerator for AsyncNodeInfo {
//     fn sequence(&self, seq: &mut ConfigSequencer, start_time: f64, duration: f64, lean: f32) {
//         let notes = self.notes.lock().unwrap().clone();
//         let timings = self.timings.lock().unwrap().clone();
//         let instrument = self.instrument.lock().unwrap().clone() >> self.effect.lock().unwrap().clone();
//         let mut mel = Melody::new_even(instrument, &notes);
//         mel.adjust_timings(&timings).unwrap();
//         mel.sequence(seq, start_time, duration, lean);
//     }

//     fn base_duration(&self) -> f64 {
//         FS_MEL_SIZE as f64
//     }
// }

impl AsyncNodeInfo {
    fn new(notes: [i32; FS_MEL_SIZE], timings: [f64; FS_MEL_SIZE], instrument: impl AudioUnit + 'static, effect: impl AudioUnit + 'static) -> Self {
        Self {
            notes: Arc::new(Mutex::new(notes)),
            timings: Arc::new(Mutex::new(timings)),
            instrument: Arc::new(Mutex::new(unit(Box::new(instrument)))),
            effect: Arc::new(Mutex::new(unit(Box::new(effect)))),
        }
        // (notes, unit(Box::new(instrument)), unit(Box::new(effect)))
    }
}

#[derive(Clone)]
pub struct AsyncStratifier
{
    params: Arc<HashMap<Tag, AsyncNodeInfo>>,
    parent: Term,
    depth: usize
}



impl AsyncStratifier {
    pub fn new() -> Self {
        let mut params= HashMap::new();
        use Tag::*;
        let tags = [Annotation, Type, Pi, Application, BoundVar, FreeVar, Zero, Finite, Lambda];
        for tag in tags {
            let info = match tag {
                Annotation => AsyncNodeInfo::new([A, D, F, A], [1.0, 0.5, 1.5, 1.0], sawfir() * 0.75, (pass() | (800.0 + 100.0 * sine_hz(2.0)) | constant(1.0)) >> highpass()),
                Type => AsyncNodeInfo::new([B, C, E, E], [1.0, 1.0, 1.0, 1.0], violinish(), pass()),
                Pi => AsyncNodeInfo::new([E, C, A, C], [0.5, 1.5, 0.5, 1.5], sinesaw() >> split() >> fbd(0.2, -5.0), split() >> fbd(0.25, -3.5)),
                Application => AsyncNodeInfo::new([A, E, C, B], [2.5, 0.5, 0.5, 0.5], sine() * 2.0, reverb_distort()),
                BoundVar => AsyncNodeInfo::new([A, A, E, C], [-1.0, 0., 0., 0.], violinish() * 1.1, major_chord() >> join::<U3>()),
                FreeVar => AsyncNodeInfo::new([B, A, C, B], [1.5, 0.5, 1.0, 1.0], three_equivalents(wobbly_sine()) * 0.7, shape(Clip(0.75))),
                Zero => AsyncNodeInfo::new([E, F, C, A], [-1.0, 0., 0., 0.], sinesaw(), shape(Clip(100.0))),
                Nat => AsyncNodeInfo::new([E, F, G, E], [-1.0, 0., 0., 0.], sinesaw(), shape(Clip(100.0))),
                Finite => AsyncNodeInfo::new([A, A + 12, E, F], [0.7, 0.7, 2.1, 0.5], violinish() * 1.1, bell_hz(200.0, 1.0, 5.0)),
                Lambda => AsyncNodeInfo::new([D, F, E, C], [1.5, 0.5, 1., 0.5], fm_basic() * 0.28, reverb_highpass()),
            };
            params.insert(tag, info);
        }
        Self {
            params: Arc::new(params),
            parent: Term::I(ITerm::Star),
            depth: 0,
        }
    }

    pub fn get(&self, tag: &Tag) -> AsyncNodeInfo {
        match self.params.get(tag) {
            Some(t) => t.clone(),
            None => { println!("{tag:?}"); panic!() }
        }
    }

    pub fn for_pair(&self, parent: Tag, child: Tag) -> MelodyAsync {
        let parent = self.get(&parent);
        let child = self.get(&child);
        MelodyAsync {
            notes: child.notes.clone(),
            timings: parent.timings.clone(),
            instrument: parent.instrument.clone(),
            effect: parent.effect.clone(),
            depth: self.depth,
        }
    }
}

impl Default for AsyncStratifier {
    fn default() -> Self {
        Self::new()
    }
}

impl Selector for AsyncStratifier
{
    fn isound(&self, term: &ITerm) -> SoundTree {
        // let mel = self.melodies.get(&(self.parent.tag(), term.tag())).unwrap();
        let mel = self.for_pair(self.parent.tag(), term.tag());
        // let deep = MelodyAsync::new_depth(mel.clone(), self.depth);
        SoundTree::sound(mel, self.imeta(term))
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        let mel = self.for_pair(self.parent.tag(), term.tag());
        // let mel = self.melodies.get(&(self.parent.tag(), term.tag())).unwrap();
        // let deep = MelodyAsync::new_depth(mel.clone(), self.depth);
        SoundTree::sound(mel, self.cmeta(term))
    }

    fn imerge(&self, term: &ITerm) -> Self {
        Self {
            params: self.params.clone(),
            parent: Term::I(term.clone()),
            depth: self.depth + 1
        }
    }

    fn cmerge(&self, term: &CTerm) -> Self {
        Self {
            params: self.params.clone(),
            parent: Term::C(term.clone()),
            depth: self.depth + 1
        }
    }

    fn imeta(&self, term: &ITerm) -> TreeMetadata {
        FullStratifier {
            parent: self.parent.clone(),
            depth: self.depth,
        }.imeta(term)
    }

    fn cmeta(&self, term: &CTerm) -> TreeMetadata {
        FullStratifier {
            parent: self.parent.clone(),
            depth: self.depth,
        }.cmeta(term)
    }
}

#[derive(Clone)]
pub struct FullStratifier {
    // instrument: An<Unit<U1, U1>>,
    // effect: An<Unit<U1, U1>>,
    // rhythm: Vec<f64>,
    parent: Term,
    depth: usize,
}

pub const FS_MEL_SIZE: usize = 4;

impl FullStratifier {
    pub fn new() -> Self {
        Self {
            parent: Term::I(ITerm::Star),
            depth: 1
        }
    }

    pub fn inotes(&self, term: &ITerm) -> [i32; FS_MEL_SIZE] {
        match term {
            ITerm::Ann(_, _) => [A, D, F, A],
            ITerm::Star => [B, C, E, E],
            ITerm::Pi(_, _) => [E, C, A, C],
            ITerm::Bound(_) => [A, E, C, B],
            ITerm::Free(_) => [A, A, E, C],
            ITerm::App(_, _) => [B, A, C, B],
            ITerm::Zero => [E, F, C, A],
            ITerm::Fin(_) => [A, A + 12, E, F],
            // ITerm::Nat => todo!(),
            // ITerm::Succ(cterm) => todo!(),
            // ITerm::NatElim(cterm, cterm1, cterm2, cterm3) => todo!(),
            // ITerm::FZero(cterm) => todo!(),
            // ITerm::FSucc(cterm, cterm1) => todo!(),
            // ITerm::FinElim(cterm, cterm1, cterm2, cterm3, cterm4) => todo!(),
            _ => todo!(),
        }
    }

    pub fn cnotes(&self, term: &CTerm) -> [i32; FS_MEL_SIZE] {
        match term {
            CTerm::Lam(_) => [D, F, E, C],
            _ => unimplemented!()
        }
    }

    pub fn irhythm(&self, term: &ITerm) -> [f64; FS_MEL_SIZE] {
        match term {
            ITerm::Ann(_, _) => [1.0, 0.5, 1.5, 1.0],
            ITerm::Star => [1.0, 1.0, 1.0, 1.0],
            ITerm::Pi(_, _) => [0.5, 1.5, 0.5, 1.5],
            ITerm::Free(_) => [1.5, 0.5, 1.0, 1.0],
            ITerm::App(_, _) => [2.5, 0.5, 0.5, 0.5],
            ITerm::Zero => [-1.0, 0.0, 0.0, 0.0],
            ITerm::Fin(_) => [0.7, 0.7, 2.1, 0.5],
            // ITerm::Bound(_) => todo!(),
            // ITerm::Nat => todo!(),
            // ITerm::Succ(cterm) => todo!(),
            // ITerm::NatElim(cterm, cterm1, cterm2, cterm3) => todo!(),
            // ITerm::FZero(cterm) => todo!(),
            // ITerm::FSucc(cterm, cterm1) => todo!(),
            // ITerm::FinElim(cterm, cterm1, cterm2, cterm3, cterm4) => todo!(),
            _ => todo!(),
        }
    }

    pub fn crhythm(&self, term: &CTerm) -> [f64; FS_MEL_SIZE] {
        match term {
            CTerm::Lam(_) => [1.5, 0.5, 1., 0.5],
            _ => unimplemented!()
        }
    }

    pub fn ieffect(&self, term: &ITerm) -> An<Unit<U1, U1>> {
        match term {
            ITerm::Ann(_, _) => unit(Box::new((pass() | (800.0 + 100.0 * sine_hz(2.0)) | constant(1.0)) >> highpass())),
            ITerm::Star => unit(Box::new(pass())),
            ITerm::Pi(_, _) => unit(Box::new(split() >> fbd(0.25, -3.5))),
            ITerm::Bound(_) => unit(Box::new(major_chord() >> join::<U3>())),
            ITerm::Free(Name::Local(i)) => unit(Box::new(shape(Clip(1.0 + 0.5 * (*i as f32))))),
            ITerm::Free(_) => unit(Box::new(shape(Clip(0.75)))),
            ITerm::App(_, _) => unit(Box::new(reverb_distort())),
            ITerm::Zero => unit(Box::new(shape(Clip(100.0)))),
            ITerm::Fin(_) => unit(Box::new(bell_hz(200.0, 1.0, 5.0))),
            // ITerm::Nat => todo!(),
            // ITerm::Succ(cterm) => todo!(),
            // ITerm::NatElim(cterm, cterm1, cterm2, cterm3) => todo!(),
            // ITerm::FZero(cterm) => todo!(),
            // ITerm::FSucc(cterm, cterm1) => todo!(),
            // ITerm::FinElim(cterm, cterm1, cterm2, cterm3, cterm4) => todo!(),
            _ => todo!(),
        }
    }

    pub fn ceffect(&self, term: &CTerm) -> An<Unit<U1, U1>> {
        match term {
            CTerm::Lam(_) => unit(Box::new(reverb_highpass())),
            _ => unimplemented!()
        }
    }

    pub fn imelody(&self, term: &ITerm, notes: [i32; FS_MEL_SIZE]) -> Melody {
        let mut mel = match term {
            ITerm::Ann(_, _) => Melody::new_even(sawfir() * 0.75, &notes),
            ITerm::Star => Melody::new_even(violinish(), &notes),
            ITerm::Pi(_, _) => Melody::new_even(sinesaw() >> split() >> fbd(0.2, -5.0), &notes),
            ITerm::Bound(_) => Melody::new_even(sine() * 2.0, &notes),
            ITerm::Free(_) => Melody::new_even(violinish() * 1.1, &notes),
            ITerm::App(_, _) => Melody::new_even(three_equivalents(wobbly_sine()) * 0.7, &notes),
            ITerm::Zero => Melody::new_even(sinesaw(), &notes),
            ITerm::Fin(_) => Melody::new_even(violinish() * 1.1, &notes),
            _ => unimplemented!()
        };
        let rhythm = self.irhythm(term);
        mel.map_indexed(|i, (n, _d)| (*n, rhythm[i]));
        mel.adjust_depth(self.depth);
        mel
    }

    pub fn cmelody(&self, term: &CTerm, notes: [i32; FS_MEL_SIZE]) -> Melody {
        let mut mel = match term {
            CTerm::Lam(_) => Melody::new_even(fm_basic() * 0.28, &notes),
            _ => unimplemented!()
        };
        let rhythm = self.crhythm(term);
        mel.adjust_timings(&rhythm).unwrap();
        // mel.map_indexed(|i, (n, _d)| (*n, rhythm[i]));
        mel.adjust_depth(self.depth);
        mel
    }

    pub fn with_effect(&self, notes: [i32; FS_MEL_SIZE]) -> EffectMel<Unit<U1, U1>> {
        match &self.parent {
            Term::I(parent) => {
                let mel = self.imelody(parent, notes);
                let effect = self.ieffect(parent);
                EffectMel::new(mel, effect)
            },
            Term::C(parent) => {
                let mel = self.cmelody(parent, notes);
                let effect = self.ceffect(parent);
                EffectMel::new(mel, effect)
            }
        }
    }
}

impl Default for FullStratifier {
    fn default() -> Self {
        Self::new()
    }
}

impl Selector for FullStratifier {
    fn isound(&self, term: &ITerm) -> SoundTree {
        let notes = self.inotes(term);
        let eff = self.with_effect(notes);
        SoundTree::sound(eff, self.imeta(term))
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        let notes = self.cnotes(term);
        let eff = self.with_effect(notes);
        SoundTree::sound(eff, self.cmeta(term))
    }

    fn imerge(&self, term: &ITerm) -> Self {
        Self {
            parent: Term::I(term.clone()),
            depth: self.depth + 1
        }
    }

    fn cmerge(&self, term: &CTerm) -> Self {
        Self {
            parent: Term::C(term.clone()),
            depth: self.depth + 1
        }
    }

    fn imeta(&self, term: &ITerm) -> TreeMetadata {
        const PURPLES: (Color, Color) = (Color::rgb8(0x59, 0x08, 0x7E), Color::rgb8(0xAE, 0x69, 0xE2));
        const ORANGES: (Color, Color) = (ORANGE, PALEORANGE);
        const YELLOWS: (Color, Color) = (Color::rgb8(0xC1, 0xA1, 0x00), Color::rgb8(0xE3, 0xFA, 0x5F));
        const PINKS: (Color, Color) = (Color::rgb8(0xB2, 0x00, 0x74), Color::rgb8(0xFA, 0x75, 0x9E));
        const GREENS: (Color, Color) = (Color::rgb8(0x00, 0x96, 0x44), Color::rgb8(0x8E, 0xFC, 0x62)); 
        let (alt_color, base_color) = match term {
            // ITerm::Ann(_, _) => Color::rgb8(0x86, 0xD8, 0x4B),
            // ITerm::Ann(_, _) => Color::GREEN,
            // ITerm::Ann(_, _) => Color::rgb8(0xBA, 0x7F, 0x3E),
            ITerm::Ann(_, _) => PINKS,
            // ITerm::Star => Color::rgb8(0x4D, 0x91, 0xEF),
            ITerm::Star => ORANGES,
            // ITerm::Pi(_, _) => Color::rgb8(0x2E, 0x6F, 0x94),
            // ITerm::Pi(_, _) => Color::rgb8(0xC5, 0xFF, 0x00),
            // ITerm::Pi(_, _) => Color::rgb8(0xA3, 0xF0, 0x95),
            // ITerm::Pi(_, _) => (Color::rgb8(0x6C, 0x04, 0x1C), Color::RED),
            ITerm::Pi(_, _) => PURPLES,
            ITerm::Bound(_) => (Color::GRAY, Color::GRAY),
            // ITerm::Free(_) => Color::rgb8(0x5E, 0xB4, 0x97),
            // ITerm::Free(_) => Color::rgb8(0x4A, 0xE4, 0x87),
            // ITerm::Free(_) => Color::rgb8(0x6C, 0x04, 0x1C),
            ITerm::Free(_) => YELLOWS,
            // ITerm::App(_, _) => Color::rgb8(0x8C, 0x3E, 0x64),
            ITerm::App(_, _) => GREENS,
            // ITerm::App(_, _) => Color::rgb8(0xE6, 0xDB, 0x61),
            // ITerm::Zero => (DEEPPURPLE, Color::PURPLE),
            ITerm::Zero => (Color::grey(0.373), Color::grey(0.5)),
            // ITerm::Fin(_) => (Color::TEAL, Color::rgb8(0x43, 0xA6, 0xB5)),
            ITerm::Fin(_) => (Color::RED, Color::MAROON),
            // _ => unimplemented!()
            _ => (Color::BLACK, Color::BLACK)
        };
        TreeMetadata {
            name: format!("{:?}", term.tag()),
            // parent: format!("{:?}", term.tag()),
            base_color,
            alt_color,
            max_depth: self.depth
        }
    }

    fn cmeta(&self, term: &CTerm) -> TreeMetadata {
        const BLUES: (Color, Color) = (Color::rgb8(0x00, 0x73, 0x96), Color::rgb8(0x40, 0xF2, 0xE9));
        match term {
            CTerm::Inf(iterm) => self.imeta(iterm),
            // CTerm::Lam(_) => TreeMetadata { name: term.to_string(), color: Color::rgb8(0x52, 0x1D, 0x67) },
            // CTerm::Lam(_) => TreeMetadata { name: format!("{:?}", term.tag()), color: Color::TEAL },
            // CTerm::Lam(_) => TreeMetadata { name: format!("{:?}", term.tag()), color: Color::rgb8(0x18, 0x4D, 0x02) },
            CTerm::Lam(_) => TreeMetadata {
                name: format!("{:?}", term.tag()),
                base_color: BLUES.1, 
                alt_color: BLUES.0,
                max_depth: self.depth
            },
        }
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

impl Default for Rhythmizer {
    fn default() -> Self {
        Self::new()
    }
}

impl Selector for Rhythmizer {
    fn isound(&self, term: &ITerm) -> SoundTree {
        let mel = self.imelody(term);
        SoundTree::sound(mel, self.imeta(term))
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        let mel = self.cmelody(term);
        SoundTree::sound(mel, self.cmeta(term))
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
        Self { rhythm, depth: self.depth + 1 }
    }

    fn cmerge(&self, term: &CTerm) -> Self {
        let rhythm = match term {
            CTerm::Inf(_) => panic!("inf merge???"),
            CTerm::Lam(_) => vec![1.5, 0.5, 1., 0.5],
        };
        Self { rhythm, depth: self.depth + 1 }
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
        SoundTree::sound(lp, self.imeta(term))
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        let base_mel = match self.body.csound(term) {
            SoundTree::Sound(sequenceable, _) => sequenceable,
            _ => panic!("csound can only construct Sound")
        };
        let mut lp = Loop::new(base_mel);
        lp.set_duration(lp.loop_duration() * 32.0 * 0.5_f64.powi(self.depth.div_ceil(4) as i32));
        SoundTree::sound(lp, self.cmeta(term))
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
            clip1: WaveClip::from_file("files/universalquantifier.mp3"), 
            clip2: WaveClip::from_file("files/boundvariable.mp3"),
            depth: 1
        }
    }
}

impl Default for MixedOutput {
    fn default() -> Self {
        Self::new()
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
                SoundTree::sound(mel, self.imeta(term))
            },
            Mixer::Clip(mut wav) => {
                wav.adjust_depth(self.depth);
                SoundTree::sound(wav, self.imeta(term))
            },
            Mixer::Texture(mut tex) => {
                tex.adjust_depth(self.depth);
                SoundTree::sound(tex, self.imeta(term))
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
                SoundTree::sound(mel, self.cmeta(term))
            },
            Mixer::Clip(mut wav) => {
                wav.adjust_depth(self.depth);
                SoundTree::sound(wav, self.cmeta(term))
            },
            Mixer::Texture(mut tex) => {
                tex.adjust_depth(self.depth);
                SoundTree::sound(tex, self.cmeta(term))
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

impl Default for Effector {
    fn default() -> Self {
        Self::new()
    }
}

impl Selector for Effector {
    fn isound(&self, term: &ITerm) -> SoundTree {
        let melody = MelodySelector::F.imelody(term, self.depth);
        let eff = EffectMel::new(melody, self.effect.clone());
        // melody.instrument = unit(Box::new(melody.instrument >> self.effect.clone()));
        SoundTree::sound(eff, self.imeta(term))
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        let melody = MelodySelector::F.cmelody(term, self.depth);
        let eff = EffectMel::new(melody, self.effect.clone());
        // melody.instrument = unit(Box::new(melody.instrument >> self.effect.clone()));
        SoundTree::sound(eff, self.cmeta(term))
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
pub struct StratifyInstrument {
    pub mel: Melody,
    depth: usize,
}

impl Default for StratifyInstrument {
    fn default() -> Self {
        Self { mel: Melody::new_even(violinish(), &[A, D, F, A]), depth: 0 }
    }
}

impl Selector for StratifyInstrument {
    fn isound(&self, term: &ITerm) -> SoundTree {
        let mut mel = self.mel.clone();
        mel.adjust_depth(self.depth);
        SoundTree::sound(mel, self.imeta(term))
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        let mut mel = self.mel.clone();
        mel.adjust_depth(self.depth);
        SoundTree::sound(mel, self.cmeta(term))
    }

    fn imerge(&self, term: &ITerm) -> StratifyInstrument {
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
        StratifyInstrument { mel, depth: self.depth + 1 }
    }

    fn cmerge(&self, term: &CTerm) -> StratifyInstrument {
        let mel = match term {
            CTerm::Lam(_) => Melody::new_even(fm_basic() * 0.28, &[D, F, E, C]),
            _ => Melody::new_even(sine(), &[G, G, G, G]),  //value will never be used, but we have to call this for ownership reasons
        };
        StratifyInstrument { mel, depth: self.depth + 1 }
    }
}

#[derive(Clone)]
pub struct ClipSelector {
    ann: Arc<Wave>,
    star: Arc<Wave>,
    pi: Arc<Wave>,
    bound: Arc<Wave>,
    free: Arc<Wave>,
    app: Arc<Wave>,
    zero: Arc<Wave>,
    fin: Arc<Wave>,
    lam: Arc<Wave>,
}

impl ClipSelector {
    pub fn names() -> Self {
        Self {
            ann: Arc::new(Wave::load("files/annotation.mp3").unwrap()), // type annotation
            star: Arc::new(Wave::load("files/star.mp3").unwrap()),  // universe type
            pi: Arc::new(Wave::load("files/forall.mp3").unwrap()),  // universal quantifier
            bound: Arc::new(Wave::load("files/bound.mp3").unwrap()),  // bound variable
            free: Arc::new(Wave::load("files/free.mp3").unwrap()),  // free variable
            app: Arc::new(Wave::load("files/application.mp3").unwrap()),  // function application
            zero: Arc::new(Wave::load("files/zero.mp3").unwrap()),  // natural number zero
            fin: Arc::new(Wave::load("files/finite.mp3").unwrap()),  // finite type
            lam: Arc::new(Wave::load("files/lambdaabstraction.mp3").unwrap()),  // lambda abstraction
        }
    }

    pub fn names_long() -> Self {
        Self {
            ann: Arc::new(Wave::load("files/annotation.mp3").unwrap()), // type annotation
            star: Arc::new(Wave::load("files/universe.mp3").unwrap()),  // universe type
            pi: Arc::new(Wave::load("files/universalquantifier.mp3").unwrap()),  // universal quantifier
            bound: Arc::new(Wave::load("files/boundvariable.mp3").unwrap()),  // bound variable
            free: Arc::new(Wave::load("files/freevariable.mp3").unwrap()),  // free variable
            app: Arc::new(Wave::load("files/functionapplication.mp3").unwrap()),  // function application
            zero: Arc::new(Wave::load("files/naturalnumberzero.mp3").unwrap()),  // natural number zero
            fin: Arc::new(Wave::load("files/finitetype.mp3").unwrap()),  // finite type
            lam: Arc::new(Wave::load("files/lambdaabstraction.mp3").unwrap()),  // lambda abstraction
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
        SoundTree::sound(Arc::clone(result), self.imeta(term))
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        let result = match term {
            CTerm::Inf(_) => &self.ann,
            CTerm::Lam(_) => &self.lam,
        };
        SoundTree::sound(Arc::clone(result), self.cmeta(term))
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
    /// Like E, but switched around and more textured. See [imelody6] and [cmelody6].
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
        SoundTree::sound(self.mel.imelody(term, self.depth), self.imeta(term))
    }

    fn csound(&self, term: &CTerm) -> SoundTree {
        SoundTree::sound(self.mel.cmelody(term, self.depth), self.cmeta(term))
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
        ITerm::Free(_) => Melody::new_even(karplus(), &[E, G, A, B]),
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