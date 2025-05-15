use std::rc::Rc;

use fundsp::hacker32::*;
use crate::{
    ast::*,
    instruments::*,
    notes::*,
    types::*,
};


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
            star: Rc::new(Wave::load("files/universetype.mp3").unwrap()),  // universe type
            pi: Rc::new(Wave::load("files/universalquantifier.mp3").unwrap()),  // universal quantifier
            bound: Rc::new(Wave::load("files/boundvariable.mp3").unwrap()),  // bound variable
            free: Rc::new(Wave::load("files/freevariable.mp3").unwrap()),  // free variable
            app: Rc::new(Wave::load("files/functionapplication.mp3").unwrap()),  // function application
            zero: Rc::new(Wave::load("files/naturalnumberzero.mp3").unwrap()),  // natural number zero
            fin: Rc::new(Wave::load("files/finitetype.mp3").unwrap()),  // finite type
            lam: Rc::new(Wave::load("files/lambdaabstraction.mp3").unwrap()),  // lambda abstraction
        }
    }



    fn imelody(&self, term: &ITerm, _depth: usize) -> Rc<Wave> {
        let result = match term {
            ITerm::Ann(_, _) => Rc::clone(&self.ann),
            ITerm::Star => Rc::clone(&self.star),
            ITerm::Pi(_, _) => Rc::clone(&self.pi),
            ITerm::Bound(_) => Rc::clone(&self.bound),
            ITerm::Free(_) => Rc::clone(&self.free),
            ITerm::App(_, _) => Rc::clone(&self.app),
            ITerm::Zero => Rc::clone(&self.zero),
            ITerm::Fin(_) => Rc::clone(&self.fin),
            _ => unimplemented!()
        };
        result
    }

    fn cmelody(&self, term: &CTerm, _depth: usize) -> Rc<Wave> {
        match term {
            CTerm::Inf(_) => Rc::clone(&self.ann),
            CTerm::Lam(_) => Rc::clone(&self.lam),
        }
    }
}

/// Determines which set of melodies to use when generating melodies for a term.
#[derive(Clone)]
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
    Concrete(ClipSelector),
}

impl MelodySelector {
    /// Get the appropriate melody for a certain type of [ITerm].
    pub fn imelody(&self, term: &ITerm, depth: usize) -> SoundTree {
        match self {
            MelodySelector::A => SoundTree::sound(imelody1(term, depth), term),
            MelodySelector::B => SoundTree::sound(imelody2(term, depth), term),
            MelodySelector::C => SoundTree::sound(imelody3(term, depth), term),
            MelodySelector::D => SoundTree::sound(imelody4(term, depth), term),
            MelodySelector::E => SoundTree::sound(imelody5(term, depth), term),
            MelodySelector::F => SoundTree::sound(imelody6(term, depth), term),
            MelodySelector::PureSine => SoundTree::sound(imelody_oneinstr(sine(), term, depth), term),
            MelodySelector::Concrete(clip_selector) => SoundTree::sound(clip_selector.imelody(term, depth), term),
        }
    }

    /// Get the appropriate melody for a certain type of [CTerm].
    pub fn cmelody(&self, term: &CTerm, depth: usize) -> SoundTree {
        match self {
            MelodySelector::A => SoundTree::sound(cmelody2(term, depth), term.clone()),
            MelodySelector::B => SoundTree::sound(cmelody2(term, depth), term.clone()),
            MelodySelector::C => SoundTree::sound(cmelody3(term, depth), term.clone()),
            MelodySelector::D => SoundTree::sound(cmelody4(term, depth), term.clone()),
            MelodySelector::E => SoundTree::sound(cmelody5(term, depth), term.clone()),
            MelodySelector::F => SoundTree::sound(cmelody6(term, depth), term.clone()),
            MelodySelector::PureSine => SoundTree::sound(cmelody_oneinstr(sine(), term, depth), term.clone()),
            MelodySelector::Concrete(clip_selector) => SoundTree::sound(clip_selector.cmelody(term, depth), term.clone()),
        }
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