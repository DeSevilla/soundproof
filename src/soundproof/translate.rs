// #![allow(unused)]
use std::rc::Rc;
use fundsp::hacker32::*;

use crate::{
    ast::*,
    term::*,
    instruments::*,
    notes::*,
    types::*,
    MelodySelector,
};

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


/// Translates ITerms into SoundTrees according to their type structure.
/// Subterms have their types checked or inferred and have their own melodies combined with their types' melodies.
pub fn itype_translate_full(ii: usize, ctx: Context, term: &ITerm, depth: usize, mel: MelodySelector) -> Result<(Type, SoundTree), String> {
    let node_melody = SoundTree::sound(mel.imelody(term, depth));
    match term {
        ITerm::Ann(ct, cty) => {
            let tytree = ctype_translate_full(ii, ctx.clone(), cty, Value::Star, depth + 1, mel)?; // is this the right way to structure this?
            let ty = cty.clone().eval(vec![]);
            let termtree = ctype_translate_full(ii, ctx, ct, ty.clone(), depth + 1, mel)?;  // should we have the type and term at diff depths?
            let tree = SoundTree::simul(&[node_melody, SoundTree::seq(&[termtree, tytree])]);
            Ok((ty, tree))
        }
        ITerm::Star => Ok((Value::Star, node_melody)),
        ITerm::Pi(src, trg) => {
            let srctree = ctype_translate_full(ii, ctx.clone(), src, Value::Star, depth + 1, mel)?;
            let ty = src.clone().eval(vec![]);
            let mut new_ctx = ctx.clone();
            new_ctx.push((Name::Local(ii), ty.clone()));
            let trgtree = ctype_translate_full(ii + 1, new_ctx, &trg.clone().subst(0, ITerm::Free(Name::Local(ii))), Value::Star, depth + 1, mel)?;
            let tree = SoundTree::simul(&[node_melody, SoundTree::seq(&[srctree, trgtree])]);
            Ok((Value::Star, tree))
        },
        ITerm::Free(name) => { 
            // want to assign a melody to the name... randomization? fixed sequence? how do we associate?
            // maybe we put an environment of some sort in the MelodySelector?
            let val = ctx.iter().find(|x| x.0 == *name).ok_or("Could not find variable".to_owned())?;
            let ty = ctype_translate_full(ii, ctx.clone(), &quote0(val.1.clone()), Value::Star, depth + 1, mel)?;
            let tree = ty;
            Ok((val.1.clone(), tree))
        },
        ITerm::App(f, x) => {
            let (fty, ftree) = itype_translate_full(ii, ctx.clone(), f, depth + 1, mel)?;
            match fty {
                Value::Pi(src, trg) => {
                    let xtree = ctype_translate_full(ii, ctx, x, *src, depth + 1, mel)?;
                    Ok((trg(x.clone().eval(vec![])), SoundTree::simul(&[node_melody, SoundTree::seq(&[ftree, xtree])])))
                },
                _ => Err("Invalid function call".to_owned())
            }
        },
        ITerm::Nat => Ok((Value::Star, node_melody)),
        ITerm::Zero => Ok((Value::Nat, node_melody)),
        ITerm::Succ(k) => {
            let subtree = ctype_translate_full(ii, ctx, k, Value::Nat, depth + 1, mel)?;
            // TODO we need succ to be a different sort of thing... not a full melody
            Ok((Value::Nat, SoundTree::seq(&[node_melody, subtree])))
        },
        ITerm::NatElim(motive, base, ind, k) => {
            let mtree = ctype_translate_full(ii, ctx.clone(), motive, Value::Pi(Box::new(Value::Nat), Rc::new(|_| Value::Star)), depth + 1, mel)?;
            let m_val = motive.clone().eval(vec![]);
            let m_val1 = m_val.clone();
            let bctree = ctype_translate_full(ii, ctx.clone(), base, vapp(m_val.clone(), Value::Zero), depth + 1, mel)?;
            let indtree = ctype_translate_full(ii, ctx.clone(), ind,
                Value::Pi(Box::new(Value::Nat),
                    Rc::new(move |l| {let m_val2 = m_val.clone(); Value::Pi(Box::new(vapp(m_val.clone(), l.clone())), 
                        Rc::new(move |_| vapp(m_val2.clone(), Value::Succ(Box::new(l.clone()))))) }
                    )
                ), depth + 1, mel
            )?;
            let ktree = ctype_translate_full(ii, ctx, k, Value::Nat, depth + 1, mel)?;
            let k_val = k.clone().eval(vec![]);
            Ok((vapp(m_val1, k_val), SoundTree::simul(&[node_melody, SoundTree::seq(&[mtree, bctree, indtree, ktree])])))
        },
        ITerm::Fin(n) => {
            let subtree = ctype_translate_full(ii, ctx, n, Value::Nat, depth + 1, mel)?;
            //TODO needs some work like Succ & in fact in parallel
            Ok((Value::Star, SoundTree::simul(&[node_melody, subtree])))
        },
        ITerm::FZero(n) => {
            let subtree = ctype_translate_full(ii, ctx, n, Value::Nat, depth + 1, mel)?;
            let n_val = n.clone().eval(vec![]);
            Ok((Value::Fin(Box::new(Value::Succ(Box::new(n_val)))), SoundTree::simul(&[node_melody, subtree])))
        },
        ITerm::FSucc(n, fp) => {
            let ntree = ctype_translate_full(ii, ctx.clone(), n, Value::Nat, depth + 1, mel)?;
            let n_val = n.clone().eval(vec![]);
            match &n_val {
                Value::Succ(m) => {
                    let fptree = ctype_translate_full(ii, ctx, fp, Value::Fin(m.clone()), depth + 1, mel)?;
                    let tree = SoundTree::simul(&[node_melody, SoundTree::seq(&[ntree, fptree])]);
                    Ok((Value::Fin(Box::new(n_val)), tree))
                }
                _ => Err("oh no bad finitism".to_owned())
            }
        },
        ITerm::FinElim(motive, base, ind, n, f) => {
            let mtree = ctype_translate_full(ii, ctx.clone(), motive, Value::Pi(Box::new(Value::Nat), Rc::new(|k| 
                Value::Pi(Box::new(Value::Fin(Box::new(k))), Rc::new(|_| Value::Star))
            )), depth + 1, mel)?;
            let ntree = ctype_translate_full(ii, ctx.clone(), n, Value::Nat, depth + 1, mel)?;
            let motive_val = motive.clone().eval(vec![]); //we'll need NameEnv instead of just ctx if we want more parsing etc.
            let n_val = n.clone().eval(vec![]);
            let motive_val2 = motive_val.clone(); //jeez
            let bctree = ctype_translate_full(ii, ctx.clone(), base, Value::Pi(Box::new(Value::Nat), Rc::new(
                move |k| vapp(vapp(motive_val.clone(), Value::Succ(Box::new(k.clone()))), Value::FZero(Box::new(k)))
            )), depth + 1, mel)?;
            let motive_val = motive_val2.clone();
            let indtree = ctype_translate_full(ii, ctx.clone(), ind, Value::Pi(Box::new(Value::Nat), Rc::new(
                move |k| { 
                    let motive_val = motive_val.clone();
                    Value::Pi(Box::new(Value::Fin(Box::new(k.clone()))), Rc::new(
                        move |fk| {
                            let k = k.clone();
                            let motive_val = motive_val.clone();
                            Value::Pi(Box::new(vapp(vapp(motive_val.clone(), k.clone()), fk.clone())), Rc::new(
                                move |_| vapp(vapp(motive_val.clone(), Value::Succ(Box::new(k.clone()))), Value::FSucc(Box::new(k.clone()), Box::new(fk.clone())))
                            ))
                        }
                    ))
                }
            )), depth + 1, mel)?;
            let ftree = ctype_translate_full(ii, ctx, f, Value::Fin(Box::new(n_val.clone())), depth + 1, mel)?;
            let f_val = f.clone().eval(vec![]);
            let tree = SoundTree::simul(&[node_melody, SoundTree::seq(&[mtree, bctree, indtree, ftree, ntree])]);
            Ok((vapp(vapp(motive_val2, n_val), f_val), tree))
        },
        ITerm::Bound(n) => Err(format!("Not sure how to assign a type to Bound({n}) in context {}, how did we get this?", 
            ctx.iter().map(|x| x.0.clone()).fold("".to_owned(), |acc, e| acc + &format!(" {:?}", e))))
    }
}

/// Translates CTerms into SoundTrees according to their type structure.
/// Subterms have their types checked or inferred and have their own melodies combined with their types' melodies.
pub fn ctype_translate_full(i: usize, ctx: Context, term: &CTerm, ty: Type, depth: usize, mel: MelodySelector) -> Result<SoundTree, String> {
    match term {
        CTerm::Inf(it) => {
            let (ity, tree) = itype_translate_full(i, ctx, it, depth, mel)?;
            if quote0(ity.clone()) != quote0(ty.clone()) {
                Err(format!("Expected {}, inferred {}, for term {}, level {i}", quote(i, ty), quote(i, ity), term))
            }
            else {
                Ok(tree)
            }
        },
        CTerm::Lam(body) => match ty {
            Value::Pi(src, trg) => {
                let mut new_ctx = ctx.clone();
                new_ctx.push((Name::Local(i), *src));
                let subtree = ctype_translate_full(i + 1, new_ctx, &body.clone().subst(0, ITerm::Free(Name::Local(i))), trg(vfree(Name::Local(i))), depth + 1, mel)?;
                Ok(SoundTree::simul(&[SoundTree::sound(mel.cmelody(term, depth)), subtree]))
            },
            _ => Err("Function must have pi type".to_owned())
        }
    }
}

/// Translates ITerms to SoundTrees on a pure subterm-to-subtree basis.
pub fn iterm_translate(term: ITerm, depth: usize, mel: MelodySelector) -> SoundTree {
    let base_mel = mel.imelody(&term, depth);
    match term {
        ITerm::Ann(cterm, cterm1) => SoundTree::simul(&[SoundTree::sound(base_mel), SoundTree::seq(&[cterm_translate(cterm, depth + 1, mel), cterm_translate(cterm1, depth + 1, mel)])]),
        ITerm::Star => SoundTree::sound(base_mel),
        ITerm::Pi(cterm, cterm1) => SoundTree::simul(&[SoundTree::sound(base_mel), SoundTree::seq(&[cterm_translate(cterm, depth + 1, mel), cterm_translate(cterm1, depth + 1, mel)])]),
        ITerm::Bound(_) => SoundTree::sound(base_mel),
        ITerm::Free(_) => SoundTree::sound(base_mel),
        ITerm::App(iterm, cterm) => SoundTree::simul(&[SoundTree::sound(base_mel), SoundTree::seq(&[iterm_translate(*iterm, depth + 1, mel), cterm_translate(cterm, depth + 1, mel)])]),
        ITerm::Nat => SoundTree::sound(base_mel),
        ITerm::Zero => SoundTree::sound(base_mel),
        ITerm::Succ(cterm) => SoundTree::simul(&[SoundTree::sound(base_mel), SoundTree::seq(&[cterm_translate(cterm, depth + 1, mel)])]),
        ITerm::NatElim(motive, base, ind, k) => SoundTree::simul(&[SoundTree::sound(base_mel), SoundTree::seq(&[
            cterm_translate(motive, depth + 1, mel),
            cterm_translate(base, depth + 1, mel),
            cterm_translate(ind, depth + 1, mel),
            cterm_translate(k, depth + 1, mel),
        ])]),
        ITerm::Fin(cterm) => SoundTree::simul(&[SoundTree::sound(base_mel), SoundTree::seq(&[cterm_translate(cterm, depth + 1, mel)])]),
        ITerm::FinElim(motive, base, ind, n, f) => SoundTree::simul(&[SoundTree::sound(base_mel), SoundTree::seq(&[
            cterm_translate(motive, depth + 1, mel),
            cterm_translate(base, depth + 1, mel),
            cterm_translate(ind, depth + 1, mel),
            cterm_translate(n, depth + 1, mel),
            cterm_translate(f, depth + 1, mel),
        ])]),
        ITerm::FZero(cterm) => SoundTree::simul(&[SoundTree::sound(base_mel), SoundTree::seq(&[cterm_translate(cterm, depth + 1, mel)])]),
        ITerm::FSucc(cterm, cterm1) => SoundTree::simul(&[SoundTree::sound(base_mel), SoundTree::seq(&[cterm_translate(cterm, depth + 1, mel), cterm_translate(cterm1, depth + 1, mel)])]),
    }
}

/// Translates CTerms to SoundTrees on a pure subterm-to-subtree basis.
pub fn cterm_translate(term: CTerm, depth: usize, mel: MelodySelector) -> SoundTree {
    let melody = mel.cmelody(&term, depth);
    match term {
        CTerm::Inf(iterm) => iterm_translate(*iterm, depth, mel),
        CTerm::Lam(cterm) => SoundTree::simul(&[SoundTree::sound(melody), SoundTree::seq(&[cterm_translate(*cterm, depth + 1, mel)])]),
    }
}