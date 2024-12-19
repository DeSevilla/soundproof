// #![allow(unused)]
use std::rc::Rc;
use fundsp::hacker32::*;

use crate::{
    ast::*,
    eval::*,
    term::*,
    instruments::*,
    notes::*,
    types::*,
    MelodySelector,
};

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
    result.adjust_depth(depth);
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
    result.adjust_depth(depth);
    result
}

pub fn cmelody2(term: &CTerm, depth: usize) -> Melody {
    let mut result = match term {
        CTerm::Inf(_) => Melody::new_even(sine(), &[G, B, E, C]), //value will never be used
        CTerm::Lam(_) => Melody::new_even(fm_epi(), &[G, B, E, C]),
    };
    result.adjust_depth(depth);
    result
}

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

pub fn cmelody3(term: &CTerm, depth: usize) -> Melody {
    let mut result = match term {
        CTerm::Inf(_) => Melody::new_even(sine(), &[]), //value will never be used
        CTerm::Lam(_) => Melody::new_even(fm_epi(), &[G, B, E, C]),
    };
    result.adjust_depth(depth);
    result
}

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

pub fn cmelody4(term: &CTerm, depth: usize) -> Melody {
    let mut result = match term {
        CTerm::Inf(_) => Melody::new_even(sine(), &[]), //never used
        CTerm::Lam(_) => Melody::new_even(wobbly_sine(), &[G, B, E, C]),
    };
    result.adjust_depth(depth);
    result
}

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

pub fn cmelody_oneinstr(instrument: impl AudioUnit + 'static, term: &CTerm, depth: usize) -> Melody {
    let mut result = match term {
        CTerm::Inf(_) => Melody::new_even(sine(), &[A, A, A, A]), //value will never be used
        CTerm::Lam(_) => Melody::new_even(instrument, &[G, B, E, C]),
    };
    result.adjust_depth(depth);
    result
}


pub fn itype_translate_full(i: usize, ctx: Context, term: &ITerm, depth: usize, mel: MelodySelector) -> Result<(Type, SoundTree), String> {
    // println!("{i}typing term {term:?} in context {}", ctx.iter().fold("".to_owned(), |acc, e| acc + &format!(" {:?}", e.0)));
    let base_tree = SoundTree::sound(mel.imelody(term, depth));
    match term {
        ITerm::Ann(ct, cty) => {
            let tytree = ctype_translate_full(i, ctx.clone(), cty, Value::Star, depth + 1, mel)?; // TODO figure this out better
            let ty = c_eval(cty.clone(), vec![]);
            let termtree = ctype_translate_full(i, ctx, ct, ty.clone(), depth + 1, mel)?;
            let tree = SoundTree::simul(&[base_tree, termtree, tytree]);
            Ok((ty, tree))
        }
        ITerm::Star => Ok((Value::Star, base_tree)),
        ITerm::Pi(src, trg) => {
            let srctree = ctype_translate_full(i, ctx.clone(), src, Value::Star, depth + 1, mel)?;
            let ty = c_eval(src.clone(), vec![]);
            let mut new_ctx = ctx.clone();
            new_ctx.push((Name::Local(i), ty.clone()));
            // println!("Pushing {i} to ctx");
            let trgtree = ctype_translate_full(i + 1, new_ctx, &c_subst(0, ITerm::Free(Name::Local(i)), trg.clone()), Value::Star, depth + 1, mel)?;
            let tree = SoundTree::simul(&[base_tree, SoundTree::seq(&[srctree, trgtree])]); // TODO fill this in
            Ok((Value::Star, tree))
        },
        ITerm::Free(name) => ctx.iter().find(|x| x.0 == *name)
            .map(|x| (x.1.clone(), SoundTree::sound(mel.cmelody(&quote0(x.1.clone()), 0))))
            .ok_or("Could not find variable".to_owned()),
        ITerm::App(f, x) => {
            let (fty, ftree) = itype_translate_full(i, ctx.clone(), f, depth + 1, mel)?;
            match fty {
                Value::Pi(src, trg) => {
                    let xtree = ctype_translate_full(i, ctx, x, *src, depth + 1, mel)?;
                    Ok((trg(c_eval(x.clone(), vec![])), SoundTree::simul(&[base_tree, SoundTree::seq(&[ftree, xtree])])))
                },
                _ => Err("Invalid function call".to_owned())
            }
        },
        ITerm::Nat => Ok((Value::Star, base_tree)),
        ITerm::Zero => Ok((Value::Nat, base_tree)),
        ITerm::Succ(k) => {
            let subtree = ctype_translate_full(i, ctx, k, Value::Nat, depth + 1, mel)?;
            // TODO we need succ to be a different sort of thing... not a full melody
            Ok((Value::Nat, SoundTree::seq(&[base_tree, subtree])))
        },
        ITerm::NatElim(motive, base, ind, k) => {
            let mtree = ctype_translate_full(i, ctx.clone(), motive, Value::Pi(Box::new(Value::Nat), Rc::new(|_| Value::Star)), depth + 1, mel)?;
            let m_val = c_eval(motive.clone(), vec![]);
            let m_val1 = m_val.clone();
            let bctree = ctype_translate_full(i, ctx.clone(), base, vapp(m_val.clone(), Value::Zero), depth + 1, mel)?;
            let indtree = ctype_translate_full(i, ctx.clone(), ind,
                Value::Pi(Box::new(Value::Nat),
                    Rc::new(move |l| {let m_val2 = m_val.clone(); Value::Pi(Box::new(vapp(m_val.clone(), l.clone())), 
                        Rc::new(move |_| vapp(m_val2.clone(), Value::Succ(Box::new(l.clone()))))) }
                    )
                ), depth + 1, mel
            )?;
            let ktree = ctype_translate_full(i, ctx, k, Value::Nat, depth + 1, mel)?;
            let k_val = c_eval(k.clone(), vec![]);
            Ok((vapp(m_val1, k_val), SoundTree::simul(&[base_tree, SoundTree::seq(&[mtree, bctree, indtree, ktree])])))
        },
        ITerm::Fin(n) => {
            let subtree = ctype_translate_full(i, ctx, n, Value::Nat, depth + 1, mel)?;
            //TODO needs some work like Succ & in fact in parallel
            Ok((Value::Star, SoundTree::simul(&[base_tree, subtree])))
        },
        ITerm::FZero(n) => {
            let subtree = ctype_translate_full(i, ctx, n, Value::Nat, depth + 1, mel)?;
            let n_val = c_eval(n.clone(), vec![]);
            Ok((Value::Fin(Box::new(Value::Succ(Box::new(n_val)))), SoundTree::simul(&[base_tree, subtree])))
        },
        ITerm::FSucc(n, fp) => {
            let ntree = ctype_translate_full(i, ctx.clone(), n, Value::Nat, depth + 1, mel)?;
            let n_val = c_eval(n.clone(), vec![]);
            match &n_val {
                Value::Succ(m) => {
                    let fptree = ctype_translate_full(i, ctx, fp, Value::Fin(m.clone()), depth + 1, mel)?;
                    let tree = SoundTree::simul(&[base_tree, SoundTree::seq(&[ntree, fptree])]);
                    Ok((Value::Fin(Box::new(n_val)), tree))
                }
                _ => Err("oh no bad finitism".to_owned())
            }
        },
        ITerm::FinElim(motive, base, ind, n, f) => {
            let mtree = ctype_translate_full(i, ctx.clone(), motive, Value::Pi(Box::new(Value::Nat), Rc::new(|k| 
                Value::Pi(Box::new(Value::Fin(Box::new(k))), Rc::new(|_| Value::Star))
            )), depth + 1, mel)?;
            let ntree = ctype_translate_full(i, ctx.clone(), n, Value::Nat, depth + 1, mel)?;
            let motive_val = c_eval(motive.clone(), vec![]); //we'll need NameEnv instead of just ctx if we want more parsing etc.
            let n_val = c_eval(n.clone(), vec![]);
            let motive_val2 = motive_val.clone(); //jeez
            let bctree = ctype_translate_full(i, ctx.clone(), base, Value::Pi(Box::new(Value::Nat), Rc::new(
                move |k| vapp(vapp(motive_val.clone(), Value::Succ(Box::new(k.clone()))), Value::FZero(Box::new(k)))
            )), depth + 1, mel)?;
            let motive_val = motive_val2.clone();
            let indtree = ctype_translate_full(i, ctx.clone(), ind, Value::Pi(Box::new(Value::Nat), Rc::new(
                move |k| { 
                    // let k = k.clone(); 
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
            let ftree = ctype_translate_full(i, ctx, f, Value::Fin(Box::new(n_val.clone())), depth + 1, mel)?;
            let f_val = c_eval(f.clone(), vec![]);
            let tree = SoundTree::simul(&[base_tree, SoundTree::seq(&[mtree, bctree, indtree, ftree, ntree])]);
            Ok((vapp(vapp(motive_val2, n_val), f_val), tree))
        },
        ITerm::Bound(n) => Err(format!("Not sure how to assign a type to Bound({n}) in context {}, how did we get this?", 
            ctx.iter().map(|x| x.0.clone()).fold("".to_owned(), |acc, e| acc + &format!(" {:?}", e))))
    }
}

pub fn ctype_translate_full(i: usize, ctx: Context, term: &CTerm, ty: Type, depth: usize, mel: MelodySelector) -> Result<SoundTree, String> {
    // let tm = term.clone();
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
                // println!("Pushed {i} to ctx");
                let subtree = ctype_translate_full(i + 1, new_ctx, &c_subst(0, ITerm::Free(Name::Local(i)), *body.clone()), trg(vfree(Name::Local(i))), depth + 1, mel)?;
                Ok(SoundTree::simul(&[SoundTree::sound(mel.cmelody(term, depth)), subtree]))
            },
            _ => Err("Function must have pi type".to_owned())
        }
    }
}

pub fn iterm_translate(term: ITerm, depth: usize, mel: MelodySelector) -> SoundTree {
    //to customize the sound we edit this to change the function that produces the melody
    //if we wanted more customization I think we'd need to pass in files
    // let mel = imelody2(&term, depth);
    let base_mel = mel.imelody(&term, depth);
    // let mel = imelody_oneinstr((sinesaw() ^ pass() | constant(1.0)) >> lowpass(), &term, depth);
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

pub fn cterm_translate(term: CTerm, depth: usize, mel: MelodySelector) -> SoundTree {
    //to customize the sound we edit this to change function that produces the melody
    // let mel = cmelody_oneinstr(sinesaw(), &term, depth);
    let melody = mel.cmelody(&term, depth);
    match term {
        CTerm::Inf(iterm) => iterm_translate(*iterm, depth, mel),
        CTerm::Lam(cterm) => SoundTree::simul(&[SoundTree::sound(melody), SoundTree::seq(&[cterm_translate(*cterm, depth + 1, mel)])]),
    }
}

// fn depth_amp(incr: f32, depth: usize) -> An<impl AudioNode<Inputs=U0, Outputs=U1>> {
//     onepress(incr * lerp(0.25, 0.95, 1.0 / depth as f32), incr) >>
//         adsr_live(incr * 0.2 / (depth as f32 + 0.1), incr * 0.25, lerp(0.25, 0.5, 1.0 / depth as f32), incr * 0.1)
// }
