// #![allow(unused)]
use std::rc::Rc;

use crate::{
    ast::*,
    term::*,
    types::*,
    select::*,
};

/// Translates [ITerm]s into [SoundTree]s according to their type structure.
/// Subterms have their types checked or inferred and have their melodies combined with their types' melodies.
pub fn type_translate(term: ITerm, mel: &MelodySelector) -> SoundTree {
    itype_translate_full(0, vec![], &term, 1, mel).unwrap().1
}

/// Translates [ITerm]s into [SoundTree]s according to their type structure.
/// Subterms have their types checked or inferred and have their melodies combined with their types' melodies.
pub fn strat_translate(term: ITerm) -> SoundTree {
    itype_stratified_translate(0, vec![], &term, 1, StratifiedInfo::default()).unwrap().1
}

/// Translates [ITerm]s into [SoundTree]s according to their term structure.
/// Subterms are converted to melodies and run along with their outer terms.
pub fn term_translate(term: ITerm, mel: &MelodySelector) -> SoundTree {
    iterm_translate_full(&term, 0, mel)
}

fn itype_stratified_translate(ii: usize, ctx: Context, term: &ITerm, depth: usize, meta: StratifiedInfo) -> Result<(Type, SoundTree), String> {
    // the process for defining the sounds is fairly subjective - there's no one correct answer,
    // we just want something that sounds good and represents the structure.
    // let node_melody = SoundTree::sound(mel.imelody(term, depth), term);
    let node_melody = meta.istratify(term, depth);
    let meta = StratifiedInfo::imelody(term, depth);
    match term {
        ITerm::Ann(ct, cty) => {
            // should we modify something in this to present an annotation better? term-translate the type, maybe?
            let tytree = ctype_stratified_translate(ii, ctx.clone(), cty, Value::Star, depth + 1, meta.clone())?;
            let ty = cty.clone().eval(vec![]);
            let termtree = ctype_stratified_translate(ii, ctx, ct, ty.clone(), depth + 1, meta)?;  // should we have the type and term at diff depths?
            let tree = SoundTree::simul(&[node_melody, SoundTree::seq(&[termtree, tytree])]);
            Ok((ty, tree))
        }
        ITerm::Star => Ok((Value::Star, node_melody)),
        ITerm::Pi(src, trg) => {
            let srctree = ctype_stratified_translate(ii, ctx.clone(), src, Value::Star, depth + 1, meta.clone())?;
            let ty = src.clone().eval(vec![]);
            let mut new_ctx = ctx.clone();
            new_ctx.push((Name::Local(ii), ty.clone()));
            let trgtree = ctype_stratified_translate(
                ii + 1, 
                new_ctx, 
                &trg.clone().subst(0, ITerm::Free(Name::Local(ii))), 
                Value::Star, 
                depth + 1, 
                meta
            )?;
            let tree = SoundTree::simul(&[node_melody, SoundTree::seq(&[srctree, trgtree])]);
            Ok((Value::Star, tree))
        },
        ITerm::Free(name) => { 
            // want to assign a melody to the name... randomization? fixed sequence? how do we associate?
            // maybe we put an environment of some sort in the MelodySelector?
            let val = ctx.iter().find(|x| x.0 == *name).ok_or("Could not find variable".to_owned())?;
            let ty = ctype_stratified_translate(ii, ctx.clone(), &quote0(val.1.clone()), Value::Star, depth + 1, meta)?;
            let tree = ty;
            Ok((val.1.clone(), tree))
        },
        ITerm::App(f, x) => {
            let (fty, ftree) = itype_stratified_translate(ii, ctx.clone(), f, depth + 1, meta.clone())?;
            match fty {
                Value::Pi(src, trg) => {
                    let xtree = ctype_stratified_translate(ii, ctx, x, *src, depth + 1, meta)?;
                    Ok((trg(x.clone().eval(vec![])), SoundTree::simul(&[node_melody, SoundTree::seq(&[ftree, xtree])])))
                },
                _ => Err("Invalid function call".to_owned())
            }
        },
        ITerm::Nat => Ok((Value::Star, node_melody)),
        ITerm::Zero => Ok((Value::Nat, node_melody)),
        ITerm::Succ(k) => {
            let subtree = ctype_stratified_translate(ii, ctx, k, Value::Nat, depth + 1, meta)?;
            // TODO we need succ to be a different sort of thing... not a full melody
            // like a rising burble perhaps...
            // but I don't think we have any Succs in the paradox so it's fine for now
            Ok((Value::Nat, SoundTree::seq(&[node_melody, subtree])))
        },
        ITerm::NatElim(motive, base, ind, k) => {
            let mtree = ctype_stratified_translate(ii, ctx.clone(), motive, Value::Pi(Box::new(Value::Nat), Rc::new(|_| Value::Star)), depth + 1, meta.clone())?;
            let m_val = motive.clone().eval(vec![]);
            let m_val1 = m_val.clone();
            let bctree = ctype_stratified_translate(ii, ctx.clone(), base, vapp(m_val.clone(), Value::Zero), depth + 1, meta.clone())?;
            let indtree = ctype_stratified_translate(ii, ctx.clone(), ind,
                Value::Pi(Box::new(Value::Nat),
                    Rc::new(move |l| {let m_val2 = m_val.clone(); Value::Pi(Box::new(vapp(m_val.clone(), l.clone())), 
                        Rc::new(move |_| vapp(m_val2.clone(), Value::Succ(Box::new(l.clone()))))) }
                    )
                ), depth + 1, meta.clone()
            )?;
            let ktree = ctype_stratified_translate(ii, ctx, k, Value::Nat, depth + 1, meta)?;
            let k_val = k.clone().eval(vec![]);
            Ok((vapp(m_val1, k_val), SoundTree::simul(&[node_melody, SoundTree::seq(&[mtree, bctree, indtree, ktree])])))
        },
        ITerm::Fin(n) => {
            let subtree = ctype_stratified_translate(ii, ctx, n, Value::Nat, depth + 1, meta)?;
            //TODO finite set translation needs some work like Succ & in fact in parallel since it's so similar
            Ok((Value::Star, SoundTree::simul(&[node_melody, subtree])))
        },
        ITerm::FZero(n) => {
            let subtree = ctype_stratified_translate(ii, ctx, n, Value::Nat, depth + 1, meta)?;
            let n_val = n.clone().eval(vec![]);
            Ok((Value::Fin(Box::new(Value::Succ(Box::new(n_val)))), SoundTree::simul(&[node_melody, subtree])))
        },
        ITerm::FSucc(n, fp) => {
            let ntree = ctype_stratified_translate(ii, ctx.clone(), n, Value::Nat, depth + 1, meta.clone())?;
            let n_val = n.clone().eval(vec![]);
            match &n_val {
                Value::Succ(m) => {
                    let fptree = ctype_stratified_translate(ii, ctx, fp, Value::Fin(m.clone()), depth + 1, meta)?;
                    let tree = SoundTree::simul(&[node_melody, SoundTree::seq(&[ntree, fptree])]);
                    Ok((Value::Fin(Box::new(n_val)), tree))
                }
                _ => Err("oh no bad finitism".to_owned())
            }
        },
        ITerm::FinElim(motive, base, ind, n, f) => {
            let mtree = ctype_stratified_translate(ii, ctx.clone(), motive, Value::Pi(Box::new(Value::Nat), Rc::new(|k| 
                Value::Pi(Box::new(Value::Fin(Box::new(k))), Rc::new(|_| Value::Star))
            )), depth + 1, meta.clone())?;
            let ntree = ctype_stratified_translate(ii, ctx.clone(), n, Value::Nat, depth + 1, meta.clone())?;
            let motive_val = motive.clone().eval(vec![]); //we'll need NameEnv instead of just ctx if we want more parsing etc.
            let n_val = n.clone().eval(vec![]);
            let motive_val2 = motive_val.clone(); //jeez
            let bctree = ctype_stratified_translate(ii, ctx.clone(), base, Value::Pi(Box::new(Value::Nat), Rc::new(
                move |k| vapp(vapp(motive_val.clone(), Value::Succ(Box::new(k.clone()))), Value::FZero(Box::new(k)))
            )), depth + 1, meta.clone())?;
            let motive_val = motive_val2.clone();
            let indtree = ctype_stratified_translate(ii, ctx.clone(), ind, Value::Pi(Box::new(Value::Nat), Rc::new(
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
            )), depth + 1, meta.clone())?;
            let ftree = ctype_stratified_translate(ii, ctx, f, Value::Fin(Box::new(n_val.clone())), depth + 1, meta)?;
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
pub fn ctype_stratified_translate(i: usize, ctx: Context, term: &CTerm, ty: Type, depth: usize, meta: StratifiedInfo) -> Result<SoundTree, String> {
    match term {
        CTerm::Inf(it) => {
            let (ity, tree) = itype_stratified_translate(i, ctx, it, depth, meta)?;
            if quote0(ity.clone()) != quote0(ty.clone()) {
                Err(format!("Expected {}, inferred {}, for term {}, level {i}", quote(i, ty), quote(i, ity), term))
            }
            else {
                Ok(tree)
            }
        },
        CTerm::Lam(body) => match ty {
            Value::Pi(src, trg) => {
                let node_mel = meta.cstratify(term, depth);
                let meta = StratifiedInfo::cmelody(term, depth);
                let mut new_ctx = ctx.clone();
                new_ctx.push((Name::Local(i), *src));
                let subtree = ctype_stratified_translate(i + 1, new_ctx, 
                    &body.clone().subst(0, ITerm::Free(Name::Local(i))), trg(vfree(Name::Local(i))), depth + 1, meta.clone()
                )?;
                Ok(SoundTree::simul(&[node_mel, subtree]))
            },
            _ => Err("Function must have pi type".to_owned())
        }
    }
}

/// Translates ITerms into SoundTrees according to their type structure.
/// Subterms have their types checked or inferred and have their own melodies combined with their types' melodies.
/// The code here is basically an extended version of ITerm's [infer_type](ITerm::infer_type).
/// Should be initially called from [type_translate].
fn itype_translate_full(ii: usize, ctx: Context, term: &ITerm, depth: usize, mel: &MelodySelector) -> Result<(Type, SoundTree), String> {
    // the process for defining the sounds is fairly subjective - there's no one correct answer,
    // we just want something that sounds good and represents the structure.
    // let node_melody = SoundTree::sound(mel.imelody(term, depth), term);
    let node_melody = mel.imelody(term, depth);
    match term {
        ITerm::Ann(ct, cty) => {
            // should we modify something in this to present an annotation better? term-translate the type, maybe?
            let tytree = ctype_translate_full(ii, ctx.clone(), cty, Value::Star, depth + 1, mel)?;
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
            // like a rising burble perhaps...
            // but I don't think we have any Succs in the paradox so it's fine for now
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
            //TODO finite set translation needs some work like Succ & in fact in parallel since it's so similar
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
pub fn ctype_translate_full(i: usize, ctx: Context, term: &CTerm, ty: Type, depth: usize, mel: &MelodySelector) -> Result<SoundTree, String> {
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
                let subtree = ctype_translate_full(i + 1, new_ctx, 
                    &body.clone().subst(0, ITerm::Free(Name::Local(i))), trg(vfree(Name::Local(i))), depth + 1, mel
                )?;
                Ok(SoundTree::simul(&[mel.cmelody(term, depth), subtree]))
            },
            _ => Err("Function must have pi type".to_owned())
        }
    }
}

/// Translates ITerms to SoundTrees on a pure subterm-to-subtree basis.
pub fn iterm_translate_full(term: &ITerm, depth: usize, mel: &MelodySelector) -> SoundTree {
    let base_mel = mel.imelody(term, depth);
    match term {
        ITerm::Ann(cterm, cterm1) => SoundTree::simul(&[
            base_mel, 
            SoundTree::seq(&[
                cterm_translate_full(cterm, depth + 1, mel),
                cterm_translate_full(cterm1, depth + 1, mel)
            ])
        ]),
        ITerm::Star => base_mel,
        ITerm::Pi(cterm, cterm1) => SoundTree::simul(&[
            base_mel, 
            SoundTree::seq(&[
                cterm_translate_full(cterm, depth + 1, mel),
                cterm_translate_full(cterm1, depth + 1, mel)
            ])
        ]),
        ITerm::Bound(_) => base_mel,
        ITerm::Free(_) => base_mel,
        ITerm::App(iterm, cterm) => SoundTree::simul(&[
            base_mel, 
            SoundTree::seq(&[
                iterm_translate_full(iterm, depth + 1, mel), 
                cterm_translate_full(cterm, depth + 1, mel)
            ])
        ]),
        ITerm::Nat => base_mel,
        ITerm::Zero => base_mel,
        ITerm::Succ(cterm) => SoundTree::simul(&[base_mel, SoundTree::seq(&[cterm_translate_full(cterm, depth + 1, mel)])]),
        ITerm::NatElim(motive, base, ind, k) => SoundTree::simul(&[base_mel, SoundTree::seq(&[
            cterm_translate_full(motive, depth + 1, mel),
            cterm_translate_full(base, depth + 1, mel),
            cterm_translate_full(ind, depth + 1, mel),
            cterm_translate_full(k, depth + 1, mel),
        ])]),
        ITerm::Fin(cterm) => SoundTree::simul(&[base_mel, SoundTree::seq(&[cterm_translate_full(cterm, depth + 1, mel)])]),
        ITerm::FinElim(motive, base, ind, n, f) => SoundTree::simul(&[base_mel, SoundTree::seq(&[
            cterm_translate_full(motive, depth + 1, mel),
            cterm_translate_full(base, depth + 1, mel),
            cterm_translate_full(ind, depth + 1, mel),
            cterm_translate_full(n, depth + 1, mel),
            cterm_translate_full(f, depth + 1, mel),
        ])]),
        ITerm::FZero(cterm) => SoundTree::simul(&[base_mel, SoundTree::seq(&[cterm_translate_full(cterm, depth + 1, mel)])]),
        ITerm::FSucc(cterm, cterm1) => SoundTree::simul(&[
            base_mel, SoundTree::seq(&[
                cterm_translate_full(cterm, depth + 1, mel), 
                cterm_translate_full(cterm1, depth + 1, mel)
            ])
        ]),
    }
}

/// Translates CTerms to SoundTrees on a pure subterm-to-subtree basis.
pub fn cterm_translate_full(term: &CTerm, depth: usize, mel: &MelodySelector) -> SoundTree {
    let melody = mel.cmelody(term, depth);
    match term {
        CTerm::Inf(iterm) => iterm_translate_full(iterm, depth, mel),
        CTerm::Lam(cterm) => SoundTree::simul(&[melody, SoundTree::seq(&[cterm_translate_full(cterm, depth + 1, mel)])]),
    }
}

pub fn test_tree(mel: &MelodySelector) -> SoundTree {
    let test_terms = [
        ITerm::Star,
        ITerm::Ann(ITerm::Star.into(), ITerm::Star.into()),
        ITerm::Pi(ITerm::Star.into(), ITerm::Star.into()),
        ITerm::Bound(0),
        ITerm::Free(Name::Local(0)),
        ITerm::App(Box::new(ITerm::Star), ITerm::Star.into()),
        ITerm::Zero,
        ITerm::Fin(ITerm::Zero.into()),
    ];
    let mut sounds = Vec::new();
    let depth = 2;
    for term in test_terms {
        let onemel = mel.imelody(&term, depth);
        sounds.push(onemel.clone());
        sounds.push(onemel.clone());
        sounds.push(onemel.clone());
        sounds.push(onemel)
    }
    let lamstar = CTerm::Lam(Box::new(ITerm::Star.into()));
    sounds.push(mel.cmelody(&lamstar, depth));
    sounds.push(mel.cmelody(&lamstar, depth));
    sounds.push(mel.cmelody(&lamstar, depth));
    sounds.push(mel.cmelody(&lamstar, depth));
    SoundTree::seq(&sounds)
}
