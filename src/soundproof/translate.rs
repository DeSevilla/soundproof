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
pub fn type_translate<T: Selector>(term: ITerm, meta: T) -> SoundTree {
    itype_translate(0, vec![], &term, meta).unwrap().1
}

/// Translates [ITerm]s into [SoundTree]s according to their term structure.
/// Subterms are converted to melodies and run along with their outer terms.
pub fn term_translate<T: Selector>(term: ITerm, meta: T) -> SoundTree {
    iterm_translate(&term, 0, meta)
}

pub fn buildup<T: Selector>(terms: impl IntoIterator<Item=ITerm>, meta: T) -> SoundTree {
    let subtrees = terms.into_iter().map(|t| type_translate(t, meta.clone())).collect();
    SoundTree::Seq(subtrees, TreeMetadata { name: "".to_owned() })
}

fn itype_translate<T: Selector>(ii: usize, ctx: Context, term: &ITerm, meta: T) -> Result<(Type, SoundTree), String> {
    // the process for defining the sounds is fairly subjective - there's no one correct answer,
    // we just want something that sounds good and represents the structure.
    // let node_melody = SoundTree::sound(mel.imelody(term, depth), term);
    // let node_melody = meta.istratify(term, depth);
    let node_melody = meta.isound(term);
    let meta = meta.imerge(term);
    // let meta = StratifiedInfo::imelody(term, depth);
    match term {
        ITerm::Ann(ct, cty) => {
            // should we modify something in this to present an annotation better? term-translate the type, maybe?
            let tytree = ctype_translate(ii, ctx.clone(), cty, Value::Star, meta.clone())?;
            let ty = cty.clone().eval(vec![]);
            let termtree = ctype_translate(ii, ctx, ct, ty.clone(), meta)?;  // should we have the type and term at diff depths?
            // let tree = SoundTree::simul(&[node_melody, SoundTree::seq(&[tytree, termtree])]);
            let tree = SoundTree::simul(&[node_melody, SoundTree::seq(&[termtree, tytree])]);
            Ok((ty, tree))
        }
        ITerm::Star => Ok((Value::Star, node_melody)),
        ITerm::Pi(src, trg) => {
            let srctree = ctype_translate(ii, ctx.clone(), src, Value::Star, meta.clone())?;
            let ty = src.clone().eval(vec![]);
            let mut new_ctx = ctx.clone();
            new_ctx.push((Name::Local(ii), ty.clone()));
            let trgtree = ctype_translate(
                ii + 1,
                new_ctx, 
                &trg.clone().subst(0, ITerm::Free(Name::Local(ii))), 
                Value::Star, 
                meta
            )?;
            let tree = SoundTree::simul(&[node_melody, SoundTree::seq(&[srctree, trgtree])]);
            Ok((Value::Star, tree))
        },
        ITerm::Free(name) => { 
            // want to assign a melody to the name... randomization? fixed sequence? how do we associate?
            // maybe we put an environment of some sort in the Selector?
            let val = ctx.iter().find(|x| x.0 == *name).ok_or("Could not find variable".to_owned())?;
            let ty = ctype_translate(ii, ctx.clone(), &quote0(val.1.clone()), Value::Star, meta)?;
            let tree = ty;
            Ok((val.1.clone(), tree))
        },
        ITerm::App(f, x) => {
            let (fty, ftree) = itype_translate(ii, ctx.clone(), f, meta.clone())?;
            match fty {
                Value::Pi(src, trg) => {
                    let xtree = ctype_translate(ii, ctx, x, *src, meta)?;
                    Ok((trg(x.clone().eval(vec![])), SoundTree::simul(&[node_melody, SoundTree::seq(&[ftree, xtree])])))
                },
                _ => Err("Invalid function call".to_owned())
            }
        },
        ITerm::Nat => Ok((Value::Star, node_melody)),
        ITerm::Zero => Ok((Value::Nat, node_melody)),
        ITerm::Succ(k) => {
            let subtree = ctype_translate(ii, ctx, k, Value::Nat, meta)?;
            // TODO we need succ to be a different sort of thing... not a full melody
            // like a rising burble perhaps...
            // but I don't think we have any Succs in the paradox so it's fine for now
            Ok((Value::Nat, SoundTree::seq(&[node_melody, subtree])))
        },
        ITerm::NatElim(motive, base, ind, k) => {
            let mtree = ctype_translate(ii, ctx.clone(), motive, Value::Pi(Box::new(Value::Nat), Rc::new(|_| Value::Star)), meta.clone())?;
            let m_val = motive.clone().eval(vec![]);
            let m_val1 = m_val.clone();
            let bctree = ctype_translate(ii, ctx.clone(), base, vapp(m_val.clone(), Value::Zero), meta.clone())?;
            let indtree = ctype_translate(ii, ctx.clone(), ind,
                Value::Pi(Box::new(Value::Nat),
                    Rc::new(move |l| {let m_val2 = m_val.clone(); Value::Pi(Box::new(vapp(m_val.clone(), l.clone())), 
                        Rc::new(move |_| vapp(m_val2.clone(), Value::Succ(Box::new(l.clone()))))) }
                    )
                ), meta.clone()
            )?;
            let ktree = ctype_translate(ii, ctx, k, Value::Nat, meta)?;
            let k_val = k.clone().eval(vec![]);
            Ok((vapp(m_val1, k_val), SoundTree::simul(&[node_melody, SoundTree::seq(&[mtree, bctree, indtree, ktree])])))
        },
        ITerm::Fin(n) => {
            let subtree = ctype_translate(ii, ctx, n, Value::Nat, meta)?;
            //TODO finite set translation needs some work like Succ & in fact in parallel since it's so similar
            Ok((Value::Star, SoundTree::simul(&[node_melody, subtree])))
        },
        ITerm::FZero(n) => {
            let subtree = ctype_translate(ii, ctx, n, Value::Nat, meta)?;
            let n_val = n.clone().eval(vec![]);
            Ok((Value::Fin(Box::new(Value::Succ(Box::new(n_val)))), SoundTree::simul(&[node_melody, subtree])))
        },
        ITerm::FSucc(n, fp) => {
            let ntree = ctype_translate(ii, ctx.clone(), n, Value::Nat, meta.clone())?;
            let n_val = n.clone().eval(vec![]);
            match &n_val {
                Value::Succ(m) => {
                    let fptree = ctype_translate(ii, ctx, fp, Value::Fin(m.clone()), meta)?;
                    let tree = SoundTree::simul(&[node_melody, SoundTree::seq(&[ntree, fptree])]);
                    Ok((Value::Fin(Box::new(n_val)), tree))
                }
                _ => Err("oh no bad finitism".to_owned())
            }
        },
        ITerm::FinElim(motive, base, ind, n, f) => {
            let mtree = ctype_translate(ii, ctx.clone(), motive, Value::Pi(Box::new(Value::Nat), Rc::new(|k| 
                Value::Pi(Box::new(Value::Fin(Box::new(k))), Rc::new(|_| Value::Star))
            )), meta.clone())?;
            let ntree = ctype_translate(ii, ctx.clone(), n, Value::Nat, meta.clone())?;
            let motive_val = motive.clone().eval(vec![]); //we'll need NameEnv instead of just ctx if we want more parsing etc.
            let n_val = n.clone().eval(vec![]);
            let motive_val2 = motive_val.clone(); //jeez
            let bctree = ctype_translate(ii, ctx.clone(), base, Value::Pi(Box::new(Value::Nat), Rc::new(
                move |k| vapp(vapp(motive_val.clone(), Value::Succ(Box::new(k.clone()))), Value::FZero(Box::new(k)))
            )), meta.clone())?;
            let motive_val = motive_val2.clone();
            let indtree = ctype_translate(ii, ctx.clone(), ind, Value::Pi(Box::new(Value::Nat), Rc::new(
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
            )), meta.clone())?;
            let ftree = ctype_translate(ii, ctx, f, Value::Fin(Box::new(n_val.clone())), meta)?;
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
pub fn ctype_translate<T: Selector>(i: usize, ctx: Context, term: &CTerm, ty: Type, meta: T) -> Result<SoundTree, String> {
    match term {
        CTerm::Inf(it) => {
            let (ity, tree) = itype_translate(i, ctx, it, meta)?;
            if quote0(ity.clone()) != quote0(ty.clone()) {
                Err(format!("Expected {}, inferred {}, for term {}, level {i}", quote(i, ty), quote(i, ity), term))
            }
            else {
                Ok(tree)
            }
        },
        CTerm::Lam(body) => match ty {
            Value::Pi(src, trg) => {
                let node_mel = meta.csound(term);
                let meta = meta.cmerge(term);
                let mut new_ctx = ctx.clone();
                new_ctx.push((Name::Local(i), *src));
                let subtree = ctype_translate(i + 1, new_ctx, 
                    &body.clone().subst(0, ITerm::Free(Name::Local(i))), trg(vfree(Name::Local(i))), meta.clone()
                )?;
                Ok(SoundTree::simul(&[node_mel, subtree]))
            },
            _ => Err("Function must have pi type".to_owned())
        }
    }
}

/// Translates ITerms to SoundTrees on a pure subterm-to-subtree basis.
pub fn iterm_translate<T: Selector>(term: &ITerm, depth: usize, meta: T) -> SoundTree {
    let base_mel = meta.isound(term);
    let meta = meta.imerge(term);
    // let base_mel = SoundTree::sound(mel.imelody(term, depth), term.clone());
    match term {
        ITerm::Ann(cterm, cterm1) => SoundTree::simul(&[
            base_mel, 
            SoundTree::seq(&[
                cterm_translate(cterm, depth + 1, meta.clone()),
                cterm_translate(cterm1, depth + 1, meta)
            ])
        ]),
        ITerm::Star => base_mel,
        ITerm::Pi(cterm, cterm1) => SoundTree::simul(&[
            base_mel, 
            SoundTree::seq(&[
                cterm_translate(cterm, depth + 1, meta.clone()),
                cterm_translate(cterm1, depth + 1, meta)
            ])
        ]),
        ITerm::Bound(_) => base_mel,
        ITerm::Free(_) => base_mel,
        ITerm::App(iterm, cterm) => SoundTree::simul(&[
            base_mel, 
            SoundTree::seq(&[
                iterm_translate(iterm, depth + 1, meta.clone()), 
                cterm_translate(cterm, depth + 1, meta)
            ])
        ]),
        ITerm::Nat => base_mel,
        ITerm::Zero => base_mel,
        ITerm::Succ(cterm) => SoundTree::simul(&[base_mel, SoundTree::seq(&[cterm_translate(cterm, depth + 1, meta)])]),
        ITerm::NatElim(motive, base, ind, k) => SoundTree::simul(&[base_mel, SoundTree::seq(&[
            cterm_translate(motive, depth + 1, meta.clone()),
            cterm_translate(base, depth + 1, meta.clone()),
            cterm_translate(ind, depth + 1, meta.clone()),
            cterm_translate(k, depth + 1, meta),
        ])]),
        ITerm::Fin(cterm) => SoundTree::simul(&[base_mel, SoundTree::seq(&[cterm_translate(cterm, depth + 1, meta)])]),
        ITerm::FinElim(motive, base, ind, n, f) => SoundTree::simul(&[base_mel, SoundTree::seq(&[
            cterm_translate(motive, depth + 1, meta.clone()),
            cterm_translate(base, depth + 1, meta.clone()),
            cterm_translate(ind, depth + 1, meta.clone()),
            cterm_translate(n, depth + 1, meta.clone()),
            cterm_translate(f, depth + 1, meta),
        ])]),
        ITerm::FZero(cterm) => SoundTree::simul(&[base_mel, SoundTree::seq(&[cterm_translate(cterm, depth + 1, meta)])]),
        ITerm::FSucc(cterm, cterm1) => SoundTree::simul(&[
            base_mel, SoundTree::seq(&[
                cterm_translate(cterm, depth + 1, meta.clone()), 
                cterm_translate(cterm1, depth + 1, meta)
            ])
        ]),
    }
}

/// Translates CTerms to SoundTrees on a pure subterm-to-subtree basis.
pub fn cterm_translate<T: Selector>(term: &CTerm, depth: usize, meta: T) -> SoundTree {
    // let melody = SoundTree::sound(mel.cmelody(term, depth), term.clone());
    
    match term {
        CTerm::Inf(iterm) => iterm_translate(iterm, depth, meta),
        CTerm::Lam(cterm) => {
            let melody = meta.csound(term);
            let meta = meta.cmerge(term);
            SoundTree::simul(&[melody, SoundTree::seq(&[cterm_translate(cterm, depth + 1, meta)])])
        },
    }
}

pub fn test_tree(mel: impl Selector) -> SoundTree {
    let mel = mel.imerge(&ITerm::Star).imerge(&ITerm::Star);
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
    for term in test_terms {
        let onemel = mel.isound(&term);
        for _ in 0..4 {
            sounds.push(onemel.clone())
        }
        // sounds.push(onemel.clone());
        // sounds.push(onemel.clone());
        // sounds.push(onemel.clone());
        // sounds.push(onemel)
    }
    let lamstar = CTerm::Lam(Box::new(ITerm::Star.into()));
    for _ in 0..4 {
        sounds.push(mel.csound(&lamstar));
    }
    SoundTree::seq(&sounds)
}
