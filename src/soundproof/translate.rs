// #![allow(unused)]
use std::rc::Rc;

use fundsp::hacker32::sine;

use crate::{
    ast::*, music::notes::G, select::Selector, term::*, types::*
};

/// Translates [ITerm]s into [SoundTree]s according to their type structure.
/// Subterms have their types checked or inferred and have their melodies combined with their types' melodies.
pub fn type_translate(term: &ITerm, meta: impl Selector) -> Result<SoundTree, String> {
    itype_translate(Context::new(full_env()), term, meta).map(|r| r.1)
}

/// Translates [ITerm]s into [SoundTree]s according to their term structure.
/// Subterms are converted to melodies and run along with their outer terms.
pub fn term_translate(term: &ITerm, meta: impl Selector) -> SoundTree {
    iterm_translate(term, meta)
}

pub fn buildup(terms: impl IntoIterator<Item=ITerm>, meta: impl Selector) -> SoundTree {
    // let middle = SoundTree::Sound(Rc::new(Melody::new_even(sine(), &[])), TreeMetadata { name: "".to_owned() });
    let sep = SoundTree::sound(Melody::new_even(sine(), &[G]), meta.imeta(&ITerm::Star));
    let subtrees: Vec<SoundTree> = terms.into_iter().map(|t| type_translate(&t, meta.clone())).flat_map(|t| [t.unwrap(), sep.clone()]).collect();
    SoundTree::seq(subtrees)
}

impl ITerm {
    // TODO use this instead
    pub fn infer_translate(&self, ctx: Context, meta: impl Selector) -> Result<(Type, SoundTree), String> {
        itype_translate(ctx, self, meta)
    }

    pub fn term_translate(&self, meta: impl Selector) -> SoundTree {
        iterm_translate(self, meta)
    }
}

impl CTerm {
    // TODO use this everywhere & probably just move the code into here
    pub fn check_translate(&self, ctx: Context, ty: Type, meta: impl Selector) -> Result<SoundTree, String> {
        ctype_translate(ctx, self, ty, meta)
    }

    pub fn term_translate(&self, meta: impl Selector) -> SoundTree {
        cterm_translate(self, meta)
    }
}

pub fn itype_translate(ctx: Context, term: &ITerm, meta: impl Selector) -> Result<(Type, SoundTree), String> {
    // the process for defining the sounds is fairly subjective - there's no one correct answer,
    // we just want something that sounds good and represents the structure.
    let node_melody = meta.isound(term);
    let meta = meta.imerge(term);
    match term {
        ITerm::Ann(ct, cty) => {
            // should we modify something in this to present an annotation better? term-translate the type, maybe?
            let tytree = ctype_translate(ctx.clone(), cty, Value::Star, meta.clone())?;
            let ty = cty.clone().eval(ctx.clone());
            let termtree = ctype_translate(ctx, ct, ty.clone(), meta)?;  // should we have the type and term at diff depths?
            let tree = SoundTree::simul([node_melody, SoundTree::seq([termtree, tytree])]);
            // let tree = SoundTree::simul(&[node_melody, SoundTree::seq(&[tytree, termtree])]);    //alt 1: swap term/ty
            // let tree = SoundTree::simul(&[node_melody, tytree, termtree]);                       //alt 2: flatten
            Ok((ty, tree))
        }
        ITerm::Star => Ok((Value::Star, node_melody)),
        ITerm::Pi(src, trg) => {
            let srctree = ctype_translate(ctx.clone(), src, Value::Star, meta.clone())?;
            let ty = src.clone().eval(ctx.clone());
            let mut new_ctx = ctx.clone();
            let name = new_ctx.bind_type(ty.clone());
            let trgtree = ctype_translate(
                new_ctx,
                &trg.clone().subst(0, &ITerm::Free(name)), 
                Value::Star,
                meta
            )?;
            let tree = SoundTree::simul([node_melody, SoundTree::seq([srctree, trgtree])]);
            Ok((Value::Star, tree))
        },
        ITerm::Free(name) => {
            // want to assign a melody to the name... randomization? fixed sequence? how do we associate?
            // maybe we put an environment of some sort in the Selector?
            let (ty, _) = ctx.find_free(name).ok_or(format!("free variable {name:?} not found"))?;
            let tytree = ctype_translate(ctx.clone(), &quote0(ty.clone()), Value::Star, meta)?;
            let tree = SoundTree::simul([node_melody, tytree]);
            Ok((ty.clone(), tree))
        },
        ITerm::App(f, x) => {
            let (fty, ftree) = itype_translate(ctx.clone(), f, meta.clone())?;
            match fty {
                Value::Pi(src, trg) => {
                    let xtree = ctype_translate(ctx.clone(), x, *src, meta)?;
                    Ok((trg(x.clone().eval(ctx.clone())), SoundTree::simul([node_melody, SoundTree::seq([ftree, xtree])])))
                },
                _ => Err("Invalid function call".to_owned())
            }
        },
        ITerm::Nat => Ok((Value::Star, node_melody)),
        ITerm::Zero => Ok((Value::Nat, node_melody)),
        ITerm::Succ(k) => {
            let subtree = ctype_translate(ctx, k, Value::Nat, meta)?;
            // TODO we need succ to be a different sort of thing... not a full melody
            // like a rising burble perhaps...
            // but I don't think we have any Succs in the paradox so it's fine for now
            Ok((Value::Nat, SoundTree::seq([node_melody, subtree])))
        },
        ITerm::NatElim(motive, base, ind, k) => {
            let mtree = ctype_translate(ctx.clone(), motive, Value::Pi(Box::new(Value::Nat), Rc::new(|_| Value::Star)), meta.clone())?;
            let m_val = motive.clone().eval(ctx.clone());
            let m_val1 = m_val.clone();
            let bctree = ctype_translate(ctx.clone(), base, vapp(m_val.clone(), Value::Zero), meta.clone())?;
            let indtree = ctype_translate(ctx.clone(), ind,
                Value::Pi(Box::new(Value::Nat),
                    Rc::new(move |l| {let m_val2 = m_val.clone(); Value::Pi(Box::new(vapp(m_val.clone(), l.clone())), 
                        Rc::new(move |_| vapp(m_val2.clone(), Value::Succ(Box::new(l.clone()))))) }
                    )
                ), meta.clone()
            )?;
            let ktree = ctype_translate(ctx.clone(), k, Value::Nat, meta)?;
            let k_val = k.clone().eval(ctx.clone());
            Ok((vapp(m_val1, k_val), SoundTree::simul([node_melody, SoundTree::seq([mtree, bctree, indtree, ktree])])))
        },
        ITerm::Fin(n) => {
            let subtree = ctype_translate(ctx, n, Value::Nat, meta)?;
            //TODO finite set translation needs some work like Succ & in fact in parallel since it's so similar
            Ok((Value::Star, SoundTree::simul([node_melody, subtree])))
        },
        ITerm::FZero(n) => {
            let subtree = ctype_translate(ctx.clone(), n, Value::Nat, meta)?;
            let n_val = n.clone().eval(ctx.clone());
            Ok((Value::Fin(Box::new(Value::Succ(Box::new(n_val)))), SoundTree::simul([node_melody, subtree])))
        },
        ITerm::FSucc(n, fp) => {
            let ntree = ctype_translate(ctx.clone(), n, Value::Nat, meta.clone())?;
            let n_val = n.clone().eval(ctx.clone());
            match &n_val {
                Value::Succ(m) => {
                    let fptree = ctype_translate(ctx, fp, Value::Fin(m.clone()), meta)?;
                    let tree = SoundTree::simul([node_melody, SoundTree::seq([ntree, fptree])]);
                    Ok((Value::Fin(Box::new(n_val)), tree))
                }
                _ => Err("oh no bad finitism".to_owned())
            }
        },
        ITerm::FinElim(motive, base, ind, n, f) => {
            let mtree = ctype_translate(ctx.clone(), motive, Value::Pi(Box::new(Value::Nat), Rc::new(|k| 
                Value::Pi(Box::new(Value::Fin(Box::new(k))), Rc::new(|_| Value::Star))
            )), meta.clone())?;
            let ntree = ctype_translate(ctx.clone(), n, Value::Nat, meta.clone())?;
            let motive_val = motive.clone().eval(ctx.clone()); //we'll need NameEnv instead of just ctx if we want more parsing etc.
            let n_val = n.clone().eval(ctx.clone());
            let motive_val2 = motive_val.clone(); //jeez
            let bctree = ctype_translate(ctx.clone(), base, Value::Pi(Box::new(Value::Nat), Rc::new(
                move |k| vapp(vapp(motive_val.clone(), Value::Succ(Box::new(k.clone()))), Value::FZero(Box::new(k)))
            )), meta.clone())?;
            let motive_val = motive_val2.clone(); //needed for the closures to take ownership w/o removing it from scope
            let indtree = ctype_translate(ctx.clone(), ind, Value::Pi(Box::new(Value::Nat), Rc::new(
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
            let ftree = ctype_translate(ctx.clone(), f, Value::Fin(Box::new(n_val.clone())), meta)?;
            let f_val = f.clone().eval(ctx.clone());
            let tree = SoundTree::simul([node_melody, SoundTree::seq([mtree, bctree, indtree, ftree, ntree])]);
            Ok((vapp(vapp(motive_val2, n_val), f_val), tree))
        },
        ITerm::Eq(a, x, y) => {
            let atree = a.check_translate(ctx.clone(), Value::Star, meta.clone())?;
            let a_val = a.eval(ctx.clone());
            let xtree = x.check_translate(ctx.clone(), a_val.clone(), meta.clone())?;
            let ytree = y.check_translate(ctx.clone(), a_val, meta)?;
            let tree = SoundTree::simul([node_melody, SoundTree::seq([atree, xtree, ytree])]);
            Ok((Value::Star, tree))
        },
        ITerm::Refl(a, x) => {
            let atree = a.check_translate(ctx.clone(), Value::Star, meta.clone())?;
            let a_val = a.eval(ctx.clone());
            let xtree = x.check_translate(ctx.clone(), a_val.clone(), meta.clone())?;
            let x_val = x.eval(ctx.clone());
            let tree = SoundTree::simul([node_melody, SoundTree::seq([atree, xtree])]);
            Ok((Value::Eq(Box::new(a_val), Box::new(x_val.clone()), Box::new(x_val)), tree))
        },
        ITerm::EqElim(a, motive, base, x, y, eq) => {
            let atree = a.check_translate(ctx.clone(), Value::Star, meta.clone())?;
            let a_val = a.eval(ctx.clone());
            let a_val1 = a_val.clone();
            let mtree = motive.check_translate(ctx.clone(), 
                vpi(a_val1.clone(), move |x|
                vpi(a_val1.clone(), { let a_val1 = a_val1.clone(); move |y|
                vpi(Value::Eq(Box::new(a_val1.clone()), Box::new(x.clone()), Box::new(y.clone())), |_|
                    Value::Star
                )})), meta.clone()
            )?;
            let a_val1 = a_val.clone();
            let m_val = motive.eval(ctx.clone());
            let m_val1 = m_val.clone();
            let btree = base.check_translate(ctx.clone(), 
                vpi(a_val1.clone(), move |x| {
                    vapp(vapp(vapp(m_val1.clone(), x.clone()), x.clone()), Value::Refl(Box::new(a_val1.clone()), Box::new(x)))
                }), meta.clone()
            )?;
            let xtree = x.check_translate(ctx.clone(), a_val.clone(), meta.clone())?;
            let x_val = x.eval(ctx.clone());
            let ytree = y.check_translate(ctx.clone(), a_val.clone(), meta.clone())?;
            let y_val = y.eval(ctx.clone());
            let eqtree = eq.check_translate(ctx.clone(), 
                Value::Eq(Box::new(a_val), Box::new(x_val.clone()), Box::new(y_val.clone())), 
                meta
            )?;
            let eq_val = eq.eval(ctx);
            let tree = SoundTree::simul([node_melody, SoundTree::seq([atree, mtree, btree, xtree, ytree, eqtree])]);
            Ok((vapp(vapp(vapp(m_val, x_val), y_val), eq_val), tree))
        }
        ITerm::Bound(n) => Err(format!("Not sure how to assign a type to Bound({n}) in context, how did we get this?")), 
            // ctx.iter().map(|x| x.0.clone()).fold("".to_owned(), |acc, e| acc + &format!(" {:?}", e))))
    }
}

/// Translates CTerms into SoundTrees according to their type structure.
/// Subterms have their types checked or inferred and have their own melodies combined with their types' melodies.
pub fn ctype_translate(ctx: Context, term: &CTerm, ty: Type, meta: impl Selector) -> Result<SoundTree, String> {
    match term {
        CTerm::Inf(it) => {
            let i = ctx.bindings;
            let (ity, tree) = itype_translate(ctx, it, meta)?;
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
                let name = new_ctx.bind_type(*src);
                let subtree = ctype_translate(new_ctx,
                    &body.clone().subst(0, &ITerm::Free(name.clone())), trg(vfree(name)), meta.clone()
                )?;
                Ok(SoundTree::simul([node_mel, subtree]))
            },
            _ => Err("Function must have pi type".to_owned())
        }
    }
}

/// Translates ITerms to SoundTrees on a pure subterm-to-subtree basis.
pub fn iterm_translate(term: &ITerm, meta: impl Selector) -> SoundTree {
    let base_mel = meta.isound(term);
    let meta = meta.imerge(term);
    // let base_mel = SoundTree::sound(mel.imelody(term, depth), term.clone());
    match term {
        ITerm::Ann(cterm, cterm1) => SoundTree::simul([
            base_mel, 
            SoundTree::seq([
                cterm_translate(cterm, meta.clone()),
                cterm_translate(cterm1, meta)
            ])
        ]),
        ITerm::Star => base_mel,
        ITerm::Pi(cterm, cterm1) => SoundTree::simul([
            base_mel, 
            SoundTree::seq([
                cterm_translate(cterm, meta.clone()),
                cterm_translate(cterm1, meta)
            ])
        ]),
        ITerm::Bound(_) => base_mel,
        ITerm::Free(_) => base_mel,
        ITerm::App(iterm, cterm) => SoundTree::simul([
            base_mel, 
            SoundTree::seq([
                iterm_translate(iterm, meta.clone()), 
                cterm_translate(cterm, meta)
            ])
        ]),
        ITerm::Nat => base_mel,
        ITerm::Zero => base_mel,
        ITerm::Succ(cterm) => SoundTree::simul([base_mel, SoundTree::seq([cterm_translate(cterm, meta)])]),
        ITerm::NatElim(motive, base, ind, k) => SoundTree::simul([base_mel, SoundTree::seq([
            cterm_translate(motive, meta.clone()),
            cterm_translate(base, meta.clone()),
            cterm_translate(ind, meta.clone()),
            cterm_translate(k, meta),
        ])]),
        ITerm::Fin(cterm) => SoundTree::simul([base_mel, SoundTree::seq([cterm_translate(cterm, meta)])]),
        ITerm::FinElim(motive, base, ind, n, f) => SoundTree::simul([base_mel, SoundTree::seq([
            cterm_translate(motive, meta.clone()),
            cterm_translate(base, meta.clone()),
            cterm_translate(ind, meta.clone()),
            cterm_translate(n, meta.clone()),
            cterm_translate(f, meta),
        ])]),
        ITerm::FZero(cterm) => SoundTree::simul([base_mel, SoundTree::seq([cterm_translate(cterm, meta)])]),
        ITerm::FSucc(cterm, cterm1) => SoundTree::simul([
            base_mel, SoundTree::seq([
                cterm_translate(cterm, meta.clone()), 
                cterm_translate(cterm1, meta)
            ])
        ]),
        _ => todo!()
    }
}

/// Translates CTerms to SoundTrees on a pure subterm-to-subtree basis.
pub fn cterm_translate(term: &CTerm, meta: impl Selector) -> SoundTree {
    // let melody = SoundTree::sound(mel.cmelody(term, depth), term.clone());
    
    match term {
        CTerm::Inf(iterm) => iterm_translate(iterm, meta),
        CTerm::Lam(cterm) => {
            let melody = meta.csound(term);
            let meta = meta.cmerge(term);
            SoundTree::simul([melody, SoundTree::seq([cterm_translate(cterm, meta)])])
        },
    }
}

pub fn test_tree(meta: impl Selector) -> SoundTree {
    let meta = meta.imerge(&ITerm::Star);
    let test_terms = [
        ITerm::Star,
        ITerm::Ann(ITerm::Star.into(), ITerm::Star.into()),
        ITerm::Pi(ITerm::Star.into(), ITerm::Star.into()),
        // ITerm::Bound(0),
        ITerm::Free(Name::Local(0)),
        ITerm::App(Box::new(ITerm::Star), ITerm::Star.into()),
        ITerm::Zero,
        ITerm::Fin(ITerm::Zero.into()),
    ];
    let parent_indices = [1, 2, 4, 6];
    let mut sounds = Vec::new();
    for term in &test_terms {
        // let onemel = meta.isound(&term);
        for parent_idx in parent_indices {
            let localmeta = meta.imerge(&test_terms[parent_idx]);
            sounds.push(localmeta.isound(term))
        }
        // sounds.push(onemel.clone());
        // sounds.push(onemel.clone());
        // sounds.push(onemel.clone());
        // sounds.push(onemel)
    }
    let lamstar = CTerm::Lam(Box::new(ITerm::Star.into()));
    for parent_idx in parent_indices {
        let localmeta = meta.imerge(&test_terms[parent_idx]);
        sounds.push(localmeta.csound(&lamstar));
    }
    SoundTree::seq(sounds)
}
