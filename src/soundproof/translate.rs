// #![allow(unused)]
use std::rc::Rc;

use fundsp::prelude32::sine;

use crate::{ast::*, music::notes::G, select::Selector, sound_generators::*, term::*, types::*};

/// Translates [ITerm]s into [SoundTree]s according to their type structure.
/// Subterms have their types checked or inferred and have their melodies combined with their types' melodies.
pub fn type_translate(term: &ITerm, meta: impl Selector) -> Result<SoundTree, String> {
    term.infer_translate(&Context::new(full_env()), meta, true)
        .map(|r| r.2)
}

/// Translates [ITerm]s into [SoundTree]s according to their term structure.
/// Subterms are converted to melodies and run along with their outer terms.
pub fn term_translate(term: &ITerm, meta: impl Selector) -> SoundTree {
    term.term_translate(meta)
}

pub fn buildup(terms: impl IntoIterator<Item = ITerm>, meta: impl Selector) -> SoundTree {
    // let middle = SoundTree::Sound(Rc::new(Melody::new_even(sine(), &[])), TreeMetadata { name: "".to_owned() });
    let sep = SoundTree::sound(Melody::new_even(sine(), &[G]), meta.imeta(&ITerm::Star));
    let subtrees: Vec<SoundTree> = terms
        .into_iter()
        .map(|t| type_translate(&t, meta.clone()))
        .flat_map(|t| [t.unwrap(), sep.clone()])
        .collect();
    SoundTree::seq(subtrees)
}

impl ITerm {
    pub fn infer_translate2(
        &self,
        ctx: &Context,
        meta: impl Selector,
    ) -> Result<(Type, SoundTree), String> {
        // the process for defining the sounds is fairly subjective - there's no one correct answer,
        // we just want something that sounds good and represents the structure.
        let node_melody = meta.isound(self);
        let meta = meta.imerge(self);
        match self {
            ITerm::Ann(ct, cty) => {
                // should we modify something in this to present an annotation better? term-translate the type, maybe?
                let tytree = cty.check_translate2(ctx, &Value::Star, meta.clone())?;
                let ty = cty.eval(ctx);
                let termtree = ct.check_translate2(ctx, &ty, meta)?; // should we have the type and term at diff depths?
                let tree = SoundTree::simul([node_melody, SoundTree::seq([termtree, tytree])]);
                // let tree = SoundTree::simul(&[node_melody, SoundTree::seq(&[tytree, termtree])]);    //alt 1: swap term/ty
                // let tree = SoundTree::simul(&[node_melody, tytree, termtree]);                       //alt 2: flatten
                Ok((ty, tree))
            }
            ITerm::Star => Ok((Value::Star, node_melody)),
            ITerm::Pi(src, trg) => {
                let srctree = src.check_translate2(ctx, &Value::Star, meta.clone())?;
                let ty = src.eval(ctx);
                let mut new_ctx = ctx.clone();
                let name = new_ctx.bind_type(ty);
                let trgtree = trg.clone().subst(0, &ITerm::Free(name)).check_translate2(
                    &new_ctx,
                    &Value::Star,
                    meta,
                )?;
                let tree = SoundTree::simul([node_melody, SoundTree::seq([srctree, trgtree])]);
                Ok((Value::Star, tree))
            }
            ITerm::Free(name) => {
                // want to assign a melody to the name... randomization? fixed sequence? how do we associate?
                // maybe we put an environment of some sort in the Selector?
                let (ty, _) = ctx
                    .find_free(name)
                    .ok_or(format!("free variable {name:?} not found"))?;
                let tytree = quote0(&ty).check_translate2(ctx, &Value::Star, meta)?;
                let tree = SoundTree::simul([node_melody, tytree]);
                Ok((ty, tree))
            }
            ITerm::App(f, x) => {
                let (fty, ftree) = f.infer_translate2(ctx, meta.clone())?;
                match fty {
                    Value::Pi(src, trg) => {
                        let xtree = x.check_translate2(ctx, &src, meta)?;
                        Ok((
                            trg(x.eval(ctx)),
                            SoundTree::simul([node_melody, SoundTree::seq([ftree, xtree])]),
                        ))
                    }
                    _ => Err("Function must have Pi type".to_owned()),
                }
            }
            ITerm::Nat => Ok((Value::Star, node_melody)),
            ITerm::Zero => Ok((Value::Nat, node_melody)),
            ITerm::Succ(k) => {
                let subtree = k.check_translate2(ctx, &Value::Nat, meta)?;
                // TODO we need succ to be a different sort of thing... not a full melody
                // like a rising burble perhaps...
                // but I don't think we have any Succs in the paradox so it's fine for now
                Ok((Value::Nat, SoundTree::seq([node_melody, subtree])))
            }
            ITerm::NatElim(motive, base, ind, k) => {
                let mtree = motive.check_translate2(
                    ctx,
                    &Value::Pi(Box::new(Value::Nat), Rc::new(|_| Value::Star)),
                    meta.clone(),
                )?;
                let m_val = motive.eval(ctx);
                let m_val1 = m_val.clone();
                let bctree =
                    base.check_translate2(ctx, &vapp(m_val.clone(), Value::Zero), meta.clone())?;
                let indtree = ind.check_translate2(
                    ctx,
                    &Value::Pi(
                        Box::new(Value::Nat),
                        Rc::new(move |l| {
                            let m_val2 = m_val1.clone();
                            Value::Pi(
                                Box::new(vapp(m_val1.clone(), l.clone())),
                                Rc::new(move |_| {
                                    vapp(m_val2.clone(), Value::Succ(Box::new(l.clone())))
                                }),
                            )
                        }),
                    ),
                    meta.clone(),
                )?;
                let ktree = k.check_translate2(ctx, &Value::Nat, meta)?;
                let k_val = k.eval(ctx);
                Ok((
                    vapp(m_val, k_val),
                    SoundTree::simul([
                        node_melody,
                        SoundTree::seq([mtree, bctree, indtree, ktree]),
                    ]),
                ))
            }
            ITerm::Fin(n) => {
                let subtree = n.check_translate2(ctx, &Value::Nat, meta)?;
                //TODO finite set translation needs some work like Succ & in fact in parallel since it's so similar
                Ok((Value::Star, SoundTree::simul([node_melody, subtree])))
            }
            ITerm::FZero(n) => {
                let subtree = n.check_translate2(ctx, &Value::Nat, meta)?;
                let n_val = n.eval(ctx);
                Ok((
                    Value::Fin(Box::new(Value::Succ(Box::new(n_val)))),
                    SoundTree::simul([node_melody, subtree]),
                ))
            }
            ITerm::FSucc(n, fp) => {
                let ntree = n.check_translate2(ctx, &Value::Nat, meta.clone())?;
                let n_val = n.eval(ctx);
                match &n_val {
                    Value::Succ(m) => {
                        let fptree = fp.check_translate2(ctx, &Value::Fin(m.clone()), meta)?;
                        let tree = SoundTree::simul([node_melody, SoundTree::seq([ntree, fptree])]);
                        Ok((Value::Fin(Box::new(n_val)), tree))
                    }
                    _ => Err("oh no bad finitism".to_owned()),
                }
            }
            ITerm::FinElim(motive, base, ind, n, f) => {
                let mtree = motive.check_translate2(
                    ctx,
                    &Value::Pi(
                        Box::new(Value::Nat),
                        Rc::new(|k| {
                            Value::Pi(Box::new(Value::Fin(Box::new(k))), Rc::new(|_| Value::Star))
                        }),
                    ),
                    meta.clone(),
                )?;
                let ntree = n.check_translate2(ctx, &Value::Nat, meta.clone())?;
                let motive_val = motive.eval(ctx); //we'll need NameEnv instead of just ctx if we want more parsing etc.
                let n_val = n.eval(ctx);
                let motive_val2 = motive_val.clone(); //jeez
                let bctree = base.check_translate2(
                    ctx,
                    &Value::Pi(
                        Box::new(Value::Nat),
                        Rc::new(move |k| {
                            vapp(
                                vapp(motive_val.clone(), Value::Succ(Box::new(k.clone()))),
                                Value::FZero(Box::new(k)),
                            )
                        }),
                    ),
                    meta.clone(),
                )?;
                let motive_val = motive_val2.clone(); //needed for the closures to take ownership w/o removing it from scope
                let indtree = ind.check_translate2(
                    ctx,
                    &Value::Pi(
                        Box::new(Value::Nat),
                        Rc::new(move |k| {
                            let motive_val = motive_val.clone();
                            Value::Pi(
                                Box::new(Value::Fin(Box::new(k.clone()))),
                                Rc::new(move |fk| {
                                    let k = k.clone();
                                    let motive_val = motive_val.clone();
                                    Value::Pi(
                                        Box::new(vapp(
                                            vapp(motive_val.clone(), k.clone()),
                                            fk.clone(),
                                        )),
                                        Rc::new(move |_| {
                                            vapp(
                                                vapp(
                                                    motive_val.clone(),
                                                    Value::Succ(Box::new(k.clone())),
                                                ),
                                                Value::FSucc(
                                                    Box::new(k.clone()),
                                                    Box::new(fk.clone()),
                                                ),
                                            )
                                        }),
                                    )
                                }),
                            )
                        }),
                    ),
                    meta.clone(),
                )?;
                let ftree = f.check_translate2(ctx, &Value::Fin(Box::new(n_val.clone())), meta)?;
                let f_val = f.eval(ctx);
                let tree = SoundTree::simul([
                    node_melody,
                    SoundTree::seq([mtree, bctree, indtree, ftree, ntree]),
                ]);
                Ok((vapp(vapp(motive_val2, n_val), f_val), tree))
            }
            ITerm::Eq(a, x, y) => {
                let atree = a.check_translate2(ctx, &Value::Star, meta.clone())?;
                let a_val = a.eval(ctx);
                let xtree = x.check_translate2(ctx, &a_val, meta.clone())?;
                let ytree = y.check_translate2(ctx, &a_val, meta)?;
                let tree = SoundTree::simul([node_melody, SoundTree::seq([atree, xtree, ytree])]);
                Ok((Value::Star, tree))
            }
            ITerm::Refl(a, x) => {
                let atree = a.check_translate2(ctx, &Value::Star, meta.clone())?;
                let a_val = a.eval(ctx);
                let xtree = x.check_translate2(ctx, &a_val, meta.clone())?;
                let x_val = x.eval(ctx);
                let tree = SoundTree::simul([node_melody, SoundTree::seq([atree, xtree])]);
                Ok((
                    Value::Eq(Box::new(a_val), Box::new(x_val.clone()), Box::new(x_val)),
                    tree,
                ))
            }
            ITerm::EqElim(a, motive, base, x, y, eq) => {
                let atree = a.check_translate2(ctx, &Value::Star, meta.clone())?;
                let a_val = a.eval(ctx);
                let a_val1 = a_val.clone();
                let mtree = motive.check_translate2(
                    ctx,
                    &vpi(a_val1.clone(), move |x| {
                        vpi(a_val1.clone(), {
                            let a_val1 = a_val1.clone();
                            move |y| {
                                vpi(
                                    Value::Eq(
                                        Box::new(a_val1.clone()),
                                        Box::new(x.clone()),
                                        Box::new(y.clone()),
                                    ),
                                    |_| Value::Star,
                                )
                            }
                        })
                    }),
                    meta.clone(),
                )?;
                let a_val1 = a_val.clone();
                let m_val = motive.eval(ctx);
                let m_val1 = m_val.clone();
                let btree = base.check_translate2(
                    ctx,
                    &vpi(&a_val, move |x| {
                        vapp(
                            vapp(vapp(m_val1.clone(), x.clone()), x.clone()),
                            Value::Refl(Box::new(a_val1.clone()), Box::new(x)),
                        )
                    }),
                    meta.clone(),
                )?;
                let xtree = x.check_translate2(ctx, &a_val, meta.clone())?;
                let x_val = x.eval(ctx);
                let ytree = y.check_translate2(ctx, &a_val, meta.clone())?;
                let y_val = y.eval(ctx);
                let eqtree = eq.check_translate2(
                    ctx,
                    &Value::Eq(
                        Box::new(a_val),
                        Box::new(x_val.clone()),
                        Box::new(y_val.clone()),
                    ),
                    meta,
                )?;
                let eq_val = eq.eval(ctx);
                let tree = SoundTree::simul([
                    node_melody,
                    SoundTree::seq([atree, mtree, btree, xtree, ytree, eqtree]),
                ]);
                Ok((vapp(vapp(vapp(m_val, x_val), y_val), eq_val), tree))
            }
            ITerm::Bound(n) => Err(format!(
                "Not sure how to assign a type to Bound({n}) in context, how did we get this?"
            )),
            // ctx.iter().map(|x| x.0.clone()).fold("".to_owned(), |acc, e| acc + &format!(" {:?}", e))))
        }
    }

    // TODO use this instead
    pub fn infer_translate(
        &self,
        ctx: &Context,
        meta: impl Selector,
        may_step: bool,
    ) -> Result<(Type, bool, SoundTree), String> {
        // the process for defining the sounds is fairly subjective - there's no one correct answer,
        // we just want something that sounds good and represents the structure.
        let node_melody = meta.isound(self);
        let meta = meta.imerge(self);
        match self {
            ITerm::Ann(ct, cty) => {
                // should we modify something in this to present an annotation better? term-translate the type, maybe?
                let (mut tytree, stepped) = cty.check_translate(ctx, &Value::Star, meta.clone(), may_step)?;
                let may_step = may_step && !stepped;
                let ty = cty.eval(ctx);
                let (termtree, tmstep) = ct.check_translate(ctx, &ty, meta, may_step)?; // should we have the type and term at diff depths?
                let stepped = stepped || tmstep;
                if may_step && let CTerm::Inf(_) = ct {
                    tytree.pervade_metadata(&|m| m.will_step = Some(Highlight::Two));
                };
                let tree = SoundTree::simul([node_melody, SoundTree::seq([termtree, tytree])]);
                // let tree = SoundTree::simul(&[node_melody, SoundTree::seq(&[tytree, termtree])]);    //alt 1: swap term/ty
                // let tree = SoundTree::simul(&[node_melody, tytree, termtree]);                       //alt 2: flatten
                Ok((ty, stepped, tree))
            },
            ITerm::Star => Ok((Value::Star, false, node_melody)),
            ITerm::Pi(src, trg) => {
                let (srctree, stepped) = src.check_translate(ctx, &Value::Star, meta.clone(), may_step)?;
                let ty = src.eval(ctx);
                let mut new_ctx = ctx.clone();
                let name = new_ctx.bind_type(ty);
                let (trgtree, _) = trg.clone().subst(0, &ITerm::Free(name)).check_translate(
                    &new_ctx,
                    &Value::Star,
                    meta,
                    false,
                )?;
                let tree = SoundTree::simul([node_melody, SoundTree::seq([srctree, trgtree])]);
                Ok((Value::Star, stepped, tree))
            }
            ITerm::Free(name) => {
                // want to assign a melody to the name... randomization? fixed sequence? how do we associate?
                // maybe we put an environment of some sort in the Selector?
                let (ty, _) = ctx
                    .find_free(name)
                    .ok_or(format!("free variable {name:?} not found"))?;
                let (tytree, _) = quote0(&ty).check_translate(ctx, &Value::Star, meta, false)?;
                let tree = SoundTree::simul([node_melody, tytree]);
                Ok((ty, false, tree))
            }
            ITerm::App(f, x) => {
                let (fty, stepped, ftree) = f.infer_translate(ctx, meta.clone(), may_step)?;
                let may_step = may_step && !stepped;
                match fty {
                    Value::Pi(src, trg) => {
                        let (xtree, xstep) = x.check_translate(ctx, &src, meta, may_step)?;
                        let mut stepped = stepped || xstep;
                        let may_step = may_step && !stepped;
                        let mut tree = SoundTree::simul([node_melody, SoundTree::seq([ftree, xtree])]);
                        if may_step {
                            tree.pervade_metadata(&|t| t.will_step = Some(Highlight::One));
                            stepped = true
                        }
                        Ok((
                            trg(x.eval(ctx)),
                            stepped,
                            tree
                        ))
                    }
                    _ => Err("Function must have Pi type".to_owned()),
                }
            }
            ITerm::Nat => Ok((Value::Star, false, node_melody)),
            ITerm::Zero => Ok((Value::Nat, false, node_melody)),
            ITerm::Succ(k) => {
                let (subtree, stepped) = k.check_translate(ctx, &Value::Nat, meta, may_step)?;
                // TODO we need succ to be a different sort of thing... not a full melody
                // like a rising burble perhaps...
                // but I don't think we have any Succs in the paradox so it's fine for now
                Ok((Value::Nat, stepped, SoundTree::seq([node_melody, subtree])))
            }
            ITerm::NatElim(motive, base, ind, k) => {
                let (mtree, _) = motive.check_translate(
                    ctx,
                    &Value::Pi(Box::new(Value::Nat), Rc::new(|_| Value::Star)),
                    meta.clone(),
                    false,  // TODO we won't encounter this in the paradox & don't have a step impl yet
                )?;
                let m_val = motive.eval(ctx);
                let m_val1 = m_val.clone();
                let (bctree, _) =
                    base.check_translate(ctx, &vapp(m_val.clone(), Value::Zero), meta.clone(), false)?;
                let (indtree, _) = ind.check_translate(
                    ctx,
                    &Value::Pi(
                        Box::new(Value::Nat),
                        Rc::new(move |l| {
                            let m_val2 = m_val1.clone();
                            Value::Pi(
                                Box::new(vapp(m_val1.clone(), l.clone())),
                                Rc::new(move |_| {
                                    vapp(m_val2.clone(), Value::Succ(Box::new(l.clone())))
                                }),
                            )
                        }),
                    ),
                    meta.clone(),
                    false,
                )?;
                let (ktree,_) = k.check_translate(ctx, &Value::Nat, meta, false)?;
                let k_val = k.eval(ctx);
                Ok((
                    vapp(m_val, k_val),
                    false, // TODO
                    SoundTree::simul([
                        node_melody,
                        SoundTree::seq([mtree, bctree, indtree, ktree]),
                    ]),
                ))
            }
            ITerm::Fin(n) => {
                let (subtree, stepped) = n.check_translate(ctx, &Value::Nat, meta, may_step)?;
                //TODO finite set translation needs some work like Succ & in fact in parallel since it's so similar
                Ok((Value::Star, stepped, SoundTree::simul([node_melody, subtree])))
            }
            ITerm::FZero(n) => {
                let (subtree, stepped) = n.check_translate(ctx, &Value::Nat, meta, may_step)?;
                let n_val = n.eval(ctx);
                Ok((
                    Value::Fin(Box::new(Value::Succ(Box::new(n_val)))),
                    stepped,
                    SoundTree::simul([node_melody, subtree]),
                ))
            }
            ITerm::FSucc(n, fp) => {
                let (ntree, stepped) = n.check_translate(ctx, &Value::Nat, meta.clone(), may_step)?;
                let may_step = may_step && !stepped;
                let n_val = n.eval(ctx);
                match &n_val {
                    Value::Succ(m) => {
                        let (fptree, fpstep) = fp.check_translate(ctx, &Value::Fin(m.clone()), meta, may_step)?;
                        let tree = SoundTree::simul([node_melody, SoundTree::seq([ntree, fptree])]);
                        Ok((Value::Fin(Box::new(n_val)), stepped || fpstep, tree))
                    }
                    _ => Err("oh no bad finitism".to_owned()),
                }
            }
            ITerm::FinElim(motive, base, ind, n, f) => {
                let (mtree, _) = motive.check_translate(
                    ctx,
                    &Value::Pi(
                        Box::new(Value::Nat),
                        Rc::new(|k| {
                            Value::Pi(Box::new(Value::Fin(Box::new(k))), Rc::new(|_| Value::Star))
                        }),
                    ),
                    meta.clone(),
                    false, // TODO not in paradox, no step impl
                )?;
                let (ntree, _) = n.check_translate(ctx, &Value::Nat, meta.clone(), false)?;
                let motive_val = motive.eval(ctx); //we'll need NameEnv instead of just ctx if we want more parsing etc.
                let n_val = n.eval(ctx);
                let motive_val2 = motive_val.clone(); //jeez
                let (bctree, _) = base.check_translate(
                    ctx,
                    &Value::Pi(
                        Box::new(Value::Nat),
                        Rc::new(move |k| {
                            vapp(
                                vapp(motive_val.clone(), Value::Succ(Box::new(k.clone()))),
                                Value::FZero(Box::new(k)),
                            )
                        }),
                    ),
                    meta.clone(),
                    false,
                )?;
                let motive_val = motive_val2.clone(); //needed for the closures to take ownership w/o removing it from scope
                let (indtree, _) = ind.check_translate(
                    ctx,
                    &Value::Pi(
                        Box::new(Value::Nat),
                        Rc::new(move |k| {
                            let motive_val = motive_val.clone();
                            Value::Pi(
                                Box::new(Value::Fin(Box::new(k.clone()))),
                                Rc::new(move |fk| {
                                    let k = k.clone();
                                    let motive_val = motive_val.clone();
                                    Value::Pi(
                                        Box::new(vapp(
                                            vapp(motive_val.clone(), k.clone()),
                                            fk.clone(),
                                        )),
                                        Rc::new(move |_| {
                                            vapp(
                                                vapp(
                                                    motive_val.clone(),
                                                    Value::Succ(Box::new(k.clone())),
                                                ),
                                                Value::FSucc(
                                                    Box::new(k.clone()),
                                                    Box::new(fk.clone()),
                                                ),
                                            )
                                        }),
                                    )
                                }),
                            )
                        }),
                    ),
                    meta.clone(),
                    false,
                )?;
                let (ftree, _) = f.check_translate(ctx, &Value::Fin(Box::new(n_val.clone())), meta, false)?;
                let f_val = f.eval(ctx);
                let tree = SoundTree::simul([
                    node_melody,
                    SoundTree::seq([mtree, bctree, indtree, ftree, ntree]),
                ]);
                Ok((vapp(vapp(motive_val2, n_val), f_val), false, tree))
            }
            ITerm::Eq(a, x, y) => {
                let (atree, _) = a.check_translate(ctx, &Value::Star, meta.clone(), false)?;
                let a_val = a.eval(ctx);
                let (xtree, _) = x.check_translate(ctx, &a_val, meta.clone(), false)?;
                let (ytree, _) = y.check_translate(ctx, &a_val, meta, false)?;
                let tree = SoundTree::simul([node_melody, SoundTree::seq([atree, xtree, ytree])]);
                Ok((Value::Star, false, tree))
            }
            ITerm::Refl(a, x) => {
                let (atree, _) = a.check_translate(ctx, &Value::Star, meta.clone(), false)?;
                let a_val = a.eval(ctx);
                let (xtree, _) = x.check_translate(ctx, &a_val, meta.clone(), false)?;
                let x_val = x.eval(ctx);
                let tree = SoundTree::simul([node_melody, SoundTree::seq([atree, xtree])]);
                Ok((
                    Value::Eq(Box::new(a_val), Box::new(x_val.clone()), Box::new(x_val)),
                    false,
                    tree,
                ))
            }
            ITerm::EqElim(a, motive, base, x, y, eq) => {
                let (atree, _) = a.check_translate(ctx, &Value::Star, meta.clone(), false)?;
                let a_val = a.eval(ctx);
                let a_val1 = a_val.clone();
                let (mtree, _) = motive.check_translate(
                    ctx,
                    &vpi(a_val1.clone(), move |x| {
                        vpi(a_val1.clone(), {
                            let a_val1 = a_val1.clone();
                            move |y| {
                                vpi(
                                    Value::Eq(
                                        Box::new(a_val1.clone()),
                                        Box::new(x.clone()),
                                        Box::new(y.clone()),
                                    ),
                                    |_| Value::Star,
                                )
                            }
                        })
                    }),
                    meta.clone(),
                    false,
                )?;
                let a_val1 = a_val.clone();
                let m_val = motive.eval(ctx);
                let m_val1 = m_val.clone();
                let (btree, _) = base.check_translate(
                    ctx,
                    &vpi(&a_val, move |x| {
                        vapp(
                            vapp(vapp(m_val1.clone(), x.clone()), x.clone()),
                            Value::Refl(Box::new(a_val1.clone()), Box::new(x)),
                        )
                    }),
                    meta.clone(),
                    false,
                )?;
                let (xtree, _) = x.check_translate(ctx, &a_val, meta.clone(), false)?;
                let x_val = x.eval(ctx);
                let (ytree, _) = y.check_translate(ctx, &a_val, meta.clone(), false)?;
                let y_val = y.eval(ctx);
                let (eqtree, _) = eq.check_translate(
                    ctx,
                    &Value::Eq(
                        Box::new(a_val),
                        Box::new(x_val.clone()),
                        Box::new(y_val.clone()),
                    ),
                    meta,
                    false,
                )?;
                let eq_val = eq.eval(ctx);
                let tree = SoundTree::simul([
                    node_melody,
                    SoundTree::seq([atree, mtree, btree, xtree, ytree, eqtree]),
                ]);
                Ok((vapp(vapp(vapp(m_val, x_val), y_val), eq_val), false, tree))
            }
            ITerm::Bound(n) => Err(format!(
                "Not sure how to assign a type to Bound({n}) in context, how did we get this?"
            )),
            // ctx.iter().map(|x| x.0.clone()).fold("".to_owned(), |acc, e| acc + &format!(" {:?}", e))))
        }
    }

    pub fn term_translate(&self, meta: impl Selector) -> SoundTree {
        iterm_translate(self, meta)
    }
}

impl CTerm {
    // TODO use this everywhere & probably just move the code into here
    pub fn check_translate(
        &self,
        ctx: &Context,
        ty: &Type,
        meta: impl Selector,
        may_step: bool,
    ) -> Result<(SoundTree, bool), String> {
        match self {
            CTerm::Inf(it) => {
                let i = ctx.bindings;
                let (ity, stepped, tree) = it.infer_translate(ctx, meta, may_step)?;
                if quote0(&ity) != quote0(ty) {
                    Err(format!(
                        "Expected {}, inferred {}, for term {}, level {i}",
                        quote(i, ty),
                        quote(i, &ity),
                        self
                    ))
                } else {
                    Ok((tree, stepped))
                }
            }
            CTerm::Lam(body) => match ty {
                Value::Pi(src, trg) => {
                    let node_mel = meta.csound(self);
                    let meta = meta.cmerge(self);
                    let mut new_ctx = ctx.clone();
                    let name = new_ctx.bind_type((**src).clone());
                    let (subtree, stepped) = body
                        .clone()
                        .subst(0, &ITerm::Free(name.clone()))
                        .check_translate(&new_ctx, &trg(vfree(name)), meta.clone(), false)?;
                    if stepped {
                        println!("SHOULDN'T BE ALLOWED TO STEP WITHIN LAMBDA")
                    }
                    Ok((SoundTree::simul([node_mel, subtree]), false))
                }
                _ => Err("Function must have pi type".to_owned()),
            },
        }
    }

    pub fn check_translate2(
        &self,
        ctx: &Context,
        ty: &Type,
        meta: impl Selector,
    ) -> Result<SoundTree, String> {
        match self {
            CTerm::Inf(it) => {
                let i = ctx.bindings;
                let (ity, tree) = it.infer_translate2(ctx, meta)?;
                if quote0(&ity) != quote0(ty) {
                    Err(format!(
                        "Expected {}, inferred {}, for term {}, level {i}",
                        quote(i, ty),
                        quote(i, &ity),
                        self
                    ))
                } else {
                    Ok(tree)
                }
            }
            CTerm::Lam(body) => match ty {
                Value::Pi(src, trg) => {
                    let node_mel = meta.csound(self);
                    let meta = meta.cmerge(self);
                    let mut new_ctx = ctx.clone();
                    let name = new_ctx.bind_type((**src).clone());
                    let subtree = body
                        .clone()
                        .subst(0, &ITerm::Free(name.clone()))
                        .check_translate2(&new_ctx, &trg(vfree(name)), meta.clone())?;
                    Ok(SoundTree::simul([node_mel, subtree]))
                }
                _ => Err("Function must have pi type".to_owned()),
            },
        }
    }

    pub fn term_translate(&self, meta: impl Selector) -> SoundTree {
        cterm_translate(self, meta)
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
                cterm_translate(cterm1, meta),
            ]),
        ]),
        ITerm::Star => base_mel,
        ITerm::Pi(cterm, cterm1) => SoundTree::simul([
            base_mel,
            SoundTree::seq([
                cterm_translate(cterm, meta.clone()),
                cterm_translate(cterm1, meta),
            ]),
        ]),
        ITerm::Bound(_) => base_mel,
        ITerm::Free(_) => base_mel,
        ITerm::App(iterm, cterm) => SoundTree::simul([
            base_mel,
            SoundTree::seq([
                iterm_translate(iterm, meta.clone()),
                cterm_translate(cterm, meta),
            ]),
        ]),
        ITerm::Nat => base_mel,
        ITerm::Zero => base_mel,
        ITerm::Succ(cterm) => {
            SoundTree::simul([base_mel, SoundTree::seq([cterm_translate(cterm, meta)])])
        }
        ITerm::NatElim(motive, base, ind, k) => SoundTree::simul([
            base_mel,
            SoundTree::seq([
                cterm_translate(motive, meta.clone()),
                cterm_translate(base, meta.clone()),
                cterm_translate(ind, meta.clone()),
                cterm_translate(k, meta),
            ]),
        ]),
        ITerm::Fin(cterm) => {
            SoundTree::simul([base_mel, SoundTree::seq([cterm_translate(cterm, meta)])])
        }
        ITerm::FinElim(motive, base, ind, n, f) => SoundTree::simul([
            base_mel,
            SoundTree::seq([
                cterm_translate(motive, meta.clone()),
                cterm_translate(base, meta.clone()),
                cterm_translate(ind, meta.clone()),
                cterm_translate(n, meta.clone()),
                cterm_translate(f, meta),
            ]),
        ]),
        ITerm::FZero(cterm) => {
            SoundTree::simul([base_mel, SoundTree::seq([cterm_translate(cterm, meta)])])
        }
        ITerm::FSucc(cterm, cterm1) => SoundTree::simul([
            base_mel,
            SoundTree::seq([
                cterm_translate(cterm, meta.clone()),
                cterm_translate(cterm1, meta),
            ]),
        ]),
        _ => todo!(),
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
        }
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
