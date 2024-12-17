#![allow(unused)]
use std::rc::Rc;
use fundsp::hacker32::*;

use crate::{
    ast::*,
    eval::*,
    term::*,
    instruments::*,
    notes::*,
    types::*,
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
        ITerm::Free(n) => Melody::new_even(violinish(), &[A, G, F, D]),
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


pub fn i_type_translate_full(i: usize, ctx: Context, term: &ITerm, depth: usize) -> Result<(Type, SoundTree2), String> {
    // println!("{i}typing term {term:?} in context {}", ctx.iter().fold("".to_owned(), |acc, e| acc + &format!(" {:?}", e.0)));
    let base_tree = SoundTree2::new_sound(imelody4(&term, depth));
    match term {
        ITerm::Ann(ct, cty) => {
            let tytree = c_type_translate_full(i, ctx.clone(), cty, Value::Star, depth + 1)?; // TODO figure this out better
            let ty = c_eval(cty.clone(), vec![]);
            let termtree = c_type_translate_full(i, ctx, ct, ty.clone(), depth + 1)?;
            let tree = SoundTree2::Simul(vec![base_tree, termtree, tytree]);
            Ok((ty, tree))
        }
        ITerm::Star => Ok((Value::Star, base_tree)),
        ITerm::Pi(src, trg) => {
            let srctree = c_type_translate_full(i, ctx.clone(), src, Value::Star, depth + 1)?;
            let ty = c_eval(src.clone(), vec![]);
            let mut new_ctx = ctx.clone();
            new_ctx.push((Name::Local(i), ty.clone()));
            // println!("Pushing {i} to ctx");
            let trgtree = c_type_translate_full(i + 1, new_ctx, &c_subst(0, ITerm::Free(Name::Local(i)), trg.clone()), Value::Star, depth + 1)?;
            let tree = SoundTree2::Simul(vec![base_tree, SoundTree2::Seq(vec![srctree, trgtree])]); // TODO fill this in
            Ok((Value::Star, tree))
        },
        ITerm::Free(name) => ctx.iter().find(|x| x.0 == *name)
            .map(|x| (x.1.clone(), SoundTree2::new_sound(cmelody3(&quote0(x.1.clone()), 0))))
            .ok_or("Could not find variable".to_owned()),
        ITerm::App(f, x) => {
            let (fty, ftree) = i_type_translate_full(i, ctx.clone(), f, depth + 1)?;
            match fty {
                Value::Pi(src, trg) => {
                    let xtree = c_type_translate_full(i, ctx, x, *src, depth + 1)?;
                    Ok((trg(c_eval(x.clone(), vec![])), SoundTree2::Simul(vec![base_tree, SoundTree2::Seq(vec![ftree, xtree])])))
                },
                _ => Err("Invalid function call".to_owned())
            }
        },
        ITerm::Nat => Ok((Value::Star, base_tree)),
        ITerm::Zero => Ok((Value::Nat, base_tree)),
        ITerm::Succ(k) => {
            let subtree = c_type_translate_full(i, ctx, k, Value::Nat, depth + 1)?;
            // TODO we need succ to be a different sort of thing... not a full melody
            Ok((Value::Nat, SoundTree2::Seq(vec![base_tree, subtree])))
        },
        ITerm::NatElim(motive, base, ind, k) => {
            c_type_translate_full(i, ctx.clone(), motive, Value::Pi(Box::new(Value::Nat), Rc::new(|_| Value::Star)), depth + 1)?;
            let m_val = c_eval(motive.clone(), vec![]);
            let m_val1 = m_val.clone();
            c_type_translate_full(i, ctx.clone(), base, vapp(m_val.clone(), Value::Zero), depth + 1)?;
            c_type_translate_full(i, ctx.clone(), ind,
                Value::Pi(Box::new(Value::Nat),
                    Rc::new(move |l| {let m_val2 = m_val.clone(); Value::Pi(Box::new(vapp(m_val.clone(), l.clone())), 
                        Rc::new(move |_| vapp(m_val2.clone(), Value::Succ(Box::new(l.clone()))))) }
                    )
                ), depth + 1
            )?;
            c_type_translate_full(i, ctx, k, Value::Nat, depth + 1)?;
            let k_val = c_eval(k.clone(), vec![]);
            Ok((vapp(m_val1, k_val), todo!()))
        },
        ITerm::Fin(n) => {
            let subtree = c_type_translate_full(i, ctx, n, Value::Nat, depth + 1)?;
            //TODO needs some work like Succ & in fact in parallel
            Ok((Value::Star, SoundTree2::Simul(vec![base_tree, subtree])))
        },
        ITerm::FZero(n) => {
            let subtree = c_type_translate_full(i, ctx, n, Value::Nat, depth + 1)?;
            let n_val = c_eval(n.clone(), vec![]);
            Ok((Value::Fin(Box::new(Value::Succ(Box::new(n_val)))), SoundTree2::Simul(vec![base_tree, subtree])))
        },
        ITerm::FSucc(n, fp) => {
            c_type_translate_full(i, ctx.clone(), n, Value::Nat, depth + 1)?;
            let n_val = c_eval(n.clone(), vec![]);
            match &n_val {
                Value::Succ(m) => {
                    c_type_translate_full(i, ctx, fp, Value::Fin(m.clone()), depth + 1)?;
                    Ok((Value::Fin(Box::new(n_val)), todo!()))
                }
                _ => Err("oh no bad finitism".to_owned())
            }
        },
        ITerm::FinElim(motive, base, ind, n, f) => {
            c_type_translate_full(i, ctx.clone(), motive, Value::Pi(Box::new(Value::Nat), Rc::new(|k| 
                Value::Pi(Box::new(Value::Fin(Box::new(k))), Rc::new(|_| Value::Star))
            )), depth + 1)?;
            c_type_translate_full(i, ctx.clone(), n, Value::Nat, depth + 1)?;
            let motive_val = c_eval(motive.clone(), vec![]); //we'll need NameEnv instead of just ctx if we want more parsing etc.
            let n_val = c_eval(n.clone(), vec![]);
            let motive_val2 = motive_val.clone(); //jeez
            c_type_translate_full(i, ctx.clone(), base, Value::Pi(Box::new(Value::Nat), Rc::new(
                move |k| vapp(vapp(motive_val.clone(), Value::Succ(Box::new(k.clone()))), Value::FZero(Box::new(k)))
            )), depth + 1)?;
            let motive_val = motive_val2.clone();
            c_type_translate_full(i, ctx.clone(), ind, Value::Pi(Box::new(Value::Nat), Rc::new(
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
            )), depth + 1)?;
            c_type_translate_full(i, ctx, f, Value::Fin(Box::new(n_val.clone())), depth + 1)?;
            let f_val = c_eval(f.clone(), vec![]);
            Ok((vapp(vapp(motive_val2, n_val), f_val), todo!()))
        },
        ITerm::Bound(n) => Err(format!("Not sure how to assign a type to Bound({n}) in context {}, how did we get this?", 
            ctx.iter().map(|x| x.0.clone()).fold("".to_owned(), |acc, e| acc + &format!(" {:?}", e))))
    }
}

pub fn c_type_translate_full(i: usize, ctx: Context, term: &CTerm, ty: Type, depth: usize) -> Result<SoundTree2, String> {
    // let tm = term.clone();
    match term {
        CTerm::Inf(it) => {
            let (ity, tree) = i_type_translate_full(i, ctx, it, depth)?;
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
                let subtree = c_type_translate_full(i + 1, new_ctx, &c_subst(0, ITerm::Free(Name::Local(i)), *body.clone()), trg(vfree(Name::Local(i))), depth + 1)?;
                Ok(SoundTree2::Simul(vec![SoundTree2::new_sound(cmelody3(&term, depth)), subtree]))
            },
            _ => Err("Function must have pi type".to_owned())
        }
    }
}

//so here's the plan. 
//for annotations, we play the annotation then the  
//we thought about playing the annotation of the type in the background of the thing we're concerned with
//for star, nat, zero, etc we construct premade little melodies. should be recognizable
//fin/succ we do a premade and can play in the background or before the number parameter
//for app, we play the type of the lambda, the type of the parameter, and then the combined type
//for pi, we play a little melody for the variable, then the param type, then the body type
//variable melodies are pulled from a basic list but maybe randomly generated if necessary
//for bound variables, we keep a context of the melodies for the variables

// pub fn new_melody(i: usize) -> Melody {
//     let i = i + 2;
//     Melody::new_even(sinesaw(), &[i, i * i, i.pow(3), i.pow(4)].map(|x| (x % 12 + 30) as i8))
// }

// pub fn itypetranslate(ii: usize, depth: usize, term: ITerm, ctx: SoundContext) -> SoundTree2 {
//     let base_mel = imelody4(&term, depth);
//     match term {
//         ITerm::Ann(cterm, ty) => SoundTree2::Simul(vec![
//             ctypetranslate(ii, depth, ty, ctx.clone()),
//             ctypetranslate(ii, depth + 1, cterm, ctx)
//             //this one is tricky i think
//         ]),
//         ITerm::Star => SoundTree2::new_sound(base_mel),
//         ITerm::Pi(ty, body) => {
//             let var_mel = new_melody(ii);
//             let mut new_ctx = ctx.clone();
//             new_ctx.push(Name::Local(ii), var_mel.clone(), c_eval(ty.clone(), vec![]));
//             SoundTree2::Seq(vec![
//                 SoundTree2::new_sound(var_mel),
//                 ctypetranslate(ii, depth + 1,  ty, ctx.clone()),
//                 ctypetranslate(ii + 1, depth + 1, c_subst(0, ITerm::Free(Name::Local(ii)), body), new_ctx),
//             ])
//         },
//         ITerm::Bound(_) => todo!(),
//         ITerm::Free(name) => SoundTree2::new_sound(ctx.0.iter().find(|x| x.0 == name).map(|x| x.1.clone()).unwrap()),
//         ITerm::App(iterm, cterm) => {
//             // todo!() //this is the important but difficult one to get right
//             //we may need to thread a real context through the entire thing just to handle it
//             //okay. problem: we want to play the value with it substituted in
//             //but we can't use normal substs because they take iterms, and there are some places
//             //that we can't just put a cterm (whereas iterm -> cterm so we can put iterms anywhere)
//             //solution 1: we normalize by evaluation on the lambda and then apply the resulting function to the parameter
//             //problem: we can't evaluate lol
//             //this is a non-normalizable term
//             //solution 2: can we put it into the context as a free variable?
//             let ty = i_type(ii, ctx.clone().into(), &iterm).unwrap();
//             let param_val = c_eval(cterm, vec![]);
//             let full_ty = match ty.clone() {
//                 Value::Pi(ty, fbody) => quote0(fbody(param_val.clone())),
//                 _ => panic!("Got non-Pi type in application!")
//             };
//             // THIS IS IMPOSSIBLE BC WE HAVE A PARADOX
//             // let func = i_eval(*iterm.clone(), vec![]);
//             // let fully_substituted = match func {
//             //     Value::Lam(f) => quote0(f(param_val)),
//             //     _ => CTerm::Inf(iterm.clone())
//             // };
//             let ctx_new = ctx.clone();
//             // ctx_new.push(name, mel, ty);
//             let fully_substituted = i_subst(0, ITerm::Free(Name::Local(ii)), *iterm);
//             SoundTree2::Simul(vec![
//                 // SoundTree2::Seq(vec![
//                 itypetranslate(ii, depth, fully_substituted, ctx_new),
//                 // ]),
//                 SoundTree2::Seq(vec![
//                     ctypetranslate(ii, depth + 1, full_ty, ctx.clone()),
//                     ctypetranslate(ii, depth + 1, quote0(ty), ctx),
//                 ]),
//             ])
//         },
//         ITerm::Nat => SoundTree2::new_sound(base_mel),
//         ITerm::Zero => SoundTree2::new_sound(base_mel),
//         ITerm::Succ(cterm) => todo!(),
//         ITerm::NatElim(cterm, cterm1, cterm2, cterm3) => todo!(),
//         ITerm::Fin(cterm) => SoundTree2::Simul(vec![SoundTree2::new_sound(base_mel), ctypetranslate(ii, depth + 1, cterm, ctx)]),
//         ITerm::FinElim(cterm, cterm1, cterm2, cterm3, cterm4) => todo!(),
//         ITerm::FZero(cterm) => todo!(),
//         ITerm::FSucc(cterm, cterm1) => todo!(),
//     }
// }

// pub fn ctypetranslate(ii: usize, depth: usize, term: CTerm, ctx: SoundContext) -> SoundTree2 {
//     match term {
//         CTerm::Inf(iterm) => itypetranslate(ii, depth, *iterm, ctx),
//         CTerm::Lam(cterm) => {
//             println!("Running a lambda in type translate...");
//             panic!();
//             let mut ctx = ctx.clone();
//             ctx.push(Name::Local(ii), new_melody(ii), Value::Star);
//             ctypetranslate(ii + 1, depth + 1, c_subst(0, ITerm::Free(Name::Local(ii)), *cterm), ctx)
//         }, // TODO will depend on itypetranslate
//     }
// }

pub fn imelody4(term: &ITerm, depth: usize) -> Melody {
    let mut result = match term {
        ITerm::Ann(_, _) => Melody::new_even(violinish(), &[A, B, C, B]),
        ITerm::Star => Melody::new_even(wobbly_sine(), &[A, C, A, B]),
        ITerm::Pi(_, _) => Melody::new_even(sine(), &[C, C, B, A]),
        ITerm::Bound(n) => Melody::new_even(sinesaw() >> split() >> fbd(*n as f32 / 10.0, -5.0), &[E, E, C, B]),
        ITerm::Free(n) => Melody::new_even(violinish(), &[A, G, F, D]),
        ITerm::App(_, _) => Melody::new_even(sawfir(), &[B, C, G, E]),
        ITerm::Zero => Melody::new_even(sinesaw(), &[B, C, E, G]),
        ITerm::Fin(_) => Melody::new_even(violinish(), &[E, B, G, C]),
        _ => panic!("{term} not implemented")
    };
    result.adjust_depth(depth);
    result
}

// pub fn itypetranslate(ii: usize, depth: usize, term: ITerm, ctx: MelContext) -> SoundTree {
//     let base_mel = imelody4(&term, depth);
//     match term {
//         ITerm::Ann(cterm, ty) => SoundTree::new(base_mel, vec![
//             ctypetranslate(ii, depth + 1, ty, ctx.clone()),
//             ctypetranslate(ii, depth + 1, cterm, ctx)
//             //this one is tricky i think
//         ]),
//         ITerm::Star => SoundTree::new(base_mel, vec![]),  //need to sort this out at some point
//         ITerm::Pi(ty, body) => {
//             let var_mel = new_melody(ii);
//             let mut new_ctx = ctx.clone();
//             new_ctx.push(Name::Local(ii), var_mel.clone(), c_eval(ty.clone(), vec![]));
//             SoundTree::new(base_mel, vec![
//                 SoundTree::new(var_mel, vec![]),
//                 ctypetranslate(ii, depth + 1,  ty, ctx.clone()),
//                 ctypetranslate(ii + 1, depth + 1, c_subst(0, ITerm::Free(Name::Local(ii)), body), new_ctx),
//             ])
//         },
//         ITerm::Bound(_) => todo!(),
//         ITerm::Free(name) => SoundTree::new(ctx.0.iter().find(|x| x.0 == name).map(|x| x.1.clone()).unwrap(), vec![]),
//         ITerm::App(iterm, cterm) => {
//             // todo!() //this is the important but difficult one to get right
//             //we may need to thread a real context through the entire thing just to handle it
//             let ty = i_type(ii, ctx.clone().into(), &iterm).unwrap();
//             let full_ty = match ty.clone() {
//                 Value::Pi(ty, fbody) => quote0(fbody(c_eval(cterm.clone(), vec![]))),
//                 _ => panic!("Got non-Pi type in application!")
//             };
//             SoundTree::new(ctypetranslate(ii, depth + 1, full_ty, ctx.clone()), vec![
//                 ctypetranslate(ii, depth + 1, quote0(ty), ctx),
//             ])
//         },
//         ITerm::Nat => SoundTree::new(base_mel, vec![]),
//         ITerm::Zero => SoundTree::new(base_mel, vec![]),
//         ITerm::Succ(cterm) => todo!(),
//         ITerm::NatElim(cterm, cterm1, cterm2, cterm3) => todo!(),
//         ITerm::Fin(cterm) => SoundTree::new(base_mel, vec![ctypetranslate(ii, depth + 1, cterm, ctx)]),
//         ITerm::FinElim(cterm, cterm1, cterm2, cterm3, cterm4) => todo!(),
//         ITerm::FZero(cterm) => todo!(),
//         ITerm::FSucc(cterm, cterm1) => todo!(),
//     }
// }

// pub fn ctypetranslate(ii: usize, depth: usize, term: CTerm, ctx: MelContext) -> SoundTree {
//     match term {
//         CTerm::Inf(iterm) => itypetranslate(ii, depth, *iterm, ctx),
//         CTerm::Lam(cterm) => {
//             println!("Running a lambda in type translate...");
//             panic!();
//             let mut ctx = ctx.clone();
//             ctx.push(Name::Local(ii), new_melody(ii), Value::Star);
//             ctypetranslate(ii + 1, depth + 1, c_subst(0, ITerm::Free(Name::Local(ii)), *cterm), ctx)
//         }, // TODO will depend on itypetranslate
//     }
// }

pub fn itranslate(term: ITerm, depth: usize) -> SoundTree {
    //to customize the sound we edit this to change the function that produces the melody
    //if we wanted more customization I think we'd need to pass in files
    let mel = imelody2(&term, depth);
    // let mel = imelody_oneinstr((sinesaw() ^ pass() | constant(1.0)) >> lowpass(), &term, depth);
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
