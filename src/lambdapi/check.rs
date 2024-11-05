use std::rc::Rc;

use crate::lambdapi::{eval::c_eval, ast::*, term::*};

pub fn i_type(i: usize, ctx: Context, term: ITerm) -> Result<Type, String> {
    // println!("{i}typing term {term:?} in context {}", ctx.iter().fold("".to_owned(), |acc, e| acc + &format!(" {:?}", e.0)));
    match term {
        ITerm::Ann(ct, cty) => {
            c_type(i, ctx.clone(), cty.clone(), Value::Star)?;
            let ty = c_eval(cty.clone(), vec![]);
            c_type(i, ctx, ct, ty.clone())?;
            Ok(ty)
        }
        ITerm::Star => Ok(Value::Star),
        ITerm::Pi(src, trg) => {
            c_type(i, ctx.clone(), src.clone(), Value::Star)?;
            let ty = c_eval(src, vec![]);
            let mut new_ctx = ctx.clone();
            new_ctx.push((Name::Local(i), ty.clone()));
            // println!("Pushing {i} to ctx");
            c_type(i + 1, new_ctx, c_subst(0, ITerm::Free(Name::Local(i)), trg), Value::Star)?;
            Ok(Value::Star)
        },
        ITerm::Free(name) => ctx.iter().find(|x| x.0 == name).map(|x| x.1.clone()).ok_or("Could not find variable".to_owned()),
        ITerm::App(f, x) => {
            let fty = i_type(i, ctx.clone(), *f)?;
            match fty {
                Value::Pi(src, trg) => {
                    c_type(i, ctx, x.clone(), *src)?;
                    Ok(trg(c_eval(x, vec![])))
                },
                _ => Err("Invalid function call".to_owned())
            }
        },
        ITerm::Nat => Ok(Value::Star),
        ITerm::Zero => Ok(Value::Nat),
        ITerm::Succ(k) => {
            c_type(i, ctx, k, Value::Nat)?;
            Ok(Value::Nat)
        },
        ITerm::NatElim(motive, base, ind, k) => {
            c_type(i, ctx.clone(), motive.clone(), Value::Pi(Box::new(Value::Nat), Rc::new(|_| Value::Star)))?;
            let m_val = c_eval(motive, vec![]);
            let m_val1 = m_val.clone();
            c_type(i, ctx.clone(), base, vapp(m_val.clone(), Value::Zero))?;
            c_type(i, ctx.clone(), ind,
                Value::Pi(Box::new(Value::Nat),
                    Rc::new(move |l| {let m_val2 = m_val.clone(); Value::Pi(Box::new(vapp(m_val.clone(), l.clone())), 
                        Rc::new(move |_| vapp(m_val2.clone(), Value::Succ(Box::new(l.clone()))))) }
                    )
                )
            )?;
            c_type(i, ctx, k.clone(), Value::Nat)?;
            let k_val = c_eval(k, vec![]);
            Ok(vapp(m_val1, k_val))
        },
        ITerm::Fin(n) => {
            c_type(i, ctx, n.clone(), Value::Nat)?;
            Ok(Value::Star)
        },
        ITerm::FZero(n) => {
            c_type(i, ctx, n.clone(), Value::Nat)?;
            let n_val = c_eval(n, vec![]);
            Ok(Value::Fin(Box::new(Value::Succ(Box::new(n_val)))))
        },
        ITerm::FSucc(n, fp) => {
            c_type(i, ctx.clone(), n.clone(), Value::Nat)?;
            let n_val = c_eval(n, vec![]);
            match &n_val {
                Value::Succ(m) => {
                    c_type(i, ctx, fp, Value::Fin(m.clone()))?;
                    Ok(Value::Fin(Box::new(n_val)))
                }
                _ => Err("oh no bad finitism".to_owned())
            }
        },
        ITerm::FinElim(motive, base, ind, n, f) => {
            c_type(i, ctx.clone(), motive.clone(), Value::Pi(Box::new(Value::Nat), Rc::new(|k| 
                Value::Pi(Box::new(Value::Fin(Box::new(k))), Rc::new(|_| Value::Star))
            )))?;
            c_type(i, ctx.clone(), n.clone(), Value::Nat)?;
            let motive_val = c_eval(motive, vec![]); //we'll need NameEnv instead of just ctx if we want more parsing etc.
            let n_val = c_eval(n, vec![]);
            let motive_val2 = motive_val.clone(); //jeez
            c_type(i, ctx.clone(), base, Value::Pi(Box::new(Value::Nat), Rc::new(
                move |k| vapp(vapp(motive_val.clone(), Value::Succ(Box::new(k.clone()))), Value::FZero(Box::new(k)))
            )))?;
            let motive_val = motive_val2.clone();
            c_type(i, ctx.clone(), ind, Value::Pi(Box::new(Value::Nat), Rc::new(
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
            )))?;
            c_type(i, ctx, f.clone(), Value::Fin(Box::new(n_val.clone())))?;
            let f_val = c_eval(f, vec![]);
            Ok(vapp(vapp(motive_val2, n_val), f_val))
        },
        ITerm::Bound(n) => Err(format!("Not sure how to assign a type to Bound({n}) in context {}, how did we get this?", 
            ctx.iter().map(|x| x.0.clone()).fold("".to_owned(), |acc, e| acc + &format!(" {:?}", e))))
    }
}

pub fn c_type(i: usize, ctx: Context, term: CTerm, ty: Type) -> Result<(), String> {
    let tm = term.clone();
    match term {
        CTerm::Inf(it) => {
            let ity = i_type(i, ctx, *it)?;
            if quote0(ity.clone()) != quote0(ty.clone()) {
                Err(format!("Expected {}, inferred {}, for term {}, level {i}", quote(i, ty), quote(i, ity), tm))
            }
            else {
                Ok(())
            }
        },
        CTerm::Lam(body) => match ty {
            Value::Pi(src, trg) => {
                let mut new_ctx = ctx.clone();
                new_ctx.push((Name::Local(i), *src));
                // println!("Pushed {i} to ctx");
                c_type(i + 1, new_ctx, c_subst(0, ITerm::Free(Name::Local(i)), *body), trg(vfree(Name::Local(i))))
            },
            _ => Err("Function must have pi type".to_owned())
        }
    }
}
