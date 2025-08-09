use std::rc::Rc;

use crate::lambdapi::{ast::*, term::*};

impl ITerm {
    pub fn infer_type(&self, ctx: Context) -> Result<Type, String> {
        match self {
            ITerm::Ann(ct, cty) => {
                // the type we're assigning must be a type
                cty.check_type(ctx.clone(), Value::Star)?;
                let ty = cty.eval(ctx.clone());
                // and the annotation must be valid
                ct.check_type(ctx, ty.clone())?;
                Ok(ty)
            }
            ITerm::Star => Ok(Value::Star),
            ITerm::Pi(src, trg) => {
                // println!("pi: {src:?} |- {trg:?}");
                // the type of the parameter must be a type
                src.check_type(ctx.clone(), Value::Star)?;
                let src_ty = src.eval(ctx.clone());
                let mut new_ctx = ctx.clone();
                let name = new_ctx.bind_type(src_ty.clone());
                let modified = trg.clone().subst(0, ITerm::Free(name));
                // println!("pi2: {src:?} |- {modified:?}");
                // let trg_ty = trg.eval(new_ctx.clone());
                // the type of the body, with its local variable assigned the parameter type, must be a type
                modified.check_type(new_ctx, Value::Star)?;
                // println!("modified type done");
                Ok(Value::Star)
            },
            ITerm::Free(name) => {
                let (ty, _) = ctx.find_free(name).ok_or(format!("free variable {name:?} not found"))?;
                // println!("got {:?} for {name:?}", quote(ctx.bindings, ty.clone()));
                Ok(ty)
            },
            ITerm::App(f, x) => {
                // the function being applied must have an inferrable type (generally an annotation on the lambda)
                let fty = f.infer_type(ctx.clone())?;
                match fty {
                    // and the inferrable type must be a function type
                    Value::Pi(src, trg) => {
                        // and the argument must have the appropriate type for the function's parameter
                        x.check_type(ctx.clone(), *src)?;
                        Ok(trg(x.eval(ctx.clone())))
                    },
                    _ => Err(format!("Tried to call function {f} with non-function type {:?}", quote(ctx.bindings, fty)))
                }
            },
            ITerm::Nat => Ok(Value::Star),
            ITerm::Zero => Ok(Value::Nat),
            ITerm::Succ(k) => {
                k.check_type(ctx, Value::Nat)?;
                Ok(Value::Nat)
            },
            ITerm::NatElim(motive, base, ind, k) => {
                // motive must be a type parameterized by a natural number
                motive.check_type(ctx.clone(), Value::Pi(Box::new(Value::Nat), Rc::new(|_| Value::Star)))?;
                let m_val = motive.eval(ctx.clone());
                let m_val1 = m_val.clone();
                // base case must be a proof that the motive is true of 0
                base.check_type(ctx.clone(), vapp(m_val.clone(), Value::Zero))?;
                // inductive case must be a proof of motive(n) -> motive(n+1)
                ind.check_type(ctx.clone(),
                    Value::Pi(Box::new(Value::Nat),
                        Rc::new(move |l| {let m_val2 = m_val.clone(); Value::Pi(Box::new(vapp(m_val.clone(), l.clone())), 
                            Rc::new(move |_| vapp(m_val2.clone(), Value::Succ(Box::new(l.clone()))))) }
                        )
                    )
                )?;
                // We can only use this to prove the motive holds of a natural number
                k.check_type(ctx.clone(), Value::Nat)?;
                let k_val = k.eval(ctx);
                // The motive holds of that natural number
                Ok(vapp(m_val1, k_val))
            },
            ITerm::Fin(n) => {
                n.check_type(ctx, Value::Nat)?;
                Ok(Value::Star)
            },
            ITerm::FZero(n) => {
                // FZero(n) with n: Nat is an element of Fin(n+1), so Fin(0) has no elements
                n.check_type(ctx.clone(), Value::Nat)?;
                let n_val = n.eval(ctx);
                Ok(Value::Fin(Box::new(Value::Succ(Box::new(n_val)))))
            },
            ITerm::FSucc(n, fp) => {
                // FSucc(m+1, fp) with fp: Fin(m) is an element of Fin(m+1), so Fin(m+1) adds one additional element to Fin(m)
                n.check_type(ctx.clone(), Value::Nat)?;
                let n_val = n.eval(ctx.clone());
                fp.check_type(ctx, Value::Fin(Box::new(n_val.clone())))?;
                Ok(Value::Fin(Box::new(Value::Succ(Box::new(n_val)))))
                // match &n_val {
                //     Value::Succ(m) => {
                        // fp.check_type(ctx, Value::Fin(Box::new(n_val.clone())))?;
                        // Ok(Value::Fin(Box::new(Value::Succ(Box::new(n_val)))))
                    // }
                    // Value::Neutral(n) => 
                    // _ => Err(format!("Invalid element of finite type: {:?}", quote(ctx.bindings, n_val)))
                // }
            },
            ITerm::FinElim(motive, base, ind, n, f) => {
                // println!("typing finelim");
                // motive must be a type parameterized by n: Nat and f: Fin(n) to
                motive.check_type(ctx.clone(), Value::Pi(Box::new(Value::Nat), Rc::new(|k| 
                    Value::Pi(Box::new(Value::Fin(Box::new(k))), Rc::new(|_| Value::Star))
                )))?;
                // println!("validated motive");
                // n must be a natural number, of course
                n.check_type(ctx.clone(), Value::Nat)?;
                // println!("validated n");
                let motive_val = motive.eval(ctx.clone());
                let n_val = n.eval(ctx.clone());
                let motive_val2 = motive_val.clone(); //annoying we have to do all this for ownership but oh well
                // base case must be a proof of the motive on FZero: Fin(n) for all n
                base.check_type(ctx.clone(), Value::Pi(Box::new(Value::Nat), Rc::new(
                    move |k| vapp(vapp(motive_val.clone(), Value::Succ(Box::new(k.clone()))), Value::FZero(Box::new(k)))
                )))?;
                // println!("validated base case");
                let motive_val = motive_val2.clone();
                // induction step must be a proof that motive(n: Nat, f: Fin(n)) -> motive(n+1, FSucc(n+1, f))
                ind.check_type(ctx.clone(), Value::Pi(Box::new(Value::Nat), Rc::new(
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
                )))?;
                // println!("validated inductive case");
                // we can only use this to prove the motive holds on an element of Fin(n)
                f.check_type(ctx.clone(), Value::Fin(Box::new(n_val.clone())))?;
                // println!("validated fin");
                let f_val = f.eval(ctx);
                // the motive holds of that object
                Ok(vapp(vapp(motive_val2, n_val), f_val))
            },
            ITerm::Eq(a, x, y) => {
                a.check_type(ctx.clone(), Value::Star)?;
                let a_val = a.eval(ctx.clone());
                x.check_type(ctx.clone(), a_val.clone())?;
                y.check_type(ctx.clone(), a_val)?;
                Ok(Value::Star)
            },
            ITerm::Refl(a, x) => {
                a.check_type(ctx.clone(), Value::Star)?;
                let a_val = a.eval(ctx.clone());
                x.check_type(ctx.clone(), a_val.clone())?;
                let x_val = x.eval(ctx.clone());
                Ok(Value::Eq(Box::new(a_val), Box::new(x_val.clone()), Box::new(x_val)))
            },
            ITerm::EqElim(a, motive, base, x, y, eq) => {
                a.check_type(ctx.clone(), Value::Star)?;
                let a_val = a.eval(ctx.clone());
                let a_val1 = a_val.clone();
                motive.check_type(ctx.clone(), 
                    vpi(a_val1.clone(), move |x|
                    vpi(a_val1.clone(), { let a_val1 = a_val1.clone(); move |y|
                    vpi(Value::Eq(Box::new(a_val1.clone()), Box::new(x.clone()), Box::new(y.clone())), |_|
                        Value::Star
                    )}))
                )?;
                let a_val1 = a_val.clone();
                let m_val = motive.eval(ctx.clone());
                let m_val1 = m_val.clone();
                base.check_type(ctx.clone(), 
                    vpi(a_val1.clone(), move |x| {
                        vapp(vapp(vapp(m_val1.clone(), x.clone()), x.clone()), Value::Refl(Box::new(a_val1.clone()), Box::new(x)))
                    })
                )?;
                x.check_type(ctx.clone(), a_val.clone())?;
                let x_val = x.eval(ctx.clone());
                y.check_type(ctx.clone(), a_val.clone())?;
                let y_val = y.eval(ctx.clone());
                eq.check_type(ctx.clone(), Value::Eq(Box::new(a_val), Box::new(x_val.clone()), Box::new(y_val.clone())))?;
                let eq_val = eq.eval(ctx);
                Ok(vapp(vapp(vapp(m_val, x_val), y_val), eq_val))
            }
            ITerm::Bound(n) => Err(format!("Not sure how to assign a type to Bound({n}), how did we get this?")), 
                // ctx.iter().map(|x| x.0.clone()).fold("".to_owned(), |acc, e| acc + &format!(" {:?}", e))))
        }
    }
}

impl CTerm {
    pub fn check_type(&self, ctx: Context, ty: Type) -> Result<(), String> {
        match self {
            CTerm::Inf(it) => {
                let ii = ctx.bindings;
                let ity = it.infer_type(ctx)?;
                let infer = quote(ii, ity.clone());
                let expect = quote(ii, ty.clone());
                if infer != expect {
                    Err(format!("Mismatch: Received from callsite {:?}, inferred from term {:?}, for term {self}={self:?}, level {ii}", expect, infer))
                }
                else {
                    Ok(())
                }
            },
            CTerm::Lam(body) => match ty {
                Value::Pi(src, trg) => {
                    // let ii = ctx.len();
                    let mut new_ctx = ctx.clone();
                    let name = new_ctx.bind_type(*src);
                    body.clone().subst(0, ITerm::Free(name.clone())).check_type(new_ctx, trg(vfree(name)))
                },
                _ => Err("Function must have pi type".to_owned())
            }
        }
    }
}