use std::rc::Rc;

use crate::lambdapi::{ast::*, term::*};

impl ITerm {
    pub fn infer_type(&self, ctx: Context) -> Result<Type, String> {
        match self {
            ITerm::Ann(ct, cty) => {
                // the type we're assigning must be a type
                cty.check_type(ctx.clone(), Value::Star)?;
                let ty = cty.clone().eval(vec![]);
                // and the annotation must be valid
                ct.check_type(ctx, ty.clone())?;
                Ok(ty)
            }
            ITerm::Star => Ok(Value::Star),
            ITerm::Pi(src, trg) => {
                let ii = ctx.len();
                // the type of the parameter must be a type
                src.check_type(ctx.clone(), Value::Star)?;
                let ty = src.clone().eval(vec![]);
                let mut new_ctx = ctx.clone();
                new_ctx.push((Name::Local(ii), ty.clone()));
                // the type of the body, with its local variable assigned the parameter type, must have the body type
                trg.clone().subst(0, ITerm::Free(Name::Local(ii))).check_type(new_ctx, Value::Star)?;
                Ok(Value::Star)
            },
            ITerm::Free(name) => ctx.iter().find(|x| x.0 == *name).map(|x| x.1.clone()).ok_or("Could not find variable".to_owned()),
            ITerm::App(f, x) => {
                // the function being applied must have an inferrable type (generally an annotation on the lambda)
                let fty = f.infer_type(ctx.clone())?;
                match fty {
                    // and the inferrable type must be a function type
                    Value::Pi(src, trg) => {
                        // and the argument must have the appropriate type for the function's parameter
                        x.check_type(ctx, *src)?;
                        Ok(trg(x.clone().eval(vec![])))
                    },
                    _ => Err("Tried to call function with non-function type".to_owned())
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
                let m_val = motive.clone().eval(vec![]);
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
                k.check_type(ctx, Value::Nat)?;
                let k_val = k.clone().eval(vec![]);
                // The motive holds of that natural number
                Ok(vapp(m_val1, k_val))
            },
            ITerm::Fin(n) => {
                n.check_type(ctx, Value::Nat)?;
                Ok(Value::Star)
            },
            ITerm::FZero(n) => {
                // FZero(n) with n: Nat is an element of Fin(n+1), so Fin(0) has no elements
                n.check_type(ctx, Value::Nat)?;
                let n_val = n.clone().eval(vec![]);
                Ok(Value::Fin(Box::new(Value::Succ(Box::new(n_val)))))
            },
            ITerm::FSucc(n, fp) => {
                // FSucc(m+1, fp) with fp: Fin(m) is an element of Fin(m+1), so Fin(m+1) adds one additional element to Fin(n)
                n.check_type(ctx.clone(), Value::Nat)?;
                let n_val = n.clone().eval(vec![]);
                match &n_val {
                    Value::Succ(m) => {
                        fp.check_type(ctx, Value::Fin(m.clone()))?;
                        Ok(Value::Fin(Box::new(n_val)))
                    }
                    _ => Err("Invalid element of finite type".to_owned())
                }
            },
            ITerm::FinElim(motive, base, ind, n, f) => {
                // motive must be a type parameterized by n: Nat and f: Fin(n) to
                motive.check_type(ctx.clone(), Value::Pi(Box::new(Value::Nat), Rc::new(|k| 
                    Value::Pi(Box::new(Value::Fin(Box::new(k))), Rc::new(|_| Value::Star))
                )))?;
                // n must be a natural number, of course
                n.check_type(ctx.clone(), Value::Nat)?;
                let motive_val = motive.clone().eval(vec![]); //we'll need NameEnv instead of just ctx if we want more parsing etc.
                let n_val = n.clone().eval(vec![]);
                let motive_val2 = motive_val.clone(); //annoying we have to do all this for ownership but oh well
                // base case must be a proof of the motive on FZero: Fin(n) for all n
                base.check_type(ctx.clone(), Value::Pi(Box::new(Value::Nat), Rc::new(
                    move |k| vapp(vapp(motive_val.clone(), Value::Succ(Box::new(k.clone()))), Value::FZero(Box::new(k)))
                )))?;
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
                // we can only use this to prove the motive holds on an element of Fin(n)
                f.check_type(ctx, Value::Fin(Box::new(n_val.clone())))?;
                let f_val = f.clone().eval(vec![]);
                // the motive holds of that object
                Ok(vapp(vapp(motive_val2, n_val), f_val))
            },
            ITerm::Bound(n) => Err(format!("Not sure how to assign a type to Bound({n}) in context {}, how did we get this?", 
                ctx.iter().map(|x| x.0.clone()).fold("".to_owned(), |acc, e| acc + &format!(" {:?}", e))))
        }
    }
}

impl CTerm {
    pub fn check_type(&self, ctx: Context, ty: Type) -> Result<(), String> {
        match self {
            CTerm::Inf(it) => {
                let ii = ctx.len();
                let ity = it.infer_type(ctx)?;
                if quote0(ity.clone()) != quote0(ty.clone()) {
                    Err(format!("Mismatch: Checked for {}, inferred {}, for term {}, level {ii}", quote(ii, ty), quote(ii, ity), self))
                }
                else {
                    Ok(())
                }
            },
            CTerm::Lam(body) => match ty {
                Value::Pi(src, trg) => {
                    let ii = ctx.len();
                    let mut new_ctx = ctx.clone();
                    new_ctx.push((Name::Local(ii), *src));
                    body.clone().subst(0, ITerm::Free(Name::Local(ii))).check_type(new_ctx, trg(vfree(Name::Local(ii))))
                },
                _ => Err("Function must have pi type".to_owned())
            }
        }
    }
}