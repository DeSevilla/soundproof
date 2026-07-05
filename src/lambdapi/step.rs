use std::any::Any;

use crate::{AnnStep, CallBy, ast::*, lambdapi::term::quote0};

#[derive(Debug, Clone)]
pub enum Step<T: Stepper> {
    Cont(T, Option<ITerm>),
    Done(T, Option<ITerm>),
}

impl<T: Stepper> Step<T> {
    pub fn new(val: T) -> Self {
        Self::Cont(val, None)
    }
}

pub struct StepWithChange<T: Stepper> {
    tm: Step<T>,
    ctx: Context,
}

impl<T: Stepper> Iterator for StepWithChange<T> {
    type Item = (T, Option<ITerm>);

    fn next(&mut self) -> Option<Self::Item> {
        match &self.tm {
            Step::Done(_, _) => None,
            Step::Cont(t, c) => {
                let res = (t.clone(), c.clone());
                self.tm = t.clone().step(self.ctx.clone());
                Some(res)
            }
        }
    }
}

pub struct StepOver<T: Stepper> {
    tm: Step<T>,
    ctx: Context,
}

impl<T: Stepper> Iterator for StepOver<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match &self.tm {
            Step::Done(_, _) => None,
            Step::Cont(t, _) => {
                let res = t.clone();
                self.tm = t.clone().step(self.ctx.clone());
                Some(res)
            }
        }
    }
}

impl<T: Stepper> Step<T> {
    pub fn apply<X: Stepper>(self, f: impl FnOnce(T) -> X) -> Step<X> {
        use Step::*;
        match self {
            Cont(t, v) => Cont(f(t), v),
            Done(t, v) => Done(f(t), v),
        }
    }

    pub fn step(self, ctx: Context) -> Self {
        match self {
            Self::Cont(t, _) => t.step(ctx),
            Step::Done(_, _) => self,
        }
    }

    pub fn step_eval(self, ctx: Context) -> T {
        use Step::*;
        let mut result = self;
        loop {
            result = result.step(ctx.clone());
            match result {
                Done(t, _) => return t,
                Cont(_, _) => (),
            }
        }
    }
}

pub trait Stepper: Clone {
    fn step(self, ctx: Context) -> Step<Self>
    where
        Self: Sized;

    // fn step_and(self, ctx: Context) -> (Step<Self>, Self) where Self:Sized;

    fn step_over(self, ctx: Context) -> StepOver<Self>
    where
        Self: Sized,
    {
        StepOver {
            tm: Step::Cont(self, None),
            ctx,
        }
    }
    fn step_with_change(self, ctx: Context) -> StepWithChange<Self>
    where
        Self: Sized,
    {
        StepWithChange {
            tm: Step::Cont(self, None),
            ctx,
        }
    }
}

impl Stepper for ITerm {
    fn step(self, ctx: Context) -> Step<ITerm> {
        use Step::*;
        match self {
            ITerm::Ann(body, ty) => match ctx.ann_step {
                // neither: body inf: drop, body lam: type steps if possible
                // in other words: both step if possible, body inf: both drop
                AnnStep::Neither => match body {
                    CTerm::Inf(it) => {
                        // println!("annotating {}", );
                        Cont(*it, Some(ITerm::Ann(ty, CTerm::Inf(Box::new(ITerm::Star)))))
                    }
                    CTerm::Lam(_) => ty.step(ctx).apply(|typ| ITerm::Ann(body, typ)), //lam doesn't step
                },
                // both: ty steps if possible, otherwise body steps if possible, and if body is inf drop type
                AnnStep::Unprincipled => match ty.step(ctx.clone()) {
                    Cont(typ, v) => Cont(ITerm::Ann(body, typ), v),
                    Done(typ, _) => body.step(ctx).apply(|ct| match ct {
                        CTerm::Inf(it) => *it,
                        // CTerm::Inf(it) => {print!("ann dropping {:?} {}; ", typ.tag(), format!("{typ}").len()); *it},
                        _ => ITerm::Ann(ct, typ),
                    }),
                },
                // ty steps if possible, otherwise body steps if possible, otherwise if body is inf drop
                AnnStep::Both => match ty.step(ctx.clone()) {
                    Cont(typ, v) => Cont(ITerm::Ann(body, typ), v),
                    Done(typ, _) => match body.step(ctx) {
                        Done(bod, ch) => match bod { 
                            CTerm::Inf(it) => Cont(*it, ch),
                            // CTerm::Inf(it) => {print!("ann dropping {:?} {}; ", typ.tag(), format!("{typ}").len()); *it},
                            _ => Cont(ITerm::Ann(bod, typ), ch)
                        }
                        Cont(bod, ch) => Cont(ITerm::Ann(bod, typ), ch), 
                    }
                },
                // type: type steps if possible, otherwise if body is inf drop
                AnnStep::Type => match ty.step(ctx.clone()) {
                    Cont(typ, v) => Cont(ITerm::Ann(body, typ), v),
                    Done(typ, v) => match body {
                        CTerm::Inf(it) => Cont(
                            *it,
                            Some(ITerm::Ann(typ, CTerm::Inf(Box::new(ITerm::Star)))),
                        ),
                        _ => Done(ITerm::Ann(body, typ), v),
                    },
                },
                // // body: body steps if possible, otherwise if body is inf both step always, if body is lam type steps if possible
                // AnnStep::Body => match body.step(ctx.clone()) {
                //     Cont(bod, v) => Cont(ITerm::Ann(bod, ty), v),
                //     Done(bod, _) => match bod {
                //         CTerm::Inf(it) => {
                //             Cont(*it, Some(ITerm::Ann(ty, CTerm::Inf(Box::new(ITerm::Star)))))
                //         }
                //         CTerm::Lam(_) => ty.step(ctx).apply(|typ| ITerm::Ann(bod, typ)),
                //     },
                // },
            },
            // note: this alternative way of running Ann-step means False annotations are never removed
            // also breaks will_step somewhat
            //
            ITerm::Star => Done(ITerm::Star, None),
            ITerm::Pi(src, trg) => match src.clone().step(ctx) {
                Cont(c, v) => Cont(ITerm::Pi(c, trg), v),
                Done(c, v) => Done(ITerm::Pi(c, trg), v),
            },
            ITerm::Bound(nat) => Done(ITerm::Bound(nat), None),
            ITerm::Free(name) => match ctx.find_free(&name) {
                Some((ty, Some(val))) => {
                    // println!("free");
                    let v = ITerm::Ann(quote0(&val), quote0(&ty));
                    Cont(v.clone(), Some(v))
                }
                _ => panic!(
                    "Attempted to evaluate free variable {name:?} without a definition in context"
                ),
            },
            ITerm::App(func, arg) => {
                match func.step(ctx.clone()) {
                    Cont(f, v) => Cont(ITerm::App(Box::new(f), arg), v),
                    Done(f, _) => {
                        match ctx.call_by {
                            CallBy::Value => match arg.step(ctx.clone()) {
                                Cont(a, v) => Cont(ITerm::App(Box::new(f), a), v),
                                Done(a, _) => match f {
                                    ITerm::Ann(CTerm::Lam(body), CTerm::Inf(ty)) => {
                                        match *ty.clone() {
                                            ITerm::Pi(src, trg) => {
                                                // print!("lam @ {:?} | {} <- {} => ", arg.tag(), format!("{body}").len(), format!("{arg}").len());
                                                let tm = ITerm::Ann(a, src);
                                                let resbody = body.subst(0, &tm);
                                                let resty = trg.subst(0, &tm);
                                                // print!("{}; ", format!("{resbody}").len());
                                                Cont(ITerm::Ann(resbody, resty), Some(tm))
                                            }
                                            _ => panic!("got malformed lambda type"),
                                        }
                                    }
                                    _ => panic!("got malformed lambda value"),
                                },
                            },
                            CallBy::Name => match f {
                                ITerm::Ann(CTerm::Lam(body), CTerm::Inf(ty)) => {
                                    match *ty.clone() {
                                        ITerm::Pi(src, trg) => {
                                            // print!("lam @ {:?} | {} <- {} => ", arg.tag(), format!("{body}").len(), format!("{arg}").len());
                                            let tm = ITerm::Ann(arg, src);
                                            let resbody = body.subst(0, &tm);
                                            let resty = trg.subst(0, &tm);
                                            // print!("{}; ", format!("{resbody}").len());
                                            Cont(ITerm::Ann(resbody, resty), Some(tm))
                                        }
                                        _ => panic!("got malformed lambda type"),
                                    }
                                }
                                _ => panic!("got malformed lambda value"),
                            },
                        }
                    }
                }
            }
            ITerm::Nat => Done(ITerm::Nat, None),
            ITerm::Zero => Done(ITerm::Zero, None),
            ITerm::Succ(n) => n.step(ctx).apply(ITerm::Succ),
            ITerm::NatElim(cterm, cterm1, cterm2, cterm3) => todo!(), // these aren't used in the paradox
            ITerm::Fin(nat) => nat.step(ctx).apply(ITerm::Fin),
            ITerm::FZero(nat) => nat.step(ctx).apply(ITerm::FZero),
            ITerm::FSucc(cterm, cterm1) => todo!(),
            ITerm::FinElim(cterm, cterm1, cterm2, cterm3, cterm4) => todo!(),
            ITerm::Eq(cterm, cterm1, cterm2) => todo!(),
            ITerm::Refl(cterm, cterm1) => todo!(),
            ITerm::EqElim(cterm, cterm1, cterm2, cterm3, cterm4, cterm5) => todo!(),
        }
    }
}

impl Stepper for CTerm {
    fn step(self, ctx: Context) -> Step<CTerm> {
        use Step::*;
        match self {
            CTerm::Inf(iterm) => iterm.step(ctx).apply(|i| CTerm::Inf(Box::new(i))),
            CTerm::Lam(_) => Done(self, None), // can't step w/i body w/o doing fancy var things
        }
    }
}

#[cfg(test)]
fn eval_verify(tm: ITerm, ctx: Context) -> ITerm {
    use crate::lambdapi::term::std_env;
    tm.infer_type(&ctx).unwrap();
    let mut current = Step::Cont(tm, None);
    let mut steps = 0;
    loop {
        steps += 1;
        match current {
            Step::Cont(t, _) => {
                println!("{steps} {t:?}");
                t.infer_type(&ctx).unwrap();
                current = t.step(ctx.clone());
            }
            Step::Done(t, _) => return t,
        }
    }
}

#[cfg(test)]
fn check_match(tm: ITerm) {
    println!("Base term: {tm:?}");
    let ctx = Context::default();
    let u_eval1 = quote0(&tm.eval(&ctx));
    println!("Term 1: {u_eval1:?}");
    let u_eval2 = quote0(&eval_verify(tm, ctx.clone()).eval(&ctx));
    println!("Term 2: {u_eval2:?}");
    assert!(u_eval1 == u_eval2);
}

#[test]
fn check_tau() {
    use crate::tau;
    check_match(tau());
}

#[test]
fn check_omega() {
    use crate::omega;
    check_match(omega());
}

#[test]
fn check_lem2() {
    use crate::lem2;
    check_match(lem2());
}
