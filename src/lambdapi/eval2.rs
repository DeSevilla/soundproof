use crate::{ast::*, lambdapi::term::quote0};

#[derive(Debug, Clone)]
pub enum Step<T: Stepper> {
    Cont(T),
    Done(T)
}

pub struct StepOver<T: Stepper> {
    tm: Step<T>,
    ctx: Context,
}

impl<T: Stepper> Iterator for StepOver<T> {
    type Item = T;
    
    fn next(&mut self) -> Option<Self::Item> {
        match &self.tm {
            Step::Done(_) => None,
            Step::Cont(t) => {
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
            Cont(t) => Cont(f(t)),
            Done(t) => Done(f(t))
        }
    }

    pub fn step(self, ctx: Context) -> Self {
        match self {
            Self::Cont(t) => t.step(ctx),
            Step::Done(_) => self,
        }
    }

    pub fn step_eval(self, ctx: Context) -> T {
        use Step::*;
        let mut result = self;
        loop {
            result = result.step(ctx.clone());
            match result {
                Done(t) => return t,
                Cont(_) => (),
            }
        }
    }
}
pub trait Stepper: Clone {
    fn step(self, ctx: Context) -> Step<Self> where Self: Sized;

    fn step_over(self, ctx: Context) -> StepOver<Self> where Self: Sized {
        StepOver { tm: Step::Cont(self), ctx }
    }
}

impl Stepper for ITerm {
    fn step(self, ctx: Context) -> Step<ITerm> {
        use Step::*;
        match self {
            ITerm::Ann(body, ty) => match ty.step(ctx.clone()) {
                Cont(typ) => Cont(ITerm::Ann(body, typ)),
                Done(typ) => body.step(ctx).apply(|ct| match ct {
                    CTerm::Inf(it) => *it,
                    _ => ITerm::Ann(ct, typ)
                }),
            },
            ITerm::Star => Done(ITerm::Star),
            ITerm::Pi(src, trg) => match src.clone().step(ctx) {
                Cont(c) => Cont(ITerm::Pi(c, trg)),
                Done(c) => {
                    Done(ITerm::Pi(c, trg))
                }
            },
            ITerm::Bound(nat) => Done(ITerm::Bound(nat)),
            ITerm::Free(name) => match ctx.find_free(&name) {
                Some((ty, Some(val))) => Cont(ITerm::Ann(quote0(val), quote0(ty))),
                _ => panic!("Attempted to evaluate free variable {name:?} without a definition in context"),
            },
            ITerm::App(func, arg) => {
                match func.step(ctx.clone()) {
                    Cont(f) => Cont(ITerm::App(Box::new(f), arg)),
                    Done(f) => {
                        match f {
                            ITerm::Ann(CTerm::Lam(body), CTerm::Inf(ty)) => {
                                match *ty.clone() {
                                    ITerm::Pi(src, trg) => {
                                        let tm = ITerm::Ann(arg, src);
                                        let resbody = body.subst(0, &tm);
                                        let resty = trg.subst(0, &tm);
                                        Cont(ITerm::Ann(resbody, resty))
                                    },
                                    _ => panic!("got malformed lambda type"),
                                }
                            }
                            _ => panic!("got malformed lambda value"),
                        }
                    }
                }
            },
            ITerm::Nat => Done(ITerm::Nat),
            ITerm::Zero => Done(ITerm::Zero),
            ITerm::Succ(n) => n.step(ctx).apply(ITerm::Succ),
            ITerm::NatElim(cterm, cterm1, cterm2, cterm3) => todo!(),
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
            CTerm::Lam(_) => Done(self),
        }
    }
}

#[cfg(test)]
fn eval_verify(tm: ITerm, ctx: Context) -> ITerm {
    use crate::lambdapi::term::std_env;
    tm.infer_type(ctx.clone()).unwrap();
    let mut current = Step::Cont(tm);
    let mut steps = 0;
    loop {
        steps += 1;
        match current {
            Step::Cont(t) => {
                println!("{steps} {t:?}");
                t.infer_type(ctx.clone()).unwrap();
                current = t.step(ctx.clone());
            },
            Step::Done(t) => return t
        }
    } 
}

#[cfg(test)]
fn check_match(tm: ITerm) {
    use crate::lambdapi::std_env;
    println!("Base term: {tm:?}");
    let ctx = Context::new(std_env());
    let u_eval1 = quote0(tm.eval(ctx.clone()));
    println!("Term 1: {u_eval1:?}");
    let u_eval2 = quote0(eval_verify(tm, ctx.clone()).eval(ctx));
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



