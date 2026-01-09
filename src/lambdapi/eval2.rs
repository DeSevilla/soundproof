use crate::{ast::*, lambdapi::term::quote0};

#[derive(Clone)]
pub enum Step<T: Stepper> {
    Cont(T),
    Done(T)
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

    pub fn multistep(self, ctx: Context) -> T {
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
pub trait Stepper {
    fn step(self, ctx: Context) -> Step<Self> where Self: Sized;
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
            ITerm::Pi(src, trg) => match src.clone().step(ctx.clone()) {
                Cont(c) => Cont(ITerm::Pi(c, trg)),
                Done(c) => {
                    Done(ITerm::Pi(c, trg))
                    // let mut ctx2 = ctx.clone();
                    // println!("We are about to evaluate type {c:?} in context {} based on {src:?} with target {trg:?}", ctx.info_string());
                    // ctx2.bind_type(c.eval(ctx.clone()));
                    // trg.step(ctx2).apply(|t| ITerm::Pi(c, t))
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
                                        let res = body.subst(0, &ITerm::Ann(arg, src));
                                        Cont(ITerm::Ann(res, CTerm::Inf(ty)))
                                    },
                                    _ => panic!("got malformed lambda type"),
                                }
                                // let new = ;
                                // Cont(new)
                            }
                            _ => panic!("got malformed lambda value"),
                        }
                    }
                }
                // let fn_ty = func.infer_type(ctx).unwrap();
                // match fn_ty {
                //     ITerm::Pi(src, trg) => 
                //     _ => panic!("Got invalid type for function while evaluating")
                // }

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


#[test]
fn check_omega_match() {
    use crate::omega;
    let u_tm = omega();
    println!("Base term: {u_tm:?}");
    let u_eval1 = quote0(u_tm.eval(Context::new(vec![])));
    println!("Term 1: {u_eval1:?}");
    let u_eval2 = match Step::Cont(u_tm).multistep(Context::new(vec![])) {
        ITerm::Ann(ut, ty) => ut,
        _ => panic!("inferrable!!"),
    };
    println!("Term 2: {u_eval2:?}");
    assert!(u_eval1 == u_eval2);
}

