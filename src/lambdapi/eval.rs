use std::rc::Rc;
use crate::lambdapi::{ast::*, term::*};

fn nat_elim_rec(motive: &CTerm, base: &CTerm, ind: &CTerm, val: Value, ctx: &Context) -> Value {
    match val {
        Value::Zero => base.eval(ctx),
        Value::Succ(n) => vapp(vapp(ind.clone().eval(ctx), *n.clone()), nat_elim_rec(motive, base, ind, *n, ctx)),
        Value::Neutral(neu) => Value::Neutral(Neutral::NatElim(
            Box::new(motive.eval(ctx)), Box::new(base.eval(ctx)), Box::new(ind.eval(ctx)), Box::new(neu)
        )),
        _ => panic!("trying to eliminate Nat on non-Nat case")
    }
}

fn fin_elim_rec(motive: &CTerm, base: Value, ind: Value, n: &CTerm, val: Value, ctx: &Context) -> Value {
    match val {
        Value::FZero(k) => vapp(base, *k),
        Value::FSucc(k, g) => vapp(vapp(vapp(ind.clone(), *k), *g.clone()), fin_elim_rec(motive, base, ind, n, *g, ctx)),
        Value::Neutral(neu) => Value::Neutral(Neutral::FinElim(
            Box::new(motive.eval(ctx)), Box::new(base), Box::new(ind), Box::new(n.eval(ctx)), Box::new(neu)
        )),
        _ => panic!("trying to eliminate Fin on non-Fin case")
    }
}

fn eq_elim_rec(ty: &CTerm, motive: &CTerm, base: Value, x: &CTerm, y: &CTerm, eq: Value, ctx: &Context) -> Value {
    match eq {
        Value::Refl(_, x) => vapp(base, *x),
        Value::Neutral(neu) => Value::Neutral(Neutral::EqElim(
            Box::new(ty.eval(ctx)), Box::new(motive.eval(ctx)), Box::new(base), 
            Box::new(x.eval(ctx)), Box::new(y.eval(ctx)), Box::new(neu)
        )),
        _ => panic!("trying to eliminate Eq on non-Eq case"),
    }
}

impl ITerm {
    pub fn eval(&self, ctx: &Context) -> Value {
        match self {
            ITerm::Ann(e, _) => e.eval(ctx),
            ITerm::Star => Value::Star,
            ITerm::Pi(ty, body) => {
                let body = body.clone();
                let ctx2 = ctx.clone();
                Value::Pi(Box::new(ty.eval(ctx)), 
                    Rc::new(move |x| {
                        let mut ctx2 = ctx2.clone();
                        ctx2.bind_value(x);
                        body.clone().eval(&ctx2) 
                    }))
            },
            ITerm::Free(x) => if let Some((_, Some(v))) = ctx.find_free(x) { v } else { vfree(x.clone()) },
            //     _ => vfree(x),
            // },
            ITerm::Bound(i) => ctx.get_bound(*i),
            ITerm::App(lam, param) => vapp(lam.eval(ctx), param.eval(ctx)),
            ITerm::Nat => Value::Nat,
            ITerm::Zero => Value::Zero,
            ITerm::Succ(n) => Value::Succ(Box::new(n.eval(ctx))),
            ITerm::NatElim(motive, base, ind, k) => nat_elim_rec(motive, base, ind, k.eval(ctx), ctx),
            ITerm::Fin(cterm) => Value::Fin(Box::new(cterm.eval(ctx))),
            ITerm::FZero(cterm) => Value::FZero(Box::new(cterm.eval(ctx))),
            ITerm::FSucc(n, f) => Value::FSucc(Box::new(n.eval(ctx)), Box::new(f.eval(ctx))),
            ITerm::FinElim(motive, base, ind, n, f) => fin_elim_rec(motive, base.eval(ctx), ind.eval(ctx), n, f.eval(ctx), ctx),
            ITerm::Eq(a, x, y) => Value::Eq(Box::new(a.eval(ctx)), Box::new(x.eval(ctx)), Box::new(y.eval(ctx))),
            ITerm::Refl(a, x) => Value::Refl(Box::new(a.eval(ctx)), Box::new(x.eval(ctx))),
            ITerm::EqElim(a, motive, base, x, y, eq) => eq_elim_rec(a, motive, base.eval(ctx), x, y, eq.eval(ctx), ctx)
        }
    }
}

impl CTerm {
    pub fn eval(&self, ctx: &Context) -> Value {
        match self {
            CTerm::Inf(i) => i.clone().eval(ctx),
            CTerm::Lam(e) => {
                let e = e.clone();
                let ctx = ctx.clone();
                Value::Lam(Rc::new(move |x| e.clone().eval(&{let mut ctx = ctx.clone(); ctx.bind_value(x); ctx})))
            }
        }
    }
}
