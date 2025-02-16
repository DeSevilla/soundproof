use std::rc::Rc;
use crate::lambdapi::{ast::*, term::*};

fn nat_elim_rec(motive: CTerm, base: CTerm, ind: CTerm, val: Value, env: Env) -> Value {
    match val {
        Value::Zero => base.eval(env.clone()),
        Value::Succ(n) => vapp(vapp(ind.clone().eval(env.clone()), *n.clone()), nat_elim_rec(motive, base, ind, *n, env)),
        Value::Neutral(neu) => Value::Neutral(Neutral::NatElim(
            Box::new(motive.eval(env.clone())), Box::new(base.eval(env.clone())), Box::new(ind.eval(env.clone())), Box::new(neu)
        )),
        _ => panic!("trying to eliminate Nat on non-Nat case")
    }
}

fn fin_elim_rec(motive: CTerm, base: Value, ind: Value, n: CTerm, val: Value, env: Env) -> Value {
    match val {
        Value::FZero(k) => vapp(base, *k),
        Value::FSucc(k, g) => vapp(vapp(vapp(ind.clone(), *k), *g.clone()), fin_elim_rec(motive, base, ind, n, *g, env)),
        Value::Neutral(neu) => Value::Neutral(Neutral::FinElim(
            Box::new(motive.eval(env.clone())), Box::new(base), Box::new(ind), Box::new(n.eval(env)), Box::new(neu))
        ),
        _ => panic!("trying to eliminate Fin on non-Fin case")
    }
}

impl ITerm {
    pub fn eval(self, env: Env) -> Value {
        match self {
            ITerm::Ann(e, _) => e.eval(env),
            ITerm::Star => Value::Star,
            ITerm::Pi(ty, body) => Value::Pi(Box::new(ty.eval(env.clone())), 
                Rc::new(move |x| body.clone().eval({let mut env = env.clone(); env.push(x); env}))
            ),
            ITerm::Free(x) => vfree(x),
            ITerm::Bound(i) => env[env.len() - i - 1].clone(),
            ITerm::App(lam, param) => vapp(lam.eval(env.clone()), param.eval(env)),
            ITerm::Nat => Value::Nat,
            ITerm::Zero => Value::Zero,
            ITerm::Succ(n) => Value::Succ(Box::new(n.eval(env))),
            ITerm::NatElim(motive, base, ind, k) => nat_elim_rec(motive, base, ind, k.eval(env.clone()), env),
            ITerm::Fin(cterm) => Value::Fin(Box::new(cterm.eval(env))),
            ITerm::FZero(cterm) => Value::FZero(Box::new(cterm.eval(env))),
            ITerm::FSucc(n, f) => Value::FSucc(Box::new(n.eval(env.clone())), Box::new(f.eval(env))),
            ITerm::FinElim(motive, base, ind, n, f) => fin_elim_rec(motive, base.eval(env.clone()), ind.eval(env.clone()), n, f.eval(env.clone()), env.clone()),
        }
    }
}

impl CTerm {
    pub fn eval(self, env: Env) -> Value {
        match self {
            CTerm::Inf(i) => i.eval(env),
            CTerm::Lam(e) => Value::Lam(Rc::new(move |x| e.clone().eval({let mut env = env.clone(); env.push(x); env})))
        }
    }
}
