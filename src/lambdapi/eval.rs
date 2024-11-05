use std::rc::Rc;
use crate::lambdapi::{ast::*, term::*};

fn nat_elim_rec(motive: CTerm, base: CTerm, ind: CTerm, val: Value, env: Env) -> Value {
    match val {
        Value::Zero => c_eval(base, env.clone()),
        Value::Succ(n) => vapp(vapp(c_eval(ind.clone(), env.clone()), *n.clone()), nat_elim_rec(motive, base, ind, *n, env)),
        Value::Neutral(neu) => Value::Neutral(Neutral::NatElim(
            Box::new(c_eval(motive, env.clone())), Box::new(c_eval(base, env.clone())), Box::new(c_eval(ind, env.clone())), Box::new(neu)
        )),
        _ => panic!("nat passed typecheck but wasn't 0, succ, or neutral")
    }
}

fn fin_elim_rec(motive: CTerm, base: Value, ind: Value, n: CTerm, val: Value, env: Env) -> Value {
    // let base_val = cEval(base, env.clone());
    match val {
        Value::FZero(k) => vapp(base, *k),
        Value::FSucc(k, g) => vapp(vapp(vapp(ind.clone(), *k), *g.clone()), fin_elim_rec(motive, base, ind, n, *g, env)),
        Value::Neutral(neu) => Value::Neutral(Neutral::FinElim(
            Box::new(c_eval(motive, env.clone())), Box::new(base), Box::new(ind), Box::new(c_eval(n, env)), Box::new(neu))
        ),
        _ => unreachable!("trying to eliminate Fin on non-Fin case")
    }
}

pub fn i_eval(term: ITerm, env: Env) -> Value {
    match term {
        ITerm::Ann(e, _) => c_eval(e, env),
        ITerm::Star => Value::Star,
        ITerm::Pi(ty, body) => Value::Pi(Box::new(c_eval(ty, env.clone())), 
            Rc::new(move |x| c_eval(body.clone(), {let mut env = env.clone(); env.push(x); env}))
        ),
        ITerm::Free(x) => vfree(x),
        ITerm::Bound(i) => env[env.len() - i - 1].clone(),
        ITerm::App(lam, param) => vapp(i_eval(*lam, env.clone()), c_eval(param, env)),
        ITerm::Nat => Value::Nat,
        ITerm::Zero => Value::Zero,
        ITerm::Succ(n) => Value::Succ(Box::new(c_eval(n, env))),
        ITerm::NatElim(motive, base, ind, k) => nat_elim_rec(motive, base, ind, c_eval(k, env.clone()), env),
        ITerm::Fin(cterm) => Value::Fin(Box::new(c_eval(cterm, env))),
        ITerm::FZero(cterm) => Value::FZero(Box::new(c_eval(cterm, env))),
        ITerm::FSucc(n, f) => Value::FSucc(Box::new(c_eval(n, env.clone())), Box::new(c_eval(f, env))),
        ITerm::FinElim(motive, base, ind, n, f) => fin_elim_rec(motive, c_eval(base, env.clone()), c_eval(ind, env.clone()), n, c_eval(f, env.clone()), env.clone()),
    }
}

pub fn c_eval(term: CTerm, env: Env) -> Value {
    match term {
        CTerm::Inf(i) => i_eval(*i, env),
        CTerm::Lam(e) => Value::Lam(Rc::new(move |x| c_eval(*(e.clone()), {let mut env = env.clone(); env.push(x); env})))
    }
}