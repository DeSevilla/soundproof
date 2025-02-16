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

// pub fn i_can_step(term: &ITerm) -> bool {
//     match term {
//         ITerm::Ann(body, ty) => c_can_step(body) || c_can_step(ty),
//         ITerm::Star => false,
//         ITerm::Pi(param_ty, body) => c_can_step(param_ty) || c_can_step(body),
//         ITerm::Bound(_) => false,
//         ITerm::Free(name) => false,
//         ITerm::App(iterm, cterm) => true,
//         ITerm::Nat => false,
//         ITerm::Zero => false,
//         ITerm::Succ(cterm) => c_can_step(cterm),
//         ITerm::NatElim(motive, base, ind, n) => c_can_step(motive) || c_can_step(base) || c_can_step(ind) || c_can_step(n),
//         ITerm::Fin(cterm) => c_can_step(cterm),
//         ITerm::FinElim(motive, base, ind, n, f) => c_can_step(motive) || c_can_step(base) || c_can_step(ind) || c_can_step(n),
//         ITerm::FZero(cterm) => c_can_step(cterm),
//         ITerm::FSucc(cterm, cterm1) => c_can_step(cterm) || c_can_step(cterm1),
//     }
// }

// pub fn c_can_step(term: &CTerm) -> bool {
//     match term {
//         CTerm::Inf(iterm) => i_can_step(iterm),
//         CTerm::Lam(cterm) => c_can_step(cterm),
//     }
// }


// pub fn small_step_c_eval(term: CTerm, env: Vec<CTerm>) -> CTerm {
//     match term {
//         CTerm::Inf(iterm) => small_step_i_eval(*iterm, env),
//         CTerm::Lam(cterm) => if c_can_step(&cterm) { 
//             CTerm::Lam(Box::new(small_step_c_eval(*cterm, env))) 
//         } else {
//             CTerm::Lam(cterm) 
//         },
//     }
// }

// pub fn small_step_i_eval(term: ITerm, env: Vec<CTerm>) -> CTerm {
//     match term {
//         ITerm::Ann(body, ty) => {
//             let (body, ty) = if c_can_step(&ty) { 
//                 (small_step_c_eval(body, env), ty)
//             } else {
//                 (body, small_step_c_eval(ty, env))
//             };
//             CTerm::Inf(Box::new(ITerm::Ann(body, ty)))
//         }
//         ITerm::Star => CTerm::Inf(Box::new(ITerm::Star)),
//         ITerm::Pi(param, body) => {
//             let (param, body) = if c_can_step(&param) {
//                 (param, small_step_c_eval(body, env))
//             } else if c_can_step(&body) {
//                 (small_step_c_eval(param, env), body)
//             } else {
//                 (param, body)
//             };
//             CTerm::Inf(Box::new(ITerm::Pi(param, body)))
//         },
//         ITerm::Bound(i) => env[env.len() - i - 1].clone(), // idk if we even want this?
//         ITerm::Free(name) => todo!(),
//         ITerm::App(iterm, cterm) => todo!(), // this seems basically impossible unfortunately
//         ITerm::Nat => todo!(),
//         ITerm::Zero => todo!(),
//         ITerm::Succ(cterm) => todo!(),
//         ITerm::NatElim(cterm, cterm1, cterm2, cterm3) => todo!(),
//         ITerm::Fin(cterm) => todo!(),
//         ITerm::FinElim(cterm, cterm1, cterm2, cterm3, cterm4) => todo!(),
//         ITerm::FZero(cterm) => todo!(),
//         ITerm::FSucc(cterm, cterm1) => todo!(),
//     }
// }