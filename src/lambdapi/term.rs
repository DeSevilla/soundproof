use std::borrow::Borrow;

use crate::lambdapi::ast::*;

pub fn vfree(name: Name) -> Value {
    Value::Neutral(Neutral::Free(name))
}

pub fn vapp(lam: Value, param: Value) -> Value {
    match lam {
        Value::Lam(f) => (*f)(param),
        Value::Neutral(n) => Value::Neutral(Neutral::App(Box::new(n), Box::new(param))),
        _ => panic!()  // should there be something else here?
    }
}

pub fn itoc<T: Borrow<ITerm>>(i: T) -> CTerm {
    CTerm::Inf(Box::new(i.borrow().clone()))
}

pub fn iapp<T: Borrow<ITerm>, U: Into<CTerm>>(f: T, x: U) -> ITerm {
    ITerm::App(Box::new(f.borrow().clone()), x.into())
}

pub fn iann<T: Into<CTerm>, U: Into<CTerm>>(term: T, ty: U) -> ITerm {
    ITerm::Ann(term.into(), ty.into())
}

pub fn ipi<T: Into<CTerm>, U: Into<CTerm>>(src: T, trg: U) -> ITerm {
    ITerm::Pi(src.into(), trg.into())
}

pub fn clam<T: Into<CTerm>>(body: T) -> CTerm {
    CTerm::Lam(Box::new(body.into()))
}

pub fn bnd(n: usize) -> ITerm {
    ITerm::Bound(n)
}

pub fn inat(n: usize) -> ITerm {
    if n == 0 {
        ITerm::Zero
    }
    else {
        ITerm::Succ(inat(n-1).into())
    }
}

pub fn i_subst(i: usize, new: ITerm, term: ITerm) -> ITerm {
    match term {
        ITerm::Ann(b, t) => ITerm::Ann(c_subst(i, new.clone(), b), c_subst(i, new, t)),
        ITerm::Star => ITerm::Star,
        ITerm::Pi(src, trg) => ITerm::Pi(c_subst(i, new.clone(), src), c_subst(i + 1, new, trg)),
        ITerm::Bound(j) => if i == j { new } else { ITerm::Bound(j) },
        ITerm::Free(n) => ITerm::Free(n),
        ITerm::App(f, x) => ITerm::App(Box::new(i_subst(i, new.clone(), *f)), c_subst(i, new, x)),
        ITerm::Nat => ITerm::Nat,
        ITerm::Zero => ITerm::Zero,
        ITerm::Succ(cterm) => ITerm::Succ(c_subst(i, new, cterm)),
        ITerm::NatElim(motive, base, ind, k) => ITerm::NatElim(
            c_subst(i, new.clone(), motive), c_subst(i, new.clone(), base), c_subst(i, new.clone(), ind), c_subst(i, new, k)
        ),
        ITerm::Fin(cterm) => ITerm::Fin(c_subst(i, new, cterm)),
        ITerm::FinElim(motive, base, ind, n, f) => ITerm::FinElim(c_subst(i, new.clone(), motive), c_subst(i, new.clone(), base), c_subst(i, new.clone(), ind), c_subst(i, new.clone(), n), c_subst(i, new, f)),
        ITerm::FZero(cterm) => ITerm::FZero(c_subst(i, new, cterm)),
        ITerm::FSucc(n, f) => ITerm::FSucc(c_subst(i, new.clone(), n), c_subst(i, new, f)),
    }
}

pub fn c_subst(i: usize, new: ITerm, term: CTerm) -> CTerm {
    match term {
        CTerm::Inf(it) => CTerm::Inf(Box::new(i_subst(i, new, *it))),
        CTerm::Lam(body) => CTerm::Lam(Box::new(c_subst(i + 1, new, *body)))
    }
}

pub fn quote0(val: Value) -> CTerm {
    quote(0, val)
}

pub fn quote(i: usize, val: Value) -> CTerm {
    match val {
        Value::Lam(f) => CTerm::Lam(Box::new(quote(i + 1, f(vfree(Name::Quote(i)))))),
        Value::Neutral(n) => itoc(neutral_quote(i, n)),
        Value::Star => itoc(ITerm::Star),
        Value::Pi(src, trg) => itoc(ITerm::Pi(quote(i, *src), quote(i + 1, trg(vfree(Name::Quote(i)))))),
        Value::Nat => itoc(ITerm::Nat),
        Value::Zero => itoc(ITerm::Zero),
        Value::Succ(value) => itoc(ITerm::Succ(quote(i, *value))),
        Value::Fin(value) => itoc(ITerm::Fin(quote(i, *value))),
        Value::FZero(value) => itoc(ITerm::FZero(quote(i, *value))),
        Value::FSucc(n, f) => itoc(ITerm::FSucc(quote(i, *n), quote(i, *f))),
    }
}

pub fn neutral_quote(i: usize, neutral: Neutral) -> ITerm {
    match neutral {
        Neutral::Free(x) => boundfree(i, x),
        Neutral::App(f, x) => ITerm::App(Box::new(neutral_quote(i, *f)), quote(i, *x)),
        Neutral::NatElim(motive, base, ind, k) => ITerm::NatElim(quote(i, *motive), quote(i, *base), quote(i, *ind), itoc(neutral_quote(i, *k))),
        Neutral::FinElim(motive, base, ind, n, f) => ITerm::FinElim(quote(i, *motive), quote(i, *base), quote(i, *ind), quote(i, *n), itoc(neutral_quote(i, *f))),
    }
}

pub fn boundfree(i: usize, name: Name) -> ITerm {
    match name {
        Name::Quote(k) => ITerm::Bound(i - k - 1),
        _ => ITerm::Free(name)
    }
}