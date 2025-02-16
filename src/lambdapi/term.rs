use std::borrow::Borrow;

use crate::lambdapi::ast::*;

/// Abbreviated constructor for free variable Values.
pub fn vfree(name: Name) -> Value {
    Value::Neutral(Neutral::Free(name))
}

/// Applies a lambda value to an argument value.
/// Panics if `lam` isn't actually a possible lambda.
pub fn vapp(lam: Value, arg: Value) -> Value {
    match lam {
        Value::Lam(f) => (*f)(arg),
        Value::Neutral(n) => Value::Neutral(Neutral::App(Box::new(n), Box::new(arg))),
        _ => panic!()  // No other Values can be lambdas
    }
}

/// Converts anything that can be borrowed as an ITerm into a CTerm by cloning.
pub fn itoc<T: Borrow<ITerm>>(i: T) -> CTerm {
    CTerm::Inf(Box::new(i.borrow().clone()))
}

/// Abbreviated constructor for lambda-application ITerms.
pub fn iapp<T: Borrow<ITerm>, U: Into<CTerm>>(f: T, x: U) -> ITerm {
    ITerm::App(Box::new(f.borrow().clone()), x.into())
}

/// Abbreviated constructor for type-annotation ITerms.
pub fn iann<T: Into<CTerm>, U: Into<CTerm>>(term: T, ty: U) -> ITerm {
    ITerm::Ann(term.into(), ty.into())
}

/// Abbreviated constructor for pi-type ITerms.
pub fn ipi<T: Into<CTerm>, U: Into<CTerm>>(src: T, trg: U) -> ITerm {
    ITerm::Pi(src.into(), trg.into())
}

/// Abbreviated constructor for lambda CTerms.
pub fn clam<T: Into<CTerm>>(body: T) -> CTerm {
    CTerm::Lam(Box::new(body.into()))
}

/// Abbreviation for bound-variable ITerm.
pub fn bnd(n: usize) -> ITerm {
    ITerm::Bound(n)
}

/// Converts usize to ITerm natural numbers.
pub fn inat(n: usize) -> ITerm {
    if n == 0 {
        ITerm::Zero
    }
    else {
        ITerm::Succ(inat(n-1).into())
    }
}

impl ITerm {
    /// Recursively substitutes in a new ITerm for a bound variable.
    pub fn subst(self, i: usize, new: ITerm) -> Self {
        match self {
            ITerm::Ann(b, t) => ITerm::Ann(b.subst(i, new.clone()), t.subst(i, new)),
            ITerm::Star => ITerm::Star,
            ITerm::Pi(src, trg) => ITerm::Pi(src.subst(i, new.clone()), trg.subst(i + 1, new)),
            ITerm::Bound(j) => if i == j { new } else { ITerm::Bound(j) },
            ITerm::Free(n) => ITerm::Free(n),
            ITerm::App(f, x) => ITerm::App(Box::new(f.subst(i, new.clone())), x.subst(i, new)),
            ITerm::Nat => ITerm::Nat,
            ITerm::Zero => ITerm::Zero,
            ITerm::Succ(cterm) => ITerm::Succ(cterm.subst(i, new)),
            ITerm::NatElim(motive, base, ind, k) => ITerm::NatElim(
                motive.subst(i, new.clone()), base.subst(i, new.clone()), ind.subst(i, new.clone()), k.subst(i, new)
            ),
            ITerm::Fin(cterm) => ITerm::Fin(cterm.subst(i, new)),
            ITerm::FinElim(motive, base, ind, n, f) => ITerm::FinElim(
                motive.subst(i, new.clone()), base.subst(i, new.clone()), ind.subst(i, new.clone()), n.subst(i, new.clone()), f.subst(i, new)
            ),
            ITerm::FZero(cterm) => ITerm::FZero(cterm.subst(i, new)),
            ITerm::FSucc(n, f) => ITerm::FSucc(n.subst(i, new.clone()), f.subst(i, new)),
        }
    }
}


impl CTerm {
    /// Recursively substitutes in a new ITerm for a bound variable.
    pub fn subst(self, i: usize, new: ITerm) -> Self {
        match self {
            CTerm::Inf(it) => CTerm::Inf(Box::new(it.subst(i, new))),
            CTerm::Lam(body) => CTerm::Lam(Box::new(body.subst(i + 1, new)))
        }
    }
}

/// Converts a Value into a term, under no binders.
pub fn quote0(val: Value) -> CTerm {
    quote(0, val)
}

/// Converts a Value into a term, under `i` binders.
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

/// Converts a neutral term (containing free variable) into an ITerm.
pub fn neutral_quote(i: usize, neutral: Neutral) -> ITerm {
    match neutral {
        Neutral::Free(x) => boundfree(i, x),
        Neutral::App(f, x) => ITerm::App(Box::new(neutral_quote(i, *f)), quote(i, *x)),
        Neutral::NatElim(motive, base, ind, k) => ITerm::NatElim(quote(i, *motive), quote(i, *base), quote(i, *ind), itoc(neutral_quote(i, *k))),
        Neutral::FinElim(motive, base, ind, n, f) => ITerm::FinElim(quote(i, *motive), quote(i, *base), quote(i, *ind), quote(i, *n), itoc(neutral_quote(i, *f))),
    }
}

/// Quotes a name into a bound or free variable term as appropriate.
pub fn boundfree(i: usize, name: Name) -> ITerm {
    match name {
        Name::Quote(k) => ITerm::Bound(i - k - 1),
        _ => ITerm::Free(name)
    }
}