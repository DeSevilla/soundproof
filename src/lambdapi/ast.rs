use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub enum ITerm {
    Ann(CTerm, CTerm),
    Star,
    Pi(CTerm, CTerm),
    Bound(usize),
    Free(Name),
    App(Box<ITerm>, CTerm),
    Nat,
    Zero,
    Succ(CTerm),
    NatElim(CTerm, CTerm, CTerm, CTerm),
    Fin(CTerm),
    FinElim(CTerm, CTerm, CTerm, CTerm, CTerm),
    FZero(CTerm),
    FSucc(CTerm, CTerm),
}

#[derive(PartialEq, Debug, Clone)]
pub enum CTerm {
    Inf(Box<ITerm>),
    Lam(Box<CTerm>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Name {
    Global(String),
    Local(usize),
    Quote(usize),
}

// #[derive(PartialEq, Debug, Clone)]
// pub enum Type {
//     Free(Name),
//     Fun(Box<Type>, Box<Type>),
// }

#[derive(Clone)]
pub enum Value {
    Lam(Rc<dyn Fn(Value) -> Value>),
    Star,
    Pi(Box<Value>, Rc<dyn Fn(Value) -> Value>),
    Neutral(Neutral),
    Nat,
    Zero,
    Succ(Box<Value>),
    Fin(Box<Value>),
    FZero(Box<Value>),
    FSucc(Box<Value>, Box<Value>),
}

#[derive(Clone)]
pub enum Neutral {
    Free(Name),
    App(Box<Neutral>, Box<Value>),
    NatElim(Box<Value>, Box<Value>, Box<Value>, Box<Neutral>),
    FinElim(Box<Value>, Box<Value>, Box<Value>, Box<Value>, Box<Neutral>)
}

pub type Env = Vec<Value>;
pub type Type = Value;
pub type Context = Vec<(Name, Type)>;

impl From<ITerm> for CTerm {
    fn from(value: ITerm) -> Self {
        CTerm::Inf(Box::new(value))
    }
}

impl From<&ITerm> for CTerm {
    // fn into(self) -> CTerm {
    //     self.clone().into()
    // }
    fn from(value: &ITerm) -> Self {
        value.clone().into()
    }
}

impl TryFrom<CTerm> for ITerm {
    type Error = String;

    fn try_from(value: CTerm) -> Result<Self, Self::Error> {
        match value {
            CTerm::Inf(iterm) => Ok(*iterm),
            CTerm::Lam(_) => Err("Lambda must be annotated to have an inferred type".to_owned()),
        }
    }
}

impl TryFrom<&CTerm> for ITerm {
    type Error = String;

    fn try_from(value: &CTerm) -> Result<Self, Self::Error> {
        value.clone().try_into()
    }
}