use std::rc::Rc;

// LambdaPi was originally written for Haskell and I had to translate it to Rust myself.
// Certain design choices (bidirectional typing, a term/value split, higher-order abstract syntax)
// are more natural in Haskell than Rust, but they work out alright -- just requires a fair amount of 
// cloning to deal with ownership and Boxes to make types finitely-sized.

/// Term whose type can be inferred
#[derive(PartialEq, Debug, Clone)]
pub enum ITerm {
    /// Annotation (term, type)
    Ann(CTerm, CTerm),
    /// Type of types
    Star, 
    /// Dependent function type (parameter type, body type)
    Pi(CTerm, CTerm),
    /// Bound variable (de Bruijn index)
    Bound(usize),
    /// Free variable (name)
    Free(Name),
    /// Application (function term, argument term)
    App(Box<ITerm>, CTerm),
    /// Type of natural numbers
    Nat,
    /// 0
    Zero,
    /// Successor of n (n)
    Succ(CTerm),
    /// Natural number eliminator, for induction (motive, base case, successor case, number to fill into motive)
    NatElim(CTerm, CTerm, CTerm, CTerm),
    /// Finite type with n elements (n)
    Fin(CTerm),
    /// 0 element of type with n+1 elements (n)
    FZero(CTerm),
    /// Successor element of the type with n+1 elements (n+1, member of Fin(n))
    FSucc(CTerm, CTerm),
    /// Eliminator for induction on finite types (motive, base case, successor case, size of type, value to fill into motive)
    FinElim(CTerm, CTerm, CTerm, CTerm, CTerm),   
}



/// Term whose type can be checked
#[derive(PartialEq, Debug, Clone)]
pub enum CTerm {
    /// Term whose type *could* be inferred, but in a context where we only need to check it
    Inf(Box<ITerm>),
    /// Lambda (body)
    Lam(Box<CTerm>),
}


#[derive(PartialEq, Debug, Clone)]
pub enum Name {
    Global(String),
    Local(usize),
    Quote(usize),
}

/// Value that can't be reduced any further
#[derive(Clone)]
pub enum Value {
    /// Lambda value using higher-order abstract syntax (closure substituting arg into body)
    Lam(Rc<dyn Fn(Value) -> Value>),
    /// Type of types
    Star,
    /// Dependent function type using higher-order abstract syntax (closure substituting arg into body)
    Pi(Box<Value>, Rc<dyn Fn(Value) -> Value>),
    /// Neutral term that can't be reduced as it contains a free variable (term)
    Neutral(Neutral),
    /// Type of natural numbers
    Nat,
    /// 0
    Zero,
    /// Successor of n (n)
    Succ(Box<Value>),
    /// Finite type with n elements (n)
    Fin(Box<Value>),
    /// Zero value for finite type with n+1 elements (n)
    FZero(Box<Value>),
    /// Successor value for finite type with n+1 elements (n+1, element of Fin(n))
    FSucc(Box<Value>, Box<Value>),
}

/// Term containing a free variable somewhere
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