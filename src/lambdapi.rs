#![allow(unused)]
/// Abstract syntax tree (AST) structs for the lambda calculus.
pub mod ast;
/// Typechecking of lambda calculus terms. Code is an impl on the AST types.
#[doc(hidden)]
pub mod check;
/// Evaluation of lambda calculus terms. Code is an impl on the AST types.
#[doc(hidden)]
pub mod eval;
/// Functions for rendering terms and types as strings.
pub mod print;
/// Functions for constructing and modifying terms beyond the basic struct definitions.
pub mod term;

use ast::*;
use term::*;
use check::*;
use eval::*;

// this mostly contains constructors for specific terms of LambdaPi
// at some point we could add a parser to extend it to more of a general tool

/// Type with 0 elements, i.e. False.
pub fn void() -> ITerm {
    ITerm::Fin(ITerm::Zero.into())
}

/// Powerset, implemented by P(X) = X -> * (each subset is a predicate).
pub fn sets_of() -> ITerm {
    iann(
        clam(ipi(bnd(0), ITerm::Star)),
        ipi(ITerm::Star, ITerm::Star)
    )
}

/// Sets of natural numbers.
pub fn sets_of_nat() -> ITerm {
    iapp(sets_of(), ITerm::Nat)
}

/// The "universe" type U used to derive Girard's Paradox: forall (X :: *) . (P(P(X)) -> X) -> P(P(X)).
pub fn u() -> ITerm {
    ipi(      //function that takes X
        ITerm::Star,          //of type *
        ipi(             //to a function that takes 
            ipi(        //a function taking z
                iapp(sets_of(), iapp(sets_of(), bnd(0))), //of type setsOf setsOf X
                bnd(1) //and producing an X
            ),
            iapp(sets_of(), iapp(sets_of(), bnd(1))) //and produces a setOf setsOf X
        )
    )
}

/// Powerset of U.
pub fn sets_of_u() -> ITerm {
    iapp(sets_of(), u())
}

/// A term of type P(P(U)) -> U.
pub fn tau() -> ITerm {
    iann(
        clam(clam(clam(clam(iapp(bnd(3),
            clam(iapp(bnd(1), iapp(bnd(2), iapp(iapp(bnd(0), bnd(3)), bnd(2)))))))))), 
        ipi(iapp(sets_of(), iapp(sets_of(), u())), u())
    )
}

/// A term of type U -> P(P(U)).
pub fn sigma() -> ITerm {
    iann(
        clam(iapp(iapp(bnd(0), u()), tau())),
        ipi(u(), iapp(sets_of(), iapp(sets_of(), u())))
    )
}


pub fn delta() -> ITerm {
    iann(
        clam(ipi(
            ipi(
                iapp(sets_of(), u()),
                ipi(
                    iapp(iapp(sigma(), bnd(1)), bnd(0)),
                    iapp(bnd(1), iapp(tau(), iapp(sigma(), bnd(2))))
                )
            ),
            void()
        )),
        iapp(sets_of(), u())
    )
}

pub fn preomega() -> ITerm {
    iann(
        clam(ipi(
            u(),
            ipi(
                iapp(iapp(sigma(), bnd(0)), bnd(1)),
                iapp(bnd(2), bnd(1))
            )
        )),
        iapp(sets_of(), iapp(sets_of(), u()))
    )
}

/// A term of type U.
pub fn omega() -> ITerm {
    iann(iapp(tau(), preomega()), u())
}

pub fn lem0() -> ITerm {
    iann(
        clam(clam(iapp(iapp(bnd(0), omega()), clam(iapp(bnd(1), iapp(tau(), iapp(sigma(), bnd(0)))))))),
        ipi(
            sets_of_u(),
            ipi(
                ipi(
                    u(),
                    ipi(
                        iapp(iapp(sigma(), bnd(0)), bnd(1)),
                        iapp(bnd(2), bnd(1))
                    )
                ),
                iapp(bnd(1), omega())
            )
        )
    )
}

pub fn sigma_omega() -> ITerm {
    iapp(sigma(), omega())
}

/// The type: forall (x: P(U)), forall (y: sigma(omega)(x)), x(tau(sigma(omega))).
/// In other words, the proposition that every subset of U which is in
/// sigma(omega) must contain tau(sigma(omega)).
pub fn d() -> ITerm {
    ipi(
        sets_of_u(),
        ipi(
            iapp(sigma_omega(), bnd(0)),
            iapp(bnd(1), iapp(tau(), sigma_omega()))
        )
    )
}

/// A proof of Not(D).
pub fn lem2() -> ITerm {
    iann(
        iapp(iapp(lem0(), delta()), clam(
            clam(
                clam(
                    iapp(iapp(iapp(bnd(0), delta()), bnd(1)), clam(
                        iapp(bnd(1), clam(
                            iapp(bnd(1), iapp(tau(), iapp(sigma(), bnd(0))))
                        ))
                    ))
                )
            )    
        )), 
        ipi(d(), void())
    )
}

/// A proof of D.
pub fn lem3() -> ITerm {
    iann(
        clam(
            iapp(lem0(), clam(
                iapp(bnd(1), iapp(tau(), iapp(sigma(), bnd(0))))
            ))
        ),
        d(),
    )
}

/// Girard's Paradox.
pub fn girard() -> ITerm {
    iann(iapp(lem2(), lem3()), void())
}

/// Girard's Paradox, with the lemmas reduced as far as possible.
pub fn girard_reduced() -> ITerm {
    let vty = quote0(ipi(d(), void()).eval(vec![]));
    let vlem2 = iann(quote0(lem2().eval(vec![])), vty);
    let vlem3 = quote0(lem3().eval(vec![]));
    iann(iapp(vlem2, vlem3), void())
}

/// for all types X, a function from False to X. Ex falso sequitur quodlibet.
pub fn exfalso() -> ITerm {
    iann(
        clam(
            clam(
                ITerm::FinElim(
                    clam(
                        ITerm::NatElim(clam(
                            ipi(
                                ITerm::Fin(bnd(0).into()),
                                ITerm::Star
                            )
                        ), clam(bnd(3)), clam(clam(clam(ITerm::Fin(inat(1).into())))), bnd(0).into())
                    ),
                    clam(ITerm::FZero(ITerm::Zero.into())),
                    clam(clam(clam(ITerm::FZero(ITerm::Zero.into())))), 
                    ITerm::Zero.into(),
                    bnd(0).into()
                )
            )
        ),
        ipi(ITerm::Star, ipi(void(), bnd(1)))
    )
}

pub fn validate(name: &str, term: &ITerm, eval: bool) {
    println!("{name}");
    // println!("{term}");
    let typ = term.infer_type(vec![]).expect("Term should be well-typed");
    if eval {
        let val = term.clone().eval(vec![]);
        let qval = quote0(val);
        qval.check_type(vec![], typ.clone()).expect("Evaluation should preserve type");
        let typ1 = quote0(typ);
        println!("\t{term} :::: {typ1}");
        println!("Passed type checks!");
        if qval == term.into() {
            println!("Is preserved by quote-eval")
        }
        else {
            println!("Not preserved by quote-eval (might just be application)")
        }
    }
}
