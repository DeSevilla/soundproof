#![allow(unused)]
/// Abstract syntax tree (AST) structs for the lambda calculus.
pub mod ast;
/// Typechecking of lambda calculus terms. Code is an impl on the AST types.
pub mod check;
/// Evaluation of lambda calculus terms. Code is an impl on the AST types.
pub mod eval;
/// Functions for rendering terms and types as strings.
pub mod print;
/// Functions for constructing and modifying terms beyond the basic struct definitions.
pub mod term;
// pub mod parse;

// this module structure is essentially Klyuchnikov's
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

/// A term of type P(P(U)) -> U. See [Hurkens et al.](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf) for details.
pub fn tau() -> ITerm {
    iann(
        clam(clam(clam(clam(iapp(bnd(3),
            clam(iapp(bnd(1), iapp(bnd(2), iapp(iapp(bnd(0), bnd(3)), bnd(2)))))))))), 
        ipi(iapp(sets_of(), iapp(sets_of(), u())), u())
    )
}

/// A term of type U -> P(P(U)). See [Hurkens et al.](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf) for details.
pub fn sigma() -> ITerm {
    iann(
        clam(iapp(iapp(bnd(0), u()), tau())),
        ipi(u(), iapp(sets_of(), iapp(sets_of(), u())))
    )
}

pub fn precedes() -> ITerm {
    iann(
        clam(
            clam(
                ipi(
                    iapp(sets_of(), u()),
                    ipi(
                        iapp(iapp(sigma(), bnd(2)), bnd(0)),
                        iapp(bnd(1), bnd(2))
                    )
                )
            )
        ),
        ipi(u(), iapp(sets_of(), u()))
    )
}

/// The subset of U such that for an element x, [tau]\([sigma]\(x)) is not a "predecessor" of x.
/// See [Hurkens et al.](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf) for details.
pub fn delta() -> ITerm {
    iann(
        clam(ipi(
            iapp(iapp(precedes(), bnd(0)), iapp(tau(), iapp(sigma(), bnd(0)))),
            // ipi(
            //     iapp(sets_of(), u()),
            //     ipi(
            //         iapp(iapp(sigma(), bnd(1)), bnd(0)),
            //         iapp(bnd(1), iapp(tau(), iapp(sigma(), bnd(2))))
            //     )
            // ),
            void()
        )),
        iapp(sets_of(), u())
    )
}

/// The set of "inductive" subsets of U, which is itself an element of P(P(U)).
/// See [Hurkens et al.](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf) for details.
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

/// An element of U -- [tau] of the set of all inductive subsets of U.
/// See [Hurkens et al.](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf) for details.
pub fn omega() -> ITerm {
    iann(iapp(tau(), preomega()), u())
}

/// Proof that [omega] is "well-founded", meaning that it's in all inductive subsets of U.
/// See [Hurkens et al.](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf) for details.
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

/// [Sigma](sigma) of [omega].
/// See [Hurkens et al.](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf) for details.
pub fn sigma_omega() -> ITerm {
    iapp(sigma(), omega())
}

/// The proposition that [tau]\([sigma]\([omega])) is a predecessor of [omega].
/// See [Hurkens et al.](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf) for details.
pub fn d() -> ITerm {
    iapp(iapp(precedes(), omega()), iapp(tau(), iapp(sigma(), omega())))
    // ipi(
    //     sets_of_u(),
    //     ipi(
    //         iapp(sigma_omega(), bnd(0)),
    //         iapp(bnd(1), iapp(tau(), sigma_omega()))
    //     )
    // )
}

pub fn delta_inductive() -> ITerm {
    iann(
        clam(
            clam(
                clam(
                    iapp(iapp(iapp(bnd(0), delta()), bnd(1)), clam(
                        iapp(bnd(1), clam(
                            iapp(bnd(1), iapp(tau(), iapp(sigma(), bnd(0))))
                        ))
                    ))
                )
            )    
        ),
        iapp(preomega(), delta())
    ) 
}

/// A proof that [tau]\([sigma]\([omega])) is not a predecessor of [omega].
/// See [Hurkens et al.](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf) for details.
pub fn lem2() -> ITerm {
    iann(
        iapp(iapp(lem0(), delta()), delta_inductive()),
        ipi(d(), void())
    )
}

/// A proof that [tau]\([sigma]\([omega])) is a predecessor of [omega].
/// See [Hurkens et al.](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf) for details.
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

/// Girard's Paradox, the combination of lemmas [2](lem2) and [3](lem3).
/// See [Hurkens et al.](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf) for details.
pub fn girard() -> ITerm {
    iann(iapp(lem2(), lem3()), void())
}

pub fn reduce(term: ITerm) -> CTerm {
    quote0(term.eval(Context::new(vec![])))
}

pub fn ireduce(term: ITerm) -> Result<ITerm, String> {
    let ty = term.infer_type(Context::new(vec![]))?;
    let reduced = reduce(term);
    match reduced {
        CTerm::Inf(it) => Ok(*it),
        CTerm::Lam(_) => Ok(iann(reduced, quote0(ty)))
    }
}

/// Girard's Paradox, with the lemmas reduced as far as possible.
/// See [Hurkens et al.](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf) for details.
pub fn girard_reduced() -> ITerm {
    iann(iapp(ireduce(lem2()).unwrap(), reduce(lem3())), void())
}

/// For all types X, a function from False to X. Ex falso sequitur quodlibet.
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

/// Make sure a term typechecks, and (if it can be evaluated) that evaluation preserves type.
pub fn validate(name: &str, term: &ITerm, eval_preserve: Option<bool>) {
    println!("{name}");
    // println!("{term}");
    let ctx = Context::new(vec![]);
    let typ = term.infer_type(ctx.clone()).expect("Term should be well-typed");
    if let Some(preserve) = eval_preserve {
        let val = term.clone().eval(ctx.clone());
        let qval = quote0(val);
        qval.check_type(ctx.clone(), typ.clone()).expect("Evaluation should preserve type");
        let typ1 = quote0(typ);
        println!("\t{term} has type {typ1}");
        // println!("Passed type checks!");
        let iqval = match qval {
            CTerm::Inf(iterm) => *iterm,
            CTerm::Lam(_) => iann(qval, typ1),
        };
        println!("Validating {iqval} vs {term} (should be {preserve})");
        assert!((iqval == term.clone()) == preserve);
        println!()
    }
}

#[test]
fn u_reducible() {
    validate("u", &u(), Some(false));
}

#[test]
fn omega_reducible() {
    validate("omega", &omega(), Some(false))
}

#[test]
fn lem2_reducible() {
    validate("lem2", &lem2(), Some(false))
}

fn pset_irreducible() {
    validate("powerset", &sets_of(), Some(true))
}