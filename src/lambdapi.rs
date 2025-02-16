#![allow(unused)]
pub mod ast;
pub mod check;
pub mod eval;
pub mod print;
pub mod term;

use ast::*;
use term::*;
use check::*;
use eval::*;

/* this mostly contains specific terms of LambdaPi */
/* at some point we should add a parser and switch to constructing from that */

pub fn void() -> ITerm {
    ITerm::Fin(ITerm::Zero.into())
}

pub fn sets_of() -> ITerm {
    iann(
        clam(ipi(bnd(0), ITerm::Star)),
        ipi(ITerm::Star, ITerm::Star)
    )
}

pub fn sets_of_nat() -> ITerm {
    iapp(sets_of(), ITerm::Nat)
}

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

pub fn sets_of_u() -> ITerm {
    iapp(sets_of(), u())
}

/// P(P(U)) -> U
pub fn tau() -> ITerm {
    iann(
        clam(clam(clam(clam(iapp(bnd(3),
            clam(iapp(bnd(1), iapp(bnd(2), iapp(iapp(bnd(0), bnd(3)), bnd(2)))))))))), 
        ipi(iapp(sets_of(), iapp(sets_of(), u())), u())
    )
}

/// U -> P(P(U))
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

pub fn d() -> ITerm {
    ipi(
        sets_of_u(),
        ipi(
            iapp(sigma_omega(), bnd(0)),
            iapp(bnd(1), iapp(tau(), sigma_omega()))
        )
    )
}

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

pub fn girard() -> ITerm {
    iann(iapp(lem2(), lem3()), void())
}

pub fn girard_reduced() -> ITerm {
    let vty = quote0(ipi(d(), void()).eval(vec![]));
    let vlem2 = iann(quote0(lem2().eval(vec![])), vty);
    let vlem3 = quote0(lem3().eval(vec![]));
    iann(iapp(vlem2, vlem3), void())
}

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
    let typ = term.infer_type(0, vec![]).expect("Term should be well-typed");
    if eval {
        let val = term.clone().eval(vec![]);
        let qval = quote0(val);
        qval.check_type(0, vec![], typ.clone()).expect("Evaluation should preserve type");
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
