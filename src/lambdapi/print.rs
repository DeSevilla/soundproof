use core::fmt;
// use std::{fmt::write, path::Display, str::{Bytes, Chars}};

use crate::lambdapi::ast::*;

fn parens_if(cond: bool, text: String) -> String {
    if cond {
        "(".to_owned() + &text + ")"
    }
    else {
        text
    }
}

const VARCHARS: &[u8] = "xyzabcdefghijklmnopqrstuvw".as_bytes();

fn varname(i: usize) -> String {
    let ct = VARCHARS.len();
    let mut val = i;
    let mut result = "".to_owned();
    while val > ct {
        result.push(VARCHARS[val % ct] as char);
        val = val / ct;
    }
    result.push(VARCHARS[val % ct] as char);
    result
}

fn i_print(p: usize, i: usize, term: ITerm) -> String {
    match term {
        ITerm::Ann(cterm, cterm1) => parens_if(p > 1, c_print(2, i, cterm) + " :: " + &c_print(1, i, cterm1)),
        ITerm::Star => "*".to_owned(),
        ITerm::Pi(cterm, cterm1) => parens_if(p > 0, "forall (".to_owned() + &varname(i + 1) + " : " + &c_print(p, i, cterm) + ") . " + &c_print(p, i + 1, cterm1)),
        ITerm::Bound(k) => varname(i - k - 1),
        ITerm::Free(Name::Global(name)) => name,
        ITerm::Free(n) => format!("{n:?}"),
        ITerm::App(f, x) => parens_if(p > 2, i_print(2, i, *f) + " " + &c_print(3, i, x)),
        ITerm::Nat => "Nat".to_owned(),
        ITerm::Zero => "0".to_owned(),
        ITerm::Succ(cterm) => "Succ".to_owned() + &parens_if(true, c_print(p, i, cterm)),
        ITerm::NatElim(base, motive, ind, n) => "finElim ".to_owned() + 
            &[base, motive, ind, n].into_iter().map(|x| c_print(p, i, x)).reduce(|acc, elem| acc + " " + &elem).unwrap_or("".to_owned()),
        ITerm::Fin(cterm) => "Fin".to_owned() + &parens_if(true, c_print(p, i, cterm)),
        ITerm::FinElim(base, motive, ind, n, f) => "finElim ".to_owned() + 
            &[base, motive, ind, n, f].into_iter().map(|x| c_print(p, i, x)).reduce(|acc, elem| acc + " " + &elem).unwrap_or("".to_owned()),
        ITerm::FZero(cterm) => "FZero<".to_owned() + &c_print(p, i, cterm) + ">",
        ITerm::FSucc(cterm, cterm1) => "FSucc<".to_owned() + &c_print(p, i, cterm) + ">(" + &c_print(p, i, cterm1) + ")",
    }
}


fn c_print(p: usize, i: usize, term: CTerm) -> String {
    match term {
        CTerm::Inf(iterm) => i_print(p, i, *iterm),
        CTerm::Lam(cterm) => "\\".to_owned() + &varname(i) + " -> " + &c_print(p, i+1, *cterm),
    }
}

impl fmt::Display for CTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", c_print(0, 0, self.clone()))
    }
}

impl fmt::Display for ITerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", i_print(0, 0, self.clone()))
    }
}