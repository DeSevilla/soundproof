// #![allow(unused)]

pub mod types;
pub mod translate;

use crate::translate::*;
use crate::lambdapi::ast::*;
use types::*;

pub fn translate_term(term: ITerm) -> SoundTree2 {
    i_type_translate_full(0, vec![], &term, 0).unwrap().1
    // ctypetranslate(0, 0, term, SoundContext(Vec::new()))
}
