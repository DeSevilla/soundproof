pub mod types;
pub mod translate;

use types::*;
use translate::*;
use crate::lambdapi::ast::*;
use crate::MelodySelector;

pub fn itype_translate(term: ITerm, mel: MelodySelector) -> SoundTree {
    itype_translate_full(0, vec![], &term, 1, mel).unwrap().1
}
