#![allow(unused)]
use fundsp::hacker32::*;

// normally 440.0 
pub const BASE_HZ: f32 = 440.0;

pub const A: i8 = 0;
pub const ASHARP: i8 = 1;
pub const BFLAT: i8 = ASHARP;
pub const B: i8 = 2;
pub const C: i8 = 3;
pub const CSHARP: i8 = 4;
pub const DFLAT: i8 = CSHARP;
pub const D: i8 = 5;
pub const DSHARP: i8 = 6;
pub const EFLAT: i8 = DSHARP;
pub const E: i8 = 7;
pub const F: i8 = 8;
pub const FSHARP: i8 = 9;
pub const GFLAT: i8 = FSHARP;
pub const G: i8 = 10;
pub const GSHARP: i8 = 11;
pub const AFLAT: i8 = GSHARP - 12;

pub fn note_hz(num: i8, octave: i8) -> f32 {
    get_hz(num + 12 * octave)
}

pub fn get_hz(num: i8) -> f32 {
    BASE_HZ * 2.0.pow((num - 48) as f32 / 12.0)
}