#![allow(unused)]
use fundsp::hacker32::*;

/// Frequency of A5 (A above middle C) in Hz. 440 Hz is standard concert pitch.
pub const BASE_HZ: f32 = 440.0;

// we should have more options for the harmonic system than just 12TET, but not currently worth the effort
// will require a mild refactor on the Melody struct to remove the lock-in
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

/// Gets the frequency of a note from 0 to 11 in a specified octave in Hz.
pub fn note_hz(num: i8, octave: i8) -> f32 {
    // we might adjust this to better fit standard notations but it works fine for now.
    get_hz(num + 12 * octave)
}

/// Gets the frequency in Hz of a note `num` semitones away from A5.
pub fn get_hz(num: i8) -> f32 {
    BASE_HZ * 2.0.pow((num - 48) as f32 / 12.0)
}