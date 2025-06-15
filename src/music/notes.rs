#![allow(unused)]
use fundsp::hacker32::*;

/// Frequency of A5 (A above middle C) in Hz. 440 Hz is standard concert pitch.
pub const BASE_HZ: f32 = 440.0;

// we should have more options for the harmonic system than just 12TET, but not currently worth the effort
// will require a mild refactor on the Melody struct to remove the lock-in
pub const A: i32 = 0;
pub const ASHARP: i32 = 1;
pub const BFLAT: i32 = ASHARP;
pub const B: i32 = 2;
pub const C: i32 = 3;
pub const CSHARP: i32 = 4;
pub const DFLAT: i32 = CSHARP;
pub const D: i32 = 5;
pub const DSHARP: i32 = 6;
pub const EFLAT: i32 = DSHARP;
pub const E: i32 = 7;
pub const F: i32 = 8;
pub const FSHARP: i32 = 9;
pub const GFLAT: i32 = FSHARP;
pub const G: i32 = 10;
pub const GSHARP: i32 = 11;
pub const AFLAT: i32 = GSHARP - 12;

/// Gets the frequency of a note from 0 to 11 in a specified octave in Hz.
pub fn note_hz(num: i32, octave: i32) -> f32 {
    // we might adjust this to better fit standard notations but it works fine for now.
    get_hz(num + 12 * octave)
}

/// Gets the frequency in Hz of a note `num` semitones away from A5.
pub fn get_hz(num: i32) -> f32 {
    BASE_HZ * 2.0.pow((num - 48) as f32 / 12.0)
}