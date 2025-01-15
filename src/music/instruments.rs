#![allow(unused)]
use fundsp::hacker32::*;
use std::{f32::consts::{E, PI}, fs};

use crate::music::{notes::BASE_HZ, sequences::onepress};

/* various little synth things, only some of which are actually used */

pub fn flanger_default() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    flanger(0.9, 0.005, 0.010, |t| lerp11(0.005, 0.010, sin_hz(0.7, t)))
}


pub fn additive_first(instrument: An<impl AudioNode<Inputs=U1, Outputs=U1>>, incr: f32) -> An<impl AudioNode<Inputs=U2, Outputs=U1>> {
    let i1 = 0.01 * incr;
    let i5 = 0.05 * incr;
    let i10 = 0.1 * incr;
    let i12 = 0.125 * incr;
    let i20 = 0.2 * incr;
    let i25 = 0.25 * incr;
    let i30 = 0.3 * incr;
    let i40 = 0.4 * incr;
    let i50 = 0.5 * incr;
    let i60 = 0.6 * incr;
    let i75 = 0.75 * incr;
    let i80 = 0.8 * incr;
    let i90 = 0.9 * incr;
    (instrument.clone() * adsr_live(i90, i5, 0.25, i1)) & 
    ((mul(2.0) >> instrument.clone()) * adsr_live(i25, i25, 0.1, i20)) &
    ((mul(3.0) >> instrument.clone()) * adsr_live(i1, i75, 0.1, i20)) &
    ((mul(4.0) >> instrument.clone()) * adsr_live(incr, 0.0, 0.0, 0.0)) &
    ((mul(5.0) >> instrument.clone()) * adsr_live(i25, i75, 0.01, 0.0)) &
    ((mul(6.0) >> instrument.clone()) * adsr_live(i10, i10, 0.1, 0.0)) &
    ((mul(7.0) >> instrument.clone()) * adsr_live(i60, i25, 0.01, i20)) &
    ((mul(8.0) >> instrument.clone()) * adsr_live(i50, i1, 0.4, i20)) &
    ((mul(9.0) >> instrument.clone()) * adsr_live(i90, i10, 0.5, 0.0)) &
    ((mul(10.0) >> instrument.clone()) * adsr_live(0.0, i90, 0.05, i10))
}

pub fn all_harmonics(adsrs: &[(f32, f32, f32, f32)]) -> Vec<(f32, f32, f32, f32, f32)> {
    adsrs.iter().enumerate().map(|(i, (a, d, s, r))| (i as f32, *a, *d, *s, *r)).collect()
}

fn additive_array(instrument: An<impl AudioNode<Inputs=U1, Outputs=U1> + 'static>, incr: f32, adsrs: &[(f32, f32, f32, f32, f32)]) -> An<impl AudioNode<Inputs=U2, Outputs=U1>>  {
    let mut net = Net::wrap(Box::new((sink() | sink()) >> constant(0.0)));
    for (h, a, d, s, r) in adsrs.iter() {
        net = net & ((mul(*h) >> instrument.clone()) * adsr_live(*a * incr, *d * incr, *s, *r * incr))
    }
    unit(Box::new(net))
}

pub fn additive_second(instrument: An<impl AudioNode<Inputs=U1, Outputs=U1> + 'static>, incr: f32) -> An<impl AudioNode<Inputs=U2, Outputs=U1>> {
    let adsrs = [
		(0.90, 0.05, 0.25, 0.01), 
		(0.25, 0.25, 0.1, 0.20),
		(0.01, 0.75, 0.1, 0.20),
        (1.0, 0.0, 0.0, 0.0),
		(0.25, 0.75, 0.01, 0.0),
		(0.10, 0.10, 0.1, 0.0),
		(0.60, 0.25, 0.01, 0.20),
		(0.50, 0.01, 0.4, 0.20),
		(0.90, 0.10, 0.05, 0.0),
        (0.0, 0.90, 0.05, 0.10)
    ];
    additive_array(instrument, incr, &all_harmonics(&adsrs))
}

pub fn additive_third(instrument: An<impl AudioNode<Inputs=U1, Outputs=U1> + 'static>, incr: f32) -> An<impl AudioNode<Inputs=U2, Outputs=U1>> {
    let adsrs = [
        // harmonic attack decay sustain release
		(1.0, 0.90, 0.05, 0.5, 0.0),
		(3.0, 0.25, 0.25, 0.5, 0.0),
		(4.0, 0.01, 0.75, 0.0, 0.0),
        (0.5, 1.0, 0.2, 0.0, 0.0),
		(6.0, 0.25, 0.75, 0.0, 0.0),
		(7.0, 0.10, 0.10, 0.0, 0.0),
		(2.5, 0.60, 0.25, 0.0, 0.0),
		(8.0, 0.50, 0.01, 0.0, 0.0),
		(9.0, 0.5, 0.10, 0.0, 0.0),
        (10.0, 0.0, 0.50, 0.0, 0.0)
    ];
    additive_array(instrument, incr, &adsrs)
}

pub fn additive_decaying(instrument: An<impl AudioNode<Inputs=U1, Outputs=U1> + 'static>, incr: f32) -> An<impl AudioNode<Inputs=U2, Outputs=U1>> {
    let adsrs: Vec<(f32, f32, f32, f32, f32)> = (1..=10).map(|h| h as f32).map(|h| (h, 0.05, 1.01 - 0.1 * h, 1.0 / h, 1.0 - 0.1 * h)).collect();
    additive_array(instrument, incr, &adsrs)
}

pub fn additive_increasing(instrument: An<impl AudioNode<Inputs=U1, Outputs=U1> + 'static>, incr: f32) -> An<impl AudioNode<Inputs=U2, Outputs=U1>> {
    let adsrs: Vec<_> = (1..=10).map(|h| h as f32).map(|h| (h, 0.05, 0.1 * h, h * 0.1, 0.1 * h)).collect();
    additive_array(instrument, incr, &adsrs)
}

pub fn attack_ascend(instrument: An<impl AudioNode<Inputs=U1, Outputs=U1> + 'static>, incr: f32) -> An<impl AudioNode<Inputs=U2, Outputs=U1>> {
    let adsrs: Vec<_> = (1..=10).map(|h| h as f32).map(|h| (h, 0.05 * h, 0.1, 0.1, 0.1)).collect();
    additive_array(instrument, incr, &adsrs)
}

pub fn attack_descend(instrument: An<impl AudioNode<Inputs=U1, Outputs=U1> + 'static>, incr: f32) -> An<impl AudioNode<Inputs=U2, Outputs=U1>> {
    let adsrs: Vec<_> = (1..=10).map(|h| h as f32).map(|h| (h, 0.5 - 0.05 * h, 0.1, 0.1, 0.1)).collect();
    additive_array(instrument, incr, &adsrs)
}

pub fn square_additive_one(instrument: An<impl AudioNode<Inputs=U1, Outputs=U1> + 'static>, incr: f32) -> An<impl AudioNode<Inputs=U2, Outputs=U1>>  {
    let adsrs: Vec<_> = (1..=10).map(|h| h as f32).map(|h| (h, h * h * 0.01, 1.0 - h * 0.1, 0.5 - h * 0.05, 0.1)).collect();
    additive_array(instrument, incr, &adsrs)
}

pub fn additive_switchup(instrument: An<impl AudioNode<Inputs=U1, Outputs=U1> + 'static>, incr: f32) -> An<impl AudioNode<Inputs=U2, Outputs=U1>>  {
    let adsrs: Vec<_> = (1..=20).filter(|x| *x == 1 || x % 3 == 0 || x % 5 == 0).map(|h| h as f32).map(|h| (h, 0.5 - h * 0.05, 1.0 - h * 0.1, 0.5 - h * 0.05, 0.1)).collect();
    additive_array(instrument, incr, &adsrs)
}

pub fn additive_another(instrument: An<impl AudioNode<Inputs=U1, Outputs=U1> + 'static>, incr: f32) -> An<impl AudioNode<Inputs=U2, Outputs=U1>>  {
    let adsrs: Vec<_> = (1..=5).map(|h| 2.pow(h) as f32).map(|h| (h, 1.0 - h * 0.07, 1.0 - h * 0.07, 0.0, 0.0)).collect();
    additive_array(instrument, incr, &adsrs)
}



// pub fn myflanger(hz: f32, min: f32, scale: f32) -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
//     pass() & ((pass() | min + scale * 0.5 + sine_hz(hz) * scale * 0.5) >> tap(min, min + scale))
// }

pub fn karplus() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    let instrument = white();
    let instrument = instrument * onepress(0.001, 0.5);
    split() >>
        ((instrument | constant(0.25) | (pass() * 1.2) | constant(0.6)) >> 
        (lowpass_hz(BASE_HZ, 0.5) | multipass::<U3>()) >> fbdf(-4.0)) >> 
        shape_fn(tanh) >> lowpass_hz(BASE_HZ, 1.0)
}

pub fn fm_pi() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    fm_sines(PI * 0.5, 15.0, 35.0, PI.pow(PI))
}

pub fn fm_epi() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    fm_sines(PI * 0.5, E * 5.0, E * 10.0, E.pow(PI))
}

pub fn fm_harmonics() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    fm_sines(2.0, 15.0, 25.0, 30.0)
}

pub fn fm_basic() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    fm_sines(1.0, 10.0, 20.0, 40.0)
}

pub fn fm_sines(mult: f32, amp_min: f32, amp_max: f32, amp_freq: f32) -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    let gap = (amp_max - amp_min) * 0.5;
    let base = amp_min + gap;
    let ratio = 1.0 / BASE_HZ;
    // let ratio = ratio + (ratio / 2.0) * sine_hz(0.25);
    (split() >> ((mul(mult) >> sine()) * (base + sine_hz(amp_freq) * gap) * pass() * ratio)) & pass() >> sine()
}


pub fn violinish() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    let instrument = pass() + 5.0 * ((saw_hz(0.33) * 4.0 + 7.0) >> sine());
    // let instrument = instrument >> saw() >> fir((1.0, 0.25, 0.25,  1.0));
    let instrument = instrument >> saw() >> fir((1.0, 0.25, 0.25,  1.0));
    // let instrument = instrument >> saw() >> split() >> (pass() | pass() * 5.0) >> moog_q(1.0);
    let instrument = split() >> (instrument | (pass() - ((sine_hz(0.33) + 1.25) >> sine()) * 20.0) | constant(5.0));
     //>> shape_fn(tanh);
    // let instrument = instrument >> split() >> !fbd(0.2, -2.0) >> !fbd(0.3, -3.0) >> fbd(0.1, -4.0);
    instrument >> highpass()
}

pub fn invert() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    envelope2(|t, x| 1.0/x)
}

pub fn wobbly_sine() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    (pass() + 20.0 * sine_hz(8.0)) >> sine()
}

pub fn sinesaw() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    (sine() * 3.0) & saw()
}

pub fn pink_sine() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    (pass() * pink() * pink()) >> sine()
}

pub fn sawfir() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    saw() >> fir((0.5, 0.5))
}

pub fn split_reverb() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    split() >> reverb_stereo(30.0, 2.0, 0.5) >> (pass() + pass())
}

pub fn reverb_highpass() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    split::<U3>() >> (pass() + (reverb_stereo(30.0, 2.0, 0.5) >> join::<U2>() >> highpass_hz(150.0, 2.0)))
}

pub fn reverb_lowpass() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    split::<U3>() >> (pass() + (reverb_stereo(30.0, 2.0, 0.5) >> join::<U2>() >> lowpass_hz(300.0, 0.6)))
}

pub fn reverb_distort() -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    split::<U3>() >> (pass() + (reverb_stereo(30.0, 2.0, 0.5) >> join::<U2>())) >> mul(5.0) >> shape_fn(tanh)
}

pub fn fbd(t: f32, db: f32) -> An<impl AudioNode<Inputs=U2, Outputs=U1>> {
    pass() + (pass() >> feedback(delay(t) * db_amp(db))) * 0.333
}

pub fn fbf(t: f32, db: f32) -> An<impl AudioNode<Inputs=U3, Outputs=U1>> {
    feedback((delay(t) | pass() | pass()) >> !lowpass() >> (mul(db_amp(db)) | mul(0.5) | mul(0.5))) >> (pass() | sink() | sink())
}

pub fn fbft(t: f32, db: f32) -> An<impl AudioNode<Inputs=U3, Outputs=U1>> {
    feedback((((pass() | constant(t)) >> tap(1.0/10000.0, 5.0)) | pass() | pass()) >> (!lowpass() * db_amp(db))) >> (pass() | sink() | sink())
}

pub fn fbf_q(t: f32, db: f32, q: f32) -> An<impl AudioNode<Inputs=U2, Outputs=U1>> {
    feedback((delay(t) | pass()) >> (!lowpass_q(q) * db_amp(db))) >> (pass() | sink())
}

/// - Input 0: audio
/// - Input 1: delay
/// - Input 2: filter cutoff
/// - Input 3: filter resonance (q)
/// - Output 0: audio with delayed and filtered feedback
pub fn fbdf(db: f32) -> An<impl AudioNode<Inputs=U4, Outputs=U1>> {
    feedback(delay_filter(db)) >> (pass() | multisink::<U3>())
}

/// - Input 0: audio
/// - Input 1: delay
/// - Input 2: filter cutoff
/// - Input 3: filter resonance (q)
/// - Output 0: delayed and filtered audio
/// - Output 1: delay
/// - Output 2: filter cutoff
/// - Output 3: filter resonance (q)
pub fn delay_filter(db: f32) -> An<impl AudioNode<Inputs=U4, Outputs=U4>> {
    // !(tap(1.0/1000.0, 1.0) | pass() | pass()) >> (!lowpass() * db_amp(db) | pass())
    // !((tap_linear(1.0/1000.0, 5.0) | multipass::<U2>()) >> lowpass()) >> (mul(db_amp(db)) | mul(0.5) | mul(0.5) | mul(0.5))
    !((tap(1.0/10000.0, 5.0) | multipass::<U2>()) >> highpass()) >> (mul(db_amp(db)) | mul(0.5) | mul(0.5) | mul(0.5))
    // !(pass() | split::<U3>() | pass() | pass() >> (tap(1.0/10.0, 1.0/10.0) | pass() | pass()) >> lowpass() * db_amp(db) * 0.33)
}



pub fn major_chord() -> An<impl AudioNode<Inputs=U1, Outputs=U3>> {
    split::<U3>() >> (pass() | mul(5.0 / 4.0) | mul(3.0 / 2.0))
}

pub fn triple(instrument: An<impl AudioNode<Inputs=U1, Outputs=U1>>) -> An<impl AudioNode<Inputs=U3, Outputs=U1>> {
    (instrument.clone() + instrument.clone() + instrument.clone() * 0.5) >> shape_fn(tanh)
}

pub fn five_equivalents(instrument: An<impl AudioNode<Inputs=U1, Outputs=U1>>) -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    split::<U5>() >>
        // (mul(0.5)           | pass()             | mul(2.0)           | mul(4.0)           | mul(8.0)) >>
        stacki::<U5, _, _>(|i| mul(2.pow(i) as f32)) >>
        // (instrument.clone() | instrument.clone() | instrument.clone() | instrument.clone() | instrument.clone()) >>
        stacki::<U5, _, _>(|i| instrument.clone()) >>
        stacki::<U5, _, _>(|i| mul(0.8.pow(i as f32))) >>
        // (mul(0.4)           | pass()             | mul(0.4)           | mul(0.2)           | mul(0.1)) >>
    join::<U5>()
}

// pub fn read_spectrum(filename: &str, instrument: An<impl AudioNode<Inputs=U1, Outputs=U1> + 'static>) -> An<impl AudioNode<Inputs=U0, Outputs=U1>> {
//     println!("Reading {filename}");
//     let file_str = fs::read_to_string(format!("input/{filename}")).expect(&format!("Could not open {filename}"));
//     let lines = file_str.lines();
//     let mut net = Net::wrap(Box::new(constant(0.0)));
//     let mut count = 0;
//     let mut spectrum = Vec::new();
//     for (ii, text) in lines.into_iter().enumerate().skip(1) {
//         let (freq_text, db_text) = text.split_once(|c: char| c.is_whitespace()).expect(&format!("Invalid format at line {ii}: {text}"));
//         let freq: f32 = freq_text.parse().expect(&format!("Invalid frequency at line {ii}: {freq_text}"));
//         let db: f32 = db_text.parse().expect(&format!("Invalid db at line {ii}: {db_text}"));
//         spectrum.push((freq, db));
//     }
//     spectrum.sort_by(|(f, d), (f2, d2)| f32::total_cmp(d2, d));
//     for (freq, db) in spectrum.into_iter().take(200) {
//         println!("{freq} {db}");
//         let node = constant(freq) >> instrument.clone() >> mul(2.0.pow(db * 0.5));
//         count += 1;
//         net = net + node;
//     }
//     net = net >> mul(1.0 / count as f32);
//     unit(Box::new(net))
// }

pub fn sample_nearby(instrument: An<impl AudioNode<Inputs=U1, Outputs=U1>>) -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    split::<U5>() >>
        (mul(0.9)          | mul(0.95)         | pass()             | mul(1.05)         | mul(1.1)) >>
        (instrument.clone() | instrument.clone() | instrument.clone() | instrument.clone() | instrument.clone()) >>
        (mul(0.4)           | mul(0.7)           | pass()             | mul(0.7)           | mul(0.4)) >>
    join::<U5>()
}



