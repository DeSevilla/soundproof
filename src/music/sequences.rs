#![allow(unused)]
use fundsp::hacker32::*;
use crate::music::notes::note_hz;

/* various envelope things, only some of which are actually used */

pub fn major_chord_notes(notes: Vec<i8>, octave: i8, increment: f32, instrument: An<impl AudioNode<Inputs=U1, Outputs=U1>>) ->
        An<impl AudioNode<Inputs=U0, Outputs=U1>> {
    let notes2 = notes.iter().map(|x| x + 4).collect();
    let notes3 = notes.iter().map(|x| x + 7).collect();
    let env1 = notes_envelope(notes, octave, increment);
    let env2 = notes_envelope(notes2, octave, increment);
    let env3 = notes_envelope(notes3, octave, increment);
    (env1 | env2 | env3) >> ((instrument.clone() + instrument.clone() + instrument.clone()) * 0.5) >> shape_fn(tanh)
}

pub fn double_envelope(notes: Vec<i8>, octave: i8, increment: f32) -> An<impl AudioNode<Inputs=U0, Outputs=U1>> {
    let (v1, v2) = notes.split_at(notes.len() / 2);
    println!("{v1:?} {v2:?}");
    (notes_envelope(v1.to_vec(), octave, increment) | notes_envelope(v2.to_vec(), octave, increment)) >> join()
    // ((e1 | e2) >> join(), t1.max(t2))
}

pub fn tremolo(freq: f32, ratio: f32, main: f32, alt: f32) -> An<impl AudioNode<Inputs=U0, Outputs=U1>> {
    envelope(move |t| {
        if t * freq * ratio % ratio < 1.0 {
            main
        }
        else {
            alt
        }
    })
}

pub fn tremolo_smooth(freq: f32, ratio: f32, main: f32, alt: f32, smooth: f32) -> An<impl AudioNode<Inputs=U0, Outputs=U1>> {
    envelope(move |t| {
        let value = t * freq * ratio % ratio;
        let gap = main - alt;
        if value < smooth {
           alt + value * gap / smooth 
        }
        else if value < 1.0 {
            main
        }
        else if value < 1.0 + smooth {
            main - (value - 1.0) * gap / smooth
        }
        else {
            alt
        }
    })
}

pub fn onepress(length: f32, time: f32) -> An<impl AudioNode<Inputs=U0, Outputs=U1>> {
    envelope(move |t| {
        if t % time < length {
            1.0
        }
        else {
            0.0
        }
    })
}

pub fn octarp(length: f32, time: f32) -> An<impl AudioNode<Inputs=U0, Outputs=U1>> {
    let onestep = time / 4.0;
    envelope(move |t| {
        if t % onestep < length {
            let cur = t % time;
            if cur < onestep {
                1.0
            }
            else if cur < 2.0 * onestep {
                5.0 * 0.25
            }
            else if cur < 3.0 * onestep {
                3.0 * 0.5
            }
            else {
                4.0 / 3.0
            }
        }
        else {
            1.0
        }
    })
}

pub fn notes_envelope(notes: Vec<i8>, octave: i8, increment: f32) -> An<impl AudioNode<Inputs=U0, Outputs=U1>> {
    let invinc = 1.0 / increment;
    envelope(move |t| {
        let i = (t * invinc).floor() as usize % notes.len();
        note_hz(notes[i], octave)
    })
}

pub fn delay_envelope(notes: Vec<i8>, octave: i8, increment: f32) -> An<impl AudioNode<Inputs=U0, Outputs=U1>> {
    let invinc = 1.0 / increment;
    envelope(move |t| {
        let i = (t * invinc).floor() as usize % notes.len();
        1.0 / note_hz(notes[i], octave)
    })
}

fn adsr_equivalents(instrument: An<impl AudioNode<Inputs=U1, Outputs=U1>>, incr: f32) -> An<impl AudioNode<Inputs=U1, Outputs=U1>> {
    let afac = 0.1;
    let dfac = 2.0;
    let sfac = 0.2;
    let rfac = 0.5;
    split::<U6>() >> (
        (mul(0.25) >> (instrument.clone() * (onepress(incr / 8.0, incr) >> adsr_live(0.1 * afac, 0.2 * dfac, 0.8 * sfac, 0.3 * rfac))) >> mul(0.4)) |
        (mul(0.5) >> (instrument.clone() * (onepress(incr / 6.0, incr) >> adsr_live(0.05 * afac, 0.6 * dfac, 0.2 * sfac, 0.2 * rfac))) >> mul(0.6)) |
        (pass() >> (instrument.clone() * (onepress(incr / 4.0, incr) >> adsr_live(0.02 * afac, 0.1 * dfac, 0.4 * sfac, 0.2 * rfac))) >> pass()) |
        (mul(2.0) >> (instrument.clone() * (onepress(incr / 3.0, incr) >> adsr_live(0.01 * afac, 0.1 * dfac, 0.4 * sfac, 0.3 * rfac))) >> mul(0.6)) |
        (mul(4.0) >> (instrument.clone() * (onepress(incr / 2.0, incr) >> adsr_live(0.0 * afac, 0.07 * dfac, 0.3 * sfac, 0.3 * rfac))) >> mul(0.4)) |
        (mul(8.0) >> (instrument.clone() * (onepress(incr / 1.5, incr) >> adsr_live(0.0 * afac, 0.05 * dfac, 0.2 * sfac, 0.3 * rfac))) >> mul(0.3))
    ) >> join::<U6>()
}




// pub fn adsr_envelope(a: f32, d: f32, s: f32, r: f32, increment: f32) -> An<impl AudioNode<Inputs=U0, Outputs=U1>> {
//     envelope(move |t| {
//         if t < a {
//             t / a
//         }
//         else if t < a + d {
//             1.0 - (t - a) / d
//         }
//         else if t < increment - r {
//             s
//         }
//         else {
//             s + (t - increment + r) / r
//         }
//     })
// }