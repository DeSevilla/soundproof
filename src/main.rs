use ast::ITerm;
// #![allow(unused)]
use fundsp::hacker32::*;

use clap::*;
use lambdapi::*;
use soundproof::*;
use music::*;

mod lambdapi;
mod music;
mod soundproof;

#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum Scaling {
    Linear,
    Size,
    SizeAligned,
    SizeRaw,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
enum NamedTerm {
    Star,
    // SetsOfNat, //these two commented out are not supported by the current preliminary melody functions
    // ExFalso,
    U,
    Tau,
    Girard,
    GirardReduced
}

// impl Default for NamedTerm {
//     fn default() -> Self {
//         Self::GirardReduced
//     }
// }

impl NamedTerm {
    fn term(&self) -> ITerm {
        match self {
            NamedTerm::Star => ITerm::Star,
            // NamedTerm::SetsOfNat => sets_of_nat(),
            // NamedTerm::ExFalso => exfalso(),
            NamedTerm::U => u(),
            NamedTerm::Tau => tau(),
            NamedTerm::Girard => girard(),
            NamedTerm::GirardReduced => girard_reduced(),
        }
    }
}

#[derive(Parser, Debug)]
#[command(version, about, long_about=None)]
struct Args {
    scaling: Option<Scaling>,
    time: Option<f64>,
    term: Option<NamedTerm>,
}

fn main() {
    let args = Args::parse();
    let scaling = args.scaling.unwrap_or(Scaling::SizeAligned);
    let time = args.time.unwrap_or(600.0);
    let term = args.term.unwrap_or(NamedTerm::GirardReduced).term();
    //validation just makes sure it typechecks; we can't evaluate the paradox
    validate(&format!("{:?}", args.term), &term, false);
    let mut seq = Sequencer::new(false, 1);
    println!("Sequencing...");
    let tree = itranslate(term, 0);
    SoundTreeScaling(tree, scaling).sequence(&mut seq, 0.0, time);
    let mut output = unit::<U0, U1>(Box::new(seq)) >> shape(Adaptive::new(0.1, Tanh(0.5)));
    save(&mut output, time);
    println!("Done");
}
