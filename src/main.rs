use ast::ITerm;
// #![allow(unused)]
use fundsp::hacker32::*;

use clap::*;
use lambdapi::*;
use soundproof::*;
use music::*;
use types::*;

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
    #[arg(short, long)]
    scaling: Option<Scaling>,
    #[arg(short, long)]
    time: Option<f64>,
    #[arg(short, long)]
    value: Option<NamedTerm>,
}

fn main() {
    let args = Args::parse();
    let scaling = args.scaling.unwrap_or(Scaling::SizeAligned);
    let term_name = args.value.unwrap_or(NamedTerm::GirardReduced);
    let term = term_name.term();
    //validation just makes sure it typechecks; we can't evaluate the paradox
    validate(&format!("{:?}", term_name), &term, false);
    // let tree = itranslate(term, 0);
    let tree = translate_term(term.into());
    let time = args.time.unwrap_or(std::cmp::min(tree.size(), 1200) as f64);
    // let tree: SoundTreeExpanding = tree.into();
    let tree = SoundTree2Scaling(tree, scaling);
    let mut seq = Sequencer::new(false, 1);
    println!("Sequencing...");
    tree.sequence(&mut seq, 0.0, time);
    let mut output = unit::<U0, U1>(Box::new(seq)) >> shape(Adaptive::new(0.1, Tanh(0.5)));
    save(&mut output, time);
    println!("Done");
}
