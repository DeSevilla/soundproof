// #![allow(unused)]
use fundsp::hacker32::*;

use clap::*;
use lambdapi::*;
use soundproof::*;
use music::*;
use types::*;
use translate::*;
use ast::{CTerm, ITerm};

mod lambdapi;
mod music;
mod soundproof;

#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum Scaling {
    Linear,
    Size,
    // SizeAligned,  //has conceptual issues & is not supported
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


#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum MelodySelector {
    First,
    Second,
    Third,
    Fourth,
    PureSine
}

impl MelodySelector {
    fn imelody(&self, term: &ITerm, depth: usize) -> Melody {
        match self {
            MelodySelector::First => imelody1(term, depth),
            MelodySelector::Second => imelody2(term, depth),
            MelodySelector::Third => imelody3(term, depth),
            MelodySelector::Fourth => imelody4(term, depth),
            MelodySelector::PureSine => imelody_oneinstr(sine(), term, depth),
        }
    }

    fn cmelody(&self, term: &CTerm, depth: usize) -> Melody {
        match self {
            MelodySelector::First => cmelody2(term, depth),
            MelodySelector::Second => cmelody2(term, depth),
            MelodySelector::Third => cmelody3(term, depth),
            MelodySelector::Fourth => cmelody4(term, depth),
            MelodySelector::PureSine => cmelody_oneinstr(sine(), term, depth),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
enum Structure {
    Term,
    Type,
}

#[derive(Parser, Debug)]
#[command(version, about, long_about=None)]
struct Args {
    #[arg(short, long, default_value="size")]
    scaling: Scaling,
    /// If unset, scales with size of term
    #[arg(short, long)]
    time: Option<f64>,
    #[arg(short, long, default_value="girard-reduced")]
    value: NamedTerm,
    #[arg(short, long, default_value="fourth")]
    melody: MelodySelector,
    #[arg(short('S'), long, default_value="type")]
    structure: Structure,
}

fn main() {
    let args = Args::parse();
    // let term_name = args.value; //.unwrap_or(NamedTerm::GirardReduced);
    let term = args.value.term();
    //validation just makes sure it typechecks; we can't evaluate the paradox
    validate(&format!("{:?}", args.value), &term, false);
    // let mel = args.melody; //.unwrap_or(MelodySelector::Fourth);
    // let translator = args.structure; //.unwrap_or(Structure::Type);
    let tree = match args.structure {
        Structure::Term => iterm_translate(term, 0, args.melody),
        Structure::Type => itype_translate(term, args.melody),
    };
    // println!("{}", tree.show(0));
    let time = args.time.unwrap_or(std::cmp::min(tree.size(), 1200) as f64);
    let mut seq = Sequencer::new(false, 1);
    println!("Sequencing...");
    // let scaling = args.scaling; //.unwrap_or(Scaling::SizeAligned);
    tree.generate_with(&mut seq, 0.0, time, args.scaling);
    // tree.print_sizes(0);
    let mut output = unit::<U0, U1>(Box::new(seq)) >> shape(Adaptive::new(0.1, Tanh(0.5)));
    save(&mut output, time);
    println!("Done");
}
