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
enum Structure {
    /// Assign melodies according to the structure of terms themselves
    Term,
    /// Assign melodies mostly according to the types of terms
    Type,
    /// Run through a series of terms designed to test the different melodies. Overrides --value.
    Test,
}

/// Determines how time is broken down between sequential segments
#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum Scaling {
    /// At each node, just splits time evenly among sequential children. Outer terms get much more time relative to deeper subtrees.
    Linear,
    /// Splits time according to the size of the subtree, but adjusted to give a bit more weight to outer terms.
    Size,
    // SizeAligned,  //goal here was to align to a rhythm but this has conceptual/structural issues
    /// Splits time according to the size of the subtree, no increased weight of outer terms.
    SizeRaw,
}

/// Predefined term from the dependently typed lambda calculus
#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
enum NamedTerm {
    /// The type of types
    Star,
    // SetsOfNat, //these two commented out are not supported by the current preliminary melody functions
    // ExFalso,
    /// The "universe" type used to derive Girard's Paradox, i.e. forall (X :: *) . (P(P(X)) -> X) -> P(P(X));
    U,
    /// Term of type P(P(U)) -> U
    Tau,
    /// Term of type U -> P(P(U))
    Sigma,
    /// A term of type U
    Omega,
    /// Girard's Paradox, full term
    Girard,
    /// Girard's Paradox, reduced as far as possible without nontermination
    GirardReduced
}

impl NamedTerm {
    fn term(&self) -> ITerm {
        match self {
            NamedTerm::Star => ITerm::Star,
            // NamedTerm::SetsOfNat => sets_of_nat(),
            // NamedTerm::ExFalso => exfalso(),
            NamedTerm::U => u(),
            NamedTerm::Tau => tau(),
            NamedTerm::Sigma => sigma(),
            NamedTerm::Omega => omega(),
            NamedTerm::Girard => girard(),
            NamedTerm::GirardReduced => girard_reduced(),
        }
    }
}

/// Determines which set of melodies to use
#[derive(PartialEq, Eq, Clone, Copy, Debug, ValueEnum)]
pub enum MelodySelector {
    A,
    B,
    C,
    D,
    E,
    PureSine
}

impl MelodySelector {
    fn imelody(&self, term: &ITerm, depth: usize) -> Melody {
        match self {
            MelodySelector::A => imelody1(term, depth),
            MelodySelector::B => imelody2(term, depth),
            MelodySelector::C => imelody3(term, depth),
            MelodySelector::D => imelody4(term, depth),
            MelodySelector::E => imelody5(term, depth),
            MelodySelector::PureSine => imelody_oneinstr(sine(), term, depth),
        }
    }

    fn cmelody(&self, term: &CTerm, depth: usize) -> Melody {
        match self {
            MelodySelector::A => cmelody2(term, depth),
            MelodySelector::B => cmelody2(term, depth),
            MelodySelector::C => cmelody3(term, depth),
            MelodySelector::D => cmelody4(term, depth),
            MelodySelector::E => cmelody5(term, depth),
            MelodySelector::PureSine => cmelody_oneinstr(sine(), term, depth),
        }
    }
}


#[derive(Parser, Debug)]
#[command(version, about, long_about=None)]
struct Args {
    /// Determines how time is broken down between sequential segments
    #[arg(short, long, default_value="size")]
    scaling: Scaling,
    /// In seconds. If unset, scales with size of term
    #[arg(short, long)]
    time: Option<f64>,
    /// Predefined term from the dependently typed lambda calculus
    #[arg(short, long, default_value="girard-reduced")]
    value: NamedTerm,
    /// Determines which set of melodies to use
    #[arg(short, long, default_value="fourth")]
    melody: MelodySelector,
    /// How to assign sound-tree structure to a term
    #[arg(short('S'), long, default_value="type")]
    structure: Structure,
}

fn main() {
    let args = Args::parse();
    let term = args.value.term();
    //validation just makes sure it typechecks; we can't evaluate the paradox
    validate(&format!("{:?}", args.value), &term, false);
    let tree = match args.structure {
        Structure::Term => iterm_translate(term, 0, args.melody),
        Structure::Type => itype_translate(term, args.melody),
        Structure::Test => {
            let test_terms = [
            	ITerm::Star,
            	ITerm::Ann(ITerm::Star.into(), ITerm::Star.into()),
            	ITerm::Pi(ITerm::Star.into(), ITerm::Star.into()),
            	ITerm::Bound(0),
            	ITerm::Free(ast::Name::Local(0)),
            	ITerm::App(Box::new(ITerm::Star), ITerm::Star.into()),
            	ITerm::Zero,
            	ITerm::Fin(ITerm::Zero.into()),
            ];
            let mut sounds = Vec::new();
            let depth = 2;
            for term in test_terms {
                let onemel = SoundTree::sound(args.melody.imelody(&term, depth));
                sounds.push(onemel.clone());
                sounds.push(onemel.clone());
                sounds.push(onemel.clone());
                sounds.push(onemel)
            }
            sounds.push(SoundTree::sound(args.melody.cmelody(&CTerm::Lam(Box::new(ITerm::Star.into())), depth)));
            sounds.push(SoundTree::sound(args.melody.cmelody(&CTerm::Lam(Box::new(ITerm::Star.into())), depth)));
            sounds.push(SoundTree::sound(args.melody.cmelody(&CTerm::Lam(Box::new(ITerm::Star.into())), depth)));
            sounds.push(SoundTree::sound(args.melody.cmelody(&CTerm::Lam(Box::new(ITerm::Star.into())), depth)));
            SoundTree::seq(&sounds)
        }
    };
    let time = args.time.unwrap_or(std::cmp::min(tree.size(), 1200) as f64);
    let mut seq = Sequencer::new(false, 1);
    println!("Sequencing...");
    tree.generate_with(&mut seq, 0.0, time, args.scaling);
    let mut output = unit::<U0, U1>(Box::new(seq)) >> shape(Adaptive::new(0.1, Tanh(0.5)));
    save(&mut output, time);
    println!("Done");
}
