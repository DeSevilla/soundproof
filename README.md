Soundproof is a tool for transforming formalized mathematical proofs into music, written in Rust.
It is currently in a very preliminary stage. The audio synthesis is built with
[FunDSP](https://github.com/SamiPerttu/fundsp). The mathematical proofs are constructed in a
custom Rust implementation of [LambdaPi](https://www.andres-loeh.de/LambdaPi/), a simple
version of the dependently-typed lambda calculus.

Notably, LambdaPi forgoes the universe hierarchy of usual dependent type theories for simplicity, 
so the type of Type is Type; this is essentially similar to the naive set theory idea 
of the "set of all sets", and leads to [Girard's Paradox](https://en.wikipedia.org/wiki/System_U).
In fact, this paradox (in the simplification due to [Hurkens](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf)) 
is the primary term I'm focusing on representing musically at the moment.

Various desirable features of LambdaPi (equality types, a parser, etc.) have been left out for now,
as they're not necessary for my current explorations of the musical side of the problem.
I would also like to add a small-step evaluator and incorporate the (unlimited)
evaluation process of the non-normalizing term for Girard's Paradox, but there are some structural and
conceptual obstacles before this can be done.

Soundproof has some command-line options, but I mostly modify it by directly editing and recompiling the code.
It should always be compiled with `--release`, as the synth portions especially depend highly
on optimizations that are not enabled in debug mode.

Usage: soundproof.exe [OPTIONS]

Options:
  -s, --scaling <SCALING>  [possible values: linear, size, size-aligned, size-raw]
  -t, --time <TIME>
  -v, --value <VALUE>      [possible values: star, u, tau, girard, girard-reduced]
  -h, --help               Print help
  -V, --version            Print version

