Soundproof is a tool for transforming formalized mathematical proofs into music, written in Rust.
It is currently in a very preliminary stage. The audio synthesis is built with
[FunDSP](https://github.com/SamiPerttu/fundsp). The mathematical proofs are constructed in a
custom Rust implementation of [LambdaPi](https://www.andres-loeh.de/LambdaPi/), a simple
version of the dependently-typed lambda calculus.
Artistic influences include Iannis Xenakis, Henry Flynt, Catherine Hennix, Drexciya, Perturbator, Sun Ra, and Odz Manouk.

Example of the output can be found on SoundCloud [here (term-structured)](https://soundcloud.com/user-619734785/system-output-v12)
and [here (type-structured)](https://soundcloud.com/user-619734785/system-output-v13).

Notably, LambdaPi forgoes the universe hierarchy of usual dependent type theories for simplicity,
so the type of Type is Type; this is essentially similar to the naive set theory idea 
of the "set of all sets", and leads to [Girard's Paradox](https://en.wikipedia.org/wiki/System_U).
In fact, this paradox (in the simplification due to [Hurkens](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf)) 
is the primary term I'm focusing on representing musically at the moment.
The choice of the paradox is in part due to [Flynt's idea of "concept art"](https://henryflynt.org/aesthetics/conart.html), 
which would incorporate mathematics but reject the idea of "discovering" truths in favor of constructing
beautiful concepts.

Various desirable features of LambdaPi (equality types, a parser, etc.) have been left out for now,
as they're not necessary for my current explorations of the musical side of the problem.
I would also like to add a small-step evaluator and incorporate the (unlimited)
evaluation process of the non-normalizing term for Girard's Paradox, but there are some structural and
conceptual obstacles before this can be done.

Soundproof has some command-line options, but I mostly use it by directly editing and recompiling the code.
It should always be compiled with `--release`, as the synth portions depend highly
on optimizations that are not enabled in debug mode.

```
Usage: cargo run --release -- [OPTIONS]

Options:
  -s, --scaling <SCALING>      [default: linear] [possible values: linear, size, size-raw]
  -t, --time <TIME>            If unset, scales with size of term
  -v, --value <VALUE>          [default: girard-reduced] [possible values: star, u, tau, girard, girard-reduced]
  -m, --melody <MELODY>        [default: fourth] [possible values: first, second, third, fourth, pure-sine]
  -S, --structure <STRUCTURE>  [default: type] [possible values: term, type]
  -h, --help                   Print help
  -V, --version                Print version
```
