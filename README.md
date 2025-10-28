Soundproof transforms formalized mathematical proofs (or propositions) into music, with a particular focus on Girard's Paradox. The audio synthesis is built with
[FunDSP](https://github.com/SamiPerttu/fundsp). The mathematical proofs are constructed in a
custom Rust implementation of [LambdaPi](https://www.andres-loeh.de/LambdaPi/) (originally in Haskell);
[Ilya Klyuchnikov's version](https://github.com/ilya-klyuchnikov/lambdapi) was a particular model.
Artistic influences include Iannis Xenakis, Catherine Hennix, Henry Flynt, Drexciya, Perturbator, Sun Ra, 
Karlheinz Stockhausen, and Odz Manouk.

The canonical output can be found on Bandcamp [here](https://isdra.bandcamp.com/album/girards-paradox).

I gave a demo of this project, and a live performance of its output, at [the FARM workshop at ICFP/SPLASH 2025](https://2025.splashcon.org/track/splash-2025-farm). The paper can be found [in the FARM proceedings here](https://dl.acm.org/doi/10.1145/3759162.3759644), and a recording of the talk is available [on the ACM SIGPLAN YouTube channel here](https://www.youtube.com/live/F_7S90vFEsE?t=2015s). Video of the live performance should be available eventually.

The [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) is a formal system for computability,
built around the concept of functions. Adding type systems to lambda calculi allows them to [represent logical
connections](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence) by having propositions as types
and proofs as elements of those types. For example, a function from type A to type B takes a proof of proposition A
and produces a proof of proposition B, which corresponds to logical implication. 
[Dependent types](https://en.wikipedia.org/wiki/Dependent_type) can represent essentially all mathematical propositions,
and thus dependently-typed lambda calculi are used as the basis for many theorem provers, including 
[Coq](https://coq.inria.fr/)/[Rocq](https://rocq-prover.org/about#history), [Agda](https://github.com/agda/agda),
and [Lean](https://lean-lang.org/).

This project's DTLC is based on [LambdaPi](https://www.andres-loeh.de/LambdaPi/), a very simple version
of the DTLC whose first priority is ease of implementation.
Notably, LambdaPi forgoes the universe hierarchy of usual dependent type theories for simplicity,
so the type of Type is Type; this is essentially similar to the naive set theory idea 
of the "set of all sets", and leads to [Girard's Paradox](https://en.wikipedia.org/wiki/System_U).
I chose LambdaPi partly for its simplicity but also because this paradox is the primary 
term I'm focusing on representing musically at the moment.
The choice of a paradox is in part due to [Henry Flynt's idea of "concept art"](https://henryflynt.org/aesthetics/conart.html), 
which would incorporate mathematics but reject the idea of "discovering" truths in favor of constructing
beautiful concepts.
I'm using the simplified version of the paradox due to [Hurkens](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf).

Proof terms and types are structured naturally as trees, so we give 
each sort of term/type a little fragment of melody. Then, for a particular term/type we wish to 
translate, we give it a duration. We play the melody for the root of the tree over the whole
duration, and then along with the root's melody we split up that duration and give it to the subtrees
to play their translations in sequential order (potentially increased in pitch, etc. for distinction).
The canonical output follows the call tree of the typechecker,
which includes all the structure of a term's abstract syntax tree but additionally includes type structure for variables, which is important for lambdas.

Along with the audio, it can generate a visualization of the "sound tree" structure, 
which is saved at `output/visualization.png` and in `output/images/`. With the `--animate` flag, it also
generates animation frames matched to the duration of the music at 30FPS; this can take
quite a long time. Frames are not automatically put together into a video, but the file names are chosen 
to make it easy for FFMPEG to handle this process.

Various desirable features of LambdaPi (equality types, etc.) have been left out for now,
as they're not necessary for the paradox or for the musical side of the problem.
I would like to add a small-step evaluator and incorporate the (unlimited)
evaluation process of the non-normalizing term for Girard's Paradox, but there are some structural and
conceptual obstacles before this can be done.

Soundproof should always be compiled with `--release`, as the synth portions using FunDSP depend highly
on optimizations that are not enabled in debug mode. It will not run in any reasonable time
otherwise.

The live performance mode takes text input and parses it into terms which are then translated and displayed as moving trees along with the tags of the currently-playing subterms. However, the text display requires a few files I haven't incorporated into the repository yet (been too busy to check what the licenses require for redistribution), so it's currently broken except on my machine. If you want to try it before I get around to that, you can edit the font in `soundproof-bevy.rs` and the version of `cosmic-text` in `cargo.toml`.

NOTE: The audio selectors `names-short`, `names-long`, and `mixed` pull from audio files in the `files` folder, 
which have not been included in Git for multiple reasons. The selectors' source in `select.rs` contains the 
relevant file names if you want to put in local replacements.

```
Usage: soundproof.exe [OPTIONS]

Options:
  -l, --live                   Run the live performance mode. If set, all other options have no effect
  -v, --value <VALUE>          Predefined terms of the dependently typed lambda calculus [default: girard] [possible values: star, sets-of, u, tau, sigma, omega, lem0, lem2, lem3, girard]
  -r, --reduce                 When set, normalize the term as far as possible before being presented
  -t, --time <TIME>            In seconds. If unset, scales with size of tree
  -d, --division <DIVISION>    Determines how time is broken down between sequential segments [default: weight] [possible values: even, weight, size]
  -c, --content <CONTENT>      Determines which audio selector to use, determining melodies, rhythm, timbre, and so on [default: full-stratified] [possible values: full-stratified, async-stratified, a, b, c, d, e, f, pure-sine, names-short, names-long, strat-instr, effects, mixed, loop, rhythmized, bare]
  -s, --structure <STRUCTURE>  How to assign sound-tree structure to a term [default: type] [possible values: term, type, test]  
  -f, --filters <FILTERS>      Additional filters added after audio generation [default: clip-lowpass] [possible values: clip-lowpass, quiet, none]
  -D, --draw-only              When set, only generate visualization (potentially including animation frames), not music
  -a, --animate                When set, generate animation frames
  -o, --output <OUTPUT>        Name of the output file [default: output.wav]
  -h, --help                   Print help (see more with '--help')
  -V, --version                Print version
```
