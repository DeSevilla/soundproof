Soundproof transforms formalized mathematical proofs (or propositions) into music.
It is currently in a very preliminary stage and has a limited selection of terms. The audio synthesis is built with
[FunDSP](https://github.com/SamiPerttu/fundsp). The mathematical proofs are constructed in a
custom Rust implementation of [LambdaPi](https://www.andres-loeh.de/LambdaPi/) (originally in Haskell);
[Ilya Klyuchnikov's version](https://github.com/ilya-klyuchnikov/lambdapi) was a particular model.
Artistic influences include Iannis Xenakis, Catherine Hennix, Henry Flynt, Drexciya, Perturbator, Sun Ra, 
Karlheinz Stockhausen, and Odz Manouk.

<!-- Examples of the output can be found on SoundCloud [here (type-structured)](https://soundcloud.com/user-619734785/system-output-v13)
and [here (term-structured)](https://soundcloud.com/user-619734785/system-output-v12). -->

An example of the output can be found on SoundCloud [here](https://soundcloud.com/user-619734785/girards-paradox-well-foundedness-of-omega).

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

The precise method of generating music is subject to change, but to sum up the term-structured 
version of the translation: proof terms and types are structured naturally as trees, so we give 
each sort of term/type a little fragment of melody. Then, for a particular term/type we wish to 
translate, we give it a duration. We play the melody for the root of the tree over the whole
duration, and then along with the root's melody we split up that duration and give it to the subtrees
to play their translations in sequential order (potentially increased in pitch, etc. for distinction).
The type-structured translation is more complicated, using the types of the terms and having more
variation in the precise interpretation of different sorts of term/type, but follows a similar approach.

Along with the audio, it generates a visualization of the "sound tree" structure, 
which is saved at `output/visualization.png` and in `output/images/`. By default, it also
generates animation frames matched to the duration of the music at 30FPS; this can take
quite a long time, so the `--noanimate` option skips the process. Frames are not
automatically put together into a video; FFMPEG can handle this process.

Various desirable features of LambdaPi (equality types, a parser, etc.) have been left out for now,
as they're not necessary for the paradox or for the musical side of the problem.
I would like to add a small-step evaluator and incorporate the (unlimited)
evaluation process of the non-normalizing term for Girard's Paradox, but there are some structural and
conceptual obstacles before this can be done. I'm also experimenting with the musical side of things.

Soundproof should always be compiled with `--release`, as the synth portions using FunDSP depend highly
on optimizations that are not enabled in debug mode. It will not run in any reasonable time
otherwise.

```
Usage: soundproof.exe [OPTIONS]

Options:
  -d, --division <DIVISION>    Determines how time is broken down between sequential segments [default: weight] [possible values: even, weight, size]
  -t, --time <TIME>            In seconds. If unset, scales with size of tree
  -v, --value <VALUE>          Predefined terms of the dependently typed lambda calculus [default: girard] [possible values: star, sets-of, u, tau, sigma, omega, lem0, lem2, lem3, girard]
  -r, --reduce                 When set, normalize the term as far as possible before being presented
  -D, --draw-only              When set, only generate visualization (including animation frames), not music
  -n, --noanimate              When set, do not generate animation frames
  -m, --melody <MELODY>        Determines which audio selector to use, determining melodies, rhythm, timbre, and so on [default: full-stratifier] [possible values: full-stratified, a, b, c, d, e, f, pure-sine, names-short, names-long, strat-instr, effects, mixed, loop, rhythmized, bare]   
  -s, --structure <STRUCTURE>  How to assign sound-tree structure to a term [default: type] [possible values: term, type, test]
  -f, --filters <FILTERS>      Additional filters added after audio generation [default: clip-lowpass] [possible values: clip-lowpass, quiet, none]
  -h, --help                   Print help (see more with '--help')
  -V, --version                Print version
```
