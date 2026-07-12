## Intro

Soundproof transforms formalized mathematical proofs (or propositions) into music, with the aim of producing a piece based on Girard's Paradox. The audio synthesis is built with
[FunDSP](https://github.com/SamiPerttu/fundsp). The mathematical proofs are constructed in a
custom Rust implementation of [LambdaPi](https://www.andres-loeh.de/LambdaPi/) (originally in Haskell);
[Ilya Klyuchnikov's version](https://github.com/ilya-klyuchnikov/lambdapi) was a particular model.
Artistic influences include Iannis Xenakis, Catherine Hennix, Henry Flynt, Drexciya, Perturbator, Sun Ra, 
Karlheinz Stockhausen, and Odz Manouk.

It has two modes: single-term (`--mode=term`) and step-based (`--mode=step`). The single-term mode presents the tree structure of proof terms across time, while the step-based mode presents their reduction behavior.

## Output

The canonical output for the single-term mode is available for free on [Bandcamp](https://isdra.bandcamp.com/album/girards-paradox). A screen recording of the live version is on [Youtube](https://www.youtube.com/watch?v=Fu4yqLvZQOI).

The canonical output for the step-based mode is available for free on [Bandcamp](https://isdra.bandcamp.com/track/girards-paradox-progression-i-two-loops).

A demo of this project, and a live performance of its output, was presented at [the FARM workshop at ICFP/SPLASH 2025](https://2025.splashcon.org/track/splash-2025-farm). The paper can be found [in the FARM proceedings here](https://dl.acm.org/doi/10.1145/3759162.3759644), and a recording of the talk is available [on the ACM SIGPLAN YouTube channel here](https://www.youtube.com/live/F_7S90vFEsE?t=2015s). Video of the FARM performance should be available eventually.

## How It Works

The [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) is a formal system for computability,
built around the concept of functions. Adding type systems to lambda calculi allows them to [represent logical
connections](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence) by having propositions as types
and proofs as elements of those types. For example, a function from type A to type B takes a proof of proposition A
and produces a proof of proposition B, which corresponds to logical implication. 
[Dependent types](https://en.wikipedia.org/wiki/Dependent_type) can represent essentially all mathematical propositions,
and dependently-typed lambda calculi are used as the basis for many theorem provers, including 
[Coq](https://coq.inria.fr/)/[Rocq](https://rocq-prover.org/about#history), [Agda](https://github.com/agda/agda),
and [Lean](https://lean-lang.org/).

This project's DTLC is based on [LambdaPi](https://www.andres-loeh.de/LambdaPi/), a very simple version
of the DTLC whose first priority is ease of implementation.
Notably, LambdaPi forgoes the universe hierarchy of usual dependent type theories for simplicity,
so the type of Type is Type; this is essentially similar to the naive set theory idea 
of the "set of all sets", and leads to [Girard's Paradox](https://en.wikipedia.org/wiki/System_U).
LambdaPi was chosen partly for its simplicity but also because this paradox is the primary 
term I'm focusing on representing musically at the moment.
The choice of a paradox is in part due to [Henry Flynt's idea of "concept art"](https://henryflynt.org/aesthetics/conart.html), 
which would incorporate mathematics but reject the idea of "discovering" truths in favor of constructing
beautiful concepts.
This uses the simplified version of the paradox due to [Hurkens](https://www.cs.cmu.edu/~kw/scans/hurkens95tlca.pdf).

Proof terms and types are structured naturally as trees. They are either in a "normal" form, or they can
take a step of "reduction" to move towards a normal form. However, the paradoxical term has no normal form.
When "reduced", it grows in an endless cycle.

In the single-term mode, we give 
each sort of term/type a little fragment of melody. Then, for a particular term/type we wish to 
translate, we give it a duration. We play the melody for the root of the tree over the whole
duration, and then along with the root's melody we split up that duration and give it to the subtrees
to play their translations in sequential order (potentially increased in pitch, etc. for distinction).
The canonical output follows the call tree of the typechecker,
which includes all the structure of a term's abstract syntax tree but additionally includes type structure for variables, which is important for lambdas.

In the step-based mode, a single term is a synth tone/texture. Each sort of term/type has a waveform, 
and we translate the tree by dividing up pitch among the subtrees in the same way we divide duration above.
The reduction of the term is then the evolution of these tones over time. Step duration can scale with the
size of the change between one and the next, or can be fixed. The `call-by` and `ann-step` options control
choices in the evaluation order.

Various desirable features of LambdaPi (equality types, etc.) have been left out for now,
as they're not necessary for the paradox or for the musical side of the problem. They may be added eventually.

## Additional Features

<!-- Along with the audio, it can generate a visualization of the "sound tree" structure, 
which is saved at `output/visualization.png` and in `output/images/`. With the `--animate` flag, it also
generates animation frames matched to the duration of the music at 30FPS; this can take
quite a long time. Frames are not automatically put together into a video, but the file names are chosen 
to make it easy for FFMPEG to handle this process. -->

The single-term live performance mode is available on the `bevy` branch, and NOT on this one due to version
issues. It takes text input and parses it into terms which are then translated and displayed as moving trees
along with the tags of the currently-playing subterms. Its display font is a modified version of JetBrains Mono
which displays backslash as lambda.

The step-based live performance mode can take a path to a config file with a list of command-line options.
By default, these will be stepped through sequentially; with the `--midi` option, they are mapped 
to MIDI notes and can be activated by a MIDI controller. The mapping currently supports notes 48-73.

## Notes

* Soundproof should always be compiled with `--release`, as the synth portions using FunDSP depend highly
  on optimizations that are not enabled in debug mode. It will not run in any reasonable time
  otherwise.
<!-- 
* While the single-term live performance version uses a font with ligatures, some are broken (most notably ->), due to [a bug in cosmic-text](https://github.com/pop-os/cosmic-text/issues/378). I solved this for my live performances by making an entire local copy of cosmic-text with a deeply bad modification, which I'm not including in this repository (sorry). -->

* The single-term live performance mode is unavailable on this branch due to dependency issues; switch to branch `bevy` to use it.

* The audio selectors `names-short`, `names-long`, and `mixed` pull from audio files in the `files` folder, 
  which have not been included in Git. The selectors' source in `select.rs` contains the 
  relevant file names if you want to put in local replacements.

## Usage

```
Usage: soundproof [OPTIONS]

Options:
  -m, --mode <MODE>              Whether to run the single-term or step-based translation [default: single] [possible values: single, step, step-variants]
  -l, --live                     Whether to render to file or run it live. Single-term live runs are currently unavailable on this branch
  -v, --value <VALUE>            Predefined terms of the dependently typed lambda calculus [default: sigma] [possible values: star, sets-of, u, tau, sigma, omega, lem0, lem2, lem3, girard]
  -r, --reduce                   When set, normalize the term as far as possible before being presented
  -t, --time <TIME>              In seconds. If unset, scales with size of tree. In step mode, determines time of one frame
  -d, --division <DIVISION>      Determines how time is broken down between sequential segments [default: weight] [possible values: even, weight, size]
  -c, --content <CONTENT>        Determines which audio selector to use, determining melodies, rhythm, timbre, and so on [default: full-stratified] [possible values: full-stratified, async-stratified, a, b, c, d, e, f, pure-sine, names-short, names-long, strat-instr, effects, mixed, loop, rhythmized, sine-rhythm, bare, tone-make]
  -s, --structure <STRUCTURE>    How to assign sound-tree structure to a term [default: type] [possible values: term, type, test]
  -f, --filters <FILTERS>        Additional filters added after audio generation [default: clip-lowpass] [possible values: clip-lowpass, quiet, none]
  -o, --output <OUTPUT>          Name of the output file [default: output]
  -L, --freq-low <FREQ_LOW>      Low end of frequency range in step mode [default: 60]
  -H, --freq-high <FREQ_HIGH>    High end of frequency range in step mode [default: 2500]
  -r, --reverse-freq             Reverse frequency range in step mode
  -S, --step-count <STEP_COUNT>  Maximum number of steps before quitting in step mode
      --step-file <STEP_FILE>    A file from which to load multiple configurations in step mode
  -D, --diff-time                Whether to vary step time with the size of the change between steps
      --call-by <CALL_BY>        Evaluation order of function application in step mode [default: name] [possible values: name, value]
      --ann-step <ANN_STEP>      Evaluation order of annotation dropping in step mode [default: unprincipled] [possible values: neither, type, both, unprincipled]
      --midi                     Whether to take MIDI input for live step mode
  -h, --help                     Print help (see more with '--help')
  -V, --version                  Print version
```
