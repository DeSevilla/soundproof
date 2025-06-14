Soundproof transforms formalized mathematical proofs (or propositions) into music.
It is currently in a very preliminary stage and has a limited selection of terms. The audio synthesis is built with
[FunDSP](https://github.com/SamiPerttu/fundsp). The mathematical proofs are constructed in a
custom Rust implementation of [LambdaPi](https://www.andres-loeh.de/LambdaPi/) (originally in Haskell);
[Ilya Klyuchnikov's version](https://github.com/ilya-klyuchnikov/lambdapi) was a particular model.
Artistic influences include Iannis Xenakis, Catherine Hennix, Henry Flynt, Drexciya, Perturbator, Sun Ra, Karlheinz Stockhausen, and Odz Manouk.

Example of the output can be found on SoundCloud [here (type-structured)](https://soundcloud.com/user-619734785/system-output-v13)
and [here (term-structured)](https://soundcloud.com/user-619734785/system-output-v12).

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

Various desirable features of LambdaPi (equality types, a parser, etc.) have been left out for now,
as they're not necessary for the paradox or for the musical side of the problem.
I would like to add a small-step evaluator and incorporate the (unlimited)
evaluation process of the non-normalizing term for Girard's Paradox, but there are some structural and
conceptual obstacles before this can be done. I'm also experimenting with the musical side of things,
which I'd like to structure in a more interesting way.

Soundproof should always be compiled with `--release`, as the synth portions using FunDSP depend highly
on optimizations that are not enabled in debug mode. It will not run in any reasonable time
otherwise.

```
Usage: soundproof.exe [OPTIONS]

Options:
  -s, --scaling <SCALING>
          Determines how time is broken down between sequential segments

          [default: weight]

          Possible values:
          - linear: At each node, just splits time evenly among sequential children. Outer terms get much more time relative to deeper subtrees
          - weight: Splits time according to the size of the subtree, but adjusted to give a bit more weight to outer terms
          - size:   Splits time according to the size of the subtree in terms of pure number of nodes, no increased weight of outer terms

  -t, --time <TIME>
          In seconds. If unset, scales with size of tree

  -v, --value <VALUE>
          Predefined terms of the dependently typed lambda calculus

          [default: girard-reduced]

          Possible values:
          - star:           The type of types
          - u:              The "universe" type U used to derive Girard's Paradox: `forall (X :: *) . (P(P(X)) -> X) -> P(P(X))`
          - tau:            A term of type P(P(U)) -> U
          - sigma:          A term of type U -> P(P(U))
          - omega:          A term of type U
          - girard:         Girard's Paradox, full term according to the Hurkens approach
          - girard-reduced: Girard's Paradox, reduced as far as possible without nontermination

  -m, --melody <MELODY>
          Determines which set of melodies to use

          [default: e]

          Possible values:
          - a:         First, highly arbitary melody suite. See [imelody1] and [cmelody2] (there is no cmelody1)
          - b:         Melodies based on B, C, E, and G with arbitrarily-chosen instruments. See [imelody2] and [cmelody2]
          - c:         More intentionally-chosen melodies with still-arbitrary instruments. See [imelody3] and [cmelody3]
          - d:         Same melodies as C but with cleaner-sounding instruments. See [imelody4] and [cmelody4]
          - e:         Melody suite with some hints of dissonance. See [imelody5] and [cmelody5]
          - pure-sine: Same melodies as B and C but exclusively as sines. See [imelody_oneinstr] and [cmelody_oneinstr]

  -S, --structure <STRUCTURE>
          How to assign sound-tree structure to a term

          [default: type]

          Possible values:
          - term: Assign melodies according to the structure of terms themselves
          - type: Assign melodies mostly according to the types of terms
          - test: Run through a series of terms designed to test the different melodies. Overrides --value

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version
```
