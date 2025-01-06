
# λ

A simple functional programming langauge using the lambda calculus.

It currently features:

1. Typical lambda calculus features (`\x . x`; or `lambda y . λ x. x + y`)
  - See `./bin/lexer.mll` and `./bin/parser.mly` for syntax.
2. Arithmetic (`+*-/`)
3. Boolean `if-then-else` conditions
4. `let-in` syntax
5. A simple [Hindley Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) typechecker and inference-engine

The [Interpreter can be found here.](https://github.com/0xmycf/lambda/blob/main/bin/interpreter.ml)

---

I use Algorithm W by Damas and Milner (1982)[^1] to infer and typecheck
the terms.
The algorithm is translated from Yi and Lee (1998)[^2].

Further plans are to implement structural/row polymorphism[^3].
For a few other links see [this overview by someone on reddit](https://www.reddit.com/r/ProgrammingLanguages/comments/ijij9o/beyond_hindleymilner_but_keeping_principal_types/),
it might be good starting point for a rabbit hole.

---

## Why?

- I wanted to learn how I parser generators such as [Menhir](https://gallium.inria.fr/~fpottier/menhir/)
  work (thats why my mly file is so ugly).
- I wanted to implement [HM](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) 
  for the longest time now.
- Its fun.
- Lots to learn (always).
- Prepare for some other language I want to make for myself.
- Ocaml is fun and I rarely use it.

---
## Bibliography

```bibtex
@inproceedings{damasMilner1982,
  title     = {Principal type-schemes for functional programs},
  author    = {Damas, Luis and Milner, Robin},
  booktitle = {Proceedings of the 9th ACM SIGPLAN-SIGACT symposium on Principles of programming languages},
  pages     = {207--212},
  year      = {1982},
  doi       = {10.1145/582153.582176},
}

@article{LeeYi1998,
  title     = {Proofs about a folklore let-polymorphic type inference algorithm},
  author    = {Lee, Oukseh and Yi, Kwangkeun},
  journal   = {ACM Transactions on Programming Languages and Systems (TOPLAS)},
  volume    = {20},
  number    = {4},
  pages     = {707--723},
  year      = {1998},
  publisher = {ACM New York, NY, USA},
  doi       = {291891.291892},
}

@inproceedings{garrigue2001simple,
  title     = {Simple Type Inference for Structural Polymorphism.},
  author    = {Garrigue, Jacques},
  booktitle = {APLAS},
  pages     = {329--343},
  year      = {2001},
}

```

[^1]: [Principal type-schemes for functional programs](https://dl.acm.org/doi/10.1145/582153.582176)
[^2]: [Proofs about a folklore let-polymorphic type inference algorithm](https://dl.acm.org/doi/abs/10.1145/291891.291892)
[^3]: [Simple Type Inference for Structural Polymorphism](https://caml.inria.fr/pub/papers/garrigue-structural_poly-fool02.pdf)

