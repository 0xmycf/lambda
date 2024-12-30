
# λ

A simple functional programming langauge using the lambda calculus.

It currently features:

1. Typical lambda calculus features (`\x . x`; or `lambda y . λ x. x + y`)
  - See `./bin/lexer.mll` and `./bin/parser.mly` for syntax.
2. Arithmetic (`+*-/`)
3. Boolean `if-then-else` conditions
4. `let-in` syntax
5. A simple [Hindley Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) typechecker

It does not actually do anything though.
Just parsing and typechecking.

The interpreter shouldn't be complicated though, 
  so thats what I'll implement next.

---

The typechecker is heavily "inspired"[^1] by the blog post of [stimsina.com](https://blog.stimsina.com/post/implementing-a-hindley-milner-type-system-part-2):

- [Part 1](https://blog.stimsina.com/post/implementing-a-hindley-milner-type-system-part-1)
- [Part 2](https://blog.stimsina.com/post/implementing-a-hindley-milner-type-system-part-2)

[^1]: I started to blindly copy half way through for different reasons. Ill rewrite it and organize it myself sometime.

## Why?

- I wanted to learn how I parser generators such as [Menhir](https://gallium.inria.fr/~fpottier/menhir/)
  work (thats why my mly file is so ugly).
- I wanted to implement [HM](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) 
  for the longest time now.
- Its fun.
- Lots to learn.
- Prepare for some other language I want to make for myself.
- Ocaml is fun and I rarely use it.

