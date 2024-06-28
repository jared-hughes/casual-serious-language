# Casual Serious Language

A serious programming language, but casually.

Going for features that I actually use in practice. Early in development.

Reference [docs/SYNTAX.md](/docs/SYNTAX.md) for the language syntax by example.

## Next Steps

- Figure how to type consequent parsing better. (macros?)
- Web preview: compile to wasm, and run in-browser.
  - Consider global `std::panic::set_hook` https://doc.rust-lang.org/beta/std/panic/fn.set_hook.html
- Compile portions of the code better (don't stop on the first err, allow for error nodes / IPs)
- `let` and better Symbol table handling (ugly inside `a35_mir` rn.)
- Find some way to guarantee all error messages are tested.
  - Panic for runtime errors instead of propagating `Result`.
- Generalize scanning for comma-separated things (function params, function args).
- Try symbol/string interning
- Change `/` to true division, add `//` for floor division

Maybe sometime: Compile instead of interpret

## Guiding Principles

The WIP is so far from these actually being relevant:

1. Easy to look up the definition of any function. If you see `.repr()` in the source, it's clear exactly what lines implement it. So there will be no VTable generation, and no class inheritance.
2. No hidden performance pitfalls. Don't implement a convenience feature (`.map()`) if it's much slower than the corresponding longhand (`for ()`).
3. Memory management is automatic, but in accordance with avoiding performance pitfalls, the programmer may still need to think about it a little (but less than they think about borrow checking in Rust).
4. Expressions read from top-to-bottom, left-to-right. No significant distinction between statements and expressions.

## Wacky Ideas (not yet tried)

1. Pointer graph must be a DAG (to make the GC easier, or just for fun). However, cycles are useful (such as passing messages to a main controller), so certain structs can choose to be "owned", preventing more than one reference to them from existing. This allows them to reference their owner via a `WeakRef` that is always valid. If the owner is deleted, the object is deleted.

## Development

To run all tests and take snapshots of what changed:

```sh
cd csl-core
./scripts/update-snapshots.sh
```

To run on a particular file

```sh
cd csl-cli
cargo run filename.csl
```

To build for web:

```sh
./build-site.sh
```

Then serve the files in public-deploy. I like doing

```sh
./build-site.sh && python3 -m http.server -d public-deploy/
```
