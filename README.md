# Casual Serious Language

A serious programming language, but casually.

Going for features that I actually use in practice. Early in development.

Reference [public-docs/syntax.md](/public-docs/syntax.md) for the language syntax by example.

## Next Steps

Features (expected)

- Add mutable variables, making if-without-else useful.
- Allow type hints on variables.
- Literals `true` and `false`.
- `return` keyword as described in syntax.md.
- Allow `let` and `ret` as expressions.
- `never` type to properly type "return"
- Chaining comparison operations

High-effort enhancements

- Compile portions of the code better (don't stop on the first err, allow for error nodes / IPs)
- Find some way to guarantee all error messages are tested.
- Try symbol/string interning. See Rust's `InternerInner` in rustc_span.
- Compile to an actual target. (E.g. via [QBE](https://github.com/garritfra/qbe-rs))
- Severity and suggestions for diagnostics.

Other enhancements

- (Maybe) Panic for runtime errors instead of propagating `Result`.
- Say "Syntax Error" etc in error messages. Maybe set this up as a category on the diagnostic object?
- Codemirror in the web preview, to underline errors.
  - Ctrl-Enter to run code.
- `TODO-errormsg` and `TODO-perf` in the code.

Bug fixes

- Change `/` to true division, add `//` for floor division

Code cleanups

- Proper lexer errors instead of `InvalidToken` storing a static string.
- Determine types _before_ adding to MIR. Big mess checking for types inside build_mir.
- Figure how to type consequent parsing better. (macros?)
- Generalize scanning for comma-separated things (function params, function args).

## Guiding Principles

The WIP is so far from these actually being relevant:

1. Easy to look up the definition of any function. If you see `.repr()` in the source, it's clear exactly what lines implement it. So there will be no VTable generation, and no class dynamic dispatch.
2. No hidden performance pitfalls. Don't implement a convenience feature (`.map()`) if it's much slower than the corresponding longhand (`for ()`).
3. Memory management is automatic, but in accordance with avoiding performance pitfalls, the programmer may still need to think about it a little (but less than they think about borrow checking in Rust).
4. Expressions read from top-to-bottom, left-to-right. No significant distinction between statements and expressions.

## Wacky Ideas (not yet tried)

1. Pointer graph must be a DAG (to make the GC easier, or just for fun). However, cycles are useful (such as passing messages to a main controller), so certain structs can choose to be "owned", preventing more than one reference to them from existing. This allows them to reference their owner via a `WeakRef` that is always valid. If the owner is deleted, the object is deleted.

## Development

We use `clippy` for linting. Run `cargo clippy` or set up your editor to use clippy. The repository includes a `.vscode` that configures rust-analyzer to use clippy.

To run all tests and take snapshots of what changed:

```sh
./scripts/update-snapshots.sh
```

To run on a particular file

```sh
cargo run filename.csl
```

Policy: do not add a `// TODO` comment unless it is one of the following types:

- `TODO-errormsg`: A comment on a test with a worse-than-expected error message
- `TODO-perf`: A comment explaining an idea for how to improve performance
- `TODO-test`: A gap in the testing.

## Build for Web

Install only needs to be run once:

```sh
cd site
npm install
```

To build for web,

```sh
./scripts/build-site.sh
```

Then serve the files in public-deploy. I like doing

```sh
./scripts/build-site.sh && npx http-server public-deploy/ -c-1
```
