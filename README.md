# Casual Serious Language

A serious programming language, but casually.

Going for features that I actually use in practice. Early in development.

Reference [public-docs/syntax.md](/public-docs/syntax.md) for the language syntax by example.

## Next Steps

Features (expected)

- Allow `let mut` variables to be uninitialized
  - This supports some if-else assignments, and (in the future) `let` expressions that may not execute.
- Allow type hints on variables.
- Literals `true` and `false`.
- `return` keyword as described in syntax.md.
- Allow `let` as an expression.
  - Returns `bool` saying if the pattern match binding was successful.
  - `let x = 5;` can desugar into `let x; x=5;`, but for `if (let x = 5 && let y = 6) {} else {}`, the `else` should not have access to `x` and `y`.
  - Note `if (let Some(x) = x1 || let Some(y) = y1) {} else {}` can bind neither `x` nor `y`.
    - Rust calls this E408: variable not bound in all patterns. Would need to replace with `Some(_)` or `Some(..)` on the LHS.
  - Regular assignment `a = 5` could be an expression too, but it shouldn't return a bool, to avoid confusion in `if (a = 5) { }`.
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
- `TODO-errormsg` and `TODO-perf` in the code.
- Reserved words to disallow some names like `union` or `yeet`.
- Pass down a `used` boolean that avoids bloating the IR with a bunch of unit literals.
  - Or maybe instead an `enum Usage { Discarded, Used(ip), Unknown }` to also avoid a bunch of `Use()`
  - See branch `usage-ret`. This would be much easier with a Typed AST.

Projects

- Codemirror in the web preview, to underline errors.
  - Ctrl-Enter to run code.
- Auto-formatter.
  - Allow last statement to not have a trailing `;`, and insert the `;`.

Bug fixes

- Don't allow variables with the same name as functions. (This counts as shadowing).

Code cleanups

- `#[must_use]` on the `(BP,IP)` pair, not on the functions (which must be used anyways, as a matter of `Result`)
- `FnDefinition` at statement-level
- Proper lexer errors instead of `InvalidToken` storing a static string.
- Determine types _before_ adding to MIR. Big mess checking for types inside build_mir.
- Figure how to type consequent parsing better. (macros?)
- Generalize scanning for comma-separated things (function params, function args).
- Move `assert_panic`.

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

To run the CLI on a particular file

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

## Testing

For running existing tests, just run `cargo test`.

We use [expect_test](https://docs.rs/expect-test/latest/expect_test/#) to make writing tests fun. In most tests, you don't have to write the expected value: just write `expect![["abc"]]`, and the following script will fill in the current results. Git is vital here to tell when results changed.

To run all tests and fill in snapshots of what changed:

```sh
./scripts/update-snapshots.sh
```

One disadvantage of this approach is that it's too easy to forgot to look at the expected value of a test. Remember to check the diff for test changes!

## Mutation Testing

Mutation Testing refers to automatically mutating the code (say, replacing a `-` with a `+`) and verifying that the resulting code fails some test. We use [cargo-mutants](https://mutants.rs/) for mutation testing.

Install `cargo-mutants`:

```sh
cargo install --locked cargo-mutants
```

Test mutants across whole codebase:

```sh
cargo mutants
```

The relevant outputs are in the `mutants.out` directory, mostly `missed.txt`.

Some useful flags:

- Use four cores: `-j 4`
- Only try mutating one file: `-f csl-core/src/a30_build_mir.rs`
- Only try mutating what changed in a diff: `--in-diff git.diff`
