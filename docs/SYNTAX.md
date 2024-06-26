# Syntax

## Expressions

- Integer literals: `12`, `1e3` (currently these are 64-bit signed)
- Float literals: `1.0`, `1.2e3`, `1.2e-4` (these are 64-bit floats)
- Add/subtract/multiply: `1 + 2`, `1 - 2`, `1 * 2`
- Divide: `1 / 2`
  - It is floor division for integers and truncated division for floats. TODO: pull a Python and make `/` consistently true division, and `//` consistently floor division.
- Unary negative: `-2`
- Parentheses for grouping: `(1 + 2) * 3`
- Identifiers: `abc` (currently unusable).

## Functions

Currently, you must have a main function to run anything, and the process prints the return value of `main()`.

```rs
fn main() -> i64 {
  ret add(3, 5);
}
fn add(x: i64, y: i64) -> i64 {
  ret x + y;
}
```

## Semicolons

Currently, semicolons are required after each statement (besides functions).

The plan is to eventually make semicolons optional everywhere (but suggested -- a hypothetical auto-formatter would insert them).

- We don't want a statement looking like `[1,2,3].map(..)` since it could be confused as an array access of the previous line. It should be something like `vec![1,2,3].map(..)` instead.
  - I don't like this though. It's so nice to just write `[1,2,3]`.
- We want to avoid introducing syntax that allows e.g. `-x < 3 || print(x)`, since that would get confused as a subtraction with the previous line.

## Blocks (not yet implemented)

Two return keywords:

- `return` exits the function and returns the value
- `ret` determines the value of the block without affecting control flow.
  - If `ret` is the last statement of the block, it corresponds to (in Rust) leaving off the semicolon.
  - `ret` is only allowed as the last statement of the block for now.

For example,

```rs
fn something(x: i64) {
  // No block introduced (despite the curly braces??)
  let m = match x {
    // No block introduced, so no need for `ret`
    0 => 5,
    // No block introduced (just parentheses), so no need for `ret`
    1 | 2 => (
      (x + 2) * x + 1
    ),
    // Curly braces here introduce a block.
    3 => {
      let y = x * x;
      ret (y + 2) * y + 1;
    }
  }
  // 'ret' would also work here.
  return m * m;
}
```
