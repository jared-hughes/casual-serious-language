# Syntax

## Expressions

Numbers and stuff

- Integer literals: `12`, `1e3` (currently these are 64-bit signed)
- Float literals: `1.0`, `1.2e3`, `1.2e-4` (these are 64-bit floats)
- Add/subtract/multiply: `1 + 2`, `1 - 2`, `1 * 2`
- Divide: `1 / 2`
  - It is floor division for integers and truncated division for floats. TODO: pull a Python and make `/` consistently true division, and `//` consistently floor division.
- Unary negative: `-2`

Booleans

- Comparison: `1 < 2`, `3 >= 3`, `3 == 3`, `4 != 2`.
- Boolean NOT: `!(3 < 2)`.
- Logical connectives: `true && true` and `true || false`.
  - These have short-circuiting semantics, so `b` will not run in `a && b` if `a` is false. Likewise, `b` will not run in `a || b` if `a` is true.
- Planned: `true` and `false`

Types

- 64-bit signed integer `i64`
- 64-bit float `f64`
- Boolean `bool`
- Unit `()` (zero bits of information)
- Planned: `never` (unreachable code, Rust calls this `!`)

Misc

- Parentheses for grouping: `(1 + 2) * 3`
- Identifiers: `abc`.

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

## Let bindings

Use `let x = 5` to assign the value `5` to `x`. Bindings are immutable, and only variables are currently supported. Shadowing is not allowed.

```rs
fn sum_square(x: f64, y: f64) -> f64 {
  let xx = x * x;
  let yy = y * y;
  ret xx + yy;
}
```

## If-else

```rs
fn max(x: i64, y: i64) -> i64 {
  ret if (x > y) x
      else y
}
```

## Nested blocks

Curly braces `{ }` introduce blocks, which are expressions. The return value of a block is provided by the last statement, which must start with `ret`. The `ret` keyword provides the return value of the block.

The keyword `ret` is only allowed as the last statement of a block, and it corresponds to (in Rust) leaving off the semicolon.

```rs
fn funny_polynomial(x: f64) -> f64 {
  let y = {
    let a = x * x;
    ret a * a + a + 1.0;
  };
  let z = {
    let a = x - 1;
    ret a * a - a - 1.0;
  };
  ret y + z;
}
```

## Optional Semicolons (not yet implemented)

Currently, semicolons are required after each statement (besides functions, which end in a curly brace `}`).

The plan is to eventually make semicolons optional everywhere (but suggested -- a hypothetical auto-formatter would insert them).

- We don't want a statement looking like `[1,2,3].map(..)` since it could be confused as an array access of the previous line. It should be something like `vec![1,2,3].map(..)` instead.
  - I don't like this though. It's so nice to just write `[1,2,3]`.
- We want to avoid introducing syntax that allows e.g. `-x < 3 || print(x)`, since that would get confused as a subtraction with the previous line.

## `return` keyword (not yet implemented)

The keyword `ret` determines the value of the block without affecting control flow.

A future `return` keyword would affect control flow: it would jump to the end of the containing function and return teh value.

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
    },
    // Block introduced, but "return" returns to the end of the function.
    _ => {
      let y = x - 1;
      return y * y;
    }
  }
  // 'ret' would also work here.
  return m * m;
}
```
