# Syntax

## Expressions

Numbers and stuff

- Integer literals: `12`, `1e3` (currently these are 64-bit signed)
- Float literals: `1.0`, `1.2e3`, `1.2e-4` (these are 64-bit floats)
- Add/subtract/multiply: `1 + 2`, `1 - 2`, `1 * 2`
- True divide: `3.0 / 2.0`
  - `/` always produces the output with no rounding besides the rounding inherent in the bit representation (floating point).
- Floor divide: `3 // 2`
  - Floor division rounds to negative infinity, matching the semantics of Python (the semantics are different than Rust's `div_euclid`)
- Unary negative: `-2`

Booleans

- Comparison: `1 < 2`, `3 >= 3`, `3 == 3`, `4 != 2`.
- Boolean NOT: `!(3 < 2)`.
- Logical connectives: `true && true` and `true || false`.
  - These have short-circuiting semantics, so `b` will not run in `a && b` if `a` is false. Likewise, `b` will not run in `a || b` if `a` is true.

Types

- 64-bit signed integer `i64`
- 64-bit float `f64`
- Boolean `bool`
- Unit `()` (zero bits of information)

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

## Mutable variables and assignments

Use `let mut x = 5` to create a mutable variable `x`. Assign to it with `x = 8`. Function parameters are _not_ mutable.

```rs
fn abs_diff_cubed(x: i64, y: i64) -> f64 {
  let mut d = x - y;
  if (d < 0) {
    d = -d;
  }
  ret d * d * d;
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

## Semicolons

Semicolons are required in most places to avoid parentheses on the next line from merging and creating a function call, among other reasons.

Semicolons can be omitted after a bare statement (i.e. no `ret` or `let`) whose right-hand-side is:

- a function definition (which ends in `}`), or
- a block (which ends in `}`), or
- an if or if-else expression whose final branch is a block.
- a `while` expression whose body is a block.

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
