# Casual Serious Language

A serious programming language, but casually.

Going for features that I actually use in practice. Early in development.

Reference [docs/SYNTAX.md](/docs/SYNTAX.md) for the language syntax by example.

## Next Steps

- Syntax like negative
- Try symbol/string interning
- Figure how to type consequent parsing better. (macros?)
- Change `/` to true division, add `//` for floor division
- Functions

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
./scripts/update-snapshots.sh
```
