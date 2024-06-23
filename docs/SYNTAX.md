Implemented so far:

- Integer literals: `12`, `1e3` (currently these are 64-bit signed)
- Float literals: `1.0`, `1.2e3`, `1.2e-4` (these are 64-bit floats)
- Add/subtract/multiply: `1 + 2`, `1 - 2`, `1 * 2`
- Divide: `1 / 2`
  - It is floor division for integers and truncated division for floats. TODO: pull a Python and make `/` consistently true division, and `//` consistently floor division.
- Parentheses for grouping: `(1 + 2) * 3`
- Identifiers: `abc` (currently unusable).
