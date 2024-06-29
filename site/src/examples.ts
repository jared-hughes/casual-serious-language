interface Example {
  name: string;
  source: string;
}

const _examples: Example[] = [
  {
    name: "i64 add",
    source: `fn add(x: i64, y: i64) -> i64 {
      ret x + y;
    }

    fn main() -> i64 {
      ret add(1, 2) * 3;
    }`,
  },
  {
    name: "f64 add",
    source: `fn add(x: f64, y: f64) -> f64 {
      ret x + y;
    }

    fn main() -> f64 {
      ret add(1.0, 2.0) * 3.5;
    }`,
  },
];

export const examples: Example[] = _examples.map((ex) => ({
  ...ex,
  source: dedent(ex.source),
}));

function dedent(s: string): string {
  const lines = s.split("\n");
  if (lines.length <= 1) return s;
  let commonWS = leadingWhitespace(lines[1]);
  for (const line of lines.slice(2)) {
    if (/^\s*$/.test(line)) {
      // Fully blank line; skip
      continue;
    }
    const ws = leadingWhitespace(line);
    commonWS = commonPrefix(commonWS, ws);
    if (commonWS.length === 0) return s;
  }
  if (commonWS.length === 0) return s;
  const wsLen = commonWS.length;
  let ret = lines[0];
  for (const line of lines.slice(1)) {
    ret += "\n" + line.slice(wsLen);
  }
  return ret;
}

function leadingWhitespace(line: string): string {
  return line.match(/^\s*/)?.[0] ?? "";
}

function commonPrefix(a: string, b: string) {
  const m = Math.min(a.length, b.length);
  for (let i = 0; i < m; i++) {
    if (a[i] !== b[i]) return a.slice(0, i);
  }
  return a.slice(0, m);
}
