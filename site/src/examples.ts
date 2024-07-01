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
  {
    name: "let binding",
    source: `fn sum_square(x: f64, y: f64) -> f64 {
      let xx = x * x;
      let yy = y * y;
      ret xx + yy;
    }

    fn main() -> f64 {
      ret sum_square(3.0, 4.0);
    }`,
  },
  {
    name: "nested block",
    source: `fn funny_polynomial(x: f64) -> f64 {
      let y = {
        let a = x * x;
        ret a * a + a + 1.0;
      };
      let z = {
        let a = x - 1.0;
        ret a * a - a - 1.0;
      };
      ret y + z;
    }
    fn main() -> f64 {
      ret funny_polynomial(2.0);
    }`,
  },
  {
    name: "fibonacci (slow)",
    source: `fn fib(n: i64) -> i64 {
      ret if (n == 0) 0
          else if (n == 1) 1
          else fib(n-1) + fib(n-2);
    }
    fn main() -> i64 {
      ret fib(12);
    }`,
  },
  {
    name: "fibonacci (fast)",
    source: `fn f(x: i64, y: i64, n: i64) -> i64 {
      ret if (n == 0) y
          else f(y+x, x, n-1);
    }
    fn fib(n: i64) -> i64 {
      ret f(1,0,n);
    }
    fn main() -> i64 { ret fib(50); }`,
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
