open Test

let assertEquals = (a, b) =>
  assertion((x, y) => x == y, a, b, ~message=`'${String.make(a)}' == '${String.make(b)}'`)

let assertEqualsTyped: 't. ('t, 't) => unit = (a, b) => assertEquals(a, b)

type testParam =
  | I(int)
  | B(bool)
  | S(string)
  | N
  | A(array<int>)

let simpleFail = (m: string) => fail(~message=m, ())
