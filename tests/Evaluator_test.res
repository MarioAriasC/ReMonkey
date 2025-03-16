open Asserts
open Test
open Evaluator

let testEval = (input: string) => {
  let lexer = Lexer.Lexer.newLexer(input)
  let parser = Parser.Parser.newParser(lexer)
  let program = parser->Parser.Parser.parseProgram
  parser.errors->Array.forEach(simpleFail)
  Eval.eval(program, Environment.newEnvironment())
}

open Objects

let assertObject = (obj: option<mObject>, body: mObject => unit) => {
  switch obj {
  | Some(o) => body(o)
  | _ => simpleFail("obj is None")
  }
}

let assertInt = (obj: option<mObject>, expected: int) => {
  assertObject(obj, o => {
    switch o {
    | Objects.MInteger({value}) => assertEqualsTyped(value, expected)
    | _ => simpleFail(`object is not MInteger, got ${o->typeDesc}, (${o->toString})`)
    }
  })
}

let assertInts = (tests: array<(string, int)>) => {
  Array.forEach(tests, row => {
    let (input, expected) = row
    let evaluated = testEval(input)
    assertInt(evaluated, expected)
  })
}

test("eval integer expression", () => {
  [
    ("5", 5),
    ("10", 10),
    ("-5", -5),
    ("-10", -10),
    ("5 + 5 + 5 + 5 - 10", 10),
    ("2 * 2 * 2 * 2 * 2", 32),
    ("-50 + 100 + -50", 0),
    ("5 * 2 + 10", 20),
    ("5 + 2 * 10", 25),
    ("20 + 2 * -10", 0),
    ("50 / 2 * 2 + 10", 60),
    ("2 * (5 + 10)", 30),
    ("3 * 3 * 3 + 10", 37),
    ("3 * (3 * 3) + 10", 37),
    ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
  ]->assertInts
})
