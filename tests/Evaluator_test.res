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
    | _ => simpleFail(`object is not MInteger, got ${o->typeDesc}, (${o->inspect})`)
    }
  })
}

let assertBool = (obj: option<mObject>, expected: bool) => {
  assertObject(obj, o => {
    switch o {
    | Objects.MBoolean({value}) => assertEqualsTyped(value, expected)
    | _ => simpleFail(`object is not MBoolean, got ${o->typeDesc}, (${o->toString})`)
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

let assertBools = (tests: array<(string, bool)>) => {
  Array.forEach(tests, row => {
    let (input, expected) = row
    let evaluated = testEval(input)
    assertBool(evaluated, expected)
  })
}

let assertError = (obj: option<mObject>, expected: string) => {
  assertObject(obj, o => {
    switch o {
    | Objects.MError({message}) => assertEqualsTyped(message, expected)
    | _ => simpleFail(`object is not a MError, got ${o->typeDesc}`)
    }
  })
}

let assertString = (obj: option<mObject>, expected: string) => {
  assertObject(obj, o => {
    switch o {
    | Objects.MString({value}) => assertEqualsTyped(value, expected)
    | _ => simpleFail(`object is not a MString, got ${o->typeDesc}`)
    }
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

test("eval boolean expression", () => {
  [
    ("true", true),
    ("false", false),
    ("1 < 2", true),
    ("1 > 2", false),
    ("1 < 1", false),
    ("1 > 1", false),
    ("1 == 1", true),
    ("1 != 1", false),
    ("1 == 2", false),
    ("1 != 2", true),
    ("true == true", true),
    ("false == false", true),
    ("true == false", false),
    ("true != false", true),
    ("false != true", true),
    ("(1 < 2) == true", true),
    ("(1 < 2) == false", false),
    ("(1 > 2) == true", false),
    ("(1 > 2) == false", true),
  ]->assertBools
})

test("bang operator", () => {
  [
    ("!true", false),
    ("!false", true),
    ("!5", false),
    ("!!true", true),
    ("!!false", false),
    ("!!5", true),
  ]->assertBools
})

test("if else expression", () => {
  [
    ("if (true) { 10 }", Some(10)),
    ("if (false) { 10 }", None),
    ("if (1) { 10 }", Some(10)),
    ("if (1 < 2) { 10 }", Some(10)),
    ("if (1 > 2) { 10 }", None),
    ("if (1 > 2) { 10 } else { 20 }", Some(20)),
    ("if (1 < 2) { 10 } else { 20 }", Some(10)),
  ]->Array.forEach(row => {
    let (input, expected) = row
    let evaluated = testEval(input)
    switch expected {
    | Some(i) => assertInt(evaluated, i)
    | None => assertEqualsTyped(evaluated->Option.getUnsafe, Evaluator.Eval.cNULL)
    }
  })
})

test("return statements", () => {
  [
    ("return 10;", 10),
    ("return 10; 9;", 10),
    ("return 2 * 5; 9;", 10),
    ("9; return 2 * 5; 9;", 10),
    (
      `if (10 > 1) {
                            if (10 > 1) {
                              return 10;
                            }
                          
                            return 1;
                          }`,
      10,
    ),
    (
      `let f = fn(x) {
                            return x;
                            x + 10;
                          };
                          f(10);`,
      10,
    ),
    (
      `let f = fn(x) {
                             let result = x + 10;
                             return result;
                             return 10;
                          };
                          f(10);`,
      20,
    ),
  ]->assertInts
})

test("recursive fibonacci", () => {
  let input = `let fibonacci = fn(x) {
    if (x == 0) {
        return 0;
    } else {
        if (x == 1) {
            return 1;
        } else {
            fibonacci(x - 1) + fibonacci(x - 2);
        }
    }
};
    fibonacci(15);`

  let evaluated = testEval(input)
  assertInt(evaluated, 610)
})

test("error handling", () => {
  [
    ("5 + true", "type mismatch: MInteger + MBoolean"),
    ("5 + true; 5;", "type mismatch: MInteger + MBoolean"),
    ("-true", "unknown operator: -MBoolean"),
    ("true + false;", "unknown operator: MBoolean + MBoolean"),
    ("true + false + true + false;", "unknown operator: MBoolean + MBoolean"),
    ("5; true + false; 5", "unknown operator: MBoolean + MBoolean"),
    ("if (10 > 1) { true + false; }", "unknown operator: MBoolean + MBoolean"),
    (
      `
                if (10 > 1) {
                  if (10 > 1) {
                    return true + false;
                  }
                
                  return 1;
                }`,
      "unknown operator: MBoolean + MBoolean",
    ),
    ("foobar", "identifier not found: foobar"),
    (`"Hello" - "World"`, "unknown operator: MString - MString"),
    (`{"name": "Monkey"}[fn(x) {x}];`, "unusable as a hash key: MFunction"),
  ]->Array.forEach(row => {
    let (input, expected) = row
    let evaluated = testEval(input)
    assertError(evaluated, expected)
  })
})

test("let statement", () => {
  [
    ("let a = 5; a;", 5),
    ("let a = 5 * 5; a;", 25),
    ("let a = 5; let b = a; b;", 5),
    ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
  ]->assertInts
})

test("function object", () => {
  let input = "fn(x) {x + 2;};"
  let evaluated = testEval(input)
  evaluated->assertObject(f => {
    switch f {
    | MFunction({parameters, body}) =>
      parameters->Option.forEach(
        params => {
          assertEqualsTyped(params->Array.length, 1)
          assertEqualsTyped(AST.Identifier(params->Array.getUnsafe(0))->AST.Statement.toString, "x")
        },
      )
      assertEqualsTyped(
        AST.BlockStatement(body->Option.getUnsafe)->AST.Statement.toString,
        "(x + 2)",
      )
    | _ => simpleFail(`obj is not an MFunction`)
    }
  })
})

test("function application", () => {
  [
    ("let identity = fn(x) { x; }; identity(5);", 5),
    ("let identity = fn(x) { return x; }; identity(5);", 5),
    ("let double = fn(x) { x * 2; }; double(5);", 10),
    ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
    ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
    ("fn(x) { x; }(5)", 5),
  ]->assertInts
})

test("enclosing environments", () => {
  let input = `let first = 10;
                  let second = 10;
                  let third = 10;
                  
                  let ourFunction = fn(first) {
                    let second = 20;
                  
                    first + second + third;
                  };
                  
                  ourFunction(20) + first + second;`

  input->testEval->assertInt(70)
})

test("string literal", () => {
  let input = `"Hello world!"`
  input->testEval->assertString("Hello world!")
})

test("string concatenation", () => {
  let input = `"Hello" + " " + "world!"`
  input->testEval->assertString("Hello world!")
})

test("builtin functions", () => {
  [
    (`len("")`, I(0)),
    (`len("four")`, I(4)),
    (`len("hello world")`, I(11)),
    (`len(1)`, S(`argument to "len" not supported, got MInteger`)),
    (`len("one", "two")`, S(`wrong number of arguments. got=2, want=1`)),
    ("len([1, 2, 3])", I(3)),
    ("len([])", I(0)),
    ("push([], 1)", A([1])),
    ("push(1,1)", S(`argument to "push" must be ARRAY, got MInteger`)),
    ("first([1, 2, 3])", I(1)),
    ("first([])", N),
    ("first(1)", S(`argument to "first" must be ARRAY, got MInteger`)),
    ("last([1,2,3])", I(3)),
    ("last([])", N),
    ("last(1)", S(`argument to "last" must be ARRAY, got MInteger`)),
    ("rest([1,2,3])", A([2, 3])),
    ("rest([])", N),
  ]->Array.forEach(row => {
    let (input, expected) = row
    let evaluated = testEval(input)
    switch expected {
    | I(value) => assertInt(evaluated, value)
    | S(error) => assertError(evaluated, error)
    | A(arr) =>
      evaluated->assertObject(
        a => {
          switch a {
          | MArray({elements}) =>
            arr->Array.forEachWithIndex(
              (element, i) => {
                assertInt(elements->Array.getUnsafe(i), element)
              },
            )
          | _ => simpleFail(`object is not MArray, got=${a->typeDesc} (${a->toString})`)
          }
        },
      )
    | N => assertEqualsTyped(evaluated, Some(Evaluator.Eval.cNULL))
    | _ => simpleFail("not implemented")
    }
  })
})
