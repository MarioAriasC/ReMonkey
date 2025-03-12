open Test
open Asserts
open Array

type testParam =
  | I(int)
  | B(bool)
  | S(string)

let simpleFail = (m: string) => fail(~message=m, ())

let createProgram = (input: string) => {
  let lexer = Lexer.Lexer.newLexer(input)
  let parser = Parser.Parser.newParser(lexer)
  let program = parser->Parser.Parser.parseProgram
  let errors = parser.errors
  let errorsLength = errors->Array.length
  if errorsLength > 0 {
    let errorText = String.concatMany("", errors->Array.map(err => err ++ "\n"))
    simpleFail(`parser has ${Int.toString(errorsLength)} errors: ${errorText}`)
  }
  program
}

let assertCountStatements = (i: int, p: AST.program) =>
  assertion(
    (x, y) => x == y,
    i,
    Array.length(p.statements),
    ~message=`program has ${Int.toString(i)} statement(s)`,
  )

let assertStatement = (statement: option<AST.statement>, body: AST.statement => unit) => {
  switch statement {
  | Some(s) => body(s)
  | None => simpleFail("statement is None")
  }
}

let assertIntegerLiteral = (statement: option<AST.statement>, i: int) => {
  statement->assertStatement(s => {
    switch s {
    | AST.IntegerLiteral(literal) => {
        assertEqualsTyped(i, literal.value)
        assertEqualsTyped(Int.toString(i), literal.token.literal)
      }
    | _ => simpleFail("statement is not an IntegerLiteral")
    }
  })
}

let assertBooleanLiteral = (statement: option<AST.statement>, b: bool) => {
  statement->assertStatement(s => {
    switch s {
    | AST.BooleanLiteral(literal) => {
        assertEqualsTyped(b, literal.value)
        assertEqualsTyped(String.make(b), literal.token.literal)
      }
    | _ => simpleFail("statement is not an BooleanLiteral")
    }
  })
}

let checkIdentifier = (statement: option<AST.statement>, body: AST.identifier => unit) => {
  statement->assertStatement(s => {
    switch s {
    | AST.Identifier(identifier) => body(identifier)
    | _ => simpleFail(`statement "${String.make(s)}" is not Identifier`)
    }
  })
}

let assertIdentifier = (statement: option<AST.statement>, i: string) => {
  statement->checkIdentifier(identifier => {
    assertEqualsTyped(i, identifier.value)
    assertEqualsTyped(i, identifier.token.literal)
  })
}

let assertLiteralExpression = (value: option<AST.statement>, expectedValue: testParam) => {
  switch expectedValue {
  | I(i) => assertIntegerLiteral(value, i)
  | B(b) => assertBooleanLiteral(value, b)
  | S(s) => assertIdentifier(value, s)
  }
}

let checkExpressionStatement = (
  statement: option<AST.statement>,
  body: AST.expressionStatement => unit,
) => {
  statement->assertStatement(s => {
    switch s {
    | AST.ExpressionStatement(expression) => body(expression)
    | _ => simpleFail(`statement "${String.make(s)}" is not a ExpressionStatement`)
    }
  })
}

let assertInfixExpression = (
  expression: option<AST.statement>,
  leftValue: testParam,
  expectedOperator: string,
  rightValue: testParam,
) => {
  expression->assertStatement(exp => {
    switch exp {
    | AST.InfixExpression({left, operator, right}) => {
        assertLiteralExpression(left, leftValue)
        assertEqualsTyped(operator, expectedOperator)
        assertLiteralExpression(right, rightValue)
      }
    | _ => simpleFail(`expression "${AST.Statement.toString(exp)}" is not an InfixExpression"`)
    }
  })
}

let chechIfExpression = (statement: option<AST.statement>, body: AST.ifExpression => unit) => {
  statement->assertStatement(s => {
    switch s {
    | AST.IfExpression(ifExpression) => body(ifExpression)
    | _ => simpleFail(`statement "${s->AST.Statement.toString} is not IfExpression"`)
    }
  })
}

let checkFunctionLiteral = (
  statement: option<AST.statement>,
  body: AST.functionLiteral => unit,
) => {
  statement->assertStatement(s => {
    switch s {
    | AST.FunctionLiteral(f) => body(f)
    | _ => simpleFail(`statement ${s->AST.Statement.toString} is not FunctionLiteral`)
    }
  })
}

let assertBlockStatementSize = (size: int, blockStatement: option<AST.blockStatement>) => {
  assertEqualsTyped(
    size,
    blockStatement
    ->Option.flatMap(bs => bs.statements->Option.map(s => s->Array.length))
    ->Option.getOr(-1),
  )
}

test("Let Statements", () => {
  [
    ("let x = 5;", "x", I(5)),
    ("let y = true;", "y", B(true)),
    ("let foobar = y;", "foobar", S("y")),
  ]->forEach(row => {
    let (input, expectedIdentifier, expectedValue) = row
    let program = createProgram(input)
    assertCountStatements(1, program)
    let statement = program.statements[0]
    statement->Option.forEach(
      letStatement => {
        switch letStatement {
        | AST.LetStatement({token, name, value}) => {
            assertEqualsTyped("let", token.literal)
            assertEqualsTyped(expectedIdentifier, name.value)
            assertEqualsTyped(expectedIdentifier, name.token.literal)
            assertLiteralExpression(value, expectedValue)
          }
        | _ => simpleFail("statement is not an LetStatement")
        }
      },
    )
  })
})

test("Return Statements", () => {
  [
    ("return 5;", I(5)),
    ("return true;", B(true)),
    ("return foobar;", S("foobar")),
  ]->forEach(row => {
    let (input, expectedValue) = row
    let program = createProgram(input)
    assertCountStatements(1, program)
    let statement = program.statements[0]
    statement->Option.forEach(
      returnStatement => {
        switch returnStatement {
        | AST.ReturnStatement({token, returnValue}) => {
            assertEqualsTyped("return", token.literal)
            assertLiteralExpression(returnValue, expectedValue)
          }
        | _ => simpleFail("statement is not an ReturnStatement")
        }
      },
    )
  })
})

test("identifier expression", () => {
  let input = "foobar;"
  let program = createProgram(input)
  assertCountStatements(1, program)
  checkExpressionStatement(program.statements[0], statement => {
    checkIdentifier(
      statement.expression,
      identifier => {
        assertEqualsTyped("foobar", identifier.token.literal)
      },
    )
  })
})

test("integer literal", () => {
  let input = "5;"
  let program = createProgram(input)
  assertCountStatements(1, program)
  checkExpressionStatement(program.statements[0], statement => {
    assertIntegerLiteral(statement.expression, 5)
  })
})

test("parsing prefix expressions", () => {
  [
    ("!5;", "!", I(5)),
    ("-15;", "-", I(15)),
    ("!true;", "!", B(true)),
    ("!false;", "!", B(false)),
  ]->forEach(row => {
    let (input, expectedOperator, expectedValue) = row
    let program = createProgram(input)
    assertCountStatements(1, program)
    checkExpressionStatement(
      program.statements[0],
      statement => {
        let expression = statement.expression
        expression->assertStatement(
          exp => {
            switch exp {
            | AST.PrefixExpression({operator, right}) => {
                assertEqualsTyped(operator, expectedOperator)
                assertLiteralExpression(right, expectedValue)
              }
            | _ => simpleFail(`expression "${String.make(expression)}" is not a PrefixExpression`)
            }
          },
        )
      },
    )
  })
})

test("parsing infix expressions", () => {
  [
    ("5 + 5", I(5), "+", I(5)),
    ("5 - 5", I(5), "-", I(5)),
    ("5 * 5", I(5), "*", I(5)),
    ("5 / 5", I(5), "/", I(5)),
    ("5 > 5", I(5), ">", I(5)),
    ("5 < 5", I(5), "<", I(5)),
    ("5 == 5", I(5), "==", I(5)),
    ("5 != 5", I(5), "!=", I(5)),
    ("true == true", B(true), "==", B(true)),
    ("true != true", B(true), "!=", B(true)),
    ("false == false", B(false), "==", B(false)),
  ]->forEach(row => {
    let (input, leftValue, operator, rightValue) = row
    let program = createProgram(input)
    assertCountStatements(1, program)
    checkExpressionStatement(
      program.statements[0],
      statement => {
        assertInfixExpression(statement.expression, leftValue, operator, rightValue)
      },
    )
  })
})

test("operator precedence", () => {
  [
    ("-a * b", "((-a) * b)"),
    ("!-a", "(!(-a))"),
    ("a + b + c", "((a + b) + c)"),
    ("a + b - c", "((a + b) - c)"),
    ("a * b * c", "((a * b) * c)"),
    ("a * b / c", "((a * b) / c)"),
    ("a + b / c", "(a + (b / c))"),
    ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
    ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
    ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
    ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
    ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
    ("true", "true"),
    ("false", "false"),
    ("3 > 5 == false", "((3 > 5) == false)"),
    ("3 < 5 == true", "((3 < 5) == true)"),
    ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
    ("(5 + 5) * 2", "((5 + 5) * 2)"),
    ("2 / (5 + 5)", "(2 / (5 + 5))"),
    ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"),
    ("-(5 + 5)", "(-(5 + 5))"),
    ("!(true == true)", "(!(true == true))"),
    ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
    (
      "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
      "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
    ),
    ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"),
    ("a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)"),
    ("add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"),
  ]->forEach(row => {
    let (input, expected) = row
    let program = createProgram(input)
    assertEqualsTyped(program->AST.Program.toString, expected)
  })
})

test("boolean expression", () => {
  [("true", B(true)), ("false", B(false))]->forEach(row => {
    let (input, expectedBoolean) = row
    let program = createProgram(input)
    assertCountStatements(1, program)
    checkExpressionStatement(
      program.statements[0],
      statement => {
        assertLiteralExpression(statement.expression, expectedBoolean)
      },
    )
  })
})

test("if expression", () => {
  let input = "if (x < y) {x}"
  let program = createProgram(input)
  assertCountStatements(1, program)
  checkExpressionStatement(program.statements[0], statement => {
    chechIfExpression(
      statement.expression,
      exp => {
        assertInfixExpression(exp.condition, S("x"), "<", S("y"))
        assertBlockStatementSize(1, exp.consequence)
        let maybeStatement =
          exp.consequence->Option.flatMap(c => c.statements->Option.flatMap(s => s[0]))
        maybeStatement->Option.forEach(
          statement => {
            checkExpressionStatement(
              statement,
              consequence => {
                assertIdentifier(consequence.expression, "x")
              },
            )
          },
        )
      },
    )
  })
})

test("if else expression", () => {
  let input = "if (x < y) {x} else {y}"
  let program = createProgram(input)
  assertCountStatements(1, program)
  checkExpressionStatement(program.statements[0], statement => {
    chechIfExpression(
      statement.expression,
      exp => {
        assertInfixExpression(exp.condition, S("x"), "<", S("y"))
        assertBlockStatementSize(1, exp.consequence)

        let maybeConsequenceStatement =
          exp.consequence->Option.flatMap(c => c.statements->Option.flatMap(s => s[0]))
        maybeConsequenceStatement->Option.forEach(
          statement => {
            checkExpressionStatement(
              statement,
              consequence => {
                assertIdentifier(consequence.expression, "x")
              },
            )
          },
        )

        let maybeAlternativeStatement =
          exp.alternative->Option.flatMap(a => a.statements->Option.flatMap(s => s[0]))
        maybeAlternativeStatement->Option.forEach(
          statement => {
            checkExpressionStatement(
              statement,
              alternative => {
                assertIdentifier(alternative.expression, "y")
              },
            )
          },
        )
      },
    )
  })
})

test("function literal parsing", () => {
  let input = "fn(x, y) { x + y;}"
  let program = createProgram(input)
  assertCountStatements(1, program)

  checkExpressionStatement(program.statements[0], statement => {
    checkFunctionLiteral(
      statement.expression,
      fun => {
        switch fun.parameters {
        | None =>
          simpleFail(`${AST.FunctionLiteral(fun)->AST.Statement.toString} should have parameters`)
        | Some(parameters) => {
            let assertParameter = (parameter, value) => {
              parameter->Option.map(p => AST.Identifier(p))->assertLiteralExpression(S(value))
            }
            assertParameter(parameters[0], "x")
            assertParameter(parameters[1], "y")
          }
        }
        assertBlockStatementSize(1, fun.body)
        let maybeBody = fun.body->Option.flatMap(b => b.statements->Option.flatMap(s => s[0]))
        maybeBody->Option.forEach(
          statement => {
            checkExpressionStatement(
              statement,
              body => {
                assertInfixExpression(body.expression, S("x"), "+", S("y"))
              },
            )
          },
        )
      },
    )
  })
})
