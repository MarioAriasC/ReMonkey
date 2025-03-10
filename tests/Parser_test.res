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
