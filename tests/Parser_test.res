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

let assertIdentifier = (statement: option<AST.statement>, i: string) => {
  statement->assertStatement(s => {
    switch s {
    | AST.Identifier(identifier) => {
        assertEqualsTyped(i, identifier.value)
        assertEqualsTyped(i, identifier.token.literal)
      }
    | _ => simpleFail("statement is not an Identifier")
    }
  })
}

let assertLiteralExpression = (value: option<AST.statement>, expectedValue: testParam) => {
  switch expectedValue {
  | I(i) => assertIntegerLiteral(value, i)
  | B(b) => assertBooleanLiteral(value, b)
  | S(s) => assertIdentifier(value, s)
  }
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
