type tokenHolder = {token: Token.token}
type literalExpression<'a> = {...tokenHolder, value: 'a}
type stringValue = {...literalExpression<string>}
type identifier = {...stringValue}
type stringLiteral = {...stringValue}
type integerLiteral = {...literalExpression<int>}
type booleanLiteral = {...literalExpression<bool>}

type rec statement =
  | Identifier(identifier)
  | StringLiteral(stringLiteral)
  | LetStatement(letStatement)
  | IntegerLiteral(integerLiteral)
  | BooleanLiteral(booleanLiteral)
  | ReturnStatement(returnStatement)
  | ExpressionStatement(expressionStatement)
  | PrefixExpression(prefixExpression)
  | InfixExpression(infixExpression)
and letStatement = {...tokenHolder, name: identifier, value: option<statement>}
and returnStatement = {...tokenHolder, returnValue: option<statement>}
and expressionStatement = {...tokenHolder, expression: option<statement>}
and prefixExpression = {...tokenHolder, operator: string, right: option<statement>}
and infixExpression = {
  ...tokenHolder,
  left: option<statement>,
  operator: string,
  right: option<statement>,
}
// let tokenLiteral: statement => string = s => {
//   switch s {
//   | Identifier(i) => i.token.literal
//   | StringLiteral(i) => i.token.literal
//   | LetStatement(l) => l.token.literal
//   }
// }

module Statement = {
  let rec toString: statement => string = s => {
    open Option
    switch s {
    | Identifier(i) => i.value
    | StringLiteral(l) => l.value
    | LetStatement({token, name, value}) =>
      `${token.literal} ${Identifier(name)->toString} = ${value
        ->map(toString)
        ->getOr("")}`
    | IntegerLiteral(i) => i.token.literal
    | BooleanLiteral(b) => b.token.literal
    | ReturnStatement({token, returnValue}) =>
      `${token.literal} ${returnValue->map(toString)->getOr("")}`
    | ExpressionStatement({expression}) => expression->map(toString)->getOr("")
    | PrefixExpression({operator, right}) => `(${operator}${right->map(toString)->getOr("")})`
    | InfixExpression({left, operator, right}) =>
      `(${left->map(toString)->getOr("")} ${operator} ${right->map(toString)->getOr("")})`
    }
  }
}

type program = {statements: array<statement>}

module Program = {
  let toString = program => {
    String.concatMany("", program.statements->Array.map(Statement.toString))
  }
}
