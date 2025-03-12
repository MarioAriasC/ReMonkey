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
  | CallExpression(callExpression)
  | ArrayLiteral(arrayLiteral)
  | IndexExpression(indexExpression)
  | BlockStatement(blockStatement)
  | IfExpression(ifExpression)
  | FunctionLiteral(functionLiteral)
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
and optionStatementArray = option<array<option<statement>>>
and callExpression = {
  ...tokenHolder,
  function: option<statement>,
  arguments: optionStatementArray,
}
and arrayLiteral = {...tokenHolder, elements: optionStatementArray}
and indexExpression = {...tokenHolder, left: option<statement>, index: option<statement>}
and blockStatement = {...tokenHolder, statements: optionStatementArray}
and ifExpression = {
  ...tokenHolder,
  condition: option<statement>,
  consequence: option<blockStatement>,
  alternative: option<blockStatement>,
}
and functionLiteral = {
  ...tokenHolder,
  parameters: option<array<identifier>>,
  body: option<blockStatement>,
}

module Statement = {
  let rec toString: statement => string = s => {
    open Option
    switch s {
    | Identifier(i) => i.value
    | StringLiteral(l) => l.value
    | LetStatement({token, name, value}) =>
      `${token.literal} ${Identifier(name)->toString} = ${value->optionToString}`
    | IntegerLiteral(i) => i.token.literal
    | BooleanLiteral(b) => b.token.literal
    | ReturnStatement({token, returnValue}) => `${token.literal} ${returnValue->optionToString}`
    | ExpressionStatement({expression}) => expression->optionToString
    | PrefixExpression({operator, right}) => `(${operator}${right->optionToString})`
    | InfixExpression({left, operator, right}) =>
      `(${left->optionToString} ${operator} ${right->optionToString})`
    | CallExpression({function, arguments}) =>
      `${function->optionToString}(${arguments->argsToString})`
    | ArrayLiteral({elements}) => `[${elements->argsToString}]`
    | IndexExpression({left, index}) => `(${left->optionToString}[${index->optionToString}])`
    | BlockStatement({statements}) => statements->argsToStringWithSeparator("")
    | IfExpression({condition, consequence, alternative}) =>
      `if ${condition->optionToString} ${consequence
        ->map(c => BlockStatement(c))
        ->optionToString} ${alternative
        ->map(alt => `else ${BlockStatement(alt)->toString}`)
        ->getOr("")}`
    | FunctionLiteral({token, parameters, body}) =>
      `${token.literal} (${parameters
        ->map(ps => ps->Array.map(p => Identifier(p)->toString)->Array.join(", "))
        ->getOr("")}}) ${body->map(b => BlockStatement(b))->optionToString}`
    }
  }
  and argsToStringWithSeparator = (arguments: optionStatementArray, separator: string) => {
    open Option
    arguments
    ->map(args => args->Array.map(optionToString))
    ->getOr([""])
    ->Array.join(separator)
  }
  and argsToString = (arguments: optionStatementArray) => argsToStringWithSeparator(arguments, ", ")
  and optionToString: option<statement> => string = arg => {
    open Option
    arg->map(toString)->getOr("")
  }
}

type program = {statements: array<statement>}

module Program = {
  let toString = program => {
    String.concatMany("", program.statements->Array.map(Statement.toString))
  }
}
