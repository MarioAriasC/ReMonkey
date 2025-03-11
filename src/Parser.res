open Lexer

type parser = {
  lexer: lexer,
  errors: array<string>,
  mutable curToken: Token.token,
  mutable peekToken: Token.token,
}

module Parser: {
  let newParser: lexer => parser
  let parseProgram: parser => AST.program
  // let errors: parser => list<string>
} = {
  type precedence =
    | @as(0) Lowest
    | @as(1) Equals
    | @as(2) LessGreater
    | @as(3) Sum
    | @as(4) Product
    | @as(5) Prefix
    | @as(6) Call
    | @as(7) Index

  let nextToken = (p: parser) => {
    p.curToken = p.peekToken
    p.peekToken = p.lexer->Lexer.nextToken
  }

  let newParser = (l: lexer) => {
    {
      lexer: l,
      errors: [],
      curToken: Token.newToken(Token.Illegal, ""),
      peekToken: Token.newToken(Token.Illegal, ""),
    }->Utils.also(p => {
      p->nextToken
      p->nextToken
    })
  }

  let peekTokenIs = (p: parser, tt: Token.tokenType) => p.peekToken.token_type == tt

  let peekError = (p: parser, tt: Token.tokenType) => {
    Array.push(
      p.errors,
      `Expected next token to be ${String.make(tt)}, got ${String.make(
          p.peekToken.token_type,
        )} instead`,
    )
  }

  let noPrefixParserError = (p: parser, tt: Token.tokenType) => {
    Array.push(p.errors, `No prefix parser for ${String.make(tt)} function`)
  }

  let expectPeek = (p: parser, tt: Token.tokenType) => {
    if p->peekTokenIs(tt) {
      p->nextToken
      true
    } else {
      p->peekError(tt)
      false
    }
  }

  let parseIntegerLiteral = (p: parser) => {
    let token = p.curToken
    switch Int.fromString(token.literal) {
    | Some(value) => Some(AST.IntegerLiteral({token, value}))
    | None => {
        Array.push(p.errors, `could not parse ${token.literal} as integer`)
        None
      }
    }
  }

  let curTokenIs = (p: parser, tt: Token.tokenType) => p.curToken.token_type == tt

  let parseBooleanLiteral = (p: parser) => {
    Some(AST.BooleanLiteral({token: p.curToken, value: p->curTokenIs(Token.True)}))
  }

  let parseIdentifier = (p: parser) => Some(
    AST.Identifier({token: p.curToken, value: p.curToken.literal}),
  )

  let findPrecedence = (tt: Token.tokenType) => {
    switch tt {
    | Token.Eq => Equals
    | Token.NotEq => Equals
    | Token.Lt => LessGreater
    | Token.Gt => LessGreater
    | Token.Plus => Sum
    | Token.Minus => Sum
    | Token.Slash => Product
    | Token.Asterisk => Product
    | Token.LParen => Call
    | Token.LBracket => Index
    | _ => Lowest
    }
  }

  let peekPrecedence = (p: parser) => findPrecedence(p.peekToken.token_type)

  let curPrecedence = (p: parser) => findPrecedence(p.curToken.token_type)

  let rec parseExpression = (p: parser, prec: precedence) => {
    let prefix = prefixParser(p.curToken.token_type)
    switch prefix {
    | None => {
        p->noPrefixParserError(p.curToken.token_type)
        None
      }
    | Some(prefixFn) => {
        let left = ref(p->prefixFn)
        let break = ref(false)
        while !break.contents && !(p->peekTokenIs(Token.Semicolon)) && prec < p->peekPrecedence {
          let infix = infixParser(p.peekToken.token_type)
          switch infix {
          | None => break := true
          | Some(infixFn) => {
              p->nextToken
              left := p->infixFn(left.contents)
            }
          }
        }
        left.contents
      }
    }
  }
  and parsePrefixExpression = (p: parser) => {
    let token = p.curToken
    let operator = token.literal
    p->nextToken
    let right = p->parseExpression(Prefix)
    Some(AST.PrefixExpression({token, operator, right}))
  }
  and prefixParser: Token.tokenType => option<parser => option<AST.statement>> = tt => {
    switch tt {
    | Token.Int => Some(parseIntegerLiteral)
    | Token.True => Some(parseBooleanLiteral)
    | Token.False => Some(parseBooleanLiteral)
    | Token.Ident => Some(parseIdentifier)
    | Token.Bang => Some(parsePrefixExpression)
    | Token.Minus => Some(parsePrefixExpression)
    | Token.LParen => Some(parseGroupExpression)
    | Token.LBracket => Some(parseArrayLiteral)
    | Token.If => Some(parseIfExpression)
    | _ => None
    }
  }
  and infixParser: Token.tokenType => option<
    (parser, option<AST.statement>) => option<AST.statement>,
  > = tt => {
    switch tt {
    | Token.Plus => Some(parseInfixExpression)
    | Token.Minus => Some(parseInfixExpression)
    | Token.Slash => Some(parseInfixExpression)
    | Token.Asterisk => Some(parseInfixExpression)
    | Token.Eq => Some(parseInfixExpression)
    | Token.NotEq => Some(parseInfixExpression)
    | Token.Lt => Some(parseInfixExpression)
    | Token.Gt => Some(parseInfixExpression)
    | Token.LParen => Some(parseCallExpression)
    | Token.LBracket => Some(parseIndexExpression)
    | _ => None
    }
  }
  and parseInfixExpression = (p: parser, left: option<AST.statement>) => {
    let token = p.curToken
    let operator = token.literal
    let prec = p->curPrecedence
    p->nextToken
    let right = p->parseExpression(prec)
    Some(AST.InfixExpression({token, left, operator, right}))
  }
  and parseGroupExpression = (p: parser) => {
    p->nextToken
    let exp = p->parseExpression(Lowest)
    if !(p->expectPeek(Token.RParen)) {
      None
    } else {
      exp
    }
  }
  and parseCallExpression = (p: parser, left: option<AST.statement>) => {
    let token = p.curToken
    let arguments = p->parseExpressionList(Token.RParen)
    Some(AST.CallExpression({token, function: left, arguments}))
  }
  and parseExpressionList = (p: parser, end: Token.tokenType) => {
    let arguments: array<option<AST.statement>> = []
    if p->peekTokenIs(end) {
      p->nextToken
      Some(arguments)
    } else {
      p->nextToken
      arguments->Array.push(p->parseExpression(Lowest))

      while p->peekTokenIs(Token.Comma) {
        p->nextToken
        p->nextToken
        arguments->Array.push(p->parseExpression(Lowest))
      }

      if !(p->expectPeek(end)) {
        None
      } else {
        Some(arguments)
      }
    }
  }
  and parseArrayLiteral = (p: parser) => {
    let token = p.curToken
    Some(AST.ArrayLiteral({token, elements: p->parseExpressionList(Token.RBracket)}))
  }
  and parseIndexExpression = (p: parser, left: option<AST.statement>) => {
    let token = p.curToken
    p->nextToken
    let index = p->parseExpression(Lowest)
    if !(p->expectPeek(Token.RBracket)) {
      None
    } else {
      Some(AST.IndexExpression({token, left, index}))
    }
  }

  and parseLetStatement: parser => option<AST.statement> = p => {
    let token = p.curToken
    if !(p->expectPeek(Token.Ident)) {
      None
    } else {
      let name: AST.identifier = {token: p.curToken, value: p.curToken.literal}

      if !(p->expectPeek(Token.Assign)) {
        None
      } else {
        p->nextToken
        let value = p->parseExpression(Lowest)

        if p->peekTokenIs(Token.Semicolon) {
          p->nextToken
        }
        Some(AST.LetStatement({token, name, value}))
      }
    }
  }

  and parseReturnStatement: parser => option<AST.statement> = p => {
    let token = p.curToken
    p->nextToken
    let returnValue = p->parseExpression(Lowest)

    while p->peekTokenIs(Token.Semicolon) {
      p->nextToken
    }

    Some(AST.ReturnStatement({token, returnValue}))
  }

  and parseExpressionStatement: parser => option<AST.statement> = p => {
    let token = p.curToken
    let expression = p->parseExpression(Lowest)

    if p->peekTokenIs(Token.Semicolon) {
      p->nextToken
    }

    Some(AST.ExpressionStatement({token, expression}))
  }

  and parseStatement = (p: parser) => {
    switch p.curToken.token_type {
    | Token.Let => p->parseLetStatement
    | Token.Return => p->parseReturnStatement
    | _ => p->parseExpressionStatement
    }
  }
  and parseBlockStatement: parser => AST.blockStatement = p => {
    let token = p.curToken
    let statements: array<option<AST.statement>> = []
    p->nextToken
    while !(p->curTokenIs(Token.RBrace)) && !(p->curTokenIs(Token.EOF)) {
      let statement = p->parseStatement
      statement->Option.forEach(st => statements->Array.push(Some(st)))
      p->nextToken
    }
    {token, statements: Some(statements)}
  }
  and parseIfExpression = (p: parser) => {
    let token = p.curToken
    if !(p->expectPeek(Token.LParen)) {
      None
    } else {
      p->nextToken
      let condition = p->parseExpression(Lowest)

      if !(p->expectPeek(Token.RParen)) {
        None
      } else if !(p->expectPeek(Token.LBrace)) {
        None
      } else {
        let consequence = p->parseBlockStatement
        let earlyReturn = ref(false)
        let alternative = if p->peekTokenIs(Token.Else) {
          p->nextToken
          if !(p->expectPeek(Token.LBrace)) {
            earlyReturn := true
            None
          } else {
            Some(p->parseBlockStatement)
          }
        } else {
          None
        }
        if earlyReturn.contents {
          None
        } else {
          Some(AST.IfExpression({token, condition, consequence: Some(consequence), alternative}))
        }
      }
    }
  }

  let parseProgram: parser => AST.program = p => {
    let statements: array<AST.statement> = []

    while p.curToken.token_type != Token.EOF {
      let statement = p->parseStatement
      statement->Option.forEach(s => statements->Array.push(s))
      p->nextToken
    }
    {statements: statements}
  }
}
