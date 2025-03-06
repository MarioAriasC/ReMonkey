module Lexer: {
  type lexer
  let newLexer: string => lexer
  let nextToken: lexer => Token.token
} = {
  type lexer = {
    input: string,
    mutable position: int,
    mutable read_position: int,
    mutable ch: string,
  }

  let zero: string = ""
  let digit_expression = %re("/^\d$/")
  let white_spaces = [" ", "\t", "\n", "\r"]

  let peakChar = (l: lexer) => {
    open String
    if l.read_position >= l.input->length {
      zero
    } else {
      l.input->charAt(l.read_position)
    }
  }

  let readChar = (l: lexer) => {
    l.ch = l->peakChar
    l.position = l.read_position
    l.read_position = l.read_position + 1
  }

  let newLexer = (input: string) => {
    let l = {input, position: 0, read_position: 0, ch: zero}
    l->readChar
    l
  }

  let token = (l: lexer, token_type: Token.tokenType) => {
    Token.newToken(token_type, l.ch)
  }

  let readValue = (l: lexer, predicate: string => bool) => {
    let current_position = l.position
    while predicate(l.ch) {
      l->readChar
    }
    open String
    l.input->substring(~start=current_position, ~end=l.position)
  }

  let isDigit = (ch: string) => {
    RegExp.test(digit_expression, ch)
  }

  let readNumber = (l: lexer) => {
    l->readValue(isDigit)
  }

  let skipWithSpaces = (l: lexer) => {
    open Array
    while white_spaces->includes(l.ch) {
      l->readChar
    }
  }

  let endsWithEqual = (
    l: lexer,
    oneChar: Token.tokenType,
    twoChars: Token.tokenType,
    ~duplicateChars: bool=true,
  ) => {
    if l->peakChar == "=" {
      let currentChar = l.ch
      l->readChar
      let value = if duplicateChars {
        `${currentChar}${currentChar}`
      } else {
        `${currentChar}${l.ch}`
      }
      Token.newToken(twoChars, value)
    } else {
      l->token(oneChar)
    }
  }

  let readString = (l: lexer) => {
    let start = l.position + 1
    let break = ref(false)
    while !break.contents {
      l->readChar
      if l.ch == "\"" || l.ch == zero {
        break := true
      }
    }
    open String
    l.input->substring(~start, ~end=l.position)
  }

  let isIdentifier = (value: string) => {
    open String
    value->toLowerCase != value->toUpperCase || value == "_"
  }

  let readIdentifier = (l: lexer) => {
    l->readValue(isIdentifier)
  }

  let nextToken = (l: lexer) => {
    l->skipWithSpaces

    let readNextChar = ref(true)
    open Utils
    switch l.ch {
    | "=" => l->endsWithEqual(Token.Assign, Token.Eq)
    | "!" => l->endsWithEqual(Token.Bang, Token.NotEq, ~duplicateChars=false)
    | ";" => l->token(Token.Semicolon)
    | ":" => l->token(Token.Colon)
    | "," => l->token(Token.Comma)
    | "(" => l->token(Token.LParen)
    | ")" => l->token(Token.RParen)
    | "{" => l->token(Token.LBrace)
    | "}" => l->token(Token.RBrace)
    | "[" => l->token(Token.LBracket)
    | "]" => l->token(Token.RBracket)
    | "+" => l->token(Token.Plus)
    | "-" => l->token(Token.Minus)
    | "*" => l->token(Token.Asterisk)
    | "/" => l->token(Token.Slash)
    | "<" => l->token(Token.Lt)
    | ">" => l->token(Token.Gt)
    | "\"" => Token.newToken(Token.String, l->readString)
    // supposed to be the constant zero but is not supported on ReScrpit
    // https://rescript-lang.org/docs/manual/v11.0.0/pattern-matching-destructuring#small-pitfall
    | "" => Token.newToken(Token.EOF, "")
    | _ =>
      if isIdentifier(l.ch) {
        let identifier = l->readIdentifier
        readNextChar := false
        Token.newToken(Token.lookupIdent(identifier), identifier)
      } else if isDigit(l.ch) {
        readNextChar := false
        Token.newToken(Token.Int, l->readNumber)
      } else {
        l->token(Token.Illegal)
      }
    }->also(_ =>
      if readNextChar.contents {
        l->readChar
      }
    )
  }
}
