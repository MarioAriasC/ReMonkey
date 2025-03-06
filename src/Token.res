type tokenType =
  | @as("ILLEGAL") Illegal
  | @as("EOF") EOF
  | @as("=") Assign
  | @as("==") Eq
  | @as("!=") NotEq
  | @as("IDENT") Ident
  | @as("INT") Int
  | @as("+") Plus
  | @as(",") Comma
  | @as(";") Semicolon
  | @as(":") Colon
  | @as("-") Minus
  | @as("!") Bang
  | @as("/") Slash
  | @as("*") Asterisk
  | @as("<") Lt
  | @as(">") Gt
  | @as("(") LParen
  | @as(")") RParen
  | @as("{") LBrace
  | @as("}") RBrace
  | @as("[") LBracket
  | @as("]") RBracket
  | @as("FUNCTION") Function
  | @as("LET") Let
  | @as("TRUE") True
  | @as("FALSE") False
  | @as("IF") If
  | @as("ELSE") Else
  | @as("RETURN") Return
  | @as("STRING") String

let lookupIdent = (ident: string) => {
  switch ident {
  | "fn" => Function
  | "let" => Let
  | "true" => True
  | "false" => False
  | "if" => If
  | "else" => Else
  | "return" => Return
  | _ => Ident
  }
}

type token = {token_type: tokenType, literal: string}

let newToken = (token_type: tokenType, literal: string) => {
  {token_type, literal}
}
