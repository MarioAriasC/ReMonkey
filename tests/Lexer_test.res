open Test

test("Validate lexer", () => {
  let code = `
let five = 5;
let ten = 10;

let add = fn(x, y) {
	x + y;
}

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
	return true;
} else {
	return false;
}

10 == 10;
10 != 9;
"foobar"
"foo bar"
[1,2];
{"foo":"bar"}`

  open Lexer

  let l = Lexer.newLexer(code)

  let expected = [
    (Token.Let, "let"),
    (Token.Ident, "five"),
    (Token.Assign, "="),
    (Token.Int, "5"),
    (Token.Semicolon, ";"),
    (Token.Let, "let"),
    (Token.Ident, "ten"),
    (Token.Assign, "="),
    (Token.Int, "10"),
    (Token.Semicolon, ";"),
    (Token.Let, "let"),
    (Token.Ident, "add"),
    (Token.Assign, "="),
    (Token.Function, "fn"),
    (Token.LParen, "("),
    (Token.Ident, "x"),
    (Token.Comma, ","),
    (Token.Ident, "y"),
    (Token.RParen, ")"),
    (Token.LBrace, "{"),
    (Token.Ident, "x"),
    (Token.Plus, "+"),
    (Token.Ident, "y"),
    (Token.Semicolon, ";"),
    (Token.RBrace, "}"),
    (Token.Let, "let"),
    (Token.Ident, "result"),
    (Token.Assign, "="),
    (Token.Ident, "add"),
    (Token.LParen, "("),
    (Token.Ident, "five"),
    (Token.Comma, ","),
    (Token.Ident, "ten"),
    (Token.RParen, ")"),
    (Token.Semicolon, ";"),
    (Token.Bang, "!"),
    (Token.Minus, "-"),
    (Token.Slash, "/"),
    (Token.Asterisk, "*"),
    (Token.Int, "5"),
    (Token.Semicolon, ";"),
    (Token.Int, "5"),
    (Token.Lt, "<"),
    (Token.Int, "10"),
    (Token.Gt, ">"),
    (Token.Int, "5"),
    (Token.Semicolon, ";"),
    (Token.If, "if"),
    (Token.LParen, "("),
    (Token.Int, "5"),
    (Token.Lt, "<"),
    (Token.Int, "10"),
    (Token.RParen, ")"),
    (Token.LBrace, "{"),
    (Token.Return, "return"),
    (Token.True, "true"),
    (Token.Semicolon, ";"),
    (Token.RBrace, "}"),
    (Token.Else, "else"),
    (Token.LBrace, "{"),
    (Token.Return, "return"),
    (Token.False, "false"),
    (Token.Semicolon, ";"),
    (Token.RBrace, "}"),
    (Token.Int, "10"),
    (Token.Eq, "=="),
    (Token.Int, "10"),
    (Token.Semicolon, ";"),
    (Token.Int, "10"),
    (Token.NotEq, "!="),
    (Token.Int, "9"),
    (Token.Semicolon, ";"),
    (Token.String, "foobar"),
    (Token.String, "foo bar"),
    (Token.LBracket, "["),
    (Token.Int, "1"),
    (Token.Comma, ","),
    (Token.Int, "2"),
    (Token.RBracket, "]"),
    (Token.Semicolon, ";"),
    (Token.LBrace, "{"),
    (Token.String, "foo"),
    (Token.Colon, ":"),
    (Token.String, "bar"),
    (Token.RBrace, "}"),
    (Token.EOF, ""),
  ]

  open Array

  open Asserts

  expected->forEach(expect => {
    let (token_type, literal) = expect
    let token = l->Lexer.nextToken
    assertEqualsTyped(token_type, token.token_type)
    assertEqualsTyped(literal, token.literal)
  })
})
