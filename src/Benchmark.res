let parse = (input: string) => {
  let lexer = Lexer.Lexer.newLexer(input)
  let parser = Parser.Parser.newParser(lexer)
  Parser.Parser.parseProgram(parser)
}

Js.log(
  Evaluator.Eval.eval(
    parse(`let fibonacci = fn(x) {    	
        if (x < 2) {
        	return x;
        } else {
        	fibonacci(x - 1) + fibonacci(x - 2);
        }
    };
    fibonacci(35);`),
    Environment.newEnvironment(),
  )
  ->Option.getUnsafe
  ->Objects.inspect,
)
