let env = Environment.newEnvironment()

@val external prompt: string => string = "prompt"

Console.log("Hello, this is the ReMonkey programing language")

while true {
  let line = prompt(">>")
  let lexer = Lexer.Lexer.newLexer(line)
  let parser = Parser.Parser.newParser(lexer)
  let program = parser->Parser.Parser.parseProgram
  if parser.errors->Array.length > 0 {
    Console.log("Woops! we ran into some monkey business here")
    Console.log("\tparser errors:")
    parser.errors->Array.forEach(error => {
      Console.log(`\t${error}`)
    })
  }

  let output = Evaluator.Eval.eval(program, env)
  output->Option.forEach(o => Console.log(o->Objects.inspect))
}
