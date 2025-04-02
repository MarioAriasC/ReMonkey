# ReMonkey

A ReScript implementation of the Monkey Language

ReMonkey has many sibling implementations

* Kotlin: [monkey.kt](https://github.com/MarioAriasC/monkey.kt)
* Crystal: [Monyet](https://github.com/MarioAriasC/monyet)
* Scala 3: [Langur](https://github.com/MarioAriasC/langur)
* Ruby 3: [Pepa](https://github.com/MarioAriasC/pepa)
* Python 3.10 [Bruno](https://github.com/MarioAriasC/bruno)
* Lua [Julieta](https://github.com/MarioAriasC/julieta)
* TypeScript [TSMonkey](https://github.com/MarioAriasC/TSMonkey)

## Status

The book ([Writing An Interpreter In Go](https://interpreterbook.com/)) is fully implemented.
ReMonkey will not have a compiler implementation

## Commands

## Prerequisites

Any recent version of Bun 1.2.* and Node LTS to run the Benchmarks

| Command                                                         | Description                                    |
|-----------------------------------------------------------------|------------------------------------------------|
| `bun install`                                                   | Install all the packages using bun             |
| `bun run res:build`                                             | Build the full project                         |
| `bun run test`                                                  | Run all tests                                  |
| `bun run src/Repl.res.mjs`                                      | Run the ReMonkey REPL                          |
| `bun run src/Benchmark.res.mjs` or `node src/Benchmark.res.mjs` | Run the benchmarks, i.e. a recursive `fib(35)` |
