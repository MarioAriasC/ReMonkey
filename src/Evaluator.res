open Objects

module Eval: {
  let eval: (AST.program, environment) => option<mObject>
  let cNULL: mObject
} = {
  let cTRUE = MBoolean({value: true})
  let cFALSE = MBoolean({value: false})
  let cNULL = MNull

  let boolToMonkey = (b: bool) =>
    if b {
      cTRUE
    } else {
      cFALSE
    }

  let ifNotError = (obj: option<mObject>, body: mObject => option<mObject>) => {
    obj->Option.flatMap(o => {
      switch o {
      | MError(_) => Some(o)
      | _ => body(o)
      }
    })
  }

  let isError = (obj: option<mObject>) => {
    switch obj {
    | Some(o) =>
      switch o {
      | MError(_) => true
      | _ => false
      }
    | None => false
    }
  }

  let evalMinusPrefixOperatorExpression = (obj: mObject) =>
    switch obj {
    | MInteger({value}) => MInteger({value: -value})
    | _ => MError({message: `unknown operator -${obj->typeDesc}`})
    }->Some

  let evalIntegerInfixExpression = (operator: string, left: int, right: int) => {
    switch operator {
    | "+" => MInteger({value: left + right})
    | "-" => MInteger({value: left - right})
    | "*" => MInteger({value: left * right})
    | "/" => MInteger({value: left / right})
    | "<" => (left < right)->boolToMonkey
    | ">" => (left > right)->boolToMonkey
    | "==" => (left == right)->boolToMonkey
    | "!=" => (left != right)->boolToMonkey
    | _ => MError({message: `unknown operator MInteger ${operator} MInteger`})
    }
  }

  let evalInfixExpression = (operator: string, left: mObject, right: mObject) => {
    switch (left, operator, right) {
    | (MInteger({value: leftValue}), _, MInteger({value: rightValue})) =>
      evalIntegerInfixExpression(operator, leftValue, rightValue)
    | (_, "==", _) => (left == right)->boolToMonkey
    | (_, "!=", _) => (left != right)->boolToMonkey
    | _ => MError({message: `unknown operator: ${left->typeDesc} ${operator} ${left->typeDesc}`})
    }->Some
  }

  let evalBangOperatorExpression = (obj: mObject) => {
    if obj == cTRUE {
      cFALSE
    } else if obj == cFALSE {
      cTRUE
    } else if obj == cNULL {
      cTRUE
    } else {
      cFALSE
    }->Some
  }

  let extendFunctionEnv = (fun: mFunction, args: array<option<mObject>>) => {
    let env = Environment.newEnclosedEnvironment(fun.env)
    fun.parameters->Option.forEach(params => {
      params->Array.forEachWithIndex((identifier, i) => {
        env->Environment.set(identifier.value, args->Array.getUnsafe(i)->Option.getUnsafe)
      })
    })
    env
  }

  let rec eval = (program: AST.program, env: environment) => {
    let result: ref<option<mObject>> = ref(None)
    let keep = ref(true)
    program.statements->Array.forEach(statement => {
      if keep.contents {
        result := evaluateStatement(Some(statement), env)

        result.contents->Option.forEach(contents => {
          switch contents {
          | MReturnValue({value}) => {
              result := Some(value)
              keep := false
            }
          | MError(_) => keep := false
          | _ => ()
          }
        })
      }
    })
    result.contents
  }
  and evaluateStatement = (statement: option<AST.statement>, env: environment) => {
    switch statement {
    | Some(st) =>
      switch st {
      | AST.Identifier({value}) => {
          let v = env->Environment.get(value)
          v
        }
      | AST.IntegerLiteral(i) => Some(Objects.MInteger({value: i.value}))
      | AST.InfixExpression({left, operator, right}) =>
        ifNotError(evaluateStatement(left, env), l => {
          ifNotError(evaluateStatement(right, env), r => {
            evalInfixExpression(operator, l, r)
          })
        })
      | AST.BlockStatement(block) => evaluateBlockStatement(block, env)
      | AST.ExpressionStatement({expression}) => evaluateStatement(expression, env)
      | AST.IfExpression({condition, consequence, alternative}) => {
          let isTruthy = (c: mObject) => {
            if c == cNULL || c == cFALSE {
              false
            } else {
              true
            }
          }
          evaluateStatement(condition, env)->ifNotError(c => {
            if isTruthy(c) {
              evaluateStatement(consequence->Option.map(b => AST.BlockStatement(b)), env)
            } else if alternative->Option.isSome {
              evaluateBlockStatement(alternative->Option.getUnsafe, env)
            } else {
              Some(cNULL)
            }
          })
        }
      | AST.CallExpression({function, arguments}) =>
        evaluateStatement(function, env)->ifNotError(fun => {
          let args = evalExpressions(arguments, env)
          if Array.length(args) == 1 && isError(args[0]->Option.getUnsafe) {
            args[0]->Option.getUnsafe
          } else {
            applyFunction(fun, args)
          }
        })
      | AST.ReturnStatement({returnValue}) =>
        evaluateStatement(returnValue, env)->ifNotError(value => {
          Some(MReturnValue({value: value}))
        })
      | AST.PrefixExpression({operator, right}) =>
        ifNotError(evaluateStatement(right, env), r => {
          switch operator {
          | "!" => evalBangOperatorExpression(r)
          | "-" => evalMinusPrefixOperatorExpression(r)
          | _ => Some(MError({message: `Unknown operator: ${operator}${r->typeDesc}`}))
          }
        })
      | AST.BooleanLiteral({value}) => Some(value->boolToMonkey)
      | AST.LetStatement({name, value}) =>
        evaluateStatement(value, env)->ifNotError(v => {
          Some(env->Environment.put(name.value, v))
        })
      | AST.FunctionLiteral({parameters, body}) => Some(MFunction({parameters, body, env}))
      | _ => {
          Js.log(%raw("statement.TAG"))
          None
        }
      }
    | None => raise(Failure("statement shouldn't be None"))
    }
  }
  and evaluateBlockStatement = (st: AST.blockStatement, env: environment) => {
    let result: ref<option<mObject>> = ref(None)
    let keep = ref(true)
    st.statements->Option.forEach(statements => {
      statements->Array.forEach(statement => {
        if keep.contents {
          result := evaluateStatement(statement, env)
          result.contents->Option.forEach(
            contents => {
              switch contents {
              | MReturnValue(_) => keep := false
              | MError(_) => keep := false
              | _ => ()
              }
            },
          )
        }
      })
    })
    result.contents
  }
  and evalExpressions: (AST.optionStatementArray, environment) => array<option<mObject>> = (
    args: AST.optionStatementArray,
    env: environment,
  ) => {
    let evalList: ref<array<option<mObject>>> = ref([])
    let keep = ref(true)
    args->Option.forEach(arguments => {
      arguments->Array.forEach(arg => {
        if keep.contents {
          let evaluated = evaluateStatement(arg, env)
          if isError(evaluated) {
            keep := false
            evalList := [evaluated]
          } else {
            let evalListContents = evalList.contents
            evalListContents->Array.push(evaluated)
            evalList := evalListContents
          }
        }
      })
    })
    evalList.contents
  }
  and applyFunction = (fun: mObject, args: array<option<mObject>>) => {
    switch fun {
    | MFunction(mFun) => {
        let extendEnv = extendFunctionEnv(mFun, args)
        let evaluated = evaluateStatement(
          mFun.body->Option.map(body => AST.BlockStatement(body)),
          extendEnv,
        )

        evaluated->Option.map(eval => {
          switch eval {
          | MReturnValue({value}) => value
          | _ => eval
          }
        })
      }
    | _ => Some(MError({message: `Not a function: ${fun->typeDesc}`}))
    }
  }
}
