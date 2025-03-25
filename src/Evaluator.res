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
    | _ => MError({message: `unknown operator: -${obj->typeDesc}`})
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
    | (MString({value: leftValue}), "+", MString({value: rightValue})) =>
      MString({value: leftValue ++ rightValue})
    | _ =>
      if left->typeDesc != right->typeDesc {
        MError({message: `type mismatch: ${left->typeDesc} ${operator} ${right->typeDesc}`})
      } else {
        MError({message: `unknown operator: ${left->typeDesc} ${operator} ${right->typeDesc}`})
      }
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

  let evalArrayIndexExpression = (elements: array<option<mObject>>, index: int) => {
    let max = Array.length(elements) + 1
    if index < 0 || index > max {
      Some(cNULL)
    } else {
      elements->Array.getUnsafe(index)
    }
  }

  let evalHashIndexExpression = (pairs: Map.t<string, hashPair>, index: option<mObject>) => {
    switch index {
    | Some(o) =>
      if o->isHashable {
        let pair = pairs->Map.get(o->hashKey)
        switch pair {
        | Some({value}) => Some(value)
        | None => Some(cNULL)
        }
      } else {
        Some(MError({message: `unusable as a hash key: ${o->typeDesc}`}))
      }
    | None => Some(MError({message: "unusable as a hash key: null"}))
    }
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
          if v->Option.isNone {
            let fn = builtins->Map.get(value)
            switch fn {
            | Some(f) => Some(MBuiltinFunction(f))
            | None => Some(MError({message: `identifier not found: ${value}`}))
            }
          } else {
            v
          }
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
      | AST.StringLiteral({value}) => Some(MString({value: value}))
      | AST.IndexExpression({left, index}) => {
          let leftEvaluated = evaluateStatement(left, env)
          if leftEvaluated->isError {
            leftEvaluated
          } else {
            let indexEvaluated = evaluateStatement(index, env)
            if indexEvaluated->isError {
              indexEvaluated
            } else {
              switch (leftEvaluated, indexEvaluated) {
              | (Some(MArray({elements})), Some(MInteger({value}))) =>
                evalArrayIndexExpression(elements, value)
              | (Some(MHash({pairs})), _) => evalHashIndexExpression(pairs, indexEvaluated)
              | _ =>
                Some(
                  MError({
                    message: `index operator not supported: ${leftEvaluated
                      ->Option.map(l => l->typeDesc)
                      ->Option.getOr("")}`,
                  }),
                )
              }
            }
          }
        }
      | AST.HashLiteral({pairs}) => {
          let bodyPairs: Map.t<string, hashPair> = Map.make()
          let keep = ref(true)
          let returnValue: ref<option<mObject>> = ref(None)
          pairs->Map.forEachWithKey((value, key) => {
            if keep.contents {
              let keyEvaluated = evaluateStatement(Some(key), env)
              if keyEvaluated->isError {
                keep := false
                returnValue := keyEvaluated
              } else {
                switch keyEvaluated {
                | Some(k) =>
                  if k->isHashable {
                    let valueEvaluated = evaluateStatement(Some(value), env)
                    if valueEvaluated->isError {
                      keep := false
                      returnValue := valueEvaluated
                    } else {
                      bodyPairs->Map.set(
                        k->hashKey,
                        {key: k, value: valueEvaluated->Option.getUnsafe},
                      )
                    }
                  } else {
                    keep := false
                    returnValue := Some(MError({message: `unusable as a hash key: ${k->typeDesc}`}))
                  }
                | None => {
                    keep := false
                    returnValue := Some(MError({message: `unusable as a hash key: null`}))
                  }
                }
              }
            }
          })
          returnValue.contents->Option.getOr(MHash({pairs: bodyPairs}))->Some
        }
      | AST.ArrayLiteral({elements}) => {
          let evalElements = evalExpressions(elements, env)
          if evalElements->Array.length == 1 && isError(evalElements->Array.getUnsafe(0)) {
            evalElements->Array.getUnsafe(0)
          } else {
            Some(MArray({elements: evalElements}))
          }
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
    | MBuiltinFunction({fn}) => {
        let result = fn(args)
        if result->Option.isSome {
          result
        } else {
          Some(cNULL)
        }
      }
    | _ => Some(MError({message: `Not a function: ${fun->typeDesc}`}))
    }
  }
}
