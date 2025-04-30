open Objects

module Eval: {
  let eval: (AST.program, environment) => option<mObject>
  let cNULL: mObject
  let cTRUE: mObject
  let cFALSE: mObject
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
    switch obj {
    | Some(MError(_) as o) => Some(o)
    | Some(o) => body(o)
    | None => None
    }
  }

  let isError = (obj: option<mObject>) => {
    switch obj {
    | Some(MError(_)) => true
    | _ => false
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
    switch fun.parameters {
    | Some(params) => {
        let i = ref(0)
        let len = params->Array.length
        while i.contents < len {
          let identifier = params->Array.getUnsafe(i.contents)
          let arg = args->Array.getUnsafe(i.contents)->Option.getUnsafe
          env->Environment.set(identifier.value, arg)
          i := i.contents + 1
        }
      }
    | None => ()
    }
    env
  }

  let evalArrayIndexExpression = (elements: array<option<mObject>>, index: int) => {
    let max = Array.length(elements) - 1
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
    let i = ref(0)
    let len = program.statements->Array.length
    while keep.contents && i.contents < len {
      let statement = program.statements->Array.getUnsafe(i.contents)

      result := evaluateStatement(Some(statement), env)

      switch result.contents {
      | Some(MReturnValue({value})) => {
          result := Some(value)
          keep := false
        }
      | Some(MError(_)) => keep := false
      | _ => ()
      }

      i := i.contents + 1
    }
    result.contents
  }
  and evaluateStatement = (statement: option<AST.statement>, env: environment) => {
    switch statement {
    | Some(st) =>
      switch st {
      | AST.Identifier({value}) =>
        switch env->Environment.get(value) {
        | None => {
            let fn = builtins->Map.get(value)
            switch fn {
            | Some(f) => Some(MBuiltinFunction(f))
            | None => Some(MError({message: `identifier not found: ${value}`}))
            }
          }
        | v => v
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
            switch (isTruthy(c), alternative) {
            | (true, _) => {
                let consequence = switch consequence {
                | Some(b) => Some(AST.BlockStatement(b))
                | None => None
                }
                evaluateStatement(consequence, env)
              }
            | (false, Some(alternative)) => evaluateBlockStatement(alternative, env)
            | _ => Some(cNULL)
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
                    message: `index operator not supported: ${switch leftEvaluated {
                      | Some(l) => l->typeDesc
                      | None => ""
                      }}`,
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
          switch returnValue.contents {
          | None => Some(MHash({pairs: bodyPairs}))
          | value => value
          }
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
    switch st.statements {
    | Some(statements) => {
        let i = ref(0)
        let len = statements->Array.length
        while keep.contents && i.contents < len {
          let statement = statements->Array.getUnsafe(i.contents)
          result := evaluateStatement(statement, env)
          switch result.contents {
          | Some(MReturnValue(_) | MError(_)) => keep := false
          | _ => ()
          }
          i := i.contents + 1
        }
      }
    | None => ()
    }
    result.contents
  }
  and evalExpressions: (AST.optionStatementArray, environment) => array<option<mObject>> = (
    args: AST.optionStatementArray,
    env: environment,
  ) => {
    let evalList: ref<array<option<mObject>>> = ref([])
    let keep = ref(true)
    switch args {
    | Some(arguments) => {
        let i = ref(0)
        let len = arguments->Array.length
        while keep.contents && i.contents < len {
          let arg = arguments->Array.getUnsafe(i.contents)
          let evaluated = evaluateStatement(arg, env)
          if isError(evaluated) {
            keep := false
            evalList := [evaluated]
          } else {
            let evalListContents = evalList.contents
            evalListContents->Array.push(evaluated)
            evalList := evalListContents
          }
          i := i.contents + 1
        }
      }
    | None => ()
    }
    evalList.contents
  }
  and applyFunction = (fun: mObject, args: array<option<mObject>>) => {
    switch fun {
    | MFunction(mFun) => {
        let extendEnv = extendFunctionEnv(mFun, args)
        let body = switch mFun.body {
        | Some(body) => Some(AST.BlockStatement(body))
        | None => None
        }
        switch evaluateStatement(body, extendEnv) {
        | Some(MReturnValue({value})) => Some(value)
        | Some(eval) => Some(eval)
        | None => None
        }
      }
    | MBuiltinFunction({fn}) =>
      switch fn(args) {
      | None => Some(cNULL)
      | result => result
      }
    | _ => Some(MError({message: `Not a function: ${fun->typeDesc}`}))
    }
  }
}
