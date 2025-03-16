open Objects

module Environment = {
  type rec environment = {store: Map.t<string, mObject>, outer: option<environment>}

  let newEnvironment: unit => environment = () => {
    {store: Map.make(), outer: None}
  }

  let newEnclosedEnvironment: environment => environment = env => {
    {store: Map.make(), outer: Some(env)}
  }

  let set = (env: environment, name: string, value: mObject) => {
    env.store->Map.set(name, value)
  }

  let put = (env: environment, name: string, value: mObject) => {
    env->set(name, value)
    env
  }

  let rec get = (env: environment, name: string) => {
    let obj = env.store->Map.get(name)
    if obj->Option.isNone && env.outer->Option.isSome {
      env.outer->Option.getUnsafe->get(name)
    } else {
      obj
    }
  }
}

module Eval: {
  let eval: (AST.program, Environment.environment) => option<mObject>
} = {
  let cTRUE = MBoolean({value: true})
  let cFALSE = MBoolean({value: false})

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

  let notNone = (obj: option<mObject>, body: mObject => option<mObject>) => {
    switch obj {
    | Some(o) => body(o)
    | None => Some(MError({message: "obj is None"}))
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

  let rec eval = (program: AST.program, env: Environment.environment) => {
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
  and evaluateStatement = (statement: option<AST.statement>, env: Environment.environment) => {
    switch statement {
    | Some(st) =>
      switch st {
      | AST.IntegerLiteral(i) => Some(Objects.MInteger({value: i.value}))
      | AST.InfixExpression({left, operator, right}) =>
        ifNotError(evaluateStatement(left, env), l => {
          ifNotError(evaluateStatement(right, env), r => {
            evalInfixExpression(operator, l, r)
          })
        })
      | AST.ExpressionStatement({expression}) => evaluateStatement(expression, env)
      | AST.PrefixExpression({operator, right}) =>
        ifNotError(evaluateStatement(right, env), r => {
          switch operator {
          // | "!" => evalBangOperatorExpression(r)
          | "-" => evalMinusPrefixOperatorExpression(r)
          | _ => Some(MError({message: `Unknown operator: ${operator}${r->typeDesc}`}))
          }
        })
      | AST.BooleanLiteral({value}) => Some(value->boolToMonkey)
      | _ => {
          Js.log(%raw("statement.TAG"))
          None
        }
      }
    | None => raise(Failure("statement shouldn't be None"))
    }
  }
}
