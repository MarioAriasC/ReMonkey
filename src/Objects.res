type mValue<'a> = {value: 'a}
type mInteger = {...mValue<int>}
type mError = {message: string}
type mBoolean = {...mValue<bool>}
type mString = {...mValue<string>}
type rec mObject =
  | MInteger(mInteger)
  | MReturnValue(mReturnValue)
  | MBoolean(mBoolean)
  | MError(mError)
  | MNull
  | MFunction(mFunction)
  | MString(mString)
  | MArray(mArray)
  | MHash(mHash)
  | MBuiltinFunction(mBuiltinFunction)
and mReturnValue = {value: mObject}
and environment = {store: Map.t<string, mObject>, outer: option<environment>}
and mFunction = {
  parameters: option<array<AST.identifier>>,
  body: option<AST.blockStatement>,
  env: environment,
}
and mArray = {elements: array<option<mObject>>}
and hashPair = {key: mObject, value: mObject}
and mHash = {pairs: Map.t<string, hashPair>}
and mBuiltinFunction = {fn: array<option<mObject>> => option<mObject>}

let isHashable = (o: mObject) => {
  switch o {
  | MInteger(_) => true
  | MString(_) => true
  | MBoolean(_) => true
  | _ => false
  }
}

let rec inspect = (o: mObject) => {
  switch o {
  | MInteger({value}) => String.make(value)
  | MBoolean({value}) => String.make(value)
  | MReturnValue({value}) => value->inspect
  | MError({message}) => `ERROR: ${message}`
  | MNull => "null"
  | MFunction({parameters, body}) => {
      let parametersString =
        parameters
        ->Option.map(params =>
          params->Array.map(param => AST.Identifier(param)->AST.Statement.toString)
        )
        ->Option.map(params => params->Array.join(", "))
        ->Option.getOr("")
      let bodyString =
        body->Option.map(b => AST.BlockStatement(b)->AST.Statement.toString)->Option.getOr("")
      `fn(${parametersString}) {\n\t${bodyString}\n}`
    }
  | MString({value}) => value
  | MArray({elements}) =>
    `[${elements
      ->Array.map(elem => elem->Option.map(inspect)->Option.getOr(""))
      ->Array.join(", ")}]`
  | MHash({pairs}) => {
      let values =
        pairs
        ->Map.values
        ->Iterator.toArrayWithMapper(entry => {
          `${entry.key->inspect}: ${entry.value->inspect}`
        })
      `{${values->Array.join(", ")}}`
    }
  | MBuiltinFunction(_) => "builtin function"
  }
}

and toString = (o: mObject) => {
  switch o {
  | MInteger({value}) => `MInteger(value=${String.make(value)})`
  | MBoolean({value}) => `MBoolean(value=${String.make(value)})`
  | MReturnValue({value}) => `MReturn(value=${value->toString})`
  | MError({message}) => `MError(message=${message})`
  | MNull => "MNull"
  | MFunction(_) => "MFunction"
  | MString({value}) => `MString(value=${value})`
  | MArray(_) => "MArray"
  | MHash(_) => "MHash"
  | MBuiltinFunction(_) => "MBuiltinFunction"
  }
}

let typeDesc = (o: mObject) => {
  %raw("o.TAG")
}

let hashKey = (o: mObject) => {
  switch o {
  | MInteger({value}) => String.make(value)
  | MString({value}) => value
  | MBoolean({value}) => String.make(value)
  | _ => raise(Invalid_argument(`${o->typeDesc} doesn't implement hashKey`))
  }
}

let cLEN = "len"
let cPUSH = "push"
let cFIRST = "first"
let cLAST = "last"
let cREST = "rest"
let argSizeCheck = (
  expectedSize: int,
  args: array<option<mObject>>,
  body: array<option<mObject>> => option<mObject>,
) => {
  let length = Array.length(args)
  if length != expectedSize {
    Some(
      MError({
        message: `wrong number of arguments. got=${String.make(length)}, want=${String.make(
            expectedSize,
          )}`,
      }),
    )
  } else {
    body(args)
  }
}

let arrayCheck = (
  name: string,
  args: array<option<mObject>>,
  body: (mArray, int) => option<mObject>,
) => {
  let first = args->Array.getUnsafe(0)
  switch first {
  | Some(f) =>
    switch f {
    | MArray(arr) => body(arr, arr.elements->Array.length)
    | _ => Some(MError({message: `argument to "${name}" must be ARRAY, got ${f->typeDesc}`}))
    }
  | None => Some(MError({message: `argument to "${name}" must be ARRAY, got null`}))
  }
}

let builtins: Map.t<string, mBuiltinFunction> = Map.fromArray([
  (
    cLEN,
    {
      fn: args =>
        argSizeCheck(1, args, it => {
          it
          ->Array.getUnsafe(0)
          ->Option.map(arg => {
            switch arg {
            | MString({value}) => MInteger({value: String.length(value)})
            | MArray({elements}) => MInteger({value: Array.length(elements)})
            | _ => MError({message: `argument to "len" not supported, got ${arg->typeDesc}`})
            }
          })
        }),
    },
  ),
  (
    cPUSH,
    {
      fn: args => {
        argSizeCheck(2, args, it => {
          arrayCheck(cPUSH, it, (arr, _) => {
            let elements = arr.elements
            // %raw("console.dir(args)")
            elements->Array.push(args->Array.getUnsafe(1))
            // %raw("console.dir(elements)")
            Some(MArray({elements: elements}))
          })
        })
      },
    },
  ),
  (
    cFIRST,
    {
      fn: args => {
        argSizeCheck(1, args, it => {
          arrayCheck(cFIRST, it, (arr, length) => {
            if length > 0 {
              arr.elements->Array.getUnsafe(0)
            } else {
              None
            }
          })
        })
      },
    },
  ),
  (
    cLAST,
    {
      fn: args => {
        argSizeCheck(1, args, it => {
          arrayCheck(cLAST, it, (arr, length) => {
            if length > 0 {
              arr.elements->Array.getUnsafe(length - 1)
            } else {
              None
            }
          })
        })
      },
    },
  ),
  (
    cREST,
    {
      fn: args => {
        argSizeCheck(1, args, it => {
          arrayCheck(cREST, it, (arr, length) => {
            if length > 0 {
              Some(MArray({elements: arr.elements->Array.sliceToEnd(~start=1)}))
            } else {
              None
            }
          })
        })
      },
    },
  ),
])
