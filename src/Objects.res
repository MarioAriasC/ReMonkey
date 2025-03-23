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
      ->Array.map(elem => elem->Option.map(o => o->inspect)->Option.getOr(""))
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
