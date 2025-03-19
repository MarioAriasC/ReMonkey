type mValue<'a> = {value: 'a}
type mInteger = {...mValue<int>}
type mError = {message: string}
type mBoolean = {...mValue<bool>}
type rec mObject =
  | MInteger(mInteger)
  | MReturnValue(mReturnValue)
  | MBoolean(mBoolean)
  | MError(mError)
  | MNull
  | MFunction(mFunction)
and mReturnValue = {value: mObject}
and environment = {store: Map.t<string, mObject>, outer: option<environment>}
and mFunction = {
  parameters: option<array<AST.identifier>>,
  body: option<AST.blockStatement>,
  env: environment,
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
  }
}

let rec toString = (o: mObject) => {
  switch o {
  | MInteger({value}) => `MInteger(value=${String.make(value)})`
  | MBoolean({value}) => `MBoolean(value=${String.make(value)})`
  | MReturnValue({value}) => `MReturn(value=${value->toString})`
  | MError({message}) => `MError(message=${message})`
  | MNull => "MNull"
  | MFunction(_) => "MFunction"
  }
}

let typeDesc = (o: mObject) => {
  %raw("o.TAG")
}
