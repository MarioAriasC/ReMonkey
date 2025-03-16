type mValue<'a> = {value: 'a}
type mInteger = {...mValue<int>}
type mError = {message: string}
type mBoolean = {...mValue<bool>}

type rec mObject =
  | MInteger(mInteger)
  | MReturnValue(mReturnValue)
  | MBoolean(mBoolean)
  | MError(mError)
and mReturnValue = {value: mObject}

let rec inspect = (o: mObject) => {
  switch o {
  | MInteger({value}) => String.make(value)
  | MBoolean({value}) => String.make(value)
  | MReturnValue({value}) => value->inspect
  | MError({message}) => `ERROR: ${message}`
  }
}

let rec toString = (o: mObject) => {
  switch o {
  | MInteger({value}) => `MInteger(value=${String.make(value)})`
  | MBoolean({value}) => `MBoolean(value=${String.make(value)})`
  | MReturnValue({value}) => `MReturn(value=${value->toString})`
  | MError({message}) => `MError(message=${message})`
  }
}

let typeDesc = (o: mObject) => {
  %raw("o.TAG")
}
