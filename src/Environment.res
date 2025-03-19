open Objects
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
  value
}

let rec get = (env: environment, name: string) => {
  let obj = env.store->Map.get(name)
  if obj->Option.isNone && env.outer->Option.isSome {
    env.outer->Option.getUnsafe->get(name)
  } else {
    obj
  }
}
