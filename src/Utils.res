let also: 'a. ('a, 'a => unit) => 'a = (a, body) => {
  body(a)
  a
}
