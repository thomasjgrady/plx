let x = 5

let f x = x + 1
let two = f 1

let mul x y = x * y
let mul_2 = mul 2

let seq x =
  let v = x + 1 in
  f v

type my_string = string
let s : my_string = "hello"

type my_enum =
  | a of nat
  | b of string

let my_enum_value_a = a 1
let my_enum_value_a_ns = my_enum.a 1

type my_tuple = (nat, string)
let my_tuple_value : my_tuple = (1, false)

let id (t : type) (x : t) = x
let id_nat = id nat

type my_type_cons (t : type) = match t with
  | nat -> nat
  | _ -> string

type my_dep_type (b : bool) = b ? nat : string