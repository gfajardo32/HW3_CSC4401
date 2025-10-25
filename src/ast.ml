type bop = 
  | Add
  | Mult
  | Leq 

type expr = 
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
  | Var of string 
  | Let of string * expr * expr
  | If of expr * expr * expr