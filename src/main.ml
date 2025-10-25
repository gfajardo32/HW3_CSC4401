(* File: basics.ml 
   Hw 3 OCaml Basics
   Author: Guido Fajardo
   Date: 10/25/2025
   The goal of this homework is to get familiar with developing a programming language in OCaml. 
   In this homework, I will extend the CalcPL initial version.
*)


open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** The error message produced if a variable is unbound. *)
let unbound_var_err = "Unbound variable"

(** The error message produced if binary operators and their
    operands do not have the correct types. *)
let bop_err = "Operator and operand type mismatch"

(** The error message produced if the guard
    of an [if] does not have type [bool]. *)
let if_guard_err = "Guard of if must have type bool"

(** [string_of_val e] converts [e] to a string.
    Requires: [e] is a value. *)
let string_of_val (e : expr) : string =
  match e with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | _ -> failwith "precondition violated"

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Int _ | Bool _ -> true
  | Var _ | Binop _ | Let _ | If _ -> false

let rec subst e v x =
  match e with
  | Var y -> if x = y then v else e
  | Bool _ -> e
  | Int _ -> e
  | Binop (bop, e1, e2) -> Binop (bop, subst e1 v x, subst e2 v x)
  | Let (y, e1, e2) ->
      let e1' = subst e1 v x in
      if x = y
      then Let (y, e1', e2)
      else Let (y, e1', subst e2 v x)
  | If (e1, e2, e3) ->
      If (subst e1 v x, subst e2 v x, subst e3 v x)

(** [step e] takes a single step of evaluation of [e]. *)
let rec step : expr -> expr = function
  | Int _ | Bool _ -> failwith "Does not step"
  | Var _ -> failwith unbound_var_err

  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, e1, e2) -> Let (x, step e1, e2)

  | If (Bool true, e2, _) -> e2
  | If (Bool false, _, e3) -> e3
  | If (Int _, _, _) -> failwith if_guard_err
  | If (e1, e2, e3) -> If (step e1, e2, e3)

  | Binop (bop, e1, e2) when is_value e1 && is_value e2 ->
      step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 ->
      Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) ->
      Binop (bop, step e1, e2)

(** [step_bop bop v1 v2] implements the primitive operation
    [v1 bop v2].  Requires: [v1] and [v2] are both values. *)
and step_bop bop e1 e2 =
  match bop, e1, e2 with
  | Add,  Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq,  Int a, Int b -> Bool (a <= b)
  | _ -> failwith bop_err

(** [eval e] fully evaluates [e] to a value [v]. *)
let rec eval (e : expr) : expr =
  if is_value e then e else
    e |> step |> eval

(** [interp s] interprets [s] by lexing and parsing,
    evaluating, and converting the result to string. *)
let interp (s : string) : expr =
  s |> parse |> eval