open OUnit2
open Interp
open Ast
open Main

(** [make_i n i s] makes an OUnit test named [n] that expects
    [s] to evalute to [Int i]. *)
let make_i n i s =
  n >:: (fun _ -> assert_equal (Int i) (interp s))

(** [make_b n b s] makes an OUnit test named [n] that expects
    [s] to evaluate to [Bool b]. *)
let make_b n b s =
  n >:: (fun _ -> assert_equal (Bool b) (interp s))

(** [make_t n s] makes an OUnit test named [n] that expects
    [s] to fail type checking with error string [s']. *)
let make_t n s' s =
  n >:: (fun _ -> assert_raises (Failure s') (fun () -> interp s))

let tests = [
  make_i "int" 22 "22";
  make_i "add" 22 "11+11";
  make_i "adds" 22 "(10+1)+(5+6)";
  make_i "let" 22 "let x=22 in x";
  make_i "lets" 22 "let x = 0 in let x = 22 in x";
  make_i "mul1" 22 "2*11";
  make_i "mul2" 22 "2+2*10";
  make_i "mul3" 14 "2*2+10";
  make_i "mul4" 40 "2*2*10";
  make_i "if1" 22 "if true then 22 else 0";
  make_b "true" true "true";
  make_b "leq" true "1<=1";
  make_i "if2" 22 "if 1+2 <= 3+4 then 22 else 0";
  make_i "if3" 22 "if 1+2 <= 3*4 then let x = 22 in x else 0";
  make_i "letif" 22 "let x = 1+2 <= 3*4 in if x then 22 else 0";
  make_t "invalid plus" bop_err "1 + true";
  make_t "invalid mult" bop_err "1 * false";
  make_t "invalid leq" bop_err "true <= 1";
  make_t "invalid guard" if_guard_err "if 1 then 2 else 3";
  make_t "unbound" unbound_var_err "x";

(* === Student tests === *)

(* Test: a boolean inside a let statement *)
make_b "bool_let_true" true "let t = true in t";

(* Test: less than or equal with negative numbers *)
make_b "leq_negatives" true "-3<=-2";

(* Test: using let twice and changing the value *)
make_i "let_shadow" 3 "let x = 1 in let x = x + 2 in x";

(* Test: if-then-else with a math condition *)
make_i "if_eq_guard" 9 "if 2*3 <= 5+1 then 9 else 0";

(* Test: adding a number and a boolean (should give an error) *)
make_t "invalid_plus_flipped" bop_err "true + 1";
]

let _ = run_test_tt_main ("suite" >::: tests)
