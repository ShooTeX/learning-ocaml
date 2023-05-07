(* Chapter 2 *)
(* Exercise: values *)
let (_ : int) = 7 * (1 + 2 + 3)
let (_ : string) = "CS " ^ string_of_int 3110

(* Exercise: operators *)
let _ = 42 * 10
let _ = 3.14 *. 2.
let _ = 4.2 ** 7.

(* Exercise: equality *)
let _ = 42 = 42 (* true *)
let _ = "hi" = "hi" (* true *)
let _ = "hi" == "hi" (* false *)

(* Exercise: assert *)
let _ = assert true
let _ = assert (2110 <> 3110)

(* Exercise: if *)
let _ = if 2 > 1 then 42 else 7

(* Exercise: double fun *)
let double x = x * 2
let _ = assert (double 5 = 10)
let _ = assert (double 0 = 0)
let _ = assert (double (-1) = -2)

(* Exercise: more fun *)
let cube x = x *. x *. x
let _ = assert (cube 0. = 0.)
let _ = assert (cube 1. = 1.)
let _ = assert (cube 42. = 74088.)
let sign x = if x > 0 then 1 else if x < 0 then -1 else 0
let _ = assert (sign 55 = 1)
let _ = assert (sign (-55) = -1)
let _ = assert (sign 0 = 0)
