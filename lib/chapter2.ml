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
let area r = Float.pi *. (r ** 2.)
let close_enough a b = Float.abs (a -. b) < 1e-5
let _ = assert (close_enough (area 1.) Float.pi)
let _ = assert (close_enough (area (Float.sqrt (1. /. Float.pi))) 1.)
let _ = assert (close_enough (area 5.) 78.5398163397)

(* Exercise: RMS *)
let rms x y = sqrt (((x ** 2.) +. (y ** 2.)) /. 2.)
let _ = assert (close_enough (rms 3. 4.) 3.5355339059327378)
let _ = assert (close_enough (rms 42. 42.) 42.)
let _ = assert (close_enough (rms 0. 0.) 0.)

(* Exercise: date fun *)
(* I do not use pattern matching because it hasn't been explained yet *)
let valid_date d m =
  let days_in_month =
    if
      m = "Jan" || m = "Mar" || m = "Jul" || m = "Aug" || m = "Oct" || m = "Dec"
    then 31
    else if m = "Apr" || m = "Jun" || m = "Sep" || m = "Nov" then 30
    else if m = "Feb" then 28
    else 0
  in
  d >= 1 && d <= days_in_month

let _ = assert (valid_date 100 "Jan" = false)
let _ = assert (valid_date 0 "Mar" = false)
let _ = assert (valid_date 1 "Feb" = true)
let _ = assert (valid_date 28 "Feb" = true)

(* Exercise: fib *)
let rec fib n =
  if n = 0 then 0 else if n = 1 || n = 2 then 1 else fib (n - 1) + fib (n - 2)

let _ = assert (fib 7 = 13)
let _ = assert (fib 3 = 2)

(* Exercise: fib fast *)
let rec h n pp p = if n = 1 then p else h (n - 1) p (pp + p)
let fib_fast n = if n = 0 then 0 else h n 0 1
let _ = assert (fib_fast 7 = 13)
let _ = assert (fib_fast 3 = 2)
let _ = assert (fib_fast 42 = 267914296)

(* Exercise: poly types *)
let (f : bool -> bool) = fun x -> if x then x else x
(* x must be a bool since it's used as a conditional*)

let (g : 'a -> bool -> 'a) = fun x y -> if y then x else x
(* x is any *)

let (h : bool -> 'a -> 'a -> 'a) = fun x y z -> if x then y else z
(* y and z must have same type *)

let (i : bool -> 'a -> 'b -> 'a) = fun x y _z -> if x then y else y
(* z could be different than y *)

(* Exercise: divide *)
let divide ~numerator ~denominator = numerator /. denominator
let _ = assert (divide ~denominator:2. ~numerator:4. = 2.)

(* Exercise: associativity *)
(* 1: int *)
(* 2: fun *)
(* 3: int *)
(* 4: error (5 is not a function) *)

(* Exercise: average *)
let ( +/. ) a b = (a +. b) /. 2.
let _ = assert (1.0 +/. 2.0 = 1.5)
let _ = assert (0. +/. 0. = 0.);;

(* Exercise: hello world *)

print_endline "Hello world!";;
print_string "Hello world!"
