open Base
open Stdlib
open Printf

(* chapter 4 *)
(* Exercise: twice, no arguments *)
let (double : int -> int) = fun x -> 2 * x
let (square : int -> int) = fun x -> x * x
let (twice : ('a -> 'a) -> 'a -> 'a) = fun f x -> f (f x)
let (quad : int -> int) = twice double
let (fourth : int -> int) = twice square
(* quad and fourth are Higher Order Functions *)

(* Exercise: mystery operator 1 *)
let ( $ ) f x = f x

(* square 2 + 2 *)
(* == (square 2) + 2 *)

(* square $ 2 + 2 *)
(* == square (2 + 2) *)

(* Exercise: mystery operator 2 *)
let ( @@ ) f g x = x |> g |> f

(* function composition *)
(* (String.length @@ string_of_int) 10 *)
(* == 2 *)

(* Exercise: repeat *)
let rec repeat f n x = match n with 0 -> x | _ -> repeat f (n - 1) (f x)

(* Exercise: product *)
let product_left = List.fold_left ( *. ) 1.
let product_right = ListLabels.fold_right ~f:( *. ) ~init:1.

(* Exercise: sum_cube_odd *)
(* Exercise: sum_cube_odd pipeline *)
let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)

let sum_cube_odd n =
  1 -- n
  |> List.filter (fun x -> x mod 2 <> 0)
  |> List.map (fun x -> x * x * x)
  |> List.fold_left ( + ) 0

(* Exercise: exists *)
let rec exists_rec p = function
  | [] -> false
  | x :: xs -> if p x then true else exists_rec p xs

let exists_fold p = List.fold_left (fun acc x -> acc || p x) false
let exists_lib = List.exists

(* Exercise: account balance *)
let account_balance_fold_left x = List.fold_left (fun acc x -> acc - x) x

let account_balance_fold_right x =
  ListLabels.fold_right ~f:(fun acc x -> x - acc) ~init:x

let rec account_balance_rec x = function
  | [] -> x
  | hd :: tl -> account_balance_rec (x - hd) tl

(* Exercise: library uncurried *)
let uncurried_nth (lst, n) = List.nth lst n
let uncurried_append (lst1, lst2) = List.append lst1 lst2
let uncurried_compare (x, y) = Char.compare x y
let uncurried_max (x, y) = Stdlib.max x y

(* Exercise: map composition *)
(* using @@ defined above *)

(* Exercise: more list fun *)
let find_greater_three = List.filter (fun x -> String.length x > 3)
let add_one = List.map (fun x -> x +. 1.0)

let join strs sep =
  match strs with
  | [] -> ""
  | hd :: tl -> List.fold_left (fun acc x -> acc ^ sep ^ x) hd tl

(* Exercise: association list keys *)
let keys lst = lst |> List.rev_map fst |> List.sort_uniq String.compare

(* Exercise: valid matrix *)
let valid_matrix = function
  | [] -> false
  | r :: rows ->
      let cols = List.length r in
      List.for_all (fun r' -> List.length r' = cols) rows

(* Exercise: row vector add *)
let add_row_vectors = List.map2 ( + )

(* Exercise: matrix add *)
let add_matrices = List.map2 add_row_vectors

(* Exercise: matrix multiply *)
(* TODO: "matrix multiply" *)

(*******************************)
(************ TESTS ************)
(*******************************)
let%expect_test "Exercise: repeat" =
  printf "%d " (repeat double 2 5);
  printf "%d " (repeat square 2 5);
  printf "%d " (repeat double 6 6);
  [%expect {| 20 625 384 |}]

let%expect_test "Exercise: product" =
  printf "product_left: %f %f %f\n"
    (product_left [ 1.1; 2.2; 3.3; 4.; 5. ])
    (product_left [ 0.; 0. ])
    (product_left [ 5. ]);
  printf "product_right: %f %f %f"
    (product_right [ 1.1; 2.2; 3.3; 4.; 5. ])
    (product_right [ 0.; 0. ])
    (product_right [ 5. ]);
  [%expect
    {|
    product_left: 159.720000 0.000000 5.000000
    product_right: 159.720000 0.000000 5.000000 |}]

let%expect_test "Exercise: sum_cube_odd" =
  printf "%d" (sum_cube_odd 10);
  [%expect {| 1225 |}]

let%expect_test "Exercise: exists" =
  printf "exists_rec: %B %B\n"
    (exists_rec (fun x -> x = 2) (1 -- 5))
    (exists_rec (fun x -> x = 2) (3 -- 5));
  printf "exists_fold: %B %B\n"
    (exists_fold (fun x -> x = 2) (1 -- 5))
    (exists_fold (fun x -> x = 2) (3 -- 5));
  printf "exists_lib: %B %B\n"
    (exists_lib (fun x -> x = 2) (1 -- 5))
    (exists_lib (fun x -> x = 2) (3 -- 5));
  [%expect
    {|
    exists_rec: true false
    exists_fold: true false
    exists_lib: true false |}]

let%expect_test "Exercise: account balance" =
  printf "account_balance_fold_left: %d\n"
    (account_balance_fold_left 5000 [ 1000; 2000 ]);
  printf "account_balance_fold_right: %d\n"
    (account_balance_fold_right 5000 [ 1000; 2000 ]);
  printf "account_balance_rec: %d\n" (account_balance_rec 5000 [ 1000; 2000 ]);
  [%expect
    {|
    account_balance_fold_left: 2000
    account_balance_fold_right: 2000
    account_balance_rec: 2000 |}]

let%expect_test "Exercise: more list fun" =
  [%sexp_of: string list] (find_greater_three [ "hello"; "no"; "world" ])
  |> Sexp.to_string_hum |> print_endline;
  [%sexp_of: float list] (add_one [ 1.4; 2.4; 3.4 ])
  |> Sexp.to_string_hum |> print_endline;
  join [ "hello"; "world!" ] " " |> print_endline;
  [%expect {|
    (hello world)
    (2.4 3.4 4.4)
    hello world! |}]

let%expect_test "Exercise: association list keys" =
  keys [ ("a", 1); ("b", 2); ("c", 3); ("c", 3) ] |> List.iter (printf "%s ");
  [%expect {| a b c |}]

let%expect_test "Exercise: valid_matrix" =
  [%sexp_of: bool] (valid_matrix []) |> Sexp.to_string_hum |> print_endline;
  [%sexp_of: bool] (valid_matrix [ [ 1; 2 ]; [ 1 ] ])
  |> Sexp.to_string_hum |> print_endline;
  [%sexp_of: bool] (valid_matrix [ [ 1; 2 ]; [ 1; 2 ] ])
  |> Sexp.to_string_hum |> print_endline;
  [%sexp_of: bool] (valid_matrix [ [ 1 ] ])
  |> Sexp.to_string_hum |> print_endline;
  [%expect {|
    false
    false
    true
    true |}]

let%expect_test "Exercise: row vector add" =
  [%sexp_of: int list] (add_row_vectors [ 1; 2; 3 ] [ 4; 5; 6 ])
  |> Sexp.to_string_hum |> print_endline;
  [%expect {|
    (5 7 9) |}]

let%expect_test "Exercise: matrix add" =
  [%sexp_of: int list list]
    (add_matrices [ [ 1; 2 ]; [ 3; 4 ] ] [ [ 5; 6 ]; [ 7; 8 ] ])
  |> Sexp.to_string_hum |> print_endline;
  [%expect {|
    ((6 8) (10 12)) |}]
