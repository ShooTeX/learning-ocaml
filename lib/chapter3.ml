open Base
open Stdlib
(* *** *)

let rec from i j l = if i > j then l else from i (j - 1) (j :: l)
let ( -- ) i j = from i j []

(* *** *)

(* Chapter 3 *)
(* Exercise: list expressions *)
let lst = [ 1; 2; 3; 4; 5 ]

(* formatter formats this automatically *)
(* let lst2 = 1 :: 2 :: 3 :: 4 :: 5 :: [] *)

let lst3 = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

(* Exercise: product *)
let rec product = function [] -> 1 | h :: t -> h * product t

(* Exercise: concat *)
let rec concat = function [] -> "" | h :: t -> h ^ concat t

(* Exercise: patterns *)
let first_element_bigred = function "bigred" :: _ -> true | _ -> false

let exactly_two_or_four = function
  | [ _; _ ] -> true
  | [ _; _; _; _ ] -> true
  | _ -> false

let first_two_equal = function a :: b :: _ -> a = b | _ -> false

(* Exercise: library *)
let fifth_el lst = match List.nth_opt lst 4 with Some i -> i | None -> 0
let sort_list lst = lst |> List.sort Stdlib.compare |> List.rev

(* Exercise: library puzzle *)
let last_element lst = lst |> List.rev |> List.hd
let any_zeroes lst = List.exists (fun x -> x = 0) lst

(* Exercise: take drop *)
let rec take n lst =
  if n = 0 then []
  else match lst with [] -> [] | h :: t -> h :: take (n - 1) t

let rec drop n lst =
  if n = 0 then lst else match lst with [] -> [] | _ :: t -> drop (n - 1) t

(* Exercise: take drop tail *)
let rec take_rev n xs acc =
  if n = 0 then acc
  else match xs with [] -> acc | h :: t -> take_rev (n - 1) t (h :: acc)

let take' n lst = take_rev n lst [] |> List.rev
let drop' = drop

(* Exercise: unimodal *)
let rec is_mon_dec = function
  | [] | [ _ ] -> true
  | a :: b :: t -> a >= b && is_mon_dec t

let rec is_unimodel = function
  | [] | [ _ ] -> true
  | a :: b :: t as lst -> if a <= b then is_unimodel t else is_mon_dec lst

(* Exercise: powerset *)
let rec powerset = function
  | [] -> [ [] ]
  | h :: t ->
      let p = powerset t in
      let subsets = List.map (List.cons h) p @ p in
      List.sort (fun a b -> compare (List.length a) (List.length b)) subsets

(* Exercise: print int list rec *)
let rec print_int_list = function
  | [] -> ()
  | h :: t ->
      print_endline (string_of_int h);
      print_int_list t

let print_int_list' lst =
  List.iter (fun x -> print_endline (string_of_int x)) lst

(* Exercise: student *)
type student = { first_name : string; last_name : string; gpa : float }

let student = { first_name = "Erik"; last_name = "Simon"; gpa = 0.0 }
let get_full_name student = (student.first_name, student.last_name)
let create_student first_name last_name gpa = { first_name; last_name; gpa }

(* Exercise: pokerecord *)
type poketype = Normal | Fire | Water [@@deriving compare, sexp]

type pokemon = { name : string; hp : int; ptype : poketype }
[@@deriving compare, sexp]

let charizard = { name = "charizard"; hp = 78; ptype = Fire }
let squirtle = { name = "squirtle"; hp = 44; ptype = Water }

(* Exercise: safe hd and tl *)
let safe_hd = function x :: _ -> Some x | _ -> None
let safe_tl = function _ :: x -> Some x | _ -> None

(* Exercise: pokefun *)
let rec max_hp = function
  | [] -> None
  | a :: t -> (
      match max_hp t with
      | None -> Some a
      | Some b -> Some (if a.hp >= b.hp then a else b))

(* Exercise: date before *)
type date = int * int * int

(************ tests ************)
let%test_unit "Exercise: list expressions" =
  [%test_eq: int list] lst [ 1; 2; 3; 4; 5 ];
  [%test_eq: int list] lst3 [ 1; 2; 3; 4; 5 ]

let%test_unit "Exercise: product" =
  [%test_eq: int] (product [ 1; 2; 3; 4; 5 ]) 120;
  [%test_eq: int] (product [ 5 ]) 5;
  [%test_eq: int] (product []) 1

let%test_unit "Exercise: concat" =
  [%test_eq: string] (concat [ "Hello "; "World" ]) "Hello World";
  [%test_eq: string] (concat [ "Hello" ]) "Hello";
  [%test_eq: string] (concat []) ""

let%test_unit "Exercise: patterns first_element_bigred" =
  [%test_eq: bool] (first_element_bigred [ "bigred" ]) true;
  [%test_eq: bool] (first_element_bigred [ "bigred"; "World" ]) true;
  [%test_eq: bool] (first_element_bigred []) false;
  [%test_eq: bool] (first_element_bigred [ "World"; "bigred" ]) false;

  [%test_eq: bool] (exactly_two_or_four [ "bigred"; "world" ]) true;
  [%test_eq: bool] (exactly_two_or_four [ 1; 2 ]) true;
  [%test_eq: bool]
    (exactly_two_or_four [ "bigred"; "World"; "two"; "four" ])
    true;
  [%test_eq: bool] (exactly_two_or_four []) false;
  [%test_eq: bool] (exactly_two_or_four [ "World"; "bigred"; "three" ]) false;

  [%test_eq: bool] (first_two_equal [ 1; 1 ]) true;
  [%test_eq: bool] (first_two_equal [ 2; 2; 3 ]) true;
  [%test_eq: bool] (first_two_equal [ 3; 2 ]) false;
  [%test_eq: bool] (first_two_equal [ 3; 2; 2 ]) false;
  [%test_eq: bool] (first_two_equal []) false

let%test_unit "Exercise: library" =
  [%test_eq: int] (fifth_el [ 1; 2; 3; 4; 5; 6 ]) 5;
  [%test_eq: int] (fifth_el [ 1; 2; 3; 4 ]) 0;

  [%test_eq: int list] (sort_list [ 8; 4; 5; 42 ]) [ 42; 8; 5; 4 ]

let%test_unit "Exercise: library puzzle" =
  [%test_eq: int] (last_element [ 1; 2; 3; 4; 5 ]) 5;
  [%test_eq: string] (last_element [ "hello"; "world" ]) "world";

  [%test_eq: bool] (any_zeroes [ 1; 0 ]) true;
  [%test_eq: bool] (any_zeroes [ 1; 2 ]) false

let%test_unit "Exercise: take drop" =
  [%test_eq: int list] (take 3 (List.init 6 Fun.id)) [ 0; 1; 2 ];
  [%test_eq: int list] (take 7 (List.init 6 Fun.id)) [ 0; 1; 2; 3; 4; 5 ];
  [%test_eq: int list] (take 0 (List.init 6 Fun.id)) [];

  [%test_eq: int list] (drop 3 (List.init 6 Fun.id)) [ 3; 4; 5 ];
  [%test_eq: int list] (drop 7 (List.init 6 Fun.id)) [];
  [%test_eq: int list] (drop 0 (List.init 6 Fun.id)) [ 0; 1; 2; 3; 4; 5 ]

let%test_unit "Exercise: take drop tail" =
  [%test_eq: int list]
    (take' 100_000 (List.init 1_100_000 Fun.id))
    (List.init 100_000 Fun.id);
  [%test_eq: int list] (drop' 100_000 (0 -- 1100000)) (100_000 -- 1100000)

let%test_unit "Exercise: unimodel" =
  [%test_eq: bool] (is_unimodel (0 -- 100)) true;
  [%test_eq: bool] (is_unimodel (100 -- 0)) true;
  [%test_eq: bool] (is_unimodel ((0 -- 100) @ (100 -- 0))) true;
  [%test_eq: bool] (is_unimodel ((100 -- 0) @ (0 -- 100))) true;
  [%test_eq: bool] (is_unimodel [ List.init 6 (fun _ -> 1) ]) true;
  [%test_eq: bool] (is_unimodel []) true;
  [%test_eq: bool] (is_unimodel [ 1; 0; 3; 4; 2 ]) false

let%test_unit "Exercise: powerset" =
  [%test_eq: int list list] (powerset [ 1; 2 ]) [ []; [ 1 ]; [ 2 ]; [ 1; 2 ] ];
  [%test_eq: int list list]
    (powerset [ 3; 4; 5 ])
    [ []; [ 3 ]; [ 4 ]; [ 5 ]; [ 3; 4 ]; [ 3; 5 ]; [ 4; 5 ]; [ 3; 4; 5 ] ]

let%expect_test "Exercise: print int list rec" =
  print_int_list (0 -- 6);
  [%expect {|
    0
    1
    2
    3
    4
    5
    6 |}]

let%expect_test "Exercise: print int list iter" =
  print_int_list' (0 -- 6);
  [%expect {|
    0
    1
    2
    3
    4
    5
    6 |}]

let%test_unit " Exercise: safe hd and tl " =
  [%test_eq: int option] (safe_hd [ 1; 2 ]) (Some 1);
  [%test_eq: int option] (safe_hd []) None;

  [%test_eq: int list option] (safe_tl [ 1; 2 ]) (Some [ 2 ]);
  [%test_eq: int option] (safe_hd []) None

let%test_unit "Exercise: pokefun" =
  let other = { name = "other"; hp = 100; ptype = Normal } in
  [%test_eq: pokemon option]
    (max_hp [ charizard; squirtle; other ])
    (Some other);
  [%test_eq: pokemon option]
    (max_hp [ other; squirtle; charizard ])
    (Some other);
  [%test_eq: pokemon option] (max_hp []) None
