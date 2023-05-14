(* chapter 5 *)
(* Exercise: complex synonym *)

module type ComplexSig = sig
  type t = float * float

  val zero : t
  val add : t -> t -> t
end

(* Exercise: complex encapsulation *)
module Complex : ComplexSig = struct
  type t = float * float

  (* removing this line will cause an error since it's required by the signature *)
  (* setting it to 0, 0 will result in type mismatch int * int !== float * float *)
  let zero = (0., 0.)

  (* removing this line will cause an error since it's required by the signature *)
  let add (r1, i1) (r2, i2) = (r1 +. r2, i1 +. i2)
end

(* Exercise: big list queue *)

module ListQueue = struct
  type 'a t = 'a list

  let empty = []
  let enqueue x q = q @ [ x ]
  let dequeue = function [] -> None | x :: xs -> Some (x, xs)
end

(** Creates a ListQueue filled with [n] elements. *)
let fill_listqueue n =
  let rec loop n q =
    if n = 0 then q else loop (n - 1) (ListQueue.enqueue n q)
  in
  loop n ListQueue.empty
