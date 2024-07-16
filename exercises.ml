(** Write a function [last : 'a list -> 'a option] that returns the last element of a list. Empty lists return [None] *)
let rec last = function
|  [] -> None
|  [x] -> Some x
|  _ :: x -> last x

(** Find the last but one (last and penultimate) elements of a list. Empty lists or lists with only one element return [None] *)
let rec last_two = function
| [] -> None
| [x] -> None
| [x; y] -> Some (x, y)
| _ :: y -> last_two y

(** Find the N'th element of a list. *)
let rec nth lst n = List.nth lst n

(** Find the number of elements of a list. OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution. *)
let rec length = function
| [] -> 0
| _ :: x -> 1 + (length x)
