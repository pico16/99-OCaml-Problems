(** Write a function [last : 'a list -> 'a option] that returns the last element of a list. Empty lists return [None] *)
let rec last = function
|  [] -> None
|  x :: [] -> Some x
|  _ :: x -> last x

(** Find the last but one (last and penultimate) elements of a list. Empty lists or lists with only one element return [None] *)
let rec last_two = function
| [] | _ :: [] -> None
| x :: y :: [] -> Some (x, y)
| _ :: y -> last_two y

(** Find the N'th element of a list. *)
let rec nth lst n = match lst with
| [] -> None
| h :: t -> 
  if n = 0 then Some h else nth t (n - 1)

(** Find the number of elements of a list. OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution. *)
let rec length = function
| [] -> 0
| _ :: x -> 1 + (length x)

(** Reverse a list. OCaml standard library has List.rev but we ask that you reimplement it. *)
let rec rev = function
| [] -> []
| h :: t :: [] -> t :: h :: []
| h :: t -> rev t @ h :: []

(** Find out whether a list is a palindrome. *)
let is_palidrome lst =
  lst = rev lst

(** Flatten a nested list structure. *)
type 'a node =
  | One of 'a 
  | Many of 'a node list

let rec flatten = function
| [] -> []
| One h :: t -> h :: flatten t
| Many h :: t -> flatten h @ flatten t