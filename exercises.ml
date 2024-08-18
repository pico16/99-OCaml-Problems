(* Write a function [last : 'a list -> 'a option] that returns the last element of a list. Empty lists return [None] *)
let rec last = function
|  [] -> None
|  h :: [] -> Some h
|  _ :: t -> last t

(* Find the last but one (last and penultimate) elements of a list. Empty lists or lists with only one element return [None] *)
let rec last_two = function
| [] | _ :: [] -> None
| h :: t :: [] -> Some (h, t)
| _ :: t -> last_two t

(* Find the N'th element of a list. *)
let rec nth lst n = match lst with
| [] -> None
| h :: t -> if n = 0 then Some h else nth t (n - 1)

(* Find the number of elements of a list. OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution. *)
let length lst =
  let rec aux n = function
  | [] -> n
  | _ :: t -> aux (n + 1) t
in aux 0 lst

(* Reverse a list. OCaml standard library has List.rev but we ask that you reimplement it. *)
let rev lst =
  let rec aux acc = function
  | [] -> acc
  | h :: t -> aux (h :: acc) t
in aux [] lst

(* Find out whether a list is a palindrome. *)
let is_palidrome lst = lst = rev lst

(* Flatten a nested list structure. *)
type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten lst = 
  let rec aux acc = function
  | [] -> acc
  | One h :: t -> aux (h :: acc) t
  | Many h :: t -> []
in aux [] lst

(* Eliminate consecutive duplicates of list elements. *)
let rec compress = function
| [] -> []
| h :: (x :: y as t) -> if h = x then compress t else h :: compress t
| x -> x

(* Pack consecutive duplicates of list elements into sublists. *)
let pack lst =
  let rec aux acc curr = function
  | [] -> []
  | h :: [] -> (h :: curr) :: acc
  | h :: (x :: _ as t) -> if h = x then aux acc (h :: curr) t else aux ((h :: curr) :: acc) [] t
in rev (aux [] [] lst)