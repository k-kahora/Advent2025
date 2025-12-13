(* let input = *)
(*   {|732321333332463233337712234322122322247222252423773321362313613333336333732233372323328332333322777|} *)

(* Decreasing *)
(* You want it to be monotonicalyl decreasing, while maintaing a max length of 12 *)
(* let mono_stack (bank : int list) = *)
(*   let pop = function *)
(*     | [] -> failwith "can not pop empty list" *)
(*     | _ :: rest -> rest *)
(*   in *)

(* core part of monotonic stack *)
let rec format_stack ~stack ~value tokens input =
  match stack with
  | item :: rest when item < value && tokens > 0 ->
      format_stack ~stack:rest ~value (tokens - 1) input
  | [] -> [ value ]
  | _ when List.length stack < 13 -> value :: stack
  | _ -> stack

(* Stack must be length 12 *)
(* Input can be arbitrarily long *)
(* Must check entire input *)
(* Only get a set number of deletes *)
let _skips input = List.length input - 12

(* monotoncially increasing *)
let rec loop stack input =
  let[@ocaml.warning "-8"] (stack_head :: _stack_tail) = stack in
  (* let input_head = Base.List.nth input 0 |> Option.value ~default:(-1) in *)
  let tokens = List.length input - (13 - List.length stack) in
  (* let input_t = input in *)
  (* Base.(Stdio.(print_s [%sexp (stack : int list)])); *)
  (* Printf.printf "stack_head -> %d\n input_head -> %d\n " stack_head input_head; *)
  (*   (List.length input) skips; *)
  (* Base.(Stdio.(print_s [%sexp (stack : int list)])); *)
  match input with
  | [] -> stack
  | batterie :: tail when batterie <= stack_head && List.length stack < 13 ->
      loop (batterie :: stack) tail
  | batterie :: tail ->
      let stack = format_stack ~stack ~value:batterie tokens input in
      loop stack tail

let maximum_joltage input = loop [ Int.max_int ] input |> List.rev |> List.tl

let bank_to_int_list =
 fun bank ->
  Base.String.to_list bank |> List.map (String.make 1) |> List.map int_of_string

open Base
open Stdio

(* let%expect_test "[mono] mono advent" = *)
(*   let output = format_stack ~stack:[ 1; 2; 5; 6; 8; 9; 10 ] ~value:7 10000 in *)
(*   print_s [%sexp (output : int list)]; *)
(*   [%expect {| *)
(*      (6 5 3) *)
(*     |}] *)

let%expect_test "[mono] long input  " =
  let input =
    bank_to_int_list
      "4732321333332463233337712234322122322247222252423773321362313613333336333732233372323328332333322777"
  in

  print_s [%sexp (input : int list)];

  let output = maximum_joltage input in
  print_s [%sexp (output : int list)];
  [%expect {|
    (4 7 3 2 3 2 1 3 3 3 3 3 2 4 6 3 2 3 3 3 3 7 7 1 2 2 3 4 3 2 2 1 2 2 3 2 2 2
     4 7 2 2 2 2 5 2 4 2 3 7 7 3 3 2 1 3 6 2 3 1 3 6 1 3 3 3 3 3 3 6 3 3 3 7 3 2
     2 3 3 3 7 2 3 2 3 3 2 8 3 3 2 3 3 3 3 2 2 7 7 7)
    (8 3 3 3 3 3 3 2 2 7 7 7)
    |}]

(*
  monon stack that enforcese a min an maximum length, how to do that!!!
  deletes are not static it is updated as you progress through the values
  how many spots left - 12 is how many tokens you have
*)
