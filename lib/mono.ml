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
let rec format_stack ~stack ~value =
  match stack with
  | item :: rest when item < value -> format_stack ~stack:rest ~value
  | [] -> [ value ]
  | _ -> value :: stack

(* Stack must be length 12 *)
(* Input can be arbitrarily long *)
(* Must check entire input *)
(* Only get a set number of deletes *)
let _skips input = List.length input - 12

(* monotoncially increasing *)
let rec loop stack input =
  let[@ocaml.warning "-8"] (stack_head :: _stack_tail) = stack in
  (* let input_t = input in *)
  (* Base.(Stdio.(print_s [%sexp (stack : int list)])); *)
  Printf.printf "head -> %d\n " stack_head;
  (*   (List.length input) skips; *)
  Base.(Stdio.(print_s [%sexp (stack : int list)]));
  match input with
  | [] -> stack
  | batterie :: tail when batterie <= stack_head ->
      loop (batterie :: stack) tail
  | batterie :: tail ->
      let stack = format_stack ~stack ~value:batterie in
      loop stack tail

let maximum_joltage input = loop [ Int.max_int ] input |> List.rev |> List.tl

let bank_to_int_list =
 fun bank ->
  Base.String.to_list bank |> List.map (String.make 1) |> List.map int_of_string

open Base
open Stdio

let%expect_test "[mono] mono advent" =
  let output = format_stack ~stack:[ 1; 2; 5; 6; 8; 9; 10 ] ~value:7 in
  print_s [%sexp (output : int list)];
  [%expect {|
     (6 5 3)
    |}]

let%expect_test "[mono] long input  " =
  let input =
    bank_to_int_list
      "4732321333332463233337712234322122322247222252423773321362313613333336333732233372323328332333322777"
  in

  print_s [%sexp (input : int list)];

  let output = maximum_joltage input in
  print_s [%sexp (output : int list)];
  [%expect {|
            Expected
    |}]
