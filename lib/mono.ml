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
let rec format_stack input stack value ~skips =
  match stack with
  | item :: rest
    when item < value && List.length input + List.length stack > skips ->
      format_stack input rest value ~skips:(skips - 1)
  | [] -> ([ value ], skips)
  | _ when List.length stack <= 12 -> (value :: stack, skips)
  | _ -> (stack, skips)

(* Stack must be length 12 *)
(* Input can be arbitrarily long *)
(* Must check entire input *)
(* Only get a set number of deletes *)
let skips input = List.length input - 12

(* monotoncially increasing *)
let rec loop stack input skips =
  let[@ocaml.warning "-8"] (stack_head :: _stack_tail) = stack in
  (* let input_t = input in *)
  (* Base.(Stdio.(print_s [%sexp (stack : int list)])); *)
  Printf.printf "head -> %d, remaining -> %d tokens -> %d" stack_head
    (List.length input) skips;
  Base.(Stdio.(print_s [%sexp (stack : int list)]));

  match input with
  | [] -> stack
  | batterie :: tail when batterie <= stack_head && List.length stack >= 12 ->
      loop stack tail skips
  | batterie :: tail when batterie <= stack_head ->
      loop (batterie :: stack) tail skips
  | batterie :: tail ->
      let stack, skips = format_stack tail stack batterie ~skips in
      loop stack tail skips

let maximum_joltage input =
  loop [ Int.max_int ] input @@ skips input |> List.rev
  |> fun [@ocaml.warning "-8"] (_ :: rest) -> rest

open Base
open Stdio

let%expect_test "[mono] craft stack " =
  let input =
    [
      8;
      1;
      8;
      1;
      8;
      1;
      9;
      1;
      1;
      1;
      1;
      2;
      1;
      1;
      1;
      4;
      5;
      8;
      5;
      3;
      4;
      5;
      8;
      1;
      6;
      4;
      9;
      9;
      9;
      9;
      9;
      9;
      2;
      3;
      9;
      8;
      0;
      1;
      4;
      4;
      8;
    ]
  in
  let output = maximum_joltage input in
  print_s [%sexp (output : int list)];
  [%expect {|
     (6 5 3)
    |}]

let%expect_test "[mono] mono advent" =
  let output =
    maximum_joltage [ 9; 8; 7; 6; 5; 4; 3; 2; 1; 1; 1; 1; 1; 1; 1 ]
  in
  print_s [%sexp (output : int list)];
  [%expect {|
     (6 5 3)
    |}]

let%expect_test "[mono] stack boot" =
  let output, _ =
    format_stack [ 1; 3; 3; 5; 6 ] [ 1; 3; 3; 5; 6 ] 5 ~skips:1000
  in
  print_s [%sexp (output : int list)];
  [%expect {|
     (6 5 3)
    |}]
