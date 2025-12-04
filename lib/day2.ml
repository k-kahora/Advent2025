let input_string =
  {| 11-22,95-115,998-1012,1188511880-1188511890,222220-222224, 1698522-1698528,446443-446449,38593856-38593862,565653-565659, 824824821-824824827,2121212118-2121212124 |}

(* NOTE -> parse out each range, csv *)
(* NOTE -> loop through each range and compuse a list*)
(* NOTE -> if the number is odd length skip it filter it out*)
(* NOTE ->  list of all even length numers in range perfomt teh check on each *)
(* NOTE ->  split the number in half and compare halfs if equal add one *)

(* NOTE -> check the permutation of each range and find any repeated patterns *)
(* NOTE ->  446446 would go -> 4,44,446,4464,44644,446444 *)

let ripped_apart input_list =
  String.split_on_char ',' input_list
  |> List.map @@ String.split_on_char '-'
  |> List.map (function
       | [ a; b ] ->
           ( Base.String.strip a |> int_of_string,
             Base.String.strip b |> int_of_string )
       | _a -> failwith "not valid")
  |> List.map (fun (start, finish) ->
         List.init (finish - start + 1) (fun i -> i + start))
  |> List.flatten |> List.map string_of_int
  |> List.filter (fun a -> String.length a |> fun a -> a mod 2 = 0)
  |> List.filter (fun a ->
         let number_length = String.length a in
         String.sub a 0 (number_length / 2)
         = String.sub a (number_length / 2) (number_length / 2))
  |> List.map int_of_string |> List.fold_left ( + ) 0

open Base
open Stdio

let%expect_test "" =
  (* let real_input = Advent.Input.get_input ~year:2025 ~day:02 in *)
  let real_input = Input.get_input ~year:2025 ~day:02 in
  (* let output = ripped_apart input_string in *)
  let output = ripped_apart real_input in
  print_s [%sexp (output : int)];
  [%expect {|
    51
    -17
    |}]
