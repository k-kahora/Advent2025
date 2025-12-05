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

let chunkify input =
  (* NOTE find the split of the string, then go through by chunks up to half of the string length, *)
  (* NOTE for each chunck compare it to the rest of the string if at any point its not equal early return*)
  (* NOTE  otherwise keep it and continue to the next chunk until there is none left then its invalid *)
  let permutations, _split_string =
    let length = String.length input / 2 in
    let split_string = String.sub input 0 length in
    let permutations =
      List.init length (fun i -> String.sub split_string 0 (i + 1))
    in
    (permutations, split_string)
  in
  (* Loop over eache permutaion and do chunk wis comparsions use fold_until  *)
  (* Base.(Stdio.(print_s [%sexp (permutations : string list)])); *)
  List.fold_left
    (fun res permutation ->
      if res then res
      else
        let perm_size = String.length permutation in
        let string_length = String.length input in
        if string_length mod perm_size <> 0 then false
        else
          let permutation_length = String.length permutation in
          let chunk = string_length / permutation_length in
          (* Printf.printf "input l -> %d, perm_length -> %d" string_length *)
          (* permutation_length; *)
          let result =
            List.init chunk (fun i ->
                permutation = String.sub input (i * perm_size) perm_size)
            |> List.fold_left ( && ) true
          in
          (* Printf.printf "result -> %b\n" result; *)
          result)
    false permutations

let ripped_apart_two input_list =
  String.split_on_char ',' input_list
  |> List.map @@ String.split_on_char '-'
  |> List.map (function
       | [ a; b ] ->
           ( Base.String.strip a |> int_of_string,
             Base.String.strip b |> int_of_string )
       | _a -> failwith "not valid")
  |> List.map (fun (start, finish) ->
         List.init (finish - start + 1) (fun i -> i + start))
  |> List.flatten
  |> List.map string_of_int (* |> List.filter chunkify *)
  |> List.filter chunkify |> List.map int_of_string |> List.fold_left ( + ) 0

open Base
open Stdio

let%expect_test "chunkify" =
  (* let real_input = Advent.Input.get_input ~year:2025 ~day:02 in *)
  (* let real_input = Input.get_input ~year:2025 ~day:02 in *)
  let output = chunkify "2121212118" in
  (* let output = ripped_apart real_input in *)
  print_s [%sexp (output : bool)];
  [%expect {| 1 |}]

let%expect_test "part2" =
  (* let real_input = Advent.Input.get_input ~year:2025 ~day:02 in *)
  (* let real_input = Input.get_input ~year:2025 ~day:02 in *)
  let real_input = Input.get_input ~year:2025 ~day:02 in
  let output = ripped_apart_two real_input in
  (* let output = ripped_apart real_input in *)
  print_s [%sexp (output : int)];
  [%expect {| 4174379265 |}]

let%expect_test "part1" =
  (* let real_input = Advent.Input.get_input ~year:2025 ~day:02 in *)
  let real_input = Input.get_input ~year:2025 ~day:02 in
  (* let output = ripped_apart input_string in *)
  let output = ripped_apart real_input in
  print_s [%sexp (output : int)];
  [%expect {| 28844599675 |}]
