let example_input =
  {| 11-22,95-115,998-1012,1188511880-1188511890,222220-222224, 1698522-1698528,446443-446449,38593856-38593862,565653-565659, 824824821-824824827,2121212118-2121212124 |}

module Part1 = struct
  let parse_range (s : string) : (int * int) list =
    String.split_on_char ',' s
    |> List.map @@ String.split_on_char '-'
    |> List.map (function
         | [ a; b ] ->
             Base.String.(strip a |> int_of_string, strip b |> int_of_string)
         | _a -> failwith "not valid")

  let expand_range (s : (int * int) list) : string list =
    List.map
      (fun (start, finish) ->
        List.init (finish - start + 1) (fun i -> i + start))
      s
    |> List.flatten |> List.map string_of_int

  let filter (s : string list) : string list =
    s
    |> List.filter (fun a -> String.length a |> fun a -> a mod 2 = 0)
    |> List.filter (fun a ->
           let number_length = String.length a in
           String.sub a 0 (number_length / 2)
           = String.sub a (number_length / 2) (number_length / 2))

  let ripped_apart (input_list : string) : int =
    let result =
      parse_range input_list |> expand_range |> filter |> List.map int_of_string
      |> List.fold_left ( + ) 0
    in
    result
end

module [@ocaml.warning "-32"] _ : sig
  val ripped_apart : string -> int
  val filter : string list -> string list
  val expand_range : (int * int) list -> string list
  val parse_range : string -> (int * int) list
end =
  Part1

include Part1

module Part2 = struct
  let compute_valid_permutation =
   fun input permutation ->
    let perm_size = String.length permutation in
    let string_length = String.length input in
    string_length mod perm_size = 0
    &&
    let permutation_length = String.length permutation in
    let chunk = string_length / permutation_length in
    List.init chunk (fun i ->
        permutation = String.sub input (i * perm_size) perm_size)
    |> List.for_all (fun a -> a)

  let chunkify input =
    let permutations =
      let length = String.length input / 2 in
      let split_string = String.sub input 0 length in
      let permutations =
        List.init length (fun i -> String.sub split_string 0 (i + 1))
      in
      permutations
    in
    List.map (compute_valid_permutation input) permutations
    |> List.exists (fun a -> a)

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
end

module [@ocaml.warning "-32"] _ : sig
  val compute_valid_permutation : string -> string -> bool
  val chunkify : string -> bool
  val ripped_apart_two : string -> int
end =
  Part2

include Part2
open Base
open Stdio

let%expect_test "chunkify" =
  (* let real_input = Advent.Input.get_input ~year:2025 ~day:02 in *)
  (* let real_input = Input.get_input ~year:2025 ~day:02 in *)
  let output = chunkify "2121212118" in
  (* let output = ripped_apart real_input in *)
  print_s [%sexp (output : bool)];
  [%expect {| false |}]

let%expect_test "part2" =
  (* let real_input = Advent.Input.get_input ~year:2025 ~day:02 in *)
  (* let real_input = Input.get_input ~year:2025 ~day:02 in *)
  let real_input = Input.get_input ~year:2025 ~day:02 in
  let output = ripped_apart_two real_input in
  (* let output = ripped_apart example_input in *)
  print_s [%sexp (output : int)];
  [%expect {| 48778605167 |}]

let%expect_test "part1" =
  (* let real_input = Advent.Input.get_input ~year:2025 ~day:02 in *)
  let real_input = Input.get_input ~year:2025 ~day:02 in
  (* let output = ripped_apart example_input in *)
  let output = ripped_apart real_input in
  print_s [%sexp (output : int)];
  [%expect {| 28844599675 |}]
