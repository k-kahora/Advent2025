(* NOTE Part2 monotonic stack with deletions credits *)

let test_input =
  {| 987654321111111
811111111111119
234234234234278
818181911112111 |}

module Part1 = struct
  let print_lists list1 list2 =
    let open Base in
    let open Stdio in
    print_s [%sexp (list1 : int list)];
    print_s [%sexp (list2 : int list)];
    print_endline ""

  let get_banks input =
    String.split_on_char '\n' input
    |> List.map Base.String.strip
    |> List.filter (fun a -> a <> "")

  let bank_to_int_list =
   fun bank ->
    Base.String.to_list bank
    |> List.map (String.make 1)
    |> List.map int_of_string

  let banks_to_int_list banks = List.map bank_to_int_list banks

  (* NOTE pre compute maximum suffix at each character non inclusive start from the back *)
  let joltage_suffix_list : int list -> int list =
   fun bank ->
    List.rev bank |> fun [@ocaml.warning "-8"] (a :: rest) ->
    List.fold_left
      (fun (maximum_jolt_list, max_jolt) batterie ->
        if batterie > max_jolt then (batterie :: maximum_jolt_list, batterie)
        else (max_jolt :: maximum_jolt_list, max_jolt))
      ([ a ], a) rest
    |> fun (_ :: a, _) -> a

  let maximum_joltage : int list -> int list -> int =
   fun bank joltage_maximum_list ->
    Base.List.fold2_exn bank joltage_maximum_list ~init:0
      ~f:(fun max_jolt batterie best_jolt ->
        max max_jolt @@ ((batterie * 10) + best_jolt))

  let total_joltage : int list -> int =
   fun joltages -> List.fold_left ( + ) 0 joltages

  let rec remove_last = function
    | [] -> []
    | [ _a ] -> []
    | hd :: rest -> hd :: remove_last rest

  let joltify_bank : int list -> int =
   fun bank ->
    let[@ocaml.warning "-8"] suffix_list = joltage_suffix_list bank in
    (* let bank = 0 :: a |> remove_last in *)
    let bank = bank |> remove_last in
    let result = maximum_joltage bank suffix_list in
    result

  let part1 input =
    let banks = get_banks input |> banks_to_int_list in
    List.map joltify_bank banks |> total_joltage
end

module [@ocaml.warning "-32"] _ : sig
  val part1 : string -> int
  val joltify_bank : int list -> int
  val remove_last : 'a list -> 'a list
  val get_banks : string -> string list
end =
  Part1

module Part2 = struct
  let get_banks input =
    String.split_on_char '\n' input
    |> List.map Base.String.strip
    |> List.filter (fun a -> a <> "")

  let banks_to_int_list banks =
    let bank_to_int_list =
     fun bank ->
      Base.String.to_list bank
      |> List.map (String.make 1)
      |> List.map int_of_string
    in
    List.map bank_to_int_list banks
end

module TestPart2 = struct
  include Part2
  open Base
  open Stdio

  let%expect_test "[part2] -> get banks" =
    let banks = get_banks test_input |> banks_to_int_list in
    print_s [%sexp (banks : int list list)];
    [%expect {||}]
end

module TestPart1 = struct
  open Base
  open Stdio
  include Part1

  let%expect_test "[day3-part1]" =
    (* let t_input = part1 test_input in *)
    let puzzle_input = Input.get_input ~year:2025 ~day:03 in
    let output = puzzle_input |> part1 in
    print_s [%sexp (output : int)];
    [%expect {| (9 9 9 9 9 9 9 2 2 2 2 2 1 1 1) |}]

  let%expect_test "[day3] maximum joltage" =
    let bank = bank_to_int_list "234234234234278" in
    let[@ocaml.warning "-8"] suffix_list = bank |> joltage_suffix_list in
    let[@ocaml.warning "-8"] bank = bank |> remove_last in
    (* print_s [%sexp (bank : int list)]; *)
    (* print_s [%sexp (suffix_list : int list)]; *)
    let output = maximum_joltage bank suffix_list in
    print_s [%sexp (bank : int list)];
    print_s [%sexp (suffix_list : int list)];
    print_s [%sexp (output : int)];
    [%expect {| (9 9 9 9 9 9 9 2 2 2 2 2 1 1 1) |}]

  let%expect_test "[day3] joltage_suffix_list" =
    let output = bank_to_int_list "818181911112111" |> joltage_suffix_list in
    print_s [%sexp (output : int list)];
    [%expect {| (9 9 9 9 9 9 9 2 2 2 2 2 1 1 1) |}]

  let%expect_test "[day3] parse input" =
    let output = get_banks test_input |> banks_to_int_list in
    print_s [%sexp (output : int list list)];
    [%expect
      {|
    ((9 8 7 6 5 4 3 2 1 1 1 1 1 1 1) (8 1 1 1 1 1 1 1 1 1 1 1 1 1 9)
     (2 3 4 2 3 4 2 3 4 2 3 4 2 7 8) (8 1 8 1 8 1 9 1 1 1 1 2 1 1 1))
    |}]
end
