(* NOTE Part2 monotonic stack with deletions credits *)
(* NOTE TODO I do not truly understand monotonci stack my functions need some serious work *)

let[@ocaml.warning "-32"] test_input =
  {| 987654321111111
811111111111119
234234234234278
818181911112111 |}

(* let test_input = *)
(*   {|4732321333332463233337712234322122322247222252423773321362313613333336333732233372323328332333322777|} *)

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
  (* NOTE size 12 is needed so! *)
  (* Create a monotnonically decreasing stack of size 12 maximum *)
  (* Only add if its less than top of the stack *)
  (* 234234234234278 *)
  (* 15 chars 12 max 3 delete tokens *)

  (*  [ 2 ] *)
  (* pop token = 2 *)
  (* [ 3 ] *)
  (* pop token = 1 *)
  (* [ 4 ] *)
  (* [ 4 ] token = 0 *)
  (* add the rest *)
  type int_list = Base.Int.t Base.List.t [@@deriving sexp]

  let total_delete_tokens bank = List.length bank - 12

  (* This is not operating like a true monotonic stack!  *)
  let super_joltage (bank : int list) =
    let skip_tokens = total_delete_tokens bank in
    let pop = function
      | [] -> failwith "can not pop empty list"
      | _ :: rest -> rest
    in

    let rec loop stack skip_tokens (bank : int list) =
      let[@ocaml.warning "-8"] (stack_head :: _) = stack in
      (* sexp_of_int_list stack |> Base.Sexp.to_string |> print_endline; *)
      (* print_endline (string_of_int skip_tokens); *)
      match bank with
      | [] -> List.rev stack
      | a when skip_tokens = 0 -> List.rev stack @ a
      (* | _ when List.length stack = 12 -> List.rev stack *)
      | batterie :: bank when batterie > stack_head && skip_tokens > 0 ->
          loop (batterie :: pop stack) (skip_tokens - 1) bank
      | batterie :: bank when batterie <= stack_head && List.length stack < 12
        ->
          loop (batterie :: stack) skip_tokens bank
      | _ :: bank -> loop stack skip_tokens bank
    in
    loop [ -1 ] (skip_tokens + 1)
      bank (* NOTE add one to skip tokens to account for the first skip of -1 *)

  let total_joltage = List.fold_left ( + ) 0

  let create_number_from_int_list : int list -> int =
   fun bank ->
    let[@ocaml.warning "-8"] (head :: tail) = bank in
    List.fold_left
      (fun final_number batt -> (final_number * 10) + batt)
      head tail

  let mutate_into_joltage : int list list -> int list =
   fun banks ->
    List.map Mono.maximum_joltage banks |> List.map create_number_from_int_list

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
end

module TestPart2 = struct
  include Part2
  open Base
  open Stdio

  let%expect_test "[part2] -> get banks" =
    let banks = bank_to_int_list "811111111111119" |> Mono.maximum_joltage in
    (* let b = banks |> Mono.maximum_joltage in *)
    (* let t_inpt = Input.get_input ~day:03 ~year:2025 in *)
    (* let banks = *)
    (*   get_banks test_input |> banks_to_int_list |> mutate_into_joltage *)
    (* in *)
    (* let length = *)
    (*   List.map ~f:(fun a -> Int.to_string a |> String.length) banks *)
    (* in *)
    let output = banks |> total_joltage in
    print_s [%sexp (banks : int list)];
    print_s [%sexp (output : int)];
    [%expect {| True Test |}]
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
