let test_input =
  {|
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
|}

module Part1 = struct
  let parse_input input =
    String.split_on_char '\n' input
    |> List.filter (fun a -> a <> "")
    |> List.map Base.String.to_list

  type papers = [ `Empty | `Paper ] [@@deriving sexp]

  let suround (row, col) (assoc_list : ((int * int) * papers) list) =
    [
      List.assoc_opt (row - 1, col - 1) assoc_list;
      List.assoc_opt (row - 1, col) assoc_list;
      List.assoc_opt (row - 1, col + 1) assoc_list;
      List.assoc_opt (row, col + 1) assoc_list;
      List.assoc_opt (row + 1, col + 1) assoc_list;
      List.assoc_opt (row + 1, col) assoc_list;
      List.assoc_opt (row + 1, col - 1) assoc_list;
      List.assoc_opt (row, col - 1) assoc_list;
    ]
    |> List.map (fun a ->
           Option.bind a (function `Empty -> None | `Paper -> Some `Paper))
    |> List.filter_map Fun.id

  let assoc_list (papers : char list list) =
    List.mapi
      (fun row_index row ->
        List.mapi
          (fun col_index spot ->
            match spot with
            | '@' -> ((row_index, col_index), `Paper)
            | _ -> ((row_index, col_index), `Empty))
          row)
      papers
    |> List.flatten

  let find_surroundings papers =
    List.filter_map
      (fun (pos, spot) ->
        match spot with `Empty -> None | `Paper -> Some (suround pos papers))
      papers
    |> List.filter (fun a -> List.length a < 4)
end

module Part2 = struct
  (* The graph is the array setup
     This is an adjaceny matrix set thisu up as an array so you can modify it
 *)
  type paper = [ `Empty | `Paper ] [@@deriving sexp]

  let craft_array (input : string list) =
    (* Get the length of everything *)
    let inner_array =
      Array.init (List.hd input |> String.length) (fun _ -> `Empty)
    in
    let base_array =
      Array.init (List.length input) (fun _ -> Array.copy inner_array)
    in
    base_array

  let convert = function '@' -> `Paper | _ -> `Empty

  let contruct_list input =
    List.map
      (fun str ->
        Base.String.to_list str |> List.map convert |> Base.List.to_array)
      input
    |> Base.List.to_array

  let parse_input input =
    String.split_on_char '\n' input
    |> List.filter (fun a -> a <> "")
    |> contruct_list
end

module TestPart2 = struct
  open! Base
  open Stdio
  include Part2

  let%expect_test "[day4] expect test " =
    (* let real_input = Input.get_input ~year:2025 ~day:04 in *)
    let out = parse_input test_input in
    print_s [%sexp (out : paper array array)];
    [%expect {|  Sigma |}]
end

include Part1
open! Base
open Stdio

let%expect_test "[day4] final answer" =
  (* let real_input = Input.get_input ~year:2025 ~day:04 in *)
  let output = parse_input test_input |> assoc_list |> find_surroundings in
  let answer = output |> List.length in
  print_s [%sexp (output : papers list list)];
  print_s [%sexp (answer : int)];
  [%expect {|
            day4
    |}]

let%expect_test "[day4] filter map" =
  let output = parse_input test_input |> assoc_list |> suround (0, 6) in
  print_s [%sexp (output : papers list)];
  [%expect {|
            day4
    |}]

let%expect_test "[day4] parse input" =
  let output = [ `Empty; `Paper ] in
  print_s [%sexp (output : papers list)];
  [%expect {|
            day4
    |}]

let%expect_test "[day4] parse input" =
  let output = parse_input test_input |> assoc_list in
  print_s [%sexp (output : ((int * int) * papers) list)];
  [%expect {|
            day4
    |}]
