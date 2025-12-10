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

open! Base
open Stdio

let%expect_test "[day4] final answer" =
  let real_input = Input.get_input ~year:2025 ~day:04 in
  let output = parse_input real_input |> assoc_list |> find_surroundings in
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
