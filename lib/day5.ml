(* Create a list of all ranges for each input iterate through the ranges *)

module Part1 = struct
  let input = {| 3-5
10-14
16-20
12-18

1
5
8
11
17
32 |}

  let rec ranges (acc : string list) (input : string list) =
    match input with
    | "" :: rest -> (acc |> List.rev, rest)
    | range :: tail -> ranges (range :: acc) tail
    | _ -> failwith "failure match should not be reached"

  let parse_input input : string list * string list =
    String.split_on_char '\n' input |> List.map String.trim |> ranges []
end

module Part1Test = struct
  open Base
  open Stdio
  include Part1

  let%expect_test "[day5] parse input" =
    let fresh, ids = parse_input input in
    print_s [%sexp (fresh : string list)];
    [%expect {| (Empty Paper) |}];
    print_s [%sexp (ids : string list)];
    [%expect {| (Empty Paper) |}]
end
