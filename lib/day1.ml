let input_string = {|L68
L30
R48
L5
R60
L55
L1
L99
R14
L82|}

(* read the entire file *)

(* read lines *)
let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents |> List.filter (fun a -> a <> "")

let read_input input = Base.String.split input ~on:'\n'

let cycle number =
  (* if nubmer is negative like -5 make it positive 5 mod it by 99 then subtract it from 99 so -101 should by 98 so 101 mod 99 gives 2 then 99 - 2 is 97 *)
  (* if number < 0 then 100 - abs number else number mod 100 *)
  let n = number mod 100 in
  if n < 0 then 100 + n else n

open Base
open Stdio

let () = print_s [%sexp (read_input input_string : string list)]

let[@ocaml.warning "-27"] parse_input str =
  let compress_list lst =
    List.fold_left lst ~init:"" ~f:(fun acc c -> acc ^ Char.to_string c)
  in
  let num =
    match String.to_list str with
    | [] -> failwith "empty list not expected"
    | 'R' :: rest -> compress_list rest |> Int.of_string
    | 'L' :: rest -> compress_list rest |> Int.of_string |> fun a -> a * -1
    | a :: _ -> failwith "non r or l not expected"
  in
  num

let part1 input =
  (* You need a running sum of all the 0's calced and the current location of a rotation *)
  let init = (0, 50) in
  let res, _ =
    List.fold_left ~init
      ~f:(fun (res, loc) c ->
        let new_loc = parse_input c |> fun a -> loc + a |> cycle in
        (* printf "%d\n" new_loc; *)
        printf "%s -> %d\n" c new_loc;
        if new_loc = 0 then (res + 1, new_loc) else (res, new_loc))
      input
  in
  res

let%expect_test "ram test" =
  let x = read_input input_string in
  print_s [%sexp (x : string list)];
  [%expect {| (L68 L30 R48 L5 R60 L55 L1 L99 R14 L82) |}]

let%expect_test "cycle test" =
  let t_number = cycle 203 in
  let neg_number = cycle (-118) in
  print_s [%sexp (t_number : int)];
  print_s [%sexp (neg_number : int)];
  [%expect {|
    |}]

let%expect_test "file_read" =
  (* let res = part1 input_string in *)
  let l_number =
    read_lines "/home/malcolm/Projects/ocaml/advent-of-code/2025/Advent/input"
    |> List.length
  in
  print_s [%sexp (l_number : int)];
  [%expect {| 4036 |}]

(* let%expect_test "part 1" = *)
(*   (\* let res = part1 input_string in *\) *)
(*   let l_number = part1 (read_input input_string) in *)
(*   print_s [%sexp (l_number : int)]; *)
(*   [%expect {| *)
(*            3 *)
(*     |}] *)

let%expect_test "part 1" =
  (* let res = part1 input_string in *)
  let input =
    read_lines "/home/malcolm/Projects/ocaml/advent-of-code/2025/Advent/input"
  in
  let l_number = part1 input in
  print_s [%sexp (l_number : int)];
  [%expect {|
            3
    |}]

let%expect_test "parse dial" =
  let r_number = "R51" |> parse_input in
  let l_number = "L17" |> parse_input in
  print_s [%sexp (r_number : int)];
  print_s [%sexp (l_number : int)];
  [%expect {|
    51
    -17
    |}]
