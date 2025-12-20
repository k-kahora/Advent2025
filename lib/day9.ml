module Part1 = struct
  let example = {|7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3|}
  (* Brain dead approach iterate through all possibilities and find the area of each one keeping track of the largest *)

  let parse_input input =
    let module A = Angstrom in
    let eat_whitespace =
      A.(skip_while @@ function ' ' | '\n' | '\t' | '\r' -> true | _ -> false)
    in
    (* got line by line splitting by comma*)
    let digit = function '0' .. '9' -> true | _ -> false in
    let each_row =
      let number = A.(eat_whitespace *> take_while digit <* char ',') in
      A.(
        lift2 (fun a b -> (int_of_string a, int_of_string b)) number
        @@ take_while digit
        <* eat_whitespace)
    in
    let many = A.(many_till each_row end_of_input) in
    match A.parse_string ~consume:All many input with
    | Ok a -> a
    | Error err -> failwith err

  let calc_area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

  let areas red_tiles =
    let fold = Base.List.fold in
    let rec loop red_tiles acc =
      match red_tiles with
      | p1 :: rest ->
          loop rest
          @@ fold ~init:acc ~f:(fun acc p2 -> calc_area p1 p2 :: acc) rest
      | [] -> acc
    in
    loop red_tiles []
end

module Part1Test = struct
  open Base
  open Stdio
  include Part1

  let%expect_test "hello form part1 test" =
    let real_input = Input.get_input ~year:2025 ~day:09 in
    print_s [%sexp (real_input : string)];
    [%expect ""];
    let red_tile_locs = real_input |> parse_input in
    print_s [%sexp (red_tile_locs : (int * int) list)];
    [%expect ""];

    let areas = areas red_tile_locs in
    print_s [%sexp (areas : int list)];
    [%expect ""];
    let max = List.fold ~init:0 ~f:max areas in
    print_s [%sexp (max : int)];
    [%expect ""]
end
(* Brute forec *)
