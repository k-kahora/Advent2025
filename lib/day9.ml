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

module Render = struct
  include Part1

  type tile = Empty | Red | Green

  let sexp_of_grid (grid : tile array array) =
    let open Base in
    let res =
      Array.fold
        ~f:(fun acc row ->
          let string_row =
            Array.fold
              ~f:(fun acc tile ->
                acc ^ match tile with Empty -> "." | Red -> "#" | Green -> "X")
              ~init:"" row
          in
          acc ^ string_row ^ "\n")
        ~init:"" grid
    in
    Sexp.Atom res

  let sexp_of_tile tile =
    let module Sex = Base.Sexp in
    (match tile with Empty -> '.' | Red -> '#' | Green -> 'X')
    |> Base.String.of_char |> Sex.Atom

  (* Each point should fill in the grid *)
  (* Reprsent as an rray provide a 1 grid buffer from x and y *)
  let place_tiles tiles grid =
    List.iter (fun (i, j) -> grid.(i).(j) <- Red) tiles

  let grid =
    let maximum_x =
      parse_input example |> List.map fst |> List.fold_left max 0
    in
    let maximum_y =
      parse_input example |> List.map snd |> List.fold_left max 0
    in
    Array.init (maximum_x + 1) @@ fun _ ->
    Array.init (maximum_y + 1) (fun _ -> Empty)
end

module TestGrid = struct
  include Render
  open Stdio
  open Base

  type grid = tile array array

  let%expect_test "hello form part1 test" =
    let g = Render.grid in
    let tiles = parse_input example in
    print_s [%sexp (tiles : (int * int) list)];
    [%expect ""];
    place_tiles tiles g;
    let g = Array.transpose g |> Stdlib.Option.get in
    print_s [%sexp (g : grid)];
    [%expect ""]
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
