(* Part2 flood fill
   Calculate the boundry points then run dfs on a point gaurnetted to be outside the boundry
   this will give you all your outside points
 *)
(*
  TODO
  English Explanation
  DONE First you must constuct a grid with (i,j)
  DONE Then you can go thrrough each pair of point
  DONE They will always be in line with eachother 
  DONE This way you can fill green tills from one point to the next
  TODO Then calculate the bounding box
  Start a bfs outside the bounding box to calculate all the outside area
  Everythin else thats not red should be green by default
  Once you have the full polygon you can begin maximal rectangle problem
  iterate through the each row constructing a histrogram from that point to the right
  once you have the histrogram you can run the monotonic stack to calculate the max rectangel in that range
  as you progress down increment for every green and when you get to a non green reset to zero
  good problem to solve -> https://leetcode.com/problems/maximal-rectangle/submissions/1868010117/
  easier problem to learn -> https://leetcode.com/problems/largest-rectangle-in-histogram/description/
  Now that I have these doen I can begin my quest to complete this proble

m
  


*)

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

module Part2 = struct
  let example = {|7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3|}

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

  let calculate_bounding_box red_tiles =
    List.fold_left
      (fun (min_x, min_y, max_x, max_y) (x, y) ->
        (min min_x x, min min_y y, max max_x x, max max_y y))
      (Int.max_int, Int.max_int, 0, 0)
      red_tiles

  (* Check bounds *)
end

module Render = struct
  include Part1

  type tile = Empty | Red | Green | Outside

  let sexp_of_grid (grid : tile array array) =
    let open Base in
    let res =
      Array.fold
        ~f:(fun acc row ->
          let string_row =
            Array.fold
              ~f:(fun acc tile ->
                acc
                ^
                match tile with
                | Empty -> "X"
                | Red -> "#"
                | Green -> "X"
                | Outside -> ".")
              ~init:"" row
          in
          acc ^ string_row ^ "\n")
        ~init:"" grid
    in
    Sexp.Atom res

  let sexp_of_tile tile =
    let module Sex = Base.Sexp in
    (match tile with
    | Empty -> 'X'
    | Red -> '#'
    | Green -> 'X'
    | Outside -> '.')
    |> Base.String.of_char |> Sex.Atom

  (* Each point should fill in the grid *)
  (* Reprsent as an rray provide a 1 grid buffer from x and y *)
  let place_tiles tiles grid =
    List.iter
      (fun (i, j) -> grid.(i).(j) <- Red)
      tiles (* Base.List.chunks_of *)

  (* This needs a way to go over each section and find the lines between them
     chunk by 2 and create a line of new items between


 *)
  let place_green tiles grid =
    let points (x, y) finish = function
      | `Same_X -> List.init (finish - y) (fun i -> (x, y + i))
      | `Same_Y -> List.init (finish - x) (fun i -> (x + i, y))
    in

    let generate_green (x1, y1) (x2, y2) =
      (if y1 == y2 then
         let x_low = min x1 x2 in
         let x_high = max x1 x2 in
         points (x_low + 1, y1) x_high `Same_Y
       else
         let y_low = min y1 y2 in
         let y_high = max y1 y2 in
         points (x1, y_low + 1) y_high `Same_X)
      |> List.iter (fun (i, j) -> grid.(i).(j) <- Green)
    in

    let start = List.hd tiles in
    let rec loop = function
      | a :: (b :: _ as rest) ->
          generate_green a b;
          loop rest
      | last_point :: rest ->
          generate_green start last_point;
          loop rest
      | [] -> ()
    in
    loop tiles

  let dfs start (grid : tile array array) =
    let bounds_check x y =
      try Some grid.(x).(y) with Invalid_argument _ -> None
    in
    let rec dfs' stack : unit =
      match stack with
      | [] -> ()
      | (x, y) :: rest -> (
          match bounds_check x y with
          | Some Empty ->
              grid.(x).(y) <- Outside;
              dfs'
              @@ ((x, y + 1) :: (x, y - 1) :: (x - 1, y) :: (x + 1, y) :: rest)
          | _ -> dfs' rest)
    in
    dfs' [ start ]

  let grid input =
    let maximum_x = parse_input input |> List.map fst |> List.fold_left max 0 in
    let maximum_y = parse_input input |> List.map snd |> List.fold_left max 0 in
    Array.init (maximum_x + 1) @@ fun _ ->
    Array.init (maximum_y + 1) (fun _ -> Empty)
end

module Part2Test = struct
  include Part2
  include Render
  open Stdio
  open! Base
  (* NOTE *)
  (*
    I need a way to define the borders of the grid with green tiles
*)

  let%expect_test "[ part2 testing ]" =
    let[@ocaml.warning "-26"] tile_grid = parse_input example in
    let viz_grid = grid example in
    place_tiles tile_grid viz_grid;
    place_green tile_grid viz_grid;
    (* print_s [%sexp (greens : (int * int) list list)]; *)
    (* [%expect {||}]; *)
    let x, y, _, _ = calculate_bounding_box tile_grid in
    dfs (x, y) viz_grid;
    let viz_grid = Array.transpose viz_grid |> Stdlib.Option.get in
    print_s [%sexp ((x, y) : int * int)];
    [%expect ""];
    print_s [%sexp (viz_grid : grid)];
    [%expect {||}]
end

module TestGrid = struct
  include Render
  open Stdio
  open Base

  type grid = tile array array

  let%expect_test "hello form part1 test" =
    let g = Render.grid example in
    let tiles = parse_input example in
    print_s [%sexp (tiles : (int * int) list)];
    [%expect ""];
    place_tiles tiles g;
    let g = Array.transpose g |> Stdlib.Option.get in
    print_s [%sexp (g : grid)];
    [%expect ""]
end

module Part1Test = struct
  let read_file filename =
    let ic = open_in filename in
    let len = in_channel_length ic in
    let s = really_input_string ic len in
    close_in ic;
    s

  open Base
  open Stdio
  include Part1

  let%expect_test "hello form part1 test" =
    let _real_input = Input.get_input ~day:09 ~year:2025 in
    print_s [%sexp (example : string)];
    [%expect ""];
    let red_tile_locs = example |> parse_input in
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
