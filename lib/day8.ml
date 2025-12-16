(* NOTE 
Use an O(n^2) approach to generate all pairwise edges between junction boxes, weighted by squared Euclidean distance.
Sort all edges by distance.
Take the first 1000 edges from this sorted list.
Initialize Union-Find with one set per junction box.
For each of the 1000 edges:
Call union(u, v) on its endpoints (even if they are already connected).
After processing all 1000 edges:
Compute the size of each connected component.
Sort component sizes in descending order.
Multiply the sizes of the three largest components and return the resu
*)

module Part1 = struct
  let example =
    {|162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
|}

  module Point : sig
    type t = { x : int; y : int; z : int }

    val make_point : x:int -> y:int -> z:int -> t
    val sexp_of_t : t -> Sexplib0.Sexp.t
  end = struct
    type t = { x : int; y : int; z : int }

    let sexp_of_t point =
      let module Sex = Base.Sexp in
      Sex.Atom (Printf.sprintf "[%d,%d,%d]" point.x point.y point.z)

    let _ = sexp_of_t
    let make_point ~x ~y ~z = { x; y; z }
  end

  let get_straight_line { Point.x = x1; y = y1; z = z1 }
      { Point.x = x2; y = y2; z = z2 } : int =
    let ( ** ) : int -> int -> int = Base.Int.( ** ) in
    ((x1 - x2) ** 2) + ((y1 - y2) ** 2) + ((z1 - z2) ** 2)

  let whole_city : Point.t list -> (int * Point.t * Point.t) list =
   fun points ->
    let f p1 p2 = (get_straight_line p1 p2, p1, p2) in
    let rec loop points acc =
      match points with
      | [] -> acc
      | point :: tail ->
          let weights =
            List.fold_left
              (fun weights pointb -> f point pointb :: weights)
              [] tail
          in
          loop tail (weights :: acc)
    in
    loop points [] |> List.flatten

  let junction_box_locations (input : string) =
    let module A = Angstrom in
    let module P = Point in
    let parser =
      let number = function '0' .. '9' -> true | _ -> false in
      let get_pos =
        A.(
          take_while number
          <* take_while @@ function ',' | '\n' -> true | _ -> false)
      in
      let[@ocaml.warning "-8"] x_y_z =
        let int = int_of_string in
        A.(
          count 3 get_pos >>| fun (x :: y :: z :: _) ->
          P.make_point ~x:(int x) ~y:(int y) ~z:(int z))
      in
      A.many_till x_y_z A.end_of_input
    in
    match A.parse_string ~consume:All parser input with
    | Ok a -> a
    | Error err -> failwith err
end

module Part1Test = struct
  open Base
  open Stdio
  include Part1

  let%expect_test "[day8] part1" =
    let s = junction_box_locations example in
    print_s [%sexp (s : Point.t list)];
    [%expect {||}];
    let city_block = s |> whole_city in
    let city_block_sorted =
      city_block |> List.sort ~compare:(fun (a, _, _) (b, _, _) -> compare a b)
    in
    print_s [%sexp (city_block_sorted : (int * Point.t * Point.t) list)];
    [%expect {||}]
end
