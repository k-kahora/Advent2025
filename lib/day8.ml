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

  let point_mapping : Point.t list -> Point.t array = Base.List.to_array

  let get_straight_line { Point.x = x1; y = y1; z = z1 }
      { Point.x = x2; y = y2; z = z2 } : int =
    let ( ** ) : int -> int -> int = Base.Int.( ** ) in
    ((x1 - x2) ** 2) + ((y1 - y2) ** 2) + ((z1 - z2) ** 2)

  let whole_city : Point.t list -> (int * int * int) list =
   fun points ->
    let f p1 p2 = get_straight_line p1 p2 in
    let rec loop i points acc =
      match points with
      | [] -> acc
      | point :: tail ->
          let weights =
            Base.List.foldi
              ~f:(fun j weights pointb ->
                (f point pointb, i, i + j + 1) :: weights)
              ~init:[] tail
          in
          loop (i + 1) tail (weights :: acc)
    in
    loop 0 points [] |> List.flatten

  module IntArray = struct
    type t = int array

    let sexp_of_t i =
      Base.Sexp.Atom
        (Array.fold_left
           (fun atom element -> atom ^ Printf.sprintf "%2d " element)
           "[" i
        ^ "]")
  end

  let union_find ~parents:par ~ranks:rank (i, j) =
    (* Keep track of N when we have unioned N times were good that last point is the one *)
    let rec find point =
      if point <> par.(point) then find par.(point) else point
    in
    let union point1 point2 =
      let parent1, parent2 = (find point1, find point2) in
      if parent1 <> parent2 then (
        if rank.(parent1) >= rank.(parent2) then (
          par.(parent2) <- parent1;
          rank.(parent1) <- rank.(parent1) + rank.(parent2))
        else (
          par.(parent1) <- parent2;
          rank.(parent2) <- rank.(parent2) + rank.(parent1));
        1)
      else 0
    in
    let res = union i j in
    Stdio.(print_s [%sexp (par : IntArray.t)]);
    Stdio.(print_s [%sexp (rank : IntArray.t)]);
    print_endline "";
    res

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
    let _true_input = Input.get_input ~year:2025 ~day:08 in
    let s = junction_box_locations _true_input in
    let n = List.length s in
    print_s [%sexp (s : Point.t list)];
    [%expect {||}];
    let mapping = point_mapping s in
    print_s [%sexp (mapping : Point.t array)];
    [%expect {||}];
    let city_block = s |> whole_city in
    let city_block_sorted =
      city_block |> List.sort ~compare:(fun (a, _, _) (b, _, _) -> compare a b)
    in
    let ten_true = city_block_sorted in
    (* List.take city_block_sorted 10 in *)
    print_s [%sexp (ten_true : (int * int * int) list)];
    [%expect {||}];
    let ten =
      List.take city_block_sorted 10
      |> Stdlib.List.map (fun (a, b, c) -> (a, mapping.(b), mapping.(c)))
    in
    print_s [%sexp (ten : (int * Point.t * Point.t) list)];
    [%expect {||}];
    let one _ = 1 in
    let par = Stdlib.Array.init (Array.length mapping) Stdlib.Fun.id in
    let rank = Stdlib.Array.init (Array.length mapping) one in
    let edges = Stdlib.List.map (fun (_, i, j) -> (i, j)) ten_true in
    print_s [%sexp (edges : (int * int) list)];
    [%expect ""];
    let count = ref n in
    let res =
      List.fold_until ~init:(0, 0)
        ~f:(fun final_edge edge ->
          if !count = 1 then Continue_or_stop.Stop final_edge
          else (
            count := !count - union_find ~parents:par ~ranks:rank edge;
            Continue_or_stop.Continue edge))
        ~finish:(fun _ -> (0, 0))
        edges
    in
    (* Stdlib.List.iter *)
    (*   (fun edge -> union_find ~parents:par ~ranks:rank edge) *)
    (*   edges; *)
    print_s [%sexp (par : int array)];
    [%expect ""];
    print_s [%sexp (rank : int array)];
    [%expect ""];
    Array.sort ~compare rank;
    let top_3 = Array.sub ~len:3 ~pos:0 (Array.rev rank) in
    print_s [%sexp (top_3 : int array)];
    [%expect ""];
    let result = Stdlib.Array.fold_left ( * ) 1 top_3 in
    print_s [%sexp (result : int)];
    [%expect "result"];
    let part2_result =
      let i, j = res in
      let { Point.x = x1; y = _; z = _ }, { Point.x = x2; z = _; y = _ } =
        (mapping.(i), mapping.(j))
      in
      print_s [%sexp ((mapping.(i), mapping.(j)) : Point.t * Point.t)];
      x1 * x2
    in
    print_s [%sexp (part2_result : int)];
    [%expect "result"]
end
