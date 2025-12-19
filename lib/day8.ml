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
    let s = junction_box_locations example in
    let n = List.length s in
    print_s [%sexp (s : Point.t list)];
    [%expect {|
      ([162,817,812] [57,618,57] [906,360,560] [592,479,940] [352,342,300]
       [466,668,158] [542,29,236] [431,825,988] [739,650,466] [52,470,668]
       [216,146,977] [819,987,18] [117,168,530] [805,96,715] [346,949,466]
       [970,615,88] [941,993,340] [862,61,35] [984,92,344] [425,690,689])
      |}];
    let mapping = point_mapping s in
    print_s [%sexp (mapping : Point.t array)];
    [%expect {|
      ([162,817,812] [57,618,57] [906,360,560] [592,479,940] [352,342,300]
       [466,668,158] [542,29,236] [431,825,988] [739,650,466] [52,470,668]
       [216,146,977] [819,987,18] [117,168,530] [805,96,715] [346,949,466]
       [970,615,88] [941,993,340] [862,61,35] [984,92,344] [425,690,689])
      |}];
    let city_block = s |> whole_city in
    let city_block_sorted =
      city_block |> List.sort ~compare:(fun (a, _, _) (b, _, _) -> compare a b)
    in
    let ten_true = city_block_sorted in
    (* List.take city_block_sorted 10 in *)
    print_s [%sexp (ten_true : (int * int * int) list)];
    [%expect {|
      ((100427 0 19) (103401 0 7) (103922 2 13) (107662 7 19) (111326 17 18)
       (114473 9 12) (118604 11 16) (120825 2 8) (123051 14 19) (124564 2 18)
       (135411 3 19) (138165 4 6) (138401 4 12) (139436 4 5) (143825 6 17)
       (147941 3 7) (149925 8 19) (153245 0 9) (166085 11 15) (169698 13 18)
       (169717 5 8) (170996 0 14) (174329 8 16) (179982 1 5) (187970 9 19)
       (188225 5 14) (197470 8 15) (207229 15 16) (210094 10 12) (210997 6 18)
       (222250 1 4) (227353 9 10) (241808 4 9) (242683 3 13) (243850 8 14)
       (245970 5 11) (253634 3 10) (257157 2 3) (261725 5 15) (272189 4 8)
       (275526 3 8) (277754 4 19) (284126 5 19) (286382 6 12) (291905 2 15)
       (295085 7 14) (303099 6 13) (315528 0 3) (320673 8 11) (321389 15 17)
       (339261 15 18) (347033 2 6) (356681 9 14) (356902 2 19) (360363 1 14)
       (364374 5 16) (365665 3 9) (366962 2 17) (371837 14 16) (372066 7 9)
       (373273 8 13) (374840 2 4) (386273 8 18) (392629 12 19) (395250 1 9)
       (396041 4 14) (397973 7 8) (409286 4 17) (418065 10 13) (420181 5 6)
       (422561 10 19) (425877 11 14) (429829 1 12) (437950 4 13) (450068 2 5)
       (450314 2 16) (463860 4 18) (466874 13 17) (470700 5 9) (477350 6 8)
       (479866 16 19) (480382 0 10) (480534 0 8) (485969 3 4) (490446 3 12)
       (497912 13 19) (501397 4 15) (502750 0 12) (506092 3 14) (507387 7 10)
       (510185 5 12) (512753 12 13) (515241 4 10) (523869 0 4) (540032 1 19)
       (540394 5 17) (542333 0 5) (545173 8 9) (547811 8 17) (548484 6 15)
       (614187 1 6) (620651 0 1) (621205 6 9) (623304 8 12) (625034 2 7)
       (633429 1 8) (634696 5 18) (643816 14 15) (655819 6 19) (658649 3 18)
       (660285 2 12) (663121 3 5) (663851 15 19) (666498 12 14) (669046 6 10)
       (669357 2 14) (689715 13 15) (693686 11 19) (694462 2 11) (695785 2 10)
       (700616 3 6) (708228 7 16) (709094 9 13) (712874 4 7) (713638 4 11)
       (714774 5 7) (718326 1 11) (740009 7 12) (745846 7 13) (745997 3 16)
       (752354 5 13) (753080 2 9) (772322 4 16) (788666 8 10) (789110 18 19)
       (792061 12 18) (811499 12 17) (813666 16 18) (825889 0 2) (834539 1 15)
       (859614 11 17) (860601 0 16) (887284 3 15) (922830 10 14) (934526 11 18)
       (937716 6 14) (942699 0 13) (958758 1 17) (963730 13 16) (967890 16 17)
       (993429 10 18) (1000291 13 14) (1002170 1 16) (1005745 5 10) (1014326 17 19)
       (1040374 1 2) (1042017 6 11) (1049486 1 7) (1066649 3 17) (1085235 1 3)
       (1090985 0 11) (1094465 1 10) (1097120 0 6) (1099313 6 16) (1116484 9 18)
       (1117688 7 11) (1122782 12 15) (1144621 7 15) (1156377 14 18) (1159677 3 11)
       (1171434 9 16) (1200149 9 15) (1211441 6 7) (1217844 0 15) (1218374 1 18)
       (1224070 9 17) (1240561 14 17) (1257834 7 18) (1264952 1 13) (1278078 9 11)
       (1279886 11 13) (1311905 10 17) (1395701 12 16) (1420333 0 18)
       (1425709 11 12) (1578798 10 15) (1648803 10 16) (1665265 0 17)
       (1677666 7 17) (1990571 10 11))
      |}];
    let ten =
      List.take city_block_sorted 10
      |> Stdlib.List.map (fun (a, b, c) -> (a, mapping.(b), mapping.(c)))
    in
    print_s [%sexp (ten : (int * Point.t * Point.t) list)];
    [%expect {|
      ((100427 [162,817,812] [425,690,689]) (103401 [162,817,812] [431,825,988])
       (103922 [906,360,560] [805,96,715]) (107662 [431,825,988] [425,690,689])
       (111326 [862,61,35] [984,92,344]) (114473 [52,470,668] [117,168,530])
       (118604 [819,987,18] [941,993,340]) (120825 [906,360,560] [739,650,466])
       (123051 [346,949,466] [425,690,689]) (124564 [906,360,560] [984,92,344]))
      |}];
    let one _ = 1 in
    let par = Stdlib.Array.init (Array.length mapping) Stdlib.Fun.id in
    let rank = Stdlib.Array.init (Array.length mapping) one in
    let edges = Stdlib.List.map (fun (_, i, j) -> (i, j)) ten_true in
    print_s [%sexp (edges : (int * int) list)];
    [%expect " 
 ((0 19) (0 7) (2 13) (7 19) (17 18) (9 12) (11 16) (2 8) (14 19) (2 18)
  (3 19) (4 6) (4 12) (4 5) (6 17) (3 7) (8 19) (0 9) (11 15) (13 18)
  (5 8) (0 14) (8 16) (1 5) (9 19) (5 14) (8 15) (15 16) (10 12) (6 18)
  (1 4) (9 10) (4 9) (3 13) (8 14) (5 11) (3 10) (2 3) (5 15) (4 8) (3 8)
  (4 19) (5 19) (6 12) (2 15) (7 14) (6 13) (0 3) (8 11) (15 17) (15 18)
  (2 6) (9 14) (2 19) (1 14) (5 16) (3 9) (2 17) (14 16) (7 9) (8 13)
  (2 4) (8 18) (12 19) (1 9) (4 14) (7 8) (4 17) (10 13) (5 6) (10 19)
  (11 14) (1 12) (4 13) (2 5) (2 16) (4 18) (13 17) (5 9) (6 8) (16 19)
  (0 10) (0 8) (3 4) (3 12) (13 19) (4 15) (0 12) (3 14) (7 10) (5 12)
  (12 13) (4 10) (0 4) (1 19) (5 17) (0 5) (8 9) (8 17) (6 15) (1 6) (0 1)
  (6 9) (8 12) (2 7) (1 8) (5 18) (14 15) (6 19) (3 18) (2 12) (3 5) (15 19)
  (12 14) (6 10) (2 14) (13 15) (11 19) (2 11) (2 10) (3 6) (7 16) (9 13)
  (4 7) (4 11) (5 7) (1 11) (7 12) (7 13) (3 16) (5 13) (2 9) (4 16) (8 10)
  (18 19) (12 18) (12 17) (16 18) (0 2) (1 15) (11 17) (0 16) (3 15) (10 14)
  (11 18) (6 14) (0 13) (1 17) (13 16) (16 17) (10 18) (13 14) (1 16)
  (5 10) (17 19) (1 2) (6 11) (1 7) (3 17) (1 3) (0 11) (1 10) (0 6) (6 16)
  (9 18) (7 11) (12 15) (7 15) (14 18) (3 11) (9 16) (9 15) (6 7) (0 15)
  (1 18) (9 17) (14 17) (7 18) (1 13) (9 11) (11 13) (10 17) (12 16) (0 18)
  (11 12) (10 15) (10 16) (0 17) (7 17) (10 11))
 "];
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
    [%expect " 
 \"[ 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18  0 ]\"
 \"[ 2  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 ]\"

 \"[ 0  1  2  3  4  5  6  0  8  9 10 11 12 13 14 15 16 17 18  0 ]\"
 \"[ 3  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 ]\"

 \"[ 0  1  2  3  4  5  6  0  8  9 10 11 12  2 14 15 16 17 18  0 ]\"
 \"[ 3  1  2  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 ]\"

 \"[ 0  1  2  3  4  5  6  0  8  9 10 11 12  2 14 15 16 17 18  0 ]\"
 \"[ 3  1  2  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 ]\"

 \"[ 0  1  2  3  4  5  6  0  8  9 10 11 12  2 14 15 16 17 17  0 ]\"
 \"[ 3  1  2  1  1  1  1  1  1  1  1  1  1  1  1  1  1  2  1  1 ]\"

 \"[ 0  1  2  3  4  5  6  0  8  9 10 11  9  2 14 15 16 17 17  0 ]\"
 \"[ 3  1  2  1  1  1  1  1  1  2  1  1  1  1  1  1  1  2  1  1 ]\"

 \"[ 0  1  2  3  4  5  6  0  8  9 10 11  9  2 14 15 11 17 17  0 ]\"
 \"[ 3  1  2  1  1  1  1  1  1  2  1  2  1  1  1  1  1  2  1  1 ]\"

 \"[ 0  1  2  3  4  5  6  0  2  9 10 11  9  2 14 15 11 17 17  0 ]\"
 \"[ 3  1  3  1  1  1  1  1  1  2  1  2  1  1  1  1  1  2  1  1 ]\"

 \"[ 0  1  2  3  4  5  6  0  2  9 10 11  9  2  0 15 11 17 17  0 ]\"
 \"[ 4  1  3  1  1  1  1  1  1  2  1  2  1  1  1  1  1  2  1  1 ]\"

 \"[ 0  1  2  3  4  5  6  0  2  9 10 11  9  2  0 15 11  2 17  0 ]\"
 \"[ 4  1  5  1  1  1  1  1  1  2  1  2  1  1  1  1  1  2  1  1 ]\"

 \"[ 0  1  2  0  4  5  6  0  2  9 10 11  9  2  0 15 11  2 17  0 ]\"
 \"[ 5  1  5  1  1  1  1  1  1  2  1  2  1  1  1  1  1  2  1  1 ]\"

 \"[ 0  1  2  0  4  5  4  0  2  9 10 11  9  2  0 15 11  2 17  0 ]\"
 \"[ 5  1  5  1  2  1  1  1  1  2  1  2  1  1  1  1  1  2  1  1 ]\"

 \"[ 0  1  2  0  4  5  4  0  2  4 10 11  9  2  0 15 11  2 17  0 ]\"
 \"[ 5  1  5  1  4  1  1  1  1  2  1  2  1  1  1  1  1  2  1  1 ]\"

 \"[ 0  1  2  0  4  4  4  0  2  4 10 11  9  2  0 15 11  2 17  0 ]\"
 \"[ 5  1  5  1  5  1  1  1  1  2  1  2  1  1  1  1  1  2  1  1 ]\"

 \"[ 0  1  4  0  4  4  4  0  2  4 10 11  9  2  0 15 11  2 17  0 ]\"
 \"[ 5  1  5  1 10  1  1  1  1  2  1  2  1  1  1  1  1  2  1  1 ]\"

 \"[ 0  1  4  0  4  4  4  0  2  4 10 11  9  2  0 15 11  2 17  0 ]\"
 \"[ 5  1  5  1 10  1  1  1  1  2  1  2  1  1  1  1  1  2  1  1 ]\"

 \"[ 4  1  4  0  4  4  4  0  2  4 10 11  9  2  0 15 11  2 17  0 ]\"
 \"[ 5  1  5  1 15  1  1  1  1  2  1  2  1  1  1  1  1  2  1  1 ]\"

 \"[ 4  1  4  0  4  4  4  0  2  4 10 11  9  2  0 15 11  2 17  0 ]\"
 \"[ 5  1  5  1 15  1  1  1  1  2  1  2  1  1  1  1  1  2  1  1 ]\"

 \"[ 4  1  4  0  4  4  4  0  2  4 10 11  9  2  0 11 11  2 17  0 ]\"
 \"[ 5  1  5  1 15  1  1  1  1  2  1  3  1  1  1  1  1  2  1  1 ]\"

 \"[ 4  1  4  0  4  4  4  0  2  4 10 11  9  2  0 11 11  2 17  0 ]\"
 \"[ 5  1  5  1 15  1  1  1  1  2  1  3  1  1  1  1  1  2  1  1 ]\"

 \"[ 4  1  4  0  4  4  4  0  2  4 10 11  9  2  0 11 11  2 17  0 ]\"
 \"[ 5  1  5  1 15  1  1  1  1  2  1  3  1  1  1  1  1  2  1  1 ]\"

 \"[ 4  1  4  0  4  4  4  0  2  4 10 11  9  2  0 11 11  2 17  0 ]\"
 \"[ 5  1  5  1 15  1  1  1  1  2  1  3  1  1  1  1  1  2  1  1 ]\"

 \"[ 4  1  4  0  4  4  4  0  2  4 10  4  9  2  0 11 11  2 17  0 ]\"
 \"[ 5  1  5  1 18  1  1  1  1  2  1  3  1  1  1  1  1  2  1  1 ]\"

 \"[ 4  4  4  0  4  4  4  0  2  4 10  4  9  2  0 11 11  2 17  0 ]\"
 \"[ 5  1  5  1 19  1  1  1  1  2  1  3  1  1  1  1  1  2  1  1 ]\"

 \"[ 4  4  4  0  4  4  4  0  2  4 10  4  9  2  0 11 11  2 17  0 ]\"
 \"[ 5  1  5  1 19  1  1  1  1  2  1  3  1  1  1  1  1  2  1  1 ]\"

 \"[ 4  4  4  0  4  4  4  0  2  4 10  4  9  2  0 11 11  2 17  0 ]\"
 \"[ 5  1  5  1 19  1  1  1  1  2  1  3  1  1  1  1  1  2  1  1 ]\"

 \"[ 4  4  4  0  4  4  4  0  2  4 10  4  9  2  0 11 11  2 17  0 ]\"
 \"[ 5  1  5  1 19  1  1  1  1  2  1  3  1  1  1  1  1  2  1  1 ]\"

 \"[ 4  4  4  0  4  4  4  0  2  4 10  4  9  2  0 11 11  2 17  0 ]\"
 \"[ 5  1  5  1 19  1  1  1  1  2  1  3  1  1  1  1  1  2  1  1 ]\"

 \"[ 4  4  4  0  4  4  4  0  2  4  4  4  9  2  0 11 11  2 17  0 ]\"
 \"[ 5  1  5  1 20  1  1  1  1  2  1  3  1  1  1  1  1  2  1  1 ]\"

 (4 4 4 0 4 4 4 0 2 4 4 4 9 2 0 11 11 2 17 0)
 "];
    print_s [%sexp (rank : int array)];
    [%expect "(5 1 5 1 20 1 1 1 1 2 1 3 1 1 1 1 1 2 1 1)"];
    Array.sort ~compare rank;
    let top_3 = Array.sub ~len:3 ~pos:0 (Array.rev rank) in
    print_s [%sexp (top_3 : int array)];
    [%expect "(20 5 5)"];
    let result = Stdlib.Array.fold_left ( * ) 1 top_3 in
    print_s [%sexp (result : int)];
    [%expect "500"];
    let part2_result =
      let i, j = res in
      let { Point.x = x1; y = _; z = _ }, { Point.x = x2; z = _; y = _ } =
        (mapping.(i), mapping.(j))
      in
      print_s [%sexp ((mapping.(i), mapping.(j)) : Point.t * Point.t)];
      x1 * x2
    in
    print_s [%sexp (part2_result : int)];
    [%expect " 
 ([216,146,977] [117,168,530])
 25272
 "]
end
