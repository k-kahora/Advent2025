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

  let junction_box_locations (input : string) =
    let module A = Angstrom in
    let parser =
      let number = function '0' .. '9' -> true | _ -> false in
      let get_pos =
        A.(
          take_while number
          <* take_while @@ function ',' | '\n' -> true | _ -> false)
      in
      let x_y_z = A.(count 3 get_pos) in
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
    print_s [%sexp (s : string list list)];
    [%expect {||}]
end
