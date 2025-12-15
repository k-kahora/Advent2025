(* I can just do a O(n) iteration *)
(* At each step just progress the beam *)
(* if a splitt is found and a beam above simply add a beam to the left and right and add one to result *)

module Part2 = struct
  (* combine based on  *)
  let int_char : int -> char = fun i -> string_of_int i |> Base.Char.of_string
  let char_int : char -> int = fun c -> Base.Char.to_string c |> int_of_string

  let list_to_array (l : char list list) =
    List.map Base.List.to_array l |> Base.List.to_array

  let parse_input input =
    String.split_on_char '\n' input
    |> List.filter (fun a -> a <> "")
    |> List.map Base.String.to_list

  let example =
    {|
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
|}

  let quantom_manifold manifold =
    for i = 1 to Array.length manifold - 1 do
      for j = 0 to Array.length manifold.(0) - 1 do
        match manifold.(i).(j) with
        | '^' -> (
            match manifold.(i - 1).(j) with
            | '1' ->
                manifold.(i).(j + 1) <- int_char 1;
                manifold.(i).(j - 1) <- int_char 1
            | _ -> ())
        | '.' -> (
            match manifold.(i - 1).(j) with
            | 'S' | '1' -> manifold.(i).(j) <- int_char 1
            | _ -> ())
        | _ -> ()
      done
    done;
    manifold
end

module Part2Test = struct
  include Part2
  open Base
  open Stdio

  let%expect_test "[day7] expect test beam" =
    let s = parse_input example |> list_to_array in
    print_s [%sexp (s : char array array)];
    [%expect {||}];
    let q_fold = s |> quantom_manifold in
    print_s [%sexp (q_fold : char array array)];
    [%expect {||}]
end

module Part1 = struct
  let input =
    {|
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
|}

  let list_to_array (l : char list list) =
    List.map Base.List.to_array l |> Base.List.to_array

  let go_through_manifold (manifold : char array array) =
    let ans = ref 0 in
    for i = 1 to Array.length manifold - 1 do
      for j = 0 to Array.length manifold.(0) - 1 do
        let () =
          match manifold.(i).(j) with
          | '^' ->
              if manifold.(i - 1).(j) = '|' then (
                incr ans;
                manifold.(i).(j + 1) <- '|';
                manifold.(i).(j - 1) <- '|')
              else ()
          | '.' -> (
              match manifold.(i - 1).(j) with
              | 'S' | '|' -> manifold.(i).(j) <- '|'
              | _ -> ())
          | _ -> ()
        in
        ()
      done
    done;
    (manifold, !ans)

  let parse_input input =
    String.split_on_char '\n' input
    |> List.filter (fun a -> a <> "")
    |> List.map Base.String.to_list
end

module Part1Test = struct
  include Part1
  open Base
  open Stdio

  let%expect_test "[day7] expect test beam" =
    let s, ans =
      Input.get_input ~year:2025 ~day:07
      |> parse_input |> list_to_array |> go_through_manifold
    in
    print_s [%sexp (s : char array array)];
    [%expect {||}];
    print_s [%sexp (ans : int)];
    [%expect {||}]
end
