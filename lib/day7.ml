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

  let v = function '.' | '^' -> 0 | a -> char_int a

  (* ADT needed *)
  type tachyon = Beam of int | Splitter | Empty | Emitter

  let sexp_of_tachyon tachyon =
    let module Sex = Base.Sexp in
    let hex_value = Printf.sprintf "%1X" in
    (match tachyon with
    | Splitter -> "^"
    | Empty -> "."
    | Emitter -> "S"
    | Beam value -> hex_value value)
    |> Sex.Atom

  let tachyons manifold =
    Array.map
      (fun a ->
        Array.map
          (function
            | '^' -> Splitter
            | '.' -> Empty
            | 'S' -> Emitter
            | _ -> failwith "invalid input")
          a)
      manifold

  let[@ocaml.warning "-8"] extract_juice = function
    | Beam a -> a
    | Empty | Splitter | Emitter -> 0

  let quantom_manifold (manifold : tachyon array array) =
    for i = 1 to Array.length manifold - 1 do
      for j = 0 to Array.length manifold.(0) - 1 do
        (* NOTE find merges *)
        let current = manifold.(i).(j) in
        let above = manifold.(i - 1).(j) in
        match current with
        | Empty -> (
            match above with
            | Emitter -> manifold.(i).(j) <- Beam 1
            | Beam _ as a -> manifold.(i).(j) <- a
            | Splitter | Empty -> ())
        | Emitter -> failwith "no emitter should be found"
        | Splitter -> (
            match above with
            | Empty | Emitter | Splitter -> ()
            | Beam juice_value ->
                manifold.(i).(j - 1) <-
                  Beam (juice_value + extract_juice manifold.(i).(j - 1));
                manifold.(i).(j + 1) <-
                  Beam (juice_value + extract_juice manifold.(i).(j + 1)))
        | Beam a -> (
            match above with
            | Beam v -> manifold.(i).(j) <- Beam (v + a)
            | Empty | Splitter | Emitter -> ())
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
    (* let q_fold = s |> quantom_manifold in *)
    (* print_s [%sexp (q_fold : char array array)]; *)
    (* [%expect {||}]; *)
    let tachs = s |> tachyons in
    print_s [%sexp (tachs : tachyon array array)];
    [%expect {|sexp|}];
    let quant = tachs |> quantom_manifold in
    print_s [%sexp (quant : tachyon array array)];
    [%expect {|tachs|}]
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
