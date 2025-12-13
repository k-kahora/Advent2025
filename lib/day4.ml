let test_input =
  {|
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
|}

module Part1 = struct
  let parse_input input =
    String.split_on_char '\n' input
    |> List.filter (fun a -> a <> "")
    |> List.map Base.String.to_list

  type papers = [ `Empty | `Paper ] [@@deriving sexp]

  let suround (row, col) (assoc_list : ((int * int) * papers) list) =
    [
      List.assoc_opt (row - 1, col - 1) assoc_list;
      List.assoc_opt (row - 1, col) assoc_list;
      List.assoc_opt (row - 1, col + 1) assoc_list;
      List.assoc_opt (row, col + 1) assoc_list;
      List.assoc_opt (row + 1, col + 1) assoc_list;
      List.assoc_opt (row + 1, col) assoc_list;
      List.assoc_opt (row + 1, col - 1) assoc_list;
      List.assoc_opt (row, col - 1) assoc_list;
    ]
    |> List.map (fun a ->
           Option.bind a (function `Empty -> None | `Paper -> Some `Paper))
    |> List.filter_map Fun.id

  let assoc_list (papers : char list list) =
    List.mapi
      (fun row_index row ->
        List.mapi
          (fun col_index spot ->
            match spot with
            | '@' -> ((row_index, col_index), `Paper)
            | _ -> ((row_index, col_index), `Empty))
          row)
      papers
    |> List.flatten

  let find_surroundings papers =
    List.filter_map
      (fun (pos, spot) ->
        match spot with `Empty -> None | `Paper -> Some (suround pos papers))
      papers
    |> List.filter (fun a -> List.length a < 4)
end

module Part2 = struct
  (* The graph is the array setup
     This is an adjaceny matrix set thisu up as an array so you can modify it
 *)
  type paper = [ `Empty | `Paper ] [@@deriving sexp]

  let initial_queue (starting_list : (int * int) list) =
    let q = Queue.create () in
    List.iter (q |> Fun.flip @@ Queue.add) starting_list;
    q

  let craft_array (input : string list) =
    (* Get the length of everything *)
    let inner_array =
      Array.init (List.hd input |> String.length) (fun _ -> `Empty)
    in
    let base_array =
      Array.init (List.length input) (fun _ -> Array.copy inner_array)
    in
    base_array

  let surrounding_papers_initial (i, j) (graph : paper array array) :
      (int * int) list =
    let directions =
      [ (1, 1); (1, 0); (1, -1); (0, -1); (-1, -1); (-1, 0); (-1, 1); (0, 1) ]
    in
    List.filter_map
      (fun (n_i, n_j) ->
        let output =
          try Some graph.(i + n_i).(j + n_j) with Invalid_argument _ -> None
        in
        Option.bind output (function
          | `Empty -> None
          | `Paper -> Some (i + n_i, j + n_j)))
      directions

  let surrounding_papers (i, j) (graph : paper array array) : (int * int) list =
    let directions =
      [ (1, 1); (1, 0); (1, -1); (0, -1); (-1, -1); (-1, 0); (-1, 1); (0, 1) ]
    in
    List.filter_map
      (fun (n_i, n_j) ->
        let output =
          try Some graph.(i + n_i).(j + n_j) with Invalid_argument _ -> None
        in
        Option.bind output (function
          | `Empty -> None
          | `Paper -> Some (i + n_i, j + n_j)))
      directions

  let[@ocaml.warning "-27"] bfs (q : (int * int) Queue.t) graph =
    let result = ref 0 in
    let steps = ref 0 in
    let max_steps = 1000000 in
    while (not @@ Queue.is_empty q) && !steps < max_steps do
      incr steps;
      let q_len = Queue.length q in
      for _ = 1 to q_len do
        let ((i, j) as picked_up_paper) = Queue.pop q in
        let surrounded_papers = surrounding_papers picked_up_paper graph in
        if List.length surrounded_papers < 4 && graph.(i).(j) = `Paper then (
          graph.(i).(j) <- `Empty;
          incr result;
          List.iter
            (fun ((ii, jj) as a) ->
              (* graph.(ii).(jj) <- `Empty; *)
              Queue.push a q)
            surrounded_papers)
      done
    done;
    (* while not @@ Queue.is_empty q do *)
    (*   let ((i, j) as picked_up_paper) = Queue.pop q in *)
    (*   graph.(i).(j) <- `Empty; *)
    (*   let surrounded_papers = surrounding_papers picked_up_paper graph in *)
    (*   List.iter (q |> Fun.flip Queue.push) surrounded_papers *)
    (* done; *)
    (graph, q, !result)

  let initial_papers (graph : paper array array) =
    Base.Array.foldi ~init:[]
      ~f:(fun i acc row ->
        Base.Array.foldi ~init:acc
          ~f:(fun j acc paper ->
            let surrounded_papers =
              List.length @@ surrounding_papers_initial (i, j) graph
            in
            if paper = `Paper && surrounded_papers < 4 then (i, j) :: acc
            else acc)
          row)
      graph

  let convert = function '@' -> `Paper | _ -> `Empty

  let q_to_list q =
    let acc = ref [] in
    Queue.iter (fun x -> acc := x :: !acc) q;
    List.rev !acc

  let contruct_list input =
    List.map
      (fun str ->
        Base.String.to_list str |> List.map convert |> Base.List.to_array)
      input
    |> Base.List.to_array

  let parse_input input =
    String.split_on_char '\n' input
    |> List.filter (fun a -> a <> "")
    |> contruct_list
end

module TestPart2 = struct
  open! Base
  open Stdio
  include Part2

  let%expect_test "[day4] expect test " =
    let real_input = Input.get_input ~year:2025 ~day:04 in
    let graph = parse_input real_input in
    let papers = surrounding_papers (0, 0) graph in
    print_s [%sexp (papers : (int * int) list)];
    [%expect {| ((1 1) (1 0) (0 1)) |}];
    let starting_papers = initial_papers graph in
    let initial_q = initial_queue starting_papers in
    let _stable, q, result = bfs initial_q graph in
    (* print_s [%sexp (stable : paper array array)]; *)
    let _list_of_q = q_to_list q in
    print_s [%sexp (_list_of_q : (int * int) list)];
    (* [%expect {| () |}]; *)
    print_s [%sexp (result : int)];
    [%expect {|
      ()
      7922
      |}]
end

include Part1
open! Base
open Stdio

let%expect_test "[day4] final answer" =
  (* let real_input = Input.get_input ~year:2025 ~day:04 in *)
  let output = parse_input test_input |> assoc_list |> find_surroundings in
  let answer = output |> List.length in
  print_s [%sexp (output : papers list list)];
  print_s [%sexp (answer : int)];
  [%expect
    {|
    ((Paper Paper Paper) (Paper Paper Paper) (Paper Paper Paper)
     (Paper Paper Paper) (Paper Paper Paper) (Paper Paper Paper) (Paper Paper)
     (Paper Paper Paper) (Paper Paper Paper) (Paper Paper) (Paper)
     (Paper Paper Paper) (Paper Paper))
    13
    |}]

let%expect_test "[day4] filter map" =
  let output = parse_input test_input |> assoc_list |> suround (0, 6) in
  print_s [%sexp (output : papers list)];
  [%expect {| (Paper Paper Paper) |}]

let%expect_test "[day4] parse input" =
  let output = [ `Empty; `Paper ] in
  print_s [%sexp (output : papers list)];
  [%expect {| (Empty Paper) |}]

let%expect_test "[day4] parse input" =
  let output = parse_input test_input |> assoc_list in
  print_s [%sexp (output : ((int * int) * papers) list)];
  [%expect
    {|
    (((0 0) Empty) ((0 1) Empty) ((0 2) Paper) ((0 3) Paper) ((0 4) Empty)
     ((0 5) Paper) ((0 6) Paper) ((0 7) Paper) ((0 8) Paper) ((0 9) Empty)
     ((1 0) Paper) ((1 1) Paper) ((1 2) Paper) ((1 3) Empty) ((1 4) Paper)
     ((1 5) Empty) ((1 6) Paper) ((1 7) Empty) ((1 8) Paper) ((1 9) Paper)
     ((2 0) Paper) ((2 1) Paper) ((2 2) Paper) ((2 3) Paper) ((2 4) Paper)
     ((2 5) Empty) ((2 6) Paper) ((2 7) Empty) ((2 8) Paper) ((2 9) Paper)
     ((3 0) Paper) ((3 1) Empty) ((3 2) Paper) ((3 3) Paper) ((3 4) Paper)
     ((3 5) Paper) ((3 6) Empty) ((3 7) Empty) ((3 8) Paper) ((3 9) Empty)
     ((4 0) Paper) ((4 1) Paper) ((4 2) Empty) ((4 3) Paper) ((4 4) Paper)
     ((4 5) Paper) ((4 6) Paper) ((4 7) Empty) ((4 8) Paper) ((4 9) Paper)
     ((5 0) Empty) ((5 1) Paper) ((5 2) Paper) ((5 3) Paper) ((5 4) Paper)
     ((5 5) Paper) ((5 6) Paper) ((5 7) Paper) ((5 8) Empty) ((5 9) Paper)
     ((6 0) Empty) ((6 1) Paper) ((6 2) Empty) ((6 3) Paper) ((6 4) Empty)
     ((6 5) Paper) ((6 6) Empty) ((6 7) Paper) ((6 8) Paper) ((6 9) Paper)
     ((7 0) Paper) ((7 1) Empty) ((7 2) Paper) ((7 3) Paper) ((7 4) Paper)
     ((7 5) Empty) ((7 6) Paper) ((7 7) Paper) ((7 8) Paper) ((7 9) Paper)
     ((8 0) Empty) ((8 1) Paper) ((8 2) Paper) ((8 3) Paper) ((8 4) Paper)
     ((8 5) Paper) ((8 6) Paper) ((8 7) Paper) ((8 8) Paper) ((8 9) Empty)
     ((9 0) Paper) ((9 1) Empty) ((9 2) Paper) ((9 3) Empty) ((9 4) Paper)
     ((9 5) Paper) ((9 6) Paper) ((9 7) Empty) ((9 8) Paper) ((9 9) Empty))
    |}]
