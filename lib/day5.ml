(* Create a list of all ranges for each input iterate through the ranges *)

module Part2 = struct
  (* NOTE Create ranges with the assertion low is less than high *)
  (* Parse the input and construct a list of ranges *)
  (* sort the ranges by the low value *)
  (* Do a O(n) iteration through the list merging overlapping intervals *)
  (* get a reduced list of intervals *)
  (* Easily do a high - low subtraction to get the total values *)
  module Range = struct
    type t = int * int

    let h (_, h) = h
    let l (l, _) = l

    let sexp_of_t ((low, high) as _range) =
      let module Sex = Base.Sexp in
      let str = string_of_int in
      let low, high = ("l:" ^ str low, "r:" ^ str high) in
      Sex.List [ Sex.Atom low; Sex.Atom high ]

    exception InvalidRange of string

    let compare_ranges : t -> t -> int =
     fun r1 r2 ->
      let (low_r1, _), (low_r2, _) = (r1, r2) in
      compare low_r1 low_r2

    let sort_range (range_list : t list) : t list =
      List.sort compare_ranges range_list

    let create : int -> int -> t =
     fun low high ->
      if low > high then
        raise
        @@ InvalidRange
             (Printf.sprintf "InvalidRange: %d is greated thean %d" low high)
      else (low, high)
  end

  let input = {|3-5
10-14
13-13
16-20
12-18

1
5
8
11
17
32 |}

  (* let convert_to_range *)
  let craft_ranges (range : string) : Range.t =
    let module A = Angstrom in
    let get_int =
      A.(
        take_while (function '0' .. '9' -> true | _ -> false)
        >>| int_of_string)
    in
    let result =
      A.lift2
        (fun low high -> Range.create low high)
        get_int
        A.(char '-' *> get_int)
    in
    match A.parse_string ~consume:All result range with
    | Ok r -> r
    | Error msg -> failwith msg

  let parse_input input =
    let module A = Angstrom in
    let _newline = A.end_of_line in
    let non_empty =
      A.(take_while1 (fun a -> a <> '\n' && a <> '\r') <* end_of_line)
    in
    let final = A.(many_till non_empty end_of_line <?> "hero") in
    match A.parse_string ~consume:Prefix final input with
    | Ok v -> v
    | Error err -> failwith err

  (* ranges only overlap when the high of one is >= the low of another OR *)
  (*  *)
  let merge_range r1 r2 : Range.t =
    let module R = Range in
    (* assert (R.h r1 < R.l r2); *)
    let low = R.l r1 in
    let high = max (R.h r1) (R.h r2) in
    Range.create low high

  let merged_ranges (ranges : Range.t list) =
    let module R = Range in
    let ans = ref [] in
    let rec fold ~ranges =
      match ranges with
      | a :: b :: rest when R.h a >= R.l b ->
          let merged = merge_range a b in
          fold ~ranges:(merged :: rest)
      (* | _ :: (_ :: _ as rest) -> fold ~acc ~ranges:rest *)
      | a :: rest ->
          ans := a :: !ans;
          fold ~ranges:rest
      | [] -> ()
    in
    fold ~ranges;
    !ans |> List.rev

  (* |> List.rev *)
  let calc_all_ranges (ranges : Range.t list) : int =
    let module R = Range in
    List.fold_left (fun acc a -> R.h a - R.l a + acc + 1) 0 ranges

  (* assert Range.low  *)

  (* Parse all characters up to the new line *)
  (* Parse each character betwewn low and hight *)
end

module Part2Test = struct
  open Base
  open Stdio
  include Part2

  let%expect_test "[day5] parse input" =
    (* let x = parse_input @@ Input.get_input ~year:2025 ~day:05 in *)
    let x = parse_input @@ Part2.input in
    print_s [%sexp (x : string list)];
    [%expect "(3-5 10-14 13-13 16-20 12-18)"];
    let range = "1234787764565-24565467845645" |> craft_ranges in
    print_s [%sexp (range : Range.t)];
    [%expect "(l:1234787764565 r:24565467845645)"];
    let ranges = Stdlib.List.map craft_ranges x in
    print_s [%sexp (ranges : Range.t list)];
    [%expect "((l:3 r:5) (l:10 r:14) (l:13 r:13) (l:16 r:20) (l:12 r:18))"];
    let ranges_sorted = Range.sort_range ranges in
    print_s [%sexp (ranges_sorted : Range.t list)];
    [%expect "((l:3 r:5) (l:10 r:14) (l:12 r:18) (l:13 r:13) (l:16 r:20))"];
    let ranges_merged = merged_ranges ranges_sorted in
    print_s [%sexp (ranges_merged : Range.t list)];
    [%expect "((l:3 r:5) (l:10 r:20))"];
    let output = calc_all_ranges ranges_merged in
    print_s [%sexp (output : int)];
    [%expect "14"]
end

module Part1 = struct
  let input = {| 3-5
10-14
16-20
12-18

1
5
8
11
17
32 |}

  module Range : sig
    type t

    val in_range : t -> int -> bool
    val init : int -> int -> t
    val sexp_of_t : t -> Sexplib0.Sexp.t
  end = struct
    open Sexplib.Std

    type t = { low : int; high : int } [@@deriving sexp]

    let in_range range value : bool = value >= range.low && value <= range.high
    let init (low : int) (high : int) = { low; high }
  end

  let x = Range.sexp_of_t

  let rec ranges (acc : string list) (input : string list) =
    match input with
    | "" :: rest -> (acc |> List.rev, rest)
    | range :: tail -> ranges (range :: acc) tail
    | _ -> failwith "failure match should not be reached"

  let parse_range ranges =
    let open Angstrom in
    let int =
      take_while (function '0' .. '9' -> true | _ -> false) >>| int_of_string
    in
    let range = lift2 (fun low high -> (low, high)) int (char '-' *> int) in
    match parse_string ~consume:All range ranges with
    | Ok (low, high) -> Range.init low high
    | Error msg -> failwith msg

  let trim_list = List.filter (fun a -> a <> "")

  let parse_input input : string list * string list =
    String.split_on_char '\n' input |> List.map String.trim |> ranges []
    |> fun (a, b) -> (trim_list a, trim_list b)

  let fresh_items (ranges : Range.t list) (ids : int list) =
    List.filter_map
      (fun id ->
        match List.exists (fun range -> Range.in_range range id) ranges with
        | true -> Some id
        | false -> None)
      ids
end
(* NOTE Part2 the goal of part2 is to merge all ranges and return every value in the final merged range*)

module Part1Test = struct
  open Base
  open Stdio
  include Part1

  let%expect_test "[day5] parse input" =
    (* let t_input = Input.get_input ~year:2025 ~day:05 in *)
    let range = "15468-16788" in
    let out = parse_range range in
    print_s [%sexp (out : Range.t)];
    [%expect {| ((low 15468) (high 16788)) |}];
    let fresh, ids = parse_input input in
    (* let fresh, ids = parse_input t_input in *)
    print_s [%sexp (fresh : string list)];
    print_s [%sexp (ids : string list)];
    [%expect {|
      (3-5 10-14 16-20 12-18)
      (1 5 8 11 17 32)
      |}];
    let ids = Stdlib.List.map Stdlib.int_of_string ids in
    print_s [%sexp (ids : int list)];
    [%expect {| (1 5 8 11 17 32) |}];
    let ranges = Stdlib.List.map parse_range fresh in
    print_s [%sexp (ranges : Range.t list)];
    [%expect {|
      (((low 3) (high 5)) ((low 10) (high 14)) ((low 16) (high 20))
       ((low 12) (high 18)))
      |}];
    let freshes = fresh_items ranges ids in
    print_s [%sexp (freshes : int list)];
    [%expect {| (5 11 17) |}];
    let answer = List.length freshes in
    print_s [%sexp (answer : int)];
    [%expect {| 3 |}]
end
