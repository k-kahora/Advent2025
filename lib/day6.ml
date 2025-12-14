(* NOTE TODO DONE  *)
module Part2 = struct
  let input =
    {|123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
|}

  let itemize : string -> char list list =
   fun input ->
    String.split_on_char '\n' input
    |> List.filter (fun a -> a <> "")
    |> List.map Base.String.to_list

  let sanezite : char list -> int list =
   fun c ->
    List.filter (fun a -> a <> ' ') c
    |> List.map (fun a -> Base.Char.to_string a |> int_of_string)

  let convert_to_numbers (thing : int list) =
    let rec fold input acc =
      match input with
      | [] -> acc
      | [ a ] -> acc + a
      | a :: rest -> fold rest @@ ((acc + a) * 10)
    in
    fold thing 0

  let true_list input = Base.List.transpose input

  let parse_input input =
    let module A = Angstrom in
    let numbers = A.(take_while (function '*' | '+' -> false | _ -> true)) in
    let symbol_parse =
      let eat_whitespace =
        A.(skip_while @@ function ' ' | '\t' | '\r' -> true | _ -> false)
      in
      let symbol = A.satisfy (function '*' | '+' -> true | _ -> false) in
      A.(many_till (eat_whitespace *> symbol <* eat_whitespace) (char '\n'))
    in
    let both = A.(lift2 (fun a b -> (a, b)) numbers symbol_parse) in
    match A.parse_string ~consume:Prefix both input with
    | Ok e -> e
    | Error e -> failwith e

  let chunk_by_zero input =
    let ans = ref [] in
    let rec fold input acc =
      match input with
      | 0 :: rest ->
          let acc = !ans :: acc in
          ans := [];
          fold rest acc
      | a :: rest ->
          ans := a :: !ans;
          fold rest acc
      | [] -> !ans :: acc
    in
    fold input [] |> List.rev

  let summate (a : int list list) (c : char list) =
    List.map2
      (fun numbers symbol ->
        match symbol with
        | '+' -> List.fold_left ( + ) 0 numbers
        | '*' -> List.fold_left ( * ) 1 numbers
        | a -> failwith @@ Printf.sprintf "%c is not valid" a)
      a c
end

module Part2Test = struct
  include Part2
  open Base
  open Stdio

  let%expect_test "[Day6] part2" =
    let[@ocaml.warning "-26"] real_input = Input.get_input ~year:2025 ~day:06 in
    let c, symbol_list =
      parse_input Part2.input
      (* |> true_list |> Stdlib.Option.get *)
    in
    print_s [%sexp (symbol_list : char list)];
    [%expect {| (* + * +) |}];
    print_s [%sexp (c : string)];
    [%expect " 
  \"123 328  51 64 \\
 \\n 45 64  387 23 \\
 \\n  6 98  215 314\\
 \\n\"
 "];
    let b = c |> itemize in
    let trans = c |> itemize |> true_list |> Stdlib.Option.get in
    print_s [%sexp (b : char list list)];
    [%expect " 
 ((1 2 3 \" \" 3 2 8 \" \" \" \" 5 1 \" \" 6 4 \" \")
  (\" \" 4 5 \" \" 6 4 \" \" \" \" 3 8 7 \" \" 2 3 \" \")
  (\" \" \" \" 6 \" \" 9 8 \" \" \" \" 2 1 5 \" \" 3 1 4))
 "];
    print_s [%sexp (trans : char list list)];
    [%expect " 
 ((1 \" \" \" \") (2 4 \" \") (3 5 6) (\" \" \" \" \" \") (3 6 9) (2 4 8) (8 \" \" \" \")
  (\" \" \" \" \" \") (\" \" 3 2) (5 8 1) (1 7 5) (\" \" \" \" \" \") (6 2 3) (4 3 1)
  (\" \" \" \" 4))
 "];
    let out_n = trans |> Stdlib.List.map sanezite in
    print_s [%sexp (b : char list list)];
    [%expect " 
 ((1 2 3 \" \" 3 2 8 \" \" \" \" 5 1 \" \" 6 4 \" \")
  (\" \" 4 5 \" \" 6 4 \" \" \" \" 3 8 7 \" \" 2 3 \" \")
  (\" \" \" \" 6 \" \" 9 8 \" \" \" \" 2 1 5 \" \" 3 1 4))
 "];
    print_s [%sexp (out_n : int list list)];
    [%expect " 
 ((1) (2 4) (3 5 6) () (3 6 9) (2 4 8) (8) () (3 2) (5 8 1) (1 7 5) ()
  (6 2 3) (4 3 1) (4))
 "];
    let numbers = out_n |> Stdlib.List.map convert_to_numbers in
    print_s [%sexp (numbers : int list)];
    [%expect "(1 24 356 0 369 248 8 0 32 581 175 0 623 431 4)"];
    let chunked = numbers |> chunk_by_zero in
    print_s [%sexp (chunked : int list list)];
    [%expect "((356 24 1) (8 248 369) (175 581 32) (4 431 623))"];
    let sum = summate chunked symbol_list in
    print_s [%sexp (sum : int list)];
    [%expect "(8544 625 3253600 1058)"];
    let final = Stdlib.List.fold_left ( + ) 0 sum in
    print_s [%sexp (final : int)];
    [%expect "3263827"]
end

module Part1 = struct
  let input =
    {| 123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +  
|}

  let convert_to_coloumns input = Base.List.transpose input

  let finish_homework (numbers : int list list) (symbols : char list) =
    List.map2
      (fun nums symbol ->
        match symbol with
        | '*' -> List.fold_left ( * ) 1 nums
        | '+' -> List.fold_left ( + ) 0 nums
        | a -> failwith (Printf.sprintf "%c is not valid" a))
      numbers symbols

  let parse_input input =
    let module A = Angstrom in
    (* Parse a row of numbers and convert to a int list  *)
    (* Do this up until math symbols *)
    let eat_whitespace =
      A.(skip_while @@ function ' ' | '\t' | '\r' -> true | _ -> false)
    in

    let integer =
      A.(take_while1 (function '0' .. '9' -> true | _ -> false))
    in
    let get_numbers =
      A.(
        many_till
          (eat_whitespace *> integer <* eat_whitespace >>| int_of_string)
          (char '\n'))
    in
    let final =
      A.(
        many_till get_numbers
          ( peek_char_fail >>= function
            | '*' | '+' -> A.return ()
            | _ -> A.fail "" ))
    in
    let get_arithmatic_symbol =
      A.(satisfy (function '*' | '+' -> true | _ -> false))
    in
    let symbols =
      A.(eat_whitespace *> get_arithmatic_symbol <* eat_whitespace)
    in
    let all_syambols = A.(many_till symbols (char '\n')) in
    let final_parse = A.(lift2 (fun a b -> (a, b)) final all_syambols) in
    match A.parse_string ~consume:Prefix final_parse input with
    | Ok a -> a
    | Error err -> failwith err
end

module Part1Test = struct
  [@@@ocaml.warning "-26-27"]

  include Part1
  open Base
  open Stdio

  let%expect_test "[day6] test" =
    let ((number, symbols) as s) = Part1.input |> parse_input in
    print_s [%sexp (s : int list list * char list)];
    [%expect {| (((123 328 51 64) (45 64 387 23) (6 98 215 314)) (* + * +)) |}];
    let transpose = number |> Part1.convert_to_coloumns |> Stdlib.Option.get in
    print_s [%sexp (transpose : int list list)];
    [%expect {| ((123 45 6) (328 64 98) (51 387 215) (64 23 314)) |}];
    let answer = Part1.finish_homework transpose symbols in
    print_s [%sexp (answer : int list)];
    [%expect {| (33210 490 4243455 401) |}];
    let final_answer =
      Part1.finish_homework transpose symbols |> Stdlib.List.fold_left ( + ) 0
    in
    print_s [%sexp (final_answer : int)];
    [%expect {| 4277556 |}]
end
