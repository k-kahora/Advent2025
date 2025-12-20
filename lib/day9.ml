module Part1 = struct
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
      A.(lift2 (fun a b -> (a, b)) number @@ take_while digit)
    in
    let many = A.(many_till each_row end_of_input) in
    match A.parse_string ~consume:Prefix many input with
    | Ok a -> a
    | Error err -> failwith err
end

module Part1Test = struct
  open Base
  open Stdio
  include Part1

  let%expect_test "hello form part1 test" =
    let x = example |> parse_input in
    print_s [%sexp (x : (string * string) list)];
    [%expect ""]
end
(* Brute forec *)
