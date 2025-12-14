(* NOTE TODO DONE  *)
module Part1 = struct
  let input =
    {| 123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +  
|}

  let parse_input input =
    let module A = Angstrom in
    (* Parse a row of numbers and convert to a int list  *)
    (* Do this up until math symbols *)
    let eat_whitespace =
      A.( >>| )
        A.(skip_while @@ function ' ' | '\t' | '\r' -> true | _ -> false)
        (fun () -> int_of_string)
    in

    let integer =
      A.(take_while1 (function '0' .. '9' -> true | _ -> false))
    in
    let get_numbers =
      A.(many_till (eat_whitespace *> integer <* eat_whitespace) (char '\n'))
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
  include Part1
  open Base
  open Stdio

  let%expect_test "[day6] test" =
    (*     let input = {|1020 45656 *)
(* |} in *)
    (* let s = input |> parse_input in *)
    let s = Part1.input |> parse_input in
    (* let s = Input.get_input ~year:2025 ~day:06 |> parse_input in *)
    print_s [%sexp (s : string list list * char list)];
    [%expect {|11|}]
end
