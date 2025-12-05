let test_input =
  {| 987654321111111
811111111111119
234234234234278
818181911112111 |}

module Part1 = struct
  let get_banks input =
    String.split_on_char '\n' input |> List.map Base.String.strip

  let banks_to_int_list banks =
    List.map
      (fun bank ->
        Base.String.to_list bank
        |> List.map (String.make 1)
        |> List.map int_of_string)
      banks

  (* NOTE pre compute maximum suffix at each character non inclusive start from the back *)
  (* let joltage_suffix_list : int list *)
end

include Part1
open Base
open Stdio

let%expect_test "[day3] parse input" =
  let output = get_banks test_input |> banks_to_int_list in
  print_s [%sexp (output : int list list)];
  [%expect {| out |}]
