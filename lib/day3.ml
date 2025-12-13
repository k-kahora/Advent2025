(* NOTE Part2 monotonic stack with deletions credits *)
(* NOTE TODO I do not truly understand monotonci stack my functions need some serious work *)

let[@ocaml.warning "-32"] test_input =
  {| 987654321111111
811111111111119
234234234234278
818181911112111 |}

(* let test_input = *)
(*   {|4732321333332463233337712234322122322247222252423773321362313613333336333732233372323328332333322777|} *)

module Part1 = struct
  let print_lists list1 list2 =
    let open Base in
    let open Stdio in
    print_s [%sexp (list1 : int list)];
    print_s [%sexp (list2 : int list)];
    print_endline ""

  let get_banks input =
    String.split_on_char '\n' input
    |> List.map Base.String.strip
    |> List.filter (fun a -> a <> "")

  let bank_to_int_list =
   fun bank ->
    Base.String.to_list bank
    |> List.map (String.make 1)
    |> List.map int_of_string

  let banks_to_int_list banks = List.map bank_to_int_list banks

  (* NOTE pre compute maximum suffix at each character non inclusive start from the back *)
  let joltage_suffix_list : int list -> int list =
   fun bank ->
    List.rev bank |> fun [@ocaml.warning "-8"] (a :: rest) ->
    List.fold_left
      (fun (maximum_jolt_list, max_jolt) batterie ->
        if batterie > max_jolt then (batterie :: maximum_jolt_list, batterie)
        else (max_jolt :: maximum_jolt_list, max_jolt))
      ([ a ], a) rest
    |> fun (_ :: a, _) -> a

  let maximum_joltage : int list -> int list -> int =
   fun bank joltage_maximum_list ->
    Base.List.fold2_exn bank joltage_maximum_list ~init:0
      ~f:(fun max_jolt batterie best_jolt ->
        max max_jolt @@ ((batterie * 10) + best_jolt))

  let total_joltage : int list -> int =
   fun joltages -> List.fold_left ( + ) 0 joltages

  let rec remove_last = function
    | [] -> []
    | [ _a ] -> []
    | hd :: rest -> hd :: remove_last rest

  let joltify_bank : int list -> int =
   fun bank ->
    let[@ocaml.warning "-8"] suffix_list = joltage_suffix_list bank in
    (* let bank = 0 :: a |> remove_last in *)
    let bank = bank |> remove_last in
    let result = maximum_joltage bank suffix_list in
    result

  let part1 input =
    let banks = get_banks input |> banks_to_int_list in
    List.map joltify_bank banks |> total_joltage
end

module [@ocaml.warning "-32"] _ : sig
  val part1 : string -> int
  val joltify_bank : int list -> int
  val remove_last : 'a list -> 'a list
  val get_banks : string -> string list
end =
  Part1

module Part2 = struct
  (* NOTE size 12 is needed so! *)
  (* Create a monotnonically decreasing stack of size 12 maximum *)
  (* Only add if its less than top of the stack *)
  (* 234234234234278 *)
  (* 15 chars 12 max 3 delete tokens *)

  (*  [ 2 ] *)
  (* pop token = 2 *)
  (* [ 3 ] *)
  (* pop token = 1 *)
  (* [ 4 ] *)
  (* [ 4 ] token = 0 *)
  (* add the rest *)
  type int_list = Base.Int.t Base.List.t [@@deriving sexp]

  let total_delete_tokens bank = List.length bank - 12

  (* This is not operating like a true monotonic stack!  *)
  let super_joltage (bank : int list) =
    let skip_tokens = total_delete_tokens bank in
    let pop = function
      | [] -> failwith "can not pop empty list"
      | _ :: rest -> rest
    in

    let rec loop stack skip_tokens (bank : int list) =
      let[@ocaml.warning "-8"] (stack_head :: _) = stack in
      (* sexp_of_int_list stack |> Base.Sexp.to_string |> print_endline; *)
      (* print_endline (string_of_int skip_tokens); *)
      match bank with
      | [] -> List.rev stack
      | a when skip_tokens = 0 -> List.rev stack @ a
      (* | _ when List.length stack = 12 -> List.rev stack *)
      | batterie :: bank when batterie > stack_head && skip_tokens > 0 ->
          loop (batterie :: pop stack) (skip_tokens - 1) bank
      | batterie :: bank when batterie <= stack_head && List.length stack < 12
        ->
          loop (batterie :: stack) skip_tokens bank
      | _ :: bank -> loop stack skip_tokens bank
    in
    loop [ -1 ] (skip_tokens + 1)
      bank (* NOTE add one to skip tokens to account for the first skip of -1 *)

  let total_joltage = List.fold_left ( + ) 0

  let create_number_from_int_list : int list -> int =
   fun bank ->
    let[@ocaml.warning "-8"] (head :: tail) = bank in
    List.fold_left
      (fun final_number batt -> (final_number * 10) + batt)
      head tail

  let mutate_into_joltage : int list list -> int list =
   fun banks ->
    List.map Mono.maximum_joltage banks |> List.map create_number_from_int_list

  let get_banks input =
    String.split_on_char '\n' input
    |> List.map Base.String.strip
    |> List.filter (fun a -> a <> "")

  let bank_to_int_list =
   fun bank ->
    Base.String.to_list bank
    |> List.map (String.make 1)
    |> List.map int_of_string

  let banks_to_int_list banks = List.map bank_to_int_list banks
end

module TestPart2 = struct
  include Part2
  open Base
  open Stdio

  let%expect_test "[part2] -> get banks" =
    (* let banks = bank_to_int_list "811111111111119" |> Mono.maximum_joltage in *)
    (* let b = banks |> Mono.maximum_joltage in *)
    let t_inpt = Input.get_input ~day:03 ~year:2025 in
    let banks = get_banks t_inpt |> banks_to_int_list |> mutate_into_joltage in
    (* let length = *)
    (*   List.map ~f:(fun a -> Int.to_string a |> String.length) banks *)
    (* in *)
    let output = banks |> total_joltage in
    print_s [%sexp (banks : int list)];
    print_s [%sexp (output : int)];
    [%expect {|
      (833333322777 966544442433 755555543422 887777292252 977747379332
       977342231132 863333333232 666665444552 887533422322 999888888668
       865544522212 776445342425 887942232232 998866374632 776666666255
       665333222232 999887754441 882323233232 999977332433 998885554338
       987777776448 555554435231 777777777789 988866644568 874223241124
       555544522241 555555555555 997666686645 766656623494 977744453224
       997665544622 999995833433 888888886554 886642312332 886534333331
       888888888889 555544242222 555555556789 999888877776 998745258241
       888765443322 877942254444 882212231241 774443333332 999998555944
       755242326282 874222255123 988885644646 872512222242 995555343336
       887642223335 663334323221 987777753322 999995222242 667311226262
       986443447441 644412322532 876653342324 943327532243 998733332413
       877777664444 966762776355 666554333334 887553273365 444332312223
       999999999999 775574454555 954244423428 765555444443 888777777547
       887544275214 766553442343 999883244346 886645355133 988876655446
       999887553331 766332432122 954444453135 887665444645 772231232322
       997544443443 555533223215 999999977226 855555443334 995423223322
       999738332226 966934832733 655565266255 765442523452 999998679376
       998653353252 988888874333 654433322333 999888546378 998887778775
       666666555333 877413226222 877755555554 977777565446 999877591242
       988888876647 644433333333 842332112131 999999987776 644443333334
       994534445237 977765734414 977644245545 999998242357 654444443342
       856546453344 885654433645 997844474564 998342733233 955333332232
       775555555442 776665555551 766665324322 776666824255 977777544742
       553322243222 999998888875 655522222222 999998574332 671233424363
       555533233332 944423223222 998886652668 743333322222 855423422433
       977434333443 777545432121 651372322222 978292212344 888654353455
       986484222539 966313346423 987444362359 885544444452 776644513343
       777655555555 957332232433 773333343944 988887764453 999866547274
       666666666789 985226438314 977774433642 855341544444 977977485216
       985553335335 944444445354 733342132321 633333222223 645222222221
       988888888877 777222412212 777777666654 999516337422 777555361431
       555555433532 998888753263 877777654443 997777453142 444443222222
       999988799798 665555554455 987333211222 875433326742 999767457133
       444444456789 755333454252 986665443345 998866435565 866655522554
       777777777774 666655333344 877742534421 642522222221 966666654443
       988885689647 877777776636 995333322224 999471456483 997777834144
       877766678445 965693731211 986666654466 953433332253 887554443564
       888554363637 664444444444 977777766743 775555643413 444444433353
       999999999999 655532223223 985555555573 962222212222 865544443222)
      172601598658203
      |}]
end

module TestPart1 = struct
  open Base
  open Stdio
  include Part1

  let%expect_test "[day3-part1]" =
    (* let t_input = part1 test_input in *)
    let puzzle_input = Input.get_input ~year:2025 ~day:03 in
    let output = puzzle_input |> part1 in
    print_s [%sexp (output : int)];
    [%expect {| 17383 |}]

  let%expect_test "[day3] maximum joltage" =
    let bank = bank_to_int_list "234234234234278" in
    let[@ocaml.warning "-8"] suffix_list = bank |> joltage_suffix_list in
    let[@ocaml.warning "-8"] bank = bank |> remove_last in
    (* print_s [%sexp (bank : int list)]; *)
    (* print_s [%sexp (suffix_list : int list)]; *)
    let output = maximum_joltage bank suffix_list in
    print_s [%sexp (bank : int list)];
    print_s [%sexp (suffix_list : int list)];
    print_s [%sexp (output : int)];
    [%expect {|
      (2 3 4 2 3 4 2 3 4 2 3 4 2 7)
      (8 8 8 8 8 8 8 8 8 8 8 8 8 8)
      78
      |}]

  let%expect_test "[day3] joltage_suffix_list" =
    let output = bank_to_int_list "818181911112111" |> joltage_suffix_list in
    print_s [%sexp (output : int list)];
    [%expect {| (9 9 9 9 9 9 2 2 2 2 2 1 1 1) |}]

  let%expect_test "[day3] parse input" =
    let output = get_banks test_input |> banks_to_int_list in
    print_s [%sexp (output : int list list)];
    [%expect
      {|
    ((9 8 7 6 5 4 3 2 1 1 1 1 1 1 1) (8 1 1 1 1 1 1 1 1 1 1 1 1 1 9)
     (2 3 4 2 3 4 2 3 4 2 3 4 2 7 8) (8 1 8 1 8 1 9 1 1 1 1 2 1 1 1))
    |}]
end
