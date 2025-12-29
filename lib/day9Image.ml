open Vg
open Day9_12
open Gg

let perform_grid () =
  let open Day9_12.Day9 in
  let tiles = Render.(parse_input real_input) in
  let g = Render.grid real_input in
  Base.(Stdio.(print_s [%sexp (g : Day9.Render.tile array array)]));
  Render.(place_tiles tiles g);
  let g = Base.Array.transpose g |> Stdlib.Option.get in
  g

let construct_cell =
  let grid = Day9.Render.grid Day9.real_input in
  let _ = perform_grid () in
  let color = function
    | Day9.Render.Empty -> Color.white
    | Red -> Color.red
    | Green -> Color.green
  in
  let final_image : image ref = ref I.void in
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      (* P.rect (Box2.v (P2.v x y) (V2.v box_size box_size)) P.empty *)
      let i_f, j_f = (float_of_int i, float_of_int j) in
      let box = P.rect (Box2.v (P2.v i_f j_f) (V2.v 1. 1.)) P.empty in
      let cell_color = I.const @@ color grid.(i).(j) in
      final_image := I.blend !final_image (I.cut box cell_color)
    done
  done;
  let f = float_of_int in
  (!final_image, Array.length grid |> f, Array.length grid.(0) |> f)

(* module FetchInput = struct *)
(*   let fetch_text url = *)
(*     let ( let* ) = Fut.bind in *)
(*     let req = Brr_io.Fetch.Request.v (Jstr.v url) in *)
(*     let* resp = Brr_io.Fetch.request req in *)
(*     match resp with *)
(*     | Ok r -> Fut.return @@ Brr_io.Fetch.Response.status r *)
(*     | Error err -> failwith (Jv.Error.message err |> Jstr.to_string) *)
(* end *)

(* let%expect_test "[render test]" = *)
(*   let x = Day9.Part1.example in *)
(*   print_s [%sexp (x : string)]; *)
(*   [%expect {||}] *)
