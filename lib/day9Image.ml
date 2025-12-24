open Vg
open Gg

let construct_cell =
  let grid = Day9.Render.grid in
  let final_image : image ref = ref I.void in
  for i = 0 to Array.length grid do
    for j = 0 to Array.length grid.(0) do
      (* P.rect (Box2.v (P2.v x y) (V2.v box_size box_size)) P.empty *)
      let i, j = (float_of_int i, float_of_int j) in
      let box = P.rect (Box2.v (P2.v i j) (V2.v 1. 1.)) P.empty in
      let color = I.const @@ Color.red in
      final_image := I.blend !final_image (I.cut box color)
    done
  done;
  !final_image

module FetchInput = struct
  let fetch_text url =
    let ( let* ) = Fut.bind in
    let req = Brr_io.Fetch.Request.v (Jstr.v url) in
    let* resp = Brr_io.Fetch.request req in
    match resp with
    | Ok r -> Fut.return @@ Brr_io.Fetch.Response.status r
    | Error err -> failwith (Jv.Error.message err |> Jstr.to_string)
end

open Base
open Stdio

let%expect_test "[render test]" =
  let x = Day9.Part1.example in
  print_s [%sexp (x : string)];
  [%expect {||}]
