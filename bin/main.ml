open Gg
open Vg
open! Brr
open! Brr_canvas
module Html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Js = Js_of_ocaml.Js

(* 1. Define your image *)

(* let aspect = 1.618 *)
let size = Size2.v 100. 100. (* mm *)
let _image = I.const (Color.v_srgb 0.314 0.784 0.471)
let image = I.const Color.blue

(* let box = *)
(*   I.cut *)
(*     (P.rect (Box2.v (P2.v 30. 0.) (P2.v 10. 10.)) P.empty) *)
(*     (I.const Color.green) *)

(* let grid, h, w = Images.Day9Image.construct_cell *)
(* let image = I.blend grid image *)
let view = Box2.v P2.o (Size2.v 1. 1.)
let _size = Size2.v (1. *. 10.) (1. *. 10.)
(* mm *)
(* 2. Render *)

let render oc =
  let title = "Vgr_svg minimal example" in
  let description = "Emerald Color" in
  let xmp = Vgr.xmp ~title ~description () in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn (Vgr_svg.target ~xmp ()) (`Channel oc) in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End)

(* 3. Main *)

let _main () =
  Out_channel.set_binary_mode stdout true;

  render stdout;
  0

(* module FetchInput = struct *)
(*   let fetch_text url = *)
(*     let ( let* ) = Fut.bind in *)
(*     let req = Brr_io.Fetch.Request.v (Jstr.v url) in *)
(*     let* resp = Brr_io.Fetch.request req in *)
(*     match resp with *)
(*     | Ok r -> *)
(*         Fut.return *)
(*           (Brr_io.Fetch.Response.as_body r *)
(*           |> Brr_io.Fetch.Body.text *)
(*           |> Fut.to_promise ~ok:(fun a -> Jstr.to_string a |> Jv.of_string)) *)
(*     | Error err -> failwith (Jv.Error.message err |> Jstr.to_string) *)
(* end *)

let canvas = Canvas.create []
let () = El.append_children (Document.body G.document) [ Canvas.to_el canvas ]
let r = Vgr.create (Vgr_htmlc.target canvas) `Other
let _ = ignore (Vgr.render r (`Image (size, view, image)))
let _ = ignore (Vgr.render r `End)

(* let _ = *)
(*   let open Fut.Syntax in *)
(*   let* out = FetchInput.fetch_text "http://localhost:8000/input/day9.txt" in *)
(*   Console.log [ out ]; *)
(*   Fut.return out *)
