open Gg
open Vg
open! Brr
open! Brr_canvas
module Html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Js = Js_of_ocaml.Js

(* 1. Define your image *)

let aspect = 1.618
let size = Size2.v (aspect *. 100.) 100. (* mm *)
let view = Box2.v P2.o (Size2.v aspect 1.)
let _image = I.const (Color.v_srgb 0.314 0.784 0.471)
let image = I.const Color.green

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

let canvas = Canvas.create ~w:600 ~h:400 []
let () = El.append_children (Document.body G.document) [ Canvas.to_el canvas ]
let r = Vgr.create (Vgr_htmlc.target canvas) `Other
let _ = ignore (Vgr.render r (`Image (size, view, image)))
let _ = ignore (Vgr.render r `End)
