[@@@ocaml.warning "-33-9-8-32"]

open Piaf

let fetch_aoc_input ~env ~sw ~session year day =
  let url =
    Uri.of_string
      (Printf.sprintf "https://adventofcode.com/%d/day/%d/input" year day)
  in

  let headers =
    [
      ("Cookie", "session=" ^ session);
      ("User-Agent", "malcolm-aoc-client") (* optional but recommended *);
    ]
  in

  match Piaf.Client.Oneshot.get ~sw ~headers env url with
  | Error e -> failwith (Piaf.Error.to_string e)
  | Ok { Response.status = `OK; body } -> (
      match Body.to_string body with
      | Ok s -> s
      | Error _ -> failwith "Failed to read body")
  | Ok r ->
      failwith
        (Printf.sprintf "Unexpected status: %s"
           (Piaf.Status.to_string r.Response.status))

let get_input ~year ~day =
  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          let session =
            "53616c7465645f5fdd7c2d1b6c9d19a8a269b0ed0af1d9f5f5be03ffd7192dce5fe098609496b0f729cae9a2fc7707909e6d24470c04bb4e95166981d3c93fc0"
          in

          let input = fetch_aoc_input ~env ~sw ~session year day in
          input))
