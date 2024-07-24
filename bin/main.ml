module Reporter = struct
  module Message = struct
    type t = Hello [@@deriving show]

    let default_severity : t -> Asai.Diagnostic.severity = function
      | Hello -> Error

    let short_code : t -> string = function Hello -> "0001"
  end

  include Asai.Reporter.Make (Message)
end

module Store = Irmin_fs_unix.KV.Make (Irmin.Contents.String)
module Term = Asai.Tty.Make (Reporter.Message)

let test () =
  let config = Irmin_fs.config "store" in
  let repo = Store.Repo.v config in
  let main = Store.main repo in
  let info () = Store.Info.v 0L in
  let key = "Hello" in
  (* Eio.Switch.run @@ fun sw -> *)
  (* Eio.Fiber.fork ~sw (fun () -> *)
  (*     Reporter.fatalf Hello "I now fail with Unhandled!"); *)
  let () =
    match Store.set main [ key ] ~info "world!" with
    | _ ->
        Reporter.fatalf Hello
          {|I won't be reported by Asai, instead you'll see "Fatal error: exception Unhandled asai effect/exception; use Reporter.run|}
  in
  let v = Store.get main [ key ] in
  print_endline v

let () =
  Reporter.run ~emit:Term.display ~fatal:Term.display @@ fun () ->
  Eio_main.run @@ fun env ->
  let path = Eio.Path.(env#cwd / "store") in
  test ()
(* let clock = Eio.Stdenv.clock env in *)
(* test ~path ~clock *)
