open Notty
open Lwt

let whenopt f = function Some x -> f x | None -> ()

let write_fd fd b =
  let n = Bytes.length b in
  let rec go acc =
    if acc < n then
      Lwt_unix.write fd b acc (n - acc) >>= fun w -> go (acc + w)
    else return_unit in
  go 0

module Lwt_condition = struct

  include Lwt_condition

  let fmap f c =
    let d = create () in
    let rec go () =
      wait c >>= fun x -> f x |> whenopt (broadcast d); go ()
    in (async go; d)

  let unburst ~t c =
    let d = create () in
    let rec go last =
      wait c >>= fun x ->
        last := false;
        let pending = ref true in
        async (fun () ->
          Lwt_unix.sleep t >|= fun () ->
            if !pending then broadcast d x);
        go pending in
    async (fun () -> go (ref false));
    d
end

module Terminal = struct

  open IO_helpers

  let winches = lazy (
    let c = Lwt_condition.create () in
    let `Revert _ = set_winch_handler (Lwt_condition.broadcast c) in
    c
  )

  let next_winch () = Lwt_condition.wait (Lazy.force winches)

  let create_input_stream fd =
    let `Revert f = setup_tcattr (Lwt_unix.unix_file_descr fd) in
    let stream =
      let flt  = Unescape.create ()
      and ibuf = Bytes.create 64 in
      let rec read () =
        match Unescape.next_k flt with
        | `Uchar _ | `Key _ as r -> return_some r
        | `End   -> return_none
        | `Await ->
            Lwt_unix.read fd ibuf 0 Bytes.(length ibuf) >>=
              fun n -> Unescape.input flt ibuf 0 n; read ()
      in Lwt_stream.from read
    in
    Lwt_stream.on_terminate stream f;
    (stream, f)

  type t = {
    ochan  : Lwt_io.output_channel;
    trm    : Tmachine.t;
    input  : [`Uchar of uchar | `Key of Unescape.key] Lwt_stream.t * (unit -> unit);
    size_c : (int * int) Lwt_condition.t
  }

  let next_resize t = Lwt_condition.wait t.size_c

  let rec write t =
    match Tmachine.output t.trm with
    | `Output s -> Lwt_io.write t.ochan s >>= fun () -> write t
    | `Await    -> Lwt_io.flush t.ochan

  let release t =
    if Tmachine.finish t.trm then
      ( snd t.input (); write t )
    else return_unit

  let redraw t       = Tmachine.refresh t.trm; write t
  let image t image  = Tmachine.image t.trm image; write t
  let cursor t curs  = Tmachine.cursor t.trm curs; write t
  let set_size t dim = Tmachine.set_size t.trm dim

  let create_size_sig fd =
    let fd = Lwt_unix.unix_file_descr fd in
    Lwt_condition.(
      Lazy.force winches |> unburst ~t:0.1 |> fmap (fun () -> winsize fd)
    )

  let winch_monitor autosize t =
    let rec go () =
      next_resize t >|= set_size t >>= fun () ->
        if autosize then redraw t >>= go else go () in
    go

  let create ?(dispose=true) ?(autosize=true) ?(input=Lwt_unix.stdin) ?(output=Lwt_unix.stdout) () =
    let fd = Lwt_unix.unix_file_descr output in
    let t = {
        trm    = Tmachine.create (cap_for_fd fd)
      ; ochan  = Lwt_io.(of_fd ~mode:output) output
      ; input  = create_input_stream input
      ; size_c = create_size_sig output
      } in
    async (fun () -> (winsize fd |> whenopt (set_size t)); redraw t);
    async (winch_monitor autosize t);
    if dispose then at_exit (fun () -> async (fun () -> release t));
    t

  let input t = fst t.input

  let size t = Tmachine.size t.trm

end

let output_image =
  IO_helpers.output_image_gen
    ~to_fd:Lwt_unix.unix_file_descr
    ~write:(fun fd buf -> write_fd fd (Buffer.contents buf))

let print_image = output_image Lwt_unix.stdout
