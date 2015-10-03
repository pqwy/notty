open Notty

module Terminal = struct

  open Lwt
  open IO_helpers

  type event = [ `Uchar of uchar | `Key of Unescape.key ]

  let map_cond f c =
    let d = Lwt_condition.create () in
    let rec go () =
      Lwt_condition.wait c >>= fun x -> Lwt_condition.broadcast d (f x); go ()
    in async go; d

  let inputs_of fd =
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

  let winches = lazy (
    let c = Lwt_condition.create () in
    let `Revert _ = set_winch_handler (Lwt_condition.broadcast c) in
    c
  )

  type t = {
    out_fd : Lwt_unix.file_descr;
    out_ch : Lwt_io.output_channel;
    trm    : Tmachine.t;
    input  : (event Lwt_stream.t * (unit -> unit));
    size_c : (int * int) Lwt_condition.t
  }

  let fdsize fd =
    match winsize Lwt_unix.(unix_file_descr fd) with
    | Some d -> d | _ -> (0, 0)

  let rec write t =
    match Tmachine.output t.trm with
    | `Output s -> Lwt_io.write t.out_ch s >>= fun () -> write t
    | `Await    -> Lwt_io.flush t.out_ch

  let resize t dim = Tmachine.resize t.trm dim; write t

  let size_sig fd =
    Lazy.force winches |> map_cond @@ fun () -> fdsize fd

  let wait_resize t = Lwt_condition.wait t.size_c

  let autoresize t =
    let rec go () = wait_resize t >>= resize t >>= fun () -> go () in
    async go

  let release t =
    if Tmachine.finish t.trm then
      ( snd t.input (); write t )
    else return_unit

  let create ?(dispose=true) ?(winch=true) ?(input=Lwt_unix.stdin) ?(output=Lwt_unix.stdout) () =
    let fd     = Lwt_unix.unix_file_descr output in
    let size_c = size_sig output
    and input  = inputs_of input
    and trm    = Tmachine.create (cap_for_fd fd)
    and out_fd = output
    and out_ch = Lwt_io.(of_fd ~mode:output) output in
    let t = { out_fd; out_ch; trm; input; size_c } in
    ignore (fdsize out_fd |> resize t);
    if winch then autoresize t;
    if dispose then at_exit (fun () -> async (fun () -> release t));
    t

  let inputs t = fst t.input
  let refresh t = Tmachine.refresh t.trm; write t
  let update t ?cursor image = Tmachine.image t.trm cursor image; write t
  let size t = Tmachine.size t.trm

end
