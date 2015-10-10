open Notty

external c_winsize : Unix.file_descr -> int = "caml_notty_winsize" "noalloc"
external winch_number : unit -> int = "caml_notty_winch_number" "noalloc"

let whenopt f = function Some x -> f x | None -> ()

let winsize fd =
  match c_winsize fd with
  | 0  -> None
  | wh -> Some (wh lsr 16, wh land 0xff)

let cap_for_fd fd =
  match (Sys.getenv "TERM", Unix.isatty fd) with
  | exception Not_found         -> Cap.dumb
  | ((""|"dumb"), _)|(_, false) -> Cap.dumb
  | _                           -> Cap.ansi

let setup_tcattr fd =
  let open Unix in
  let tc = tcgetattr fd in
  tcsetattr fd TCSANOW { tc with c_icanon = false ; c_echo = false };
  `Revert (fun () -> tcsetattr fd TCSANOW tc)

let set_winch_handler f =
  let signum = winch_number () in
  let old_hdl = Sys.(signal signum (Signal_handle (fun _ -> f ()))) in
  `Revert (fun () -> Sys.set_signal signum old_hdl)

module Terminal = struct

  module Winch = struct

    module M = Map.Make (struct
      type t = int let compare (a:int) b = compare a b
    end)

    type remove = int

    let id = ref 0

    and hs = lazy (
      let r = ref M.empty in
      set_winch_handler (fun () -> !r |> M.iter (fun _ f -> f ())) |> ignore;
      r
    )

    let update f = let m = Lazy.force hs in m := f !m

    let add fd f =
      let x = !id in
      incr id;
      M.add x (fun () -> winsize fd |> whenopt f) |> update;
      x

    let remove x = update (M.remove x)
  end

  module Input = struct

    type t = {
      fd      : Unix.file_descr
    ; flt     : Unescape.t
    ; ibuf    : bytes
    ; cleanup : unit -> unit
    }

    let create fd =
      let flt  = Unescape.create ()
      and ibuf = Bytes.create 64
      and `Revert cleanup = setup_tcattr fd in
      { fd; flt; ibuf; cleanup }

    let rec input t =
      match Unescape.next_k t.flt with
      | `End | `Uchar _ | `Key _ as r -> r
      | `Await ->
          let n = Unix.read t.fd t.ibuf 0 Bytes.(length t.ibuf) in
          Unescape.input t.flt t.ibuf 0 n; input t

  end

  type t = {
    output   : out_channel
  ; trm      : Tmachine.t
  ; input    : Input.t
  ; unwinch  : Winch.remove Lazy.t
  ; mutable winched : bool
  }

  let rec write t =
    match Tmachine.output t.trm with
    | `Output s -> output_string t.output s; write t
    | `Await    -> flush t.output

  let release t =
    if Tmachine.finish t.trm then begin
      Winch.remove Lazy.(force t.unwinch);
      t.input.Input.cleanup ();
      write t
    end

  let set_size t dim = Tmachine.set_size t.trm dim
  let redraw t       = Tmachine.refresh t.trm; write t
  let image t image  = Tmachine.image t.trm image; write t
  let cursor t curs  = Tmachine.cursor t.trm curs; write t
  let size t         = Tmachine.size t.trm

  let create ?(dispose=true) ?(autosize=true) ?(input=Unix.stdin) ?(output=Unix.stdout) () =
    let rec t = {
        output  = Unix.out_channel_of_descr output
      ; trm     = Tmachine.create (cap_for_fd input)
      ; input   = Input.create input
      ; winched = false
      ; unwinch = lazy (
          Winch.add output (fun dim ->
            t.winched <- true; set_size t dim; if autosize then redraw t
          ))
      } in
    (winsize output |> whenopt (set_size t));
    redraw t;
    Lazy.force t.unwinch |> ignore;
    if dispose then at_exit (fun () -> release t);
    t

  let rec input t =
    t.winched <- false;
    try Input.input t.input with
    Unix.Unix_error (Unix.EINTR, _, _) when t.winched -> input t

end

let output_image_gen ~to_fd ~write ?cap chan i =
  let fd = to_fd chan in
  let cap = match cap with
    | Some cap -> cap
    | None     -> cap_for_fd fd
  and dim = match winsize fd with
    | Some (w, _) -> I.(w, height i)
    | None        -> I.(width i, height i) in
  let buf = Buffer.create I.(width i * height i * 2) in
  Render.to_buffer cap dim i buf;
  write chan buf

let output_image =
  output_image_gen
    ~to_fd:Unix.descr_of_out_channel
    ~write:Buffer.output_buffer

let print_image = output_image stdout
