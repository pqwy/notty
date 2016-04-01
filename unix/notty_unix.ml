open Notty

external c_winsize : Unix.file_descr -> int = "caml_notty_winsize" "noalloc"
external winch_number : unit -> int = "caml_notty_winch_number" "noalloc"

let whenopt f = function Some x -> f x | _ -> ()

let winsize fd =
  match c_winsize fd with
  | 0  -> None
  | wh -> Some (wh lsr 16, wh lsr 1 land 0x7fff)

module Private = struct

  let once f =
    let r = ref None in fun () ->
      match !r with Some y -> y | _ -> let y = f () in r := Some y; y

  let cap_for_fd =
    let open Cap in
    match Sys.getenv "TERM" with
    | exception Not_found -> fun _ -> dumb
    | (""|"dumb")         -> fun _ -> dumb
    | _                   -> fun fd -> if Unix.isatty fd then ansi else dumb

  let setup_tcattr ~nosig fd =
    let open Unix in
    let tweak tc =
      let tc = { tc with c_icanon = false ; c_echo = false } in
      if nosig then { tc with c_isig = false ; c_ixon = false } else tc in
    try
      let tc = tcgetattr fd in
      tweak tc |> tcsetattr fd TCSANOW;
      `Revert (once @@ fun () -> tcsetattr fd TCSANOW tc)
    with Unix_error (ENOTTY, _, _) -> `Revert (fun () -> ())

  let set_winch_handler f =
    let signum = winch_number () in
    let old_hdl = Sys.(signal signum (Signal_handle (fun _ -> f ()))) in
    `Revert (once @@ fun () -> Sys.set_signal signum old_hdl)

  let output_image_gen ~to_fd ~write ?cap ?(clear=false) chan f =
    let fd = to_fd chan in
    let cap = match cap with
      | Some cap -> cap
      | None     -> cap_for_fd fd
    and size = winsize fd in
    let i = f (match size with Some s -> s | None -> (80, 24)) in
    let dim = match size with
      | Some (w, _) -> I.(w, height i)
      | None        -> I.(width i, height i) in
    let buf = Buffer.create I.(width i * height i * 2) in
    if clear then Cap.clear cap buf; Render.to_buffer buf cap dim i;
    write chan buf

end

open Private

module Term = struct

  module Winch = struct

    module M = Map.Make (struct
      type t = int let compare (a:t) b = compare a b
    end)

    type remove = int

    let id = ref 0

    and hs = lazy (
      let r = ref M.empty in
      set_winch_handler (fun () -> !r |> M.iter (fun _ f -> f ())) |> ignore;
      r
    )

    let mmap f = let m = Lazy.force hs in m := f !m

    let add fd f =
      let x = !id in
      incr id;
      M.add x (fun () -> winsize fd |> whenopt f) |> mmap;
      x

    let remove x = mmap (M.remove x)
  end

  module Input = struct

    type t = {
      fd      : Unix.file_descr
    ; flt     : Unescape.t
    ; ibuf    : bytes
    ; cleanup : unit -> unit
    }

    let bsize = 1024

    let create ~nosig fd =
      let flt  = Unescape.create ()
      and ibuf = Bytes.create bsize
      and `Revert cleanup = setup_tcattr ~nosig fd in
      { fd; flt; ibuf; cleanup }

    let rec event t =
      match Unescape.next t.flt with
      | #Unescape.event | `End as r -> r
      | `Await ->
          let n = Unix.read t.fd t.ibuf 0 bsize in
          Unescape.input t.flt t.ibuf 0 n; event t
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
    if Tmachine.release t.trm then begin
      Winch.remove Lazy.(force t.unwinch);
      t.input.Input.cleanup ();
      write t
    end

  let set_size t dim = Tmachine.set_size t.trm dim
  let refresh t      = Tmachine.refresh t.trm; write t
  let image t image  = Tmachine.image t.trm image; write t
  let cursor t curs  = Tmachine.cursor t.trm curs; write t
  let size t         = Tmachine.size t.trm

  let create ?(dispose=true) ?(nosig=true) ?(mouse=true)
             ?(input=Unix.stdin) ?(output=Unix.stdout) () =
    let rec t = {
        output  = Unix.out_channel_of_descr output
      ; trm     = Tmachine.create ~mouse (cap_for_fd input)
      ; input   = Input.create ~nosig input
      ; winched = false
      ; unwinch = lazy (
          Winch.add output (fun dim -> t.winched <- true; set_size t dim)
        )
      } in
    winsize output |> whenopt (set_size t);
    Lazy.force t.unwinch |> ignore;
    if dispose then at_exit (fun () -> release t);
    write t;
    t

  let rec event = function
    | t when Tmachine.dead t.trm -> `End
    | t when t.winched -> t.winched <- false; `Resize (size t)
    | t -> try Input.event t.input with
            Unix.Unix_error (Unix.EINTR, _, _) -> t.winched <- true; event t

  let pending t =
    not (Tmachine.dead t.trm) &&
    (t.winched || Unescape.pending t.input.Input.flt)

end

let output_image_size ?cap ?clear ?(chan=stdout) i =
  output_image_gen
    ~to_fd:Unix.descr_of_out_channel
    ~write:Buffer.output_buffer
    ?cap ?clear chan i

let output_image ?cap ?clear ?chan i =
  output_image_size ?cap ?clear ?chan (fun _ -> i)

let output_image_endline ?cap ?clear ?(chan=stdout) i =
  output_image ?cap ?clear ~chan i; output_char chan '\n'
