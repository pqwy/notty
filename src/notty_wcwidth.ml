(* Copyright (c) 2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

let mem x arr =
  let rec go arr (x : int) i j =
    i <= j &&
      let mid = (i + j) / 2 in
      let (a, b) = arr.(mid) in
      if x < a then go arr x i (mid - 1) else x <= b || go arr x (mid + 1) j
  in go arr x 0 (Array.length arr - 1)

let wcwidth u = match Uchar.to_int u with
  (* C0 (without U+0000) or DELETE and C1 is non-sensical. *)
  | u when 0 < u && u <= 0x001F || 0x007F <= u && u <= 0x009F -> -1
  (* U+0000 is actually safe to (non-)render. *)
  | 0 -> 0
  (* Soft Hyphen is the only exception to the next branch. *)
  | 0x00AD -> 0
  (* Euro-centric fast path. *)
  | u when u <= 0x02FF -> 1
  (* Kannada Vowel Sign I/E: `Mn, non-spacing combiners,
     but treated as 1 by glibc and FreeBSD's libc. *)
  | 0x0CBF | 0x0CC6 -> 1
  (* Non-spacing, unless stated otherwise (GC {Mn, Me, Cf}). *)
  | u when mem u Notty_unicode_data.gc__mn_me_cf -> 0
  (* Wide east-asian (EAW {W, F}). *)
  | u when mem u Notty_unicode_data.eaw__w_f -> 2
  (* or else... *)
  | _ -> 1
