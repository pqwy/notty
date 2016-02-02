open Notty
open Common

let hpad_sp attr l r i =
  let h = I.height i in
  I.(char attr ' ' l h <|> i <|> char attr ' ' r h)

let vpad_sp attr t b i =
  let w = I.width i in
  I.(char attr ' ' w t <-> i <-> char attr ' ' w b)

let grid xxs = xxs |> List.map I.hcat |> I.vcat

let centered attr xs =
  let lns = List.map I.(string attr) xs in
  let w   = List.fold_left (fun a i -> max a I.(width i)) 0 lns in
  lns |> List.map I.(fun ln ->
    let d = w - I.width ln in
    char attr ' ' (d / 2) 1 <|> ln <|> char attr ' ' (d - d / 2) 1
  ) |> I.vcat

let text = [
    "\225\154\160\225\155\135\225\154\187\225\155\171\225\155\146\225\155\166\225\154\166\225\155\171\225\154\160\225\154\177\225\154\169\225\154\160\225\154\162\225\154\177\225\155\171\225\154\160\225\155\129\225\154\177\225\154\170\225\155\171\225\154\183\225\155\150\225\154\187\225\154\185\225\155\166\225\155\154\225\154\179\225\154\162\225\155\151"
  ; "\225\155\139\225\154\179\225\155\150\225\154\170\225\155\154\225\155\171\225\154\166\225\155\150\225\154\170\225\154\187\225\155\171\225\155\151\225\154\170\225\154\190\225\154\190\225\154\170\225\155\171\225\154\183\225\155\150\225\154\187\225\154\185\225\155\166\225\155\154\225\154\179\225\155\171\225\155\151\225\155\129\225\154\179\225\155\154\225\154\162\225\154\190\225\155\171\225\154\187\225\155\166\225\155\143\225\155\171\225\155\158\225\154\171\225\155\154\225\154\170\225\154\190"
  ; "\225\154\183\225\155\129\225\154\160\225\155\171\225\154\187\225\155\150\225\155\171\225\154\185\225\155\129\225\155\154\225\155\150\225\155\171\225\154\160\225\154\169\225\154\177\225\155\171\225\155\158\225\154\177\225\155\129\225\154\187\225\155\143\225\154\190\225\155\150\225\155\171\225\155\158\225\154\169\225\155\151\225\155\150\225\155\139\225\155\171\225\154\187\225\155\154\225\155\135\225\155\143\225\154\170\225\154\190\225\155\172"
  ; ""
  ; "\227\129\132\227\130\141\227\129\175\227\129\171\227\129\187\227\129\184\227\129\168\227\129\161\227\130\138\227\129\172\227\130\139\227\130\146"
  ; "\227\130\143\227\129\139\227\130\136\227\129\159\227\130\140\227\129\157\227\129\164\227\129\173\227\129\170\227\130\137\227\130\128"
  ; "\227\129\134\227\130\144\227\129\174\227\129\138\227\129\143\227\130\132\227\129\190\227\129\145\227\129\181\227\129\147\227\129\136\227\129\166"
  ; "\227\129\130\227\129\149\227\129\141\227\130\134\227\130\129\227\129\191\227\129\151\227\130\145\227\129\178\227\130\130\227\129\155\227\129\153"
  ; ""
  ; "\227\130\164\227\131\173\227\131\143\227\131\139\227\131\155\227\131\152\227\131\136 \227\131\129\227\131\170\227\131\140\227\131\171\227\131\178 \227\131\175\227\130\171\227\131\168\227\130\191\227\131\172\227\130\189 \227\131\132\227\131\141\227\131\138\227\131\169\227\131\160"
  ; "\227\130\166\227\131\176\227\131\142\227\130\170\227\130\175\227\131\164\227\131\158 \227\130\177\227\131\149\227\130\179\227\130\168\227\131\134 \227\130\162\227\130\181\227\130\173\227\131\166\227\131\161\227\131\159\227\130\183 \227\131\177\227\131\146\227\131\162\227\130\187\227\130\185\227\131\179"
  ; ""
  ; "\206\158\206\181\207\131\206\186\206\181\207\128\206\172\206\182\207\137 \207\132\225\189\180\206\189 \207\136\207\133\207\135\206\191\207\134\206\184\207\140\207\129\206\177 \206\178\206\180\206\181\206\187\207\133\206\179\206\188\206\175\206\177"
  ; ""
  ; "\208\167\208\181\209\136\209\155\208\181 \209\134e\209\146\208\181\209\154\208\181 \208\188\209\128e\208\182\208\176\209\129\209\130\208\184\208\188 \209\159\208\176\208\186\208\190\208\188 \208\191\208\190\208\177\208\190\209\153\209\136\208\176\208\178\208\176"
  ; "\209\132\208\181\209\128\209\130\208\184\208\187\208\184\208\183\208\176\209\134\208\184\209\152\209\131 \208\179\208\181\208\189\209\129\208\186\208\184\209\133 \209\133\208\184\208\177\209\128\208\184\208\180\208\176!"
  ; ""
  ; "Heiz\195\182lr\195\188cksto\195\159abd\195\164mpfung."
  ; ""
  ; "\215\147\215\146 \215\161\215\167\215\168\215\159 \215\169\215\152 \215\145\215\153\215\157 \215\158\215\144\215\149\215\155\215\150\215\145 \215\149\215\156\215\164\215\170\215\162 \215\158\215\166\215\144 \215\156\215\149 \215\151\215\145\215\168\215\148 \215\144\215\153\215\154 \215\148\215\167\215\156\215\153\215\152\215\148"
  ; ""
  ; "\208\146 \209\135\208\176\209\137\208\176\209\133 \209\142\208\179\208\176 \208\182\208\184\208\187 \208\177\209\139 \209\134\208\184\209\130\209\128\209\131\209\129? \208\148\208\176, \208\189\208\190 \209\132\208\176\208\187\209\140\209\136\208\184\208\178\209\139\208\185 \209\141\208\186\208\183\208\181\208\188\208\191\208\187\209\143\209\128!"
  ; ""
  ; "\225\131\149\225\131\148\225\131\158\225\131\174\225\131\152\225\131\161 \225\131\162\225\131\167\225\131\144\225\131\157\225\131\161\225\131\144\225\131\156\225\131\152 \225\131\168\225\131\157\225\131\151\225\131\144 \225\131\160\225\131\163\225\131\161\225\131\151\225\131\144\225\131\149\225\131\148\225\131\154\225\131\152"
  ; ""
  ; "\224\174\175\224\174\190\224\174\174\224\174\177\224\174\191\224\174\168\224\175\141\224\174\164 \224\174\174\224\175\138\224\174\180\224\174\191\224\174\149\224\174\179\224\174\191\224\174\178\224\175\135 \224\174\164\224\174\174\224\174\191\224\174\180\224\175\141\224\174\174\224\175\138\224\174\180\224\174\191 \224\174\170\224\175\139\224\174\178\224\175\141 \224\174\135\224\174\169\224\174\191\224\174\164\224\174\190\224\174\181\224\174\164\224\175\129 \224\174\142\224\174\153\224\175\141\224\174\149\224\175\129\224\174\174\224\175\141 \224\174\149\224\174\190\224\174\163\224\175\139\224\174\174\224\175\141,"
  ; "\224\174\170\224\174\190\224\174\174\224\174\176\224\174\176\224\174\190\224\174\175\224\175\141 \224\174\181\224\174\191\224\174\178\224\174\153\224\175\141\224\174\149\224\175\129\224\174\149\224\174\179\224\174\190\224\174\175\224\175\141, \224\174\137\224\174\178\224\174\149\224\174\169\224\175\136\224\174\164\224\175\141\224\174\164\224\175\129\224\174\174\224\175\141 \224\174\135\224\174\149\224\174\180\224\175\141\224\174\154\224\175\141\224\174\154\224\174\191\224\174\154\224\175\138\224\174\178\224\174\170\224\175\141 \224\174\170\224\174\190\224\174\169\224\175\141\224\174\174\224\175\136 \224\174\149\224\175\134\224\174\159\224\175\141\224\174\159\224\175\129"
  ; ""
  ; "\224\178\172\224\178\190 \224\178\135\224\178\178\224\179\141\224\178\178\224\178\191 \224\178\184\224\178\130\224\178\173\224\178\181\224\178\191\224\178\184\224\179\129 \224\178\135\224\178\130\224\178\166\224\179\134\224\178\168\224\179\141\224\178\168 \224\178\185\224\179\131\224\178\166\224\178\175\224\178\166\224\178\178\224\178\191"
  ; "\224\178\168\224\178\191\224\178\164\224\179\141\224\178\175\224\178\181\224\179\130 \224\178\133\224\178\181\224\178\164\224\178\176\224\178\191\224\178\170 \224\178\184\224\178\164\224\179\141\224\178\175\224\178\190\224\178\181\224\178\164\224\178\190\224\178\176"
  ; ""
  ; "\237\130\164\236\138\164\236\157\152 \234\179\160\236\156\160\236\161\176\234\177\180\236\157\128 \236\158\133\236\136\160\235\129\188\235\166\172 \235\167\140\235\130\152\236\149\188"
  ; "\237\149\152\234\179\160 \237\138\185\235\179\132\237\149\156 \234\184\176\236\136\160\236\157\128 \237\149\132\236\154\148\236\185\152 \236\149\138\235\139\164"
  ; ""
  ; "\216\181\217\144\217\129 \216\174\217\142\217\132\217\130\217\142 \216\174\217\142\217\136\216\175\217\144 \217\131\217\142\217\133\217\144\216\171\217\132\217\144 \216\167\217\132\216\180\217\142\217\133\216\179\217\144 \216\165\217\144\216\176 \216\168\217\142\216\178\217\142\216\186\217\142\216\170 \226\128\148 \217\138\217\142\216\173\216\184\217\137 \216\167\217\132\216\182\217\142\216\172\217\138\216\185\217\143 \216\168\217\144\217\135\216\167 \217\134\217\142\216\172\217\132\216\167\216\161\217\142 \217\133\217\144\216\185\216\183\216\167\216\177\217\144"
  ; ""
  ; "\224\164\139\224\164\183\224\164\191\224\164\175\224\165\139\224\164\130 \224\164\149\224\165\139 \224\164\184\224\164\164\224\164\190\224\164\168\224\165\135 \224\164\181\224\164\190\224\164\178\224\165\135 \224\164\166\224\165\129\224\164\183\224\165\141\224\164\159 \224\164\176\224\164\190\224\164\149\224\165\141\224\164\183\224\164\184\224\165\139\224\164\130 \224\164\149\224\165\135 \224\164\176\224\164\190\224\164\156\224\164\190 \224\164\176\224\164\190\224\164\181\224\164\163 \224\164\149\224\164\190 \224\164\184\224\164\176\224\165\141\224\164\181\224\164\168\224\164\190\224\164\182 \224\164\149\224\164\176\224\164\168\224\165\135 \224\164\181\224\164\190\224\164\178\224\165\135"
  ; "\224\164\181\224\164\191\224\164\183\224\165\141\224\164\163\224\165\129\224\164\181\224\164\164\224\164\190\224\164\176 \224\164\173\224\164\151\224\164\181\224\164\190\224\164\168 \224\164\182\224\165\141\224\164\176\224\165\128\224\164\176\224\164\190\224\164\174, \224\164\133\224\164\175\224\165\139\224\164\167\224\165\141\224\164\175\224\164\190 \224\164\149\224\165\135 \224\164\174\224\164\185\224\164\190\224\164\176\224\164\190\224\164\156 \224\164\166\224\164\182\224\164\176\224\164\165 \224\164\149\224\165\135 \224\164\172\224\164\161\224\164\188\224\165\135 \224\164\184\224\164\170\224\165\129\224\164\164\224\165\141\224\164\176 \224\164\165\224\165\135\224\165\164"
  ; ""
  ; "Lu\195\173s arg\195\188ia \195\160 J\195\186lia que \194\171bra\195\167\195\181es, f\195\169, ch\195\161,"
  ; "\195\179xido, p\195\180r, z\195\162ng\195\163o\194\187 eram palavras do portugu\195\170s."
  ; ""
  ; "ding ↹ ∀ ⌘ ▓ ◭ ☃ ♠ ♋ ♕ ⚅ ♩ ☭ ✎ 🂡 bats"
]

let () =
  let attr = A.(fg lightmagenta) in
  let img =
    I.(centered attr text
      |> vpad_sp attr 1 1 |> hpad_sp attr 2 2
      |> Images.outline attr
      |> I.hpad 2 2 |> I.vpad 2 2) in
  Notty_unix.print_image_nl img;
  ()
