open Co
open Ezjs_min
open Common

let%file _ = "./personnages.html"

let%prop list : avec_label_et_phase list = {req}

let personnage_to_b64 p =
  let s = EzEncoding.construct personnage_enc p in
  let cs = Unsafe.global##._TextEncoder in
  let encoder = new%js cs in
  let uint8a = encoder##encode (string s) in
  let b = uint8a##toBase64 (object%js val alphabet = string "base64url" val omitPadding = _true end) in
  to_string b

let%meth detruit_personnage app (n: string) =
  let@ () = Common.suppression_personnage n in
  [%emit "init" app]

and telecharge_personnage _app (label: string) (p: personnage) =
  let s =
    try EzEncoding.construct ~compact:false personnage_enc p
    with _ -> to_string @@ _JSON##stringify (personnage_to_jsoo p) in
  Common.telecharge label s

and copie_lien_personnage _app (label: string) (p: personnage) =
  let b64 = personnage_to_b64 { p with image = None } in
  let origin = to_string Dom_html.window##.location##.origin in
  let pathname = to_string Dom_html.window##.location##.pathname in
  let pathname = match List.rev @@ String.split_on_char '/' pathname with
    | "index.html" :: tl -> "/" ^ String.concat "/" (List.rev @@ List.filter (fun s -> s <> "") tl)
    | [""; ""] -> ""
    | _ -> pathname in
  let s = Format.sprintf "%s%s?label=%s&perso=%s" origin pathname label b64 in
  try
    Promise.jthen ((Unsafe.coerce Dom_html.window##.navigator)##share (object%js val url = string s val title = string "CO" end)) Fun.id
  with _ ->
    Promise.jthen ((Unsafe.coerce Dom_html.window##.navigator)##.clipboard##writeText (string s)) Fun.id

and wavy_cadre _app (i: int) = Common.wavy_cadre i

and [@noconv] personnage app (p: personnage_jsoo t) = [%emit "perso" app p]

[%%comp {name="personnages"; conv}]
