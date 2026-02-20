open Co

type commande =
  | Decode of string
  | Test
[@@deriving arg { exe="test.exe" }]

let decode f =
  let ic = open_in f in
  let s = really_input_string ic (in_channel_length ic) in
  let perso = EzEncoding.destruct personnage_enc s in
  Format.printf "%a" (EzEncoding.pp ~compact:false personnage_enc) perso

let test () = ()

let () =
  let commande = parse_commande () in
  match commande with
  | Decode f -> decode f
  | Test -> test ()
