open Co
open Ezjs_min

type phase =
  | Profil
  | Peuple
  | Caracteristiques
  | Equipements
  | Niveau
  | Voies
  | Competences
  | Bonuses
  | Sauvegarde
  | Fin
[@@deriving jsoo {snake}]

type avec_phase = {
  perso: personnage;
  creation: phase option;
} [@@deriving jsoo]

type avec_label = {
  label: string;
  perso: personnage;
} [@@deriving jsoo]

type avec_label_et_phase = {
  label: string;
  perso: personnage;
  creation: phase option;
} [@@deriving jsoo]

module Store = Ezjs_idb.Store(Ezjs_idb.StringTr)(struct
    type js = avec_phase_jsoo t
    type t = avec_phase
    let to_js = avec_phase_to_jsoo
    let of_js = avec_phase_of_jsoo
  end)

let db = ref (Unsafe.obj [||] : Ezjs_idb.Types.iDBDatabase t)

let suppression_personnage label f =
  let st = Store.store ~mode:Ezjs_idb.READWRITE !db in
  Store.delete ~callback:(fun _ -> f ()) st (Store.K label)

let edition_personnage ?creation label perso f =
  let st = Store.store ~mode:Ezjs_idb.READWRITE !db in
  Store.put ~key:label ~callback:(fun _ -> f ()) st {perso; creation}

let wavy_haut : (int, string) Hashtbl.t = Hashtbl.create 10
let wavy_bas : (int, string) Hashtbl.t = Hashtbl.create 10
let wavy_cadre : (int, string) Hashtbl.t = Hashtbl.create 10

let charge_fichier url f =
  Ezjs_fetch.fetch url Ezjs_fetch.to_text @@ function
  | Error e -> js_log e; log "%s non présent" url
  | Ok r -> f r.Ezjs_fetch.body

let ouverture_fichier fichier f =
  let reader = new%js File.fileReader in
  reader##.onloadend := Dom.handler (fun _evt ->
    if reader##.readyState = File.DONE then
      Opt.iter (File.CoerceTo.string (reader##.result)) (fun s -> f (to_string s));
    _true);
  reader##(readAsText fichier)

let js_error s = new%js error_constr (string s)

let js_fail s = Js_error.(raise_ (of_error (js_error s)))

let alert ?fail app s =
  [%emit "alert" app (string s)];
  Option.iter (fun f -> f s) fail

let telecharge name s =
  let blob = File.blob_from_string ~contentType:"application/json" s in
  let href = Dom_html.window##._URL##createObjectURL blob in
  let elt = Dom_html.createA Dom_html.document in
  elt##.href := href;
  elt##.download := string (Format.sprintf "%s.json" name);
  elt##click

let pp_peuple p =
  let s = peuple_to_str p in
  let b = String.starts_with ~prefix:"demi_" s in
  String.capitalize_ascii @@ String.map (function '_' -> if b then '-' else ' ' | c -> c) s

let wavy_cadre i =
  let s = match Hashtbl.find_opt wavy_cadre i with
    | None ->
      let s = Wavy.cadre [ `haut, (7, 15); `droite, (5, 10); `bas, (7, 15); `gauche, (5, 10) ] in
      Hashtbl.add wavy_cadre i s; s
    | Some s -> s in
  string @@ Format.sprintf "clip-path:%s" s

let creation_box_de id =
  let cs = Unsafe.global##._DiceBox in
  let options = object%js
    val assetPath = string "assets/"
    val origin = string "https://unpkg.com/@3d-dice/dice-box@1.1.3/dist/"
    val container = string ("#" ^ id)
    val scale = 18
    val themeColor = string "#990000"
  end in
  new%js cs options

let lance_de id de n f =
  let s = Format.sprintf "%d%s" n de in
  let elt = Dom_html.getElementById id in
  elt##.innerHTML := string "";
  let box = creation_box_de id in
  let@ () = Promise.jthen box##init in
  let@ r = Promise.jthen (box##roll (string s)) in
  let v = Array.fold_left (fun acc r -> acc + r##.value) 0 @@ to_array r in
  f v

let fold f acc l cb =
  let rec aux acc = function
    | [] -> cb acc
    | h :: tl -> f acc h @@ fun acc -> aux acc tl in
  aux acc l
