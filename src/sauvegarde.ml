open Co
open Ezjs_min
open Common

let%file _ = "./sauvegarde.html"

let%prop p : personnage = {req}
and lbl : string = {req}

let%data ideaux : string list = "" :: (List.map fst ideal_assoc)
and travers_options : string list = "" :: (List.map fst travers_assoc)
and [@noconv] nom app = app##.p##.nom
and [@noconv] label app = app##.lbl
and ideal app : string = Option.value ~default:"" @@ to_optdef to_string app##.p##.ideal
and travers app : string = Option.value ~default:"" @@ to_optdef to_string app##.p##.travers
and [@noconv] description app = app##.p##.description
and image : string option = None
and image_url :string option = None
and has_storage : bool = Option.is_some (Optdef.to_option Unsafe.global##.navigator##.storage)

let ouverture_image fichier f =
  let reader = new%js File.fileReader in
  reader##.onloadend := Dom.handler (fun _evt ->
    if reader##.readyState = File.DONE then
      Opt.iter (File.CoerceTo.arrayBuffer (reader##.result)) (fun a -> f a);
    _true);
  reader##(readAsArrayBuffer fichier)

let sauvegarde_fichier_persistent nom a f =
  let st = Unsafe.global##.navigator##.storage in
  let@ b = Promise.jthen st##persist in
  if not (to_bool b) then () else
  let@ dir = Promise.jthen st##getDirectory in
  let@ fh = Promise.jthen (dir##getFileHandle_1 (string nom) (object%js val create= _true end)) in
  let@ wr = Promise.jthen fh##createWritable in
  let@ () = Promise.jthen (wr##write a) in
  let@ () = Promise.jthen wr##close in
  let@ fi = Promise.jthen fh##getFile in
  let url = Dom_html.window##._URL##createObjectURL fi in
  f nom url

let%meth [@noconv] charge_image app (ev: Dom_html.inputElement Dom.event t) =
  match Opt.to_option ev##.target with
  | Some target ->
    let nom = to_string app##.nom in
    let label = to_string app##.label in
    let fichier = List.hd @@ Dom.list_of_nodeList (Option.get @@ Opt.to_option target##.files) in
    let@ a = ouverture_image fichier in
    let nom = if nom = "" then label else nom in
    let ext = Filename.extension (to_string fichier##.name) in
    let nom_fichier = String.lowercase_ascii (nom ^ ext) in
    sauvegarde_fichier_persistent nom_fichier a @@ fun nom_fichier url ->
    app##.image_url_ := def url;
    app##.image := def (string nom_fichier)
  | None -> ()

and [@noconv] maj app : avec_label_jsoo t Promise.promise t =
  let perso = personnage_of_jsoo app##.p in
  let nom = to_string app##.nom in
  Promise.promise @@ fun resolve reject ->
  if nom = "" then
    let fail s = reject (js_error s) in
    alert ~fail app "nom vide"
  else
  let label = to_string app##.label in
  let ideal = to_string app##.ideal in
  let ideal = List.assoc_opt ideal ideal_assoc in
  let travers = to_string app##.travers in
  let travers = List.assoc_opt travers travers_assoc in
  let description = to_string app##.description in
  let image = to_optdef to_string app##.image in
  let perso = { perso with nom; ideal; travers; description; image } in
  resolve (avec_label_to_jsoo { perso; label })

[%%comp {name="sauvegarde"; conv}]
