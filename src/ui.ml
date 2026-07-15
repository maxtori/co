open Ezjs_min
open Ezjs_idb
open Co
open Common

type page =
  | Chargement
  | Personnages of avec_label_et_phase list
  | Personnage of avec_label
  | Nouveau
  | Importation of string
  | Creation of Creation.creation
  | Edition of avec_label
  | Backup of (Unsafe.any js_array t js_array t [@ignore])
[@@deriving jsoo {remove_undefined; snake}]

let%data page : page = Chargement
and modal_erreur : string option = None
and des : Personnage.des option = None
and tsp : int = Float.to_int @@ (to_float date##now) /. 1000.
and image : string option = None
and has_storage : bool = Option.is_some (Optdef.to_option Unsafe.global##.navigator##.storage)
and hide_modal = "hide.bs.modal"

let ouvre_db f =
  Store.set_name "personnages";
  let upgrade db e =
    if e.new_version = 1 && e.old_version = 0 then ignore (Store.create db)
    else if e.old_version = 1 && e.new_version = 0 then db##deleteObjectStore (string "personnages") in
  let@ dbh = openDB "co" ~upgrade ~version:1 in
  db := dbh;
  f ()

let backup app st =
  let@ l = Store.Raw.fold st (fun label p acc ->
    (array [| Unsafe.inject label; Unsafe.inject p##.perso; Unsafe.inject p##.creation |]) :: acc
  ) [] in
  app##.page := page_to_jsoo (Backup (of_list (List.rev l)))

let alert_aux app s =
  app##.modal_erreur_ := def (string s);
  let cs : _ constr = Unsafe.global##.bootstrap##._Modal in
  let md = new%js cs (string "#erreur-modal") in
  ignore md##show

let chargement_personnages app f =
  let st = Store.store ~mode:READONLY !db in
  let error _ = backup app st in
  let@ l = Store.fold ~error st (fun label {perso; creation} acc ->
    {label; perso; creation} :: acc) [] in
  f app (List.rev l)

let ajout_personnage ?creation app label perso f =
  let st = Store.store ~mode:READWRITE !db in
  Store.add ~key:label ~callback:(fun _ -> f app {label; perso; creation}) st {perso; creation}

let importation_personnage app label fichier f =
  let@ s = ouverture_fichier fichier in
  let p = EzEncoding.destruct personnage_enc s in
  let label = if String.trim label = "" then Format.sprintf "%s_%d" p.nom p.niveau else label in
  ajout_personnage app label p f

let to_raw x = Unsafe.global##._Vue##toRaw x

let rec unproxy x =
  let x = to_raw x in
  if x = Unsafe.pure_js_expr "undefined" then Unsafe.pure_js_expr "undefined"
  else if x = Unsafe.pure_js_expr "null" then Unsafe.pure_js_expr "null"
  else if to_bool (Unsafe.global##._Array##isArray x) then
    Unsafe.coerce (array_map unproxy (Unsafe.coerce x))
  else if to_string (typeof x) = "object" then
    let a = Unsafe.global##._Object##entries x in
    let entries = Unsafe.coerce (array_map unproxy (Unsafe.coerce a)) in
    Unsafe.coerce (Unsafe.global##._Object##fromEntries entries)
  else x

let personnage_of_b64 s =
  try
    let a = Unsafe.global##._Uint8Array##fromBase64 (string s) (object%js val alphabet = string "base64url" end) in
    let cs = Unsafe.global##._TextDecoder in
    let decoder = new%js cs in
    let s = decoder##decode a in
    Some (EzEncoding.destruct personnage_enc (to_string s))
  with _ -> None

let route ?(loading=true) ?path ?prec app p =
  app##.tsp := Float.to_int @@ (to_float date##now) /. 1000.;
  let p0, p1 = unproxy app##.page, unproxy p in
  if loading then (app##.page := page_to_jsoo Chargement);
  Firebug.console##log_3 p0 (string "-->") p1;
  let state p = some @@ Unsafe.coerce p  in
  let () = match page_of_jsoo p0 with
    | Chargement -> ()
    | _ ->
      let p0 = match prec with None -> p0 | Some p0 -> p0 in
      Dom_html.window##.history##replaceState (state p0) (string "") null in
  app##.page := p1;
  let path = opt string path in
  Dom_html.window##.history##pushState (state p1) (string "") path

let init_aux app =
  chargement_personnages app @@ fun app l -> match l with
  | [] -> route ~loading:false app (page_to_jsoo Nouveau)
  | _ -> route app (page_to_jsoo (Personnages l))

(* methods *)

let%meth commence_creation app =
  let label = Format.sprintf "perso_%d" (Float.to_int @@ (to_float date##now) /. 1000.) in
  let p = Creation { Creation.label; perso=personnage_vide; phase=Profil } in
  route app (page_to_jsoo p)

and personnage app p =
  let p = avec_label_et_phase_of_jsoo p in
  let page = match p.creation with
    | Some phase -> Creation { Creation.label=p.label; perso=p.perso; phase }
    | None -> Personnage { label=p.label; perso=p.perso } in
  route app (page_to_jsoo page)

and dir app p = route app p

and init app = init_aux app

and edition app = match page_of_jsoo app##.page with
  | Personnage { label; perso; _ } ->
    route app (page_to_jsoo (Edition {label; perso}))
  | _ -> alert_aux app "cette fonction n'est pas accessible sur cette page"

and phase_suivante app =
  let open Creation in
  match page_of_jsoo app##.page, Optdef.to_option [%ref app "creation"] with
  | Creation { label; _ }, Some elt ->
    let@ x = Promise.jthen (Unsafe.coerce elt)##maj in
    let { ancien; nouveau } = maj_of_jsoo x in
    let prec = page_to_jsoo (Creation ancien) in
    (match nouveau.phase with
     | Fin ->
       let@ () = suppression_personnage label in
       ajout_personnage app nouveau.label nouveau.perso @@ fun app {perso; _} ->
       let p = page_to_jsoo (Personnage { perso; label=nouveau.label }) in
       route ~prec app p
     | _ ->
       let@ () = edition_personnage ~creation:nouveau.phase label nouveau.perso in
       let p = page_to_jsoo (Creation nouveau) in
       route ~prec app p)
  | _ -> alert_aux app "cette fonction n'est pas accessible sur cette page"

and charge_modal_des app des =
  app##.des := def des;
  let cs : _ constr = Unsafe.global##.bootstrap##._Modal in
  let md = new%js cs (string "#des-modal") in
  ignore md##show

and lance_de app des =
  let open Personnage in
  let niveau = match page_of_jsoo app##.page with
    | Personnage { perso={niveau; _}; _ } -> niveau
    | _ -> 0 in
  des##.resultat := undefined;
  let d = des_of_jsoo des in
  let de = de_str niveau d.de in
  let@ r = lance_de "des-container" de d.nombre in
  des##.resultat := def r

and edite app = match page_of_jsoo app##.page, Optdef.to_option [%ref app "edition"] with
  | Edition _, Some elt ->
    let@ x = Promise.jthen (Unsafe.coerce elt)##maj in
    let { perso; label } : avec_label = avec_label_of_jsoo x in
    let@ () = edition_personnage label perso in
    let p = page_to_jsoo (Personnage { perso; label }) in
    route ~prec:(page_to_jsoo (Edition {perso; label})) app p
  | _ -> alert_aux app "cette fonction n'est pas accessible sur cette page"

and [@noconv] importation app (ev: Dom_html.inputElement Dom.event t) =
  match Opt.to_option ev##.target, page_of_jsoo app##.page with
  | Some target, Importation key ->
    (match Opt.to_option target##.files with
     | Some files ->
       let f = List.hd @@ Dom.list_of_nodeList files in
       importation_personnage app key f @@ fun app p ->
       route app (page_to_jsoo (Personnage {label=p.label; perso=p.perso}))
     | None -> ())
  | _ -> ()

and choisit_bonus_des app b =
  (Unsafe.coerce app)##.des##.bonus := b;
  (Unsafe.coerce app)##.des##.choix := array [||]

and alert app s = alert_aux app (to_string s)

and vide_erreur app = app##.modal_erreur_ := undefined
and vide_des app = app##.des := undefined

let () =
  let@ () = ouvre_db in
  let app = [%app {conv; mount; unhide; export; components=[Creation; Personnages; Personnage; Edition; Backup]}] in
  Dom_html.window##.onpopstate := Dom_html.handler (fun (e : Dom_html.popStateEvent t) ->
    (try route app (Unsafe.coerce e##.state) with _exn -> init_aux app); _false);
  (Unsafe.coerce Dom_html.window)##.onfocus := Dom_html.handler (fun (_e : Dom_html.popStateEvent t) ->
    let now = Float.to_int @@ (to_float date##now) /. 1000. in
    if now > app##.tsp + 3600 then route app app##.page;
    _false);
  let perso_param =
    let search = to_string Dom_html.window##.location##.search in
    if search = "" then None else
    let l = String.split_on_char '&' (String.sub search 1 (String.length search - 1)) in
    let l = List.map (fun s -> match String.split_on_char '=' s with
      | [] -> s, None | [ k ] -> k, None
      | k :: v -> k, Some (String.concat "=" v)
    ) l in
    match List.assoc_opt "perso" l with
    | Some Some s -> personnage_of_b64 s
    | _ -> None in
  try match perso_param, Opt.to_option (Unsafe.coerce Dom_html.window##.history)##.state with
    | Some perso, _ ->
      let label = Format.sprintf "%s_%d" perso.nom perso.niveau in
      let path = to_string Dom_html.window##.location##.pathname in
      route app ~path (page_to_jsoo (Personnage {label; perso}))
    | _, Some p ->
      begin match page_of_jsoo p with
        | Chargement -> init_aux app
        | _ -> route app p
      end
    | _ -> init_aux app
  with exn ->
    log "initialisation erreur: %s" (Printexc.to_string exn);
    backup app (Store.store ~mode:READONLY !db)
