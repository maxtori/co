open Ezjs_min
open Ezjs_idb
open Co

module Personnages = Store(StringTr)(struct
    type js = personnage_jsoo t
    type t = personnage
    let to_js = personnage_to_jsoo
    let of_js = personnage_of_jsoo
  end)

type personnage_et_clef = string * personnage [@@deriving jsoo]

type genre_de = [
  | `points_de_vigueur
  | `des_de_recuperation
  | `points_de_chance
  | `points_de_mana
] [@@deriving jsoo]

type de = {
  genre: genre_de;
  points: points_avec_max;
  titre: string;
} [@@deriving jsoo]

type choix_edition = {
  niveau: int;
  caracteristiques: caracteristiques;
  bonus_peuple: caracteristique_avec_valeur list * int;
  voies: (voie_type * int) list;
  equipements: equipement_nom list;
  equipement: equipement_nom;
} [@@deriving jsoo]

type edition = {
  label: string;
  personnage: personnage;
  bonuses_peuple: caracteristique_avec_valeur list list;
  voies: voie_type list;
  equipements: equipement_nom list;
  choix: choix_edition;
} [@@deriving jsoo]

type page =
  | Chargement
  | Personnages of personnage_et_clef list
  | Personnage of personnage_et_clef
  | Importation of string
  | Creation of personnage_et_clef
  | Edition of edition
[@@deriving jsoo {remove_undefined; snake}]

let voies : (voie_type * voie) list ref = ref []
let equipements : (equipement_nom * equipement) list ref = ref []

let%data page : page = Chargement
and modal_erreur : string option = None
and de : de option = None

let genre_de_titre = function
  | `points_de_vigueur -> "Points de vigeur"
  | `des_de_recuperation -> "Dés de récupération"
  | `points_de_chance -> "Points de chance"
  | `points_de_mana -> "Points de mana"

let upgrade db e =
  if e.new_version = 1 && e.old_version = 0 then ignore (Personnages.create db)
  else if e.old_version = 1 && e.new_version = 0 then db##deleteObjectStore (string "personnages")

let open_db f =
  Personnages.set_name "personnages";
  openDB "co" ~upgrade ~version:1 f

let ouverture_fichier fichier f =
  let reader = new%js File.fileReader in
  reader##.onloadend := Dom.handler (fun _evt ->
    if reader##.readyState = File.DONE then
      Opt.iter (File.CoerceTo.string (reader##.result)) (fun s -> f (to_string s));
    _true);
  reader##(readAsText fichier)

let chargement_personnages app f =
  let st = Personnages.store ~mode:READONLY app##.db in
  Personnages.fold st (fun k p acc -> (k, p) :: acc) [] @@ fun l ->
  f app (List.rev l)

let ajout_personnage app key p f =
  let st = Personnages.store ~mode:READWRITE app##.db in
  Personnages.add ~key ~callback:(fun _ -> f app (key, p)) st p

let edition_personnage app key p =
  let st = Personnages.store ~mode:READWRITE app##.db in
  Personnages.put ~key st p

let suppression_personnage app key =
  let st = Personnages.store ~mode:READWRITE app##.db in
  Personnages.delete st (Personnages.K key)

let importation_personnage app key fichier f = ouverture_fichier fichier @@ fun s ->
  let p = EzEncoding.destruct personnage_enc s in
  let key = if String.trim key = "" then Format.sprintf "%s_%d" p.nom p.niveau else key in
  ajout_personnage app key p f

let to_raw x = Unsafe.global##._Vue##toRaw x

let rec unproxy x =
  let x = to_raw x in
  if x = Unsafe.pure_js_expr "undefined" then
    Unsafe.pure_js_expr "undefined"
  else if x = Unsafe.pure_js_expr "null" then
    Unsafe.pure_js_expr "null"
  else if to_bool (Unsafe.global##._Array##isArray x) then
    Unsafe.coerce (array_map unproxy (Unsafe.coerce x))
  else if to_string (typeof x) = "object" then
    let a = Unsafe.global##._Object##entries x in
    let entries = Unsafe.coerce (array_map unproxy (Unsafe.coerce a)) in
    Unsafe.coerce (Unsafe.global##._Object##fromEntries entries)
  else x

let charge_fichier url f =
  Ezjs_fetch.fetch url Ezjs_fetch.to_text @@ function
  | Error _ -> log "%s non présent" url; f None
  | Ok r -> f (Some r.Ezjs_fetch.body)

let charge_voie v f =
  match List.assoc_opt v !voies with
  | Some v -> f (Some v)
  | None ->
    let v_str = voie_type_to_str v in
    Format.printf "chargement voie %s@." v_str;
    let url = Format.sprintf "data/%s.json" v_str in
    charge_fichier url @@ function
    | None -> f None
    | Some s ->
      let voie = EzEncoding.destruct voie_enc s in
      voies := (v, voie) :: !voies;
      f (Some voie)

let charge_equipement e f =
  match List.assoc_opt e !equipements with
  | Some e -> f (Some e)
  | None ->
    let e_str = equipement_to_str e in
    Format.printf "chargement equipement %s@." e_str;
    let url = Format.sprintf "data/%s.json" e_str in
    charge_fichier url @@ function
    | None -> f None
    | Some s ->
      let equipement = EzEncoding.destruct equipement_enc s in
      equipements := (e, equipement) :: !equipements;
      f (Some equipement)

let chargement_voies l f =
  let rec aux = function
    | [] -> f ()
    | v :: tl -> charge_voie v (fun _ -> aux tl) in
  aux l

let chargement_equipements l f =
  let rec aux = function
    | [] -> f ()
    | e :: tl -> charge_equipement e (fun _ -> aux tl) in
  aux l

let chargement_equipements_async l =
  List.iter (fun e -> charge_equipement e (fun _ -> ())) l

let alert app s =
  app##.modal_erreur_ := def (string s);
  let cs : _ constr = Unsafe.global##.bootstrap##._Modal in
  let md = new%js cs (string "#erreur-modal") in
  ignore md##show

let route app p =
  let p0, p1 = unproxy app##.page, unproxy p in
  Firebug.console##log_3 p0 (string "-->") p1;
  let state p = some @@ Unsafe.coerce p  in
  Dom_html.window##.history##replaceState (state p0) (string "") null;
  let f app =
    app##.page := p1;
    Dom_html.window##.history##pushState (state p1) (string "") null in
  match page_of_jsoo p with
  | Personnages l ->
    let rec aux = function
      | [] -> f app
      | (_, (p: personnage)) :: tl ->
        chargement_voies (List.map fst p.voies) @@ fun () ->
        chargement_equipements p.equipements @@ fun _ ->
        aux tl in
    aux l
  | Personnage (_, p) ->
    chargement_voies (List.map fst p.voies) @@ fun _ ->
    chargement_equipements p.equipements @@ fun _ ->
    f app
  | Creation (_, { creation = Some Voies {voies; _}; equipements; _}) ->
    chargement_voies voies @@ fun _ ->
    chargement_equipements equipements @@ fun _ ->
    f app
  | Edition { voies; equipements=l; choix={equipements; _}; _ } ->
    chargement_voies voies @@ fun _ ->
    chargement_equipements equipements @@ fun _ ->
    chargement_equipements_async l;
    f app
  | _ -> f app

let init app =
  chargement_personnages app @@ fun app l ->
  route app (page_to_jsoo (Personnages l))

let%meth commence_creation app =
  let profils = profils () in
  let nom = Format.sprintf "perso_%ld" (to_int32 @@ date##now) in
  let p = Creation (nom, { personnage_vide with creation=Some (Profil {profils; choix=None}) }) in
  route app (page_to_jsoo p)

and personnage app p =
  let k, p = personnage_et_clef_of_jsoo p in
  let page = match p.creation with
    | Some _ -> Creation (k, p)
    | None -> Personnage (k, p) in
  route app (page_to_jsoo page)

and dir app p = route app p

and home app = init app
and edition app = match page_of_jsoo app##.page with
  | Personnage (label, p) ->
    let caracteristiques = ajoute_caracteristiques ~factor:(-1) p.caracteristiques p.caracteristiques_bonus in
    let bonuses_peuple = bonuses_peuple p.peuple caracteristiques in
    let bonus_peuple_index, _ = List.fold_left (fun (i, acc) b ->
      if b = p.caracteristiques_bonus then acc, acc+1 else i, acc+1) (0, 0) bonuses_peuple in
    let choix = {
      niveau=p.niveau; caracteristiques;
      bonus_peuple = (p.caracteristiques_bonus, bonus_peuple_index);
      voies=p.voies; equipements=p.equipements; equipement=`autre "" } in
    let voies = voies_peuple p.peuple @ voies_profil p.profil in
    let edition = {
      label; personnage=p; voies; choix;
      equipements=List.map snd equipement_nom_assoc; bonuses_peuple } in
    route app (page_to_jsoo (Edition edition))
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"

and [@noconv] importation app (ev: Dom_html.inputElement Dom.event t) =
  match Opt.to_option ev##.target, page_of_jsoo app##.page with
  | Some target, Importation key ->
    let f = List.hd @@ Dom.list_of_nodeList target##.files in
    importation_personnage app key f @@ fun app p ->
    route app (page_to_jsoo (Personnage p))
  | _ -> ()

and voie _app v =
  Firebug.console##log_2 (string "voie") v;
  match List.assoc_opt (voie_type_of_jsoo v) !voies with
  | None -> log_str "voie non trouvée"; undefined
  | Some v -> def (voie_to_jsoo v)

and phase_suivante app =
  match page_of_jsoo app##.page with
  | Creation (n, personnage) ->
    begin match personnage.creation with
      | Some Profil {choix=None; _ } ->
        alert app "profil non choisi"
      | Some Profil {choix=Some (famille, profil); _ } ->
        let peuples = List.map snd peuple_assoc in
        let equipements = equipements_profil profil in
        let personnage = {
          personnage with famille; profil; equipements;
                          creation=Some (Peuple { peuples; choix=None }) } in
        let p = Creation (n, personnage) in
        ajout_personnage app n personnage @@ fun app _ ->
        route app (page_to_jsoo p)
      | Some Peuple {choix=None; _ } ->
        alert app "peuple non choisi"
      | Some Peuple {choix=Some peuple; _ } ->
        let choix = caracteristiques_par_defaut (Some peuple) in
        let bonuses = bonuses_peuple peuple choix in
        let personnage = {
          personnage with
          peuple; caracteristiques=choix;
          creation = Some (Caracteristiques { bonuses; choix; choix_bonus=None })
        } in
        edition_personnage app n personnage;
        let p = Creation (n, personnage) in
        route app (page_to_jsoo p)
      | Some Caracteristiques { choix_bonus=None; _ } ->
        alert app "caracteristiques bonus non choisies"
      | Some Caracteristiques { choix; choix_bonus=Some (cb, _); _ } ->
        if verifie_caracteristiques choix then
          let caracteristiques = ajoute_caracteristiques choix cb in
          let voies = voies_peuple personnage.peuple @ voies_profil personnage.profil in
          let personnage = {
            personnage with caracteristiques; creation = Some (Voies { voies; choix=[] });
                            caracteristiques_bonus=cb } in
          edition_personnage app n personnage;
          let p = Creation (n, personnage) in
          route app (page_to_jsoo p)
        else alert app "caracteristiques non valables"
      | Some Voies { choix=l; _ } ->
        begin match verifie_voies personnage l with
          | None ->
            let personnage = { personnage with voies=l } in
            let nb_sorts = nombre_sorts @@ List.filter_map (fun (v, rg) -> Option.map (fun v -> v, rg) @@ List.assoc_opt v !voies) l in
            let def_equipement, agi_max = List.fold_left (fun (def, agi) e ->
              match List.assoc_opt e !equipements with
              | Some Armure { defense; agilite_max; _ } ->
                def + defense, Option.fold ~none:agi ~some:(fun a -> min a agi) agilite_max
              | _ -> def, agi) (0, 8) personnage.equipements in
            let personnage = remplit_caracteristiques personnage nb_sorts def_equipement agi_max in
            edition_personnage app n personnage;
            let ideaux = "" :: (List.map fst ideal_assoc) in
            let travers_options = "" :: (List.map fst travers_assoc) in
            let phase = Enregistrement {ideaux; travers_options; nom=""; label=""; ideal=""; travers=""} in
            let p = Creation (n, { personnage with creation = Some phase }) in
            route app (page_to_jsoo p)
          | Some e -> alert app e
        end
      | Some Enregistrement {nom; label; ideal; travers; _} ->
        if nom = "" then alert app "nom vide" else
        let ideal = List.assoc_opt ideal ideal_assoc in
        let travers = List.assoc_opt travers travers_assoc in
        let personnage = { personnage with nom; ideal; travers; creation=None } in
        let label = if label = "" then Format.sprintf "%s_%d" nom personnage.niveau else label in
        ajout_personnage app label personnage @@ fun app (k, p) ->
        suppression_personnage app n;
        let p = Personnage (k, p) in
        route app (page_to_jsoo p)
      | _ -> init app
    end
  | _ -> init app

and peuple_bonus _app peuple caracteristiques =
  let bonuses = bonuses_peuple (peuple_of_jsoo peuple) (caracteristiques_of_jsoo caracteristiques) in
  of_listf (of_listf caracteristique_avec_valeur_to_jsoo) bonuses

and capacite_choisie app vt rg =
  match page_of_jsoo app##.page with
  | Creation (_, personnage) ->
    begin match personnage.creation with
      | Some Voies { choix; _ } ->
        List.exists (fun (c, i) -> c = voie_type_of_jsoo vt && i >= rg) choix
      | _ -> false
    end
  | Edition { choix={voies; _}; _ } ->
    List.exists (fun (c, i) -> c = voie_type_of_jsoo vt && i >= rg) voies
  | _ -> false

and choisit_capacite app vt rg =
  let rec findi f i = function
    | [] -> None
    | h :: _ when f h -> Some (i, h)
    | _ :: q -> findi f (i+1) q in
  let splice choix start dcount x =
    match x with
    | None -> ignore ((Unsafe.coerce choix)##splice start dcount)
    | Some rg ->
      ignore (choix##splice start dcount (array [|Unsafe.inject vt; Unsafe.inject rg|])) in
  let aux c choix =  match findi (fun (c, _i) -> c = voie_type_of_jsoo vt) 0 choix with
    | None -> splice c (List.length choix) 0 (Some rg)
    | Some (i, (_, rg0)) ->
      if rg0 < rg then splice c i 1 (Some rg)
      else if rg0 = rg && rg = 1 then splice c i 1 None
      else if rg0 = rg then splice c i 1 (Some (rg-1))
      else splice c i 1 (Some rg) in
  match page_of_jsoo app##.page with
  | Creation (_, personnage) ->
    begin match personnage.creation with
      | Some Voies { choix; _ } ->
        let c = (Unsafe.coerce app)##.page##.creation##._1##.creation##.voies##.choix in
        aux c choix
      | _ -> ()
    end
  | Edition { choix={voies; _}; _} ->
    let c = (Unsafe.coerce app)##.page##.edition##.choix##.voies in
    aux c voies
  | _ -> ()

and detruit_personnage app n =
  suppression_personnage app (to_string n);
  init app

and telecharge_personnage _app label p =
  let s = EzEncoding.construct ~compact:false personnage_enc (personnage_of_jsoo p) in
  let blob = File.blob_from_string ~contentType:"application/json" s in
  let href = Dom_html.window##._URL##createObjectURL blob in
  let elt = Dom_html.createA Dom_html.document in
  elt##.href := href;
  elt##.download := string (Format.sprintf "%s.json" label);
  elt##click

and charge_modal_de app g = match page_of_jsoo app##.page with
  | Personnage (_, p) ->
    let genre = genre_de_of_jsoo g in
    let titre = genre_de_titre genre in
    let points = match genre with
      | `points_de_vigueur -> p.points_de_vigueur
      | `des_de_recuperation -> p.des_de_recuperation
      | `points_de_chance -> p.points_de_chance
      | `points_de_mana -> p.points_de_mana in
    app##.de := def (de_to_jsoo { titre; genre; points });
    let cs : _ constr = Unsafe.global##.bootstrap##._Modal in
    let md = new%js cs (string "#des-modal") in
    ignore md##show
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"

and change_de app g points =
  match page_of_jsoo app##.page with
  | Personnage (n, p) ->
    let points = points_avec_max_of_jsoo points in
    if points.max < points.courant then alert app "valeur supérieur au maximum" else
    let p = match genre_de_of_jsoo g with
      | `points_de_vigueur -> { p with points_de_vigueur = points }
      | `des_de_recuperation -> { p with des_de_recuperation = points }
      | `points_de_chance -> { p with points_de_chance = points }
      | `points_de_mana -> { p with points_de_mana = points } in
    edition_personnage app n p;
    let p = Personnage (n, p) in
    app##.de := undefined;
    route app (page_to_jsoo p)
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"

and armes _app l =
  let l = to_listf equipement_nom_of_jsoo l in
  of_list @@ List.filter_map (fun e -> match List.assoc_opt e !equipements with
    | Some (Arme_au_contact _ as eq) | Some (Arme_a_distance _ as eq) ->
      Some (array [| Unsafe.inject (equipement_nom_to_jsoo e); Unsafe.inject (equipement_to_jsoo eq) |])
    | _ -> None) l

and armures _app l =
  let l = to_listf equipement_nom_of_jsoo l in
  of_list @@ List.filter_map (fun e -> match List.assoc_opt e !equipements with
    | Some (Armure _ as eq) ->
      Some (array [| Unsafe.inject (equipement_nom_to_jsoo e); Unsafe.inject (equipement_to_jsoo eq) |])
    | _ -> None) l

and edite app = match page_of_jsoo app##.page with
  | Edition e ->
    if not (verifie_caracteristiques e.choix.caracteristiques) then alert app "caracteristiques non valables" else
    let caracteristiques = ajoute_caracteristiques e.choix.caracteristiques (fst e.choix.bonus_peuple) in
    let p = { e.personnage with caracteristiques; niveau = e.choix.niveau; equipements=e.choix.equipements } in
    begin match verifie_voies p e.choix.voies with
      | None ->
        let p = { p with voies=e.choix.voies } in
        let nb_sorts = nombre_sorts @@ List.filter_map (fun (v, rg) -> Option.map (fun v -> v, rg) @@ List.assoc_opt v !voies) e.choix.voies in
        let def_equipement, agi_max = List.fold_left (fun (def, agi) e ->
          match List.assoc_opt e !equipements with
          | Some Armure { defense; agilite_max; _ } ->
            def + defense, Option.fold ~none:agi ~some:(fun a -> min a agi) agilite_max
          | _ -> def, agi) (0, 8) p.equipements in
        let p = remplit_caracteristiques p nb_sorts def_equipement agi_max in
        let p = Personnage (e.label, p) in
        route app (page_to_jsoo p)
      | Some e -> alert app e
    end
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"

and retire_equipement app e = match page_of_jsoo app##.page with
  | Edition { choix={equipements; _}; _} ->
    let e = equipement_nom_of_jsoo e in
    let equipements = List.filter (fun x -> x <> e) equipements in
    (Unsafe.coerce app)##.page##.edition##.choix##.equipements := of_listf equipement_nom_to_jsoo equipements;
  | _ -> ()

and pp_equipement _app e =
  let s = equipement_to_str (equipement_nom_of_jsoo e) in
  string @@ String.capitalize_ascii @@ String.map (function '_' -> ' ' | c -> c) s

and ajout_equipement app = match page_of_jsoo app##.page with
  | Edition { choix = { equipement; equipements; _ }; _ } ->
    let equipements = equipements @ [ equipement ] in
    (Unsafe.coerce app)##.page##.edition##.choix##.equipements := of_listf equipement_nom_to_jsoo equipements
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"

and pp_peuple _app p =
  let s = match Json_encoding.construct peuple_enc p with `String s -> s | _ -> assert false in
  if String.starts_with ~prefix:"Demi_" s then
    string @@ String.map (function '_' -> '-' | c -> c) s
  else string @@ String.map (function '_' -> ' ' | c -> c) s

and pp_voie _app v =
  let s = match Json_encoding.construct voie_type_enc v with `String s -> s | _ -> assert false in
  if String.starts_with ~prefix:"Demi_" s then
    string @@ String.map (function '_' -> '-' | c -> c) s
  else string @@ String.map (function '_' -> ' ' | c -> c) s

[%%mounted fun app ->
  let elt = Dom_html.getElementById "erreur-modal" in
  ignore @@ Js_of_ocaml.Dom_events.listen elt (Js_of_ocaml.Dom_events.Typ.make "hide.bs.modal") @@ fun _ _ ->
  app##.modal_erreur_ := undefined; true
]

let () =
  open_db @@ fun db ->
  let%data db : Ezjs_idb.Types.iDBDatabase t = db [@@noconv] in
  let app = [%app {conv; mount; unhide; export}] in
  Dom_html.window##.onpopstate := Dom_html.handler (fun (e : Dom_html.popStateEvent t) ->
    route app (Unsafe.coerce e##.state); _false);
  match Opt.to_option (Unsafe.coerce Dom_html.window##.history)##.state with
  | Some p ->
    begin match page_of_jsoo p with
      | Chargement -> init app
      | _ -> route app p
      | exception _ -> init app
    end
  | None -> init app
