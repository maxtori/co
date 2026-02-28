open Ezjs_min
open Ezjs_idb
open Co

type phase =
  | Profil of { profils: (famille * profil list) list; choix: (famille * profil) option }
  | Peuple of { peuples: peuple list; choix: peuple option }
  | Caracteristiques of {
      bonuses: avec_nom list list;
      choix: caracteristiques; choix_bonus: (avec_nom list * int) option
    }
  | Equipements of {
      possibilites: equipement_et_nombre list list;
      choix: equipement_et_nombre list }
  | Voies of { voies: voie_type list; choix: (voie_type * int) list }
  | Enregistrement of {
      ideaux: string list; travers_options: string list;
      nom: string; label: string; ideal: string; travers: string;
    }
[@@deriving jsoo {remove_undefined; snake}]

type avec_phase = {
  perso: personnage;
  creation: phase option;
} [@@deriving jsoo]

type avec_label = {
  label: string;
  perso: personnage;
  creation: phase option;
} [@@deriving jsoo]

module Personnages = Store(StringTr)(struct
    type js = avec_phase_jsoo t
    type t = avec_phase
    let to_js = avec_phase_to_jsoo
    let of_js = avec_phase_of_jsoo
  end)

type points = {
  genre: genre_points;
  points: points_avec_max;
  titre: string;
  de: de option;
  resultat: int option; [@mutable]
} [@@deriving jsoo]

type des = {
  titre: string option;
  de: de;
  nombre: int;
  bonus: int;
  extra: int;
  resultat: int option; [@mutable]
} [@@deriving jsoo]

type choix_edition = {
  niveau: int;
  caracteristiques: caracteristiques;
  bonuses: avec_nom list;
  bonus: avec_nom;
  voies: (voie_type * int) list;
  equipements: equipement_et_nombre list;
  equipement: equipement_nom * int;
} [@@deriving jsoo]

type edition = {
  label: string;
  perso: personnage;
  voies: voie_type list;
  equipements: equipement_et_nombre list;
  choix: choix_edition;
  ids: caracteristique_ou_bonus list;
} [@@deriving jsoo]

type page =
  | Chargement
  | Personnages of avec_label list
  | Personnage of { label: string; perso: personnage }
  | Importation of string
  | Creation of { label: string; perso: personnage; creation: phase }
  | Edition of edition
  | Backup of (Unsafe.any js_array t js_array t [@ignore])
[@@deriving jsoo {remove_undefined; snake}]

let voies : (voie_type * voie) list ref = ref []
let equipements : (equipement_nom * equipement) list ref = ref []

let%data page : page = Chargement
and modal_erreur : string option = None
and points : points option = None
and des : des option = None

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
  Personnages.fold st (fun label {perso; creation} acc -> {label; perso; creation} :: acc) [] @@ fun l ->
  f app (List.rev l)

let ajout_personnage ?creation app label perso f =
  let st = Personnages.store ~mode:READWRITE app##.db in
  Personnages.add ~key:label ~callback:(fun _ -> f app {label; perso; creation}) st {perso; creation}

let edition_personnage ?creation app label perso =
  let st = Personnages.store ~mode:READWRITE app##.db in
  Personnages.put ~key:label st {perso; creation}

let suppression_personnage app label =
  let st = Personnages.store ~mode:READWRITE app##.db in
  Personnages.delete st (Personnages.K label)

let importation_personnage app label fichier f = ouverture_fichier fichier @@ fun s ->
  let p = EzEncoding.destruct personnage_enc s in
  let label = if String.trim label = "" then Format.sprintf "%s_%d" p.nom p.niveau else label in
  ajout_personnage app label p f

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
  | Error e -> js_log e; log "%s non présent" url; f None
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
    match e with
    | `autre _ ->
      let equipement = Autre { description=""; prix=None } in
      equipements := (e, equipement) :: !equipements;
      f (Some equipement)
    | _ ->
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
      | ({perso; _}: avec_label) :: tl ->
        chargement_voies (List.map fst perso.voies) @@ fun () ->
        chargement_equipements (List.map fst perso.equipements) @@ fun _ ->
        aux tl in
    aux l
  | Personnage {perso; _} ->
    chargement_voies (List.map fst perso.voies) @@ fun _ ->
    chargement_equipements (List.map fst perso.equipements) @@ fun _ ->
    f app
  | Creation { creation = Voies {voies; _}; _ } ->
    chargement_voies voies @@ fun _ ->
    f app
  | Creation { creation = Equipements {possibilites; _}; perso = {equipements; _}; _ } ->
    chargement_equipements (List.map fst (equipements @ List.flatten possibilites)) @@ fun _ ->
    f app
  | Edition { voies; equipements=l; choix={equipements; _}; _ } ->
    chargement_voies voies @@ fun _ ->
    chargement_equipements (List.map fst equipements) @@ fun _ ->
    chargement_equipements_async (List.map fst l);
    f app
  | _ -> f app

let init app =
  chargement_personnages app @@ fun app l ->
  route app (page_to_jsoo (Personnages l))

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
  Promise.jthen box##init @@ fun () ->
  Promise.jthen (box##roll (string s)) @@ fun r ->
  let v = Array.fold_left (fun acc r -> acc + r##.value) 0 @@ to_array r in
  f v

let backup app =
  let st = Personnages.store ~mode:READONLY app##.db in
  Personnages.Raw.fold st (fun label p acc ->
    (array [| Unsafe.inject label; Unsafe.inject p##.perso; Unsafe.inject p##.creation |]) :: acc
  ) [] @@ fun l ->
  app##.page := page_to_jsoo (Backup (of_list (List.rev l)))

let telecharge name s =
  let blob = File.blob_from_string ~contentType:"application/json" s in
  let href = Dom_html.window##._URL##createObjectURL blob in
  let elt = Dom_html.createA Dom_html.document in
  elt##.href := href;
  elt##.download := string (Format.sprintf "%s.json" name);
  elt##click

let%meth commence_creation app =
  let profils = profils () in
  let label = Format.sprintf "perso_%ld" (to_int32 @@ date##now) in
  let p = Creation { label; perso=personnage_vide; creation=Profil {profils; choix=None} } in
  route app (page_to_jsoo p)

and personnage app p =
  let p = avec_label_of_jsoo p in
  let page = match p.creation with
    | Some creation -> Creation { label=p.label; perso=p.perso; creation }
    | None -> Personnage { label=p.label; perso=p.perso } in
  route app (page_to_jsoo page)

and dir app p = route app p

and home app = init app
and edition app = match page_of_jsoo app##.page with
  | Personnage { label; perso=p } ->
    let ids = List.map snd caracteristique_assoc @ List.map snd bonus_assoc in
    let choix = {
      niveau=p.niveau; caracteristiques=p.caracteristiques_base;
      bonuses = p.bonuses; bonus = ("", Bonus {id=`AGI; valeur=`int 0; opt=None});
      voies=p.voies; equipements=p.equipements; equipement=(`autre "", 1) } in
    let voies = voies_peuple p.peuple @ voies_profil p.profil in
    let edition = {
      label; perso=p; voies; choix; ids;
      equipements=List.map (fun (_, e) -> e, None) equipement_nom_assoc } in
    route app (page_to_jsoo (Edition edition))
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"

and [@noconv] importation app (ev: Dom_html.inputElement Dom.event t) =
  match Opt.to_option ev##.target, page_of_jsoo app##.page with
  | Some target, Importation key ->
    let f = List.hd @@ Dom.list_of_nodeList target##.files in
    importation_personnage app key f @@ fun app p ->
    route app (page_to_jsoo (Personnage {label=p.label; perso=p.perso}))
  | _ -> ()

and voie _app v =
  match List.assoc_opt (voie_type_of_jsoo v) !voies with
  | None -> log_str "voie non trouvée"; undefined
  | Some v -> def (voie_to_jsoo v)

and phase_suivante app =
  match page_of_jsoo app##.page with
  | Creation { perso; creation; label } ->
    begin match creation with
      | Profil {choix=None; _ } ->
        alert app "profil non choisi"
      | Profil {choix=Some (famille, profil); _ } ->
        let peuples = List.map snd peuple_assoc in
        let creation = Peuple { peuples; choix=None } in
        let perso = { perso with famille; profil } in
        let p = Creation { perso; creation; label } in
        ajout_personnage ~creation app label perso @@ fun app _ ->
        route app (page_to_jsoo p)
      | Peuple {choix=None; _ } ->
        alert app "peuple non choisi"
      | Peuple {choix=Some peuple; _ } ->
        let choix = caracteristiques_par_defaut (Some peuple) in
        let bonuses = bonuses_peuple peuple choix in
        let creation = Caracteristiques { bonuses; choix; choix_bonus=None } in
        let perso = { perso with peuple } in
        edition_personnage ~creation app label perso;
        let p = Creation { perso; creation; label } in
        route app (page_to_jsoo p)
      | Caracteristiques { choix_bonus=None; _ } ->
        alert app "caracteristiques bonus non choisies"
      | Caracteristiques { choix; choix_bonus=Some (bonuses, _); _ } ->
        if verifie_caracteristiques choix then
          let l = equipements_profil perso.profil in
          let leq, possibilites = List.partition (function [ _ ] -> true | _ -> false) l in
          let leq = List.flatten leq in
          let perso = { perso with caracteristiques_base=choix; bonuses; equipements=leq } in
          let def_equipement, agi_max = List.fold_left (fun (def, agi) (e, _) ->
            match List.assoc_opt e !equipements with
            | Some Armure { defense; agilite_max; _ } ->
              def + defense, Option.fold ~none:agi ~some:(fun a -> min a agi) agilite_max
            | _ -> def, agi) (0, 8) perso.equipements in
          let perso = remplit_caracteristiques perso def_equipement agi_max in
          let creation = match possibilites with
            | [] ->
              let voies = voies_peuple perso.peuple @ voies_profil perso.profil in
              Voies { voies; choix=[] }
            | _ ->
              let choix = List.map List.hd possibilites in
              Equipements { possibilites; choix } in
          edition_personnage ~creation app label perso;
          let p = Creation { perso; creation; label } in
          route app (page_to_jsoo p)
        else alert app "caracteristiques non valables"
      | Equipements { choix; _ } ->
        let equipements = perso.equipements @ choix in
        let voies = voies_peuple perso.peuple @ voies_profil perso.profil in
        let perso = { perso with equipements } in
        let creation = Voies { voies; choix=[] } in
        edition_personnage ~creation app label perso;
        let p = Creation { perso; creation; label } in
        route app (page_to_jsoo p)
      | Voies { choix=l; _ } ->
        begin match verifie_voies perso l with
          | None ->
            let perso = { perso with voies=l } in
            let bonus_voies = bonus_voies @@ List.filter_map (fun (v, rg) -> Option.map (fun v -> v, rg) @@ List.assoc_opt v !voies) l in
            let def_equipement, agi_max = List.fold_left (fun (def, agi) (e, _) ->
              match List.assoc_opt e !equipements with
              | Some Armure { defense; agilite_max; _ } ->
                def + defense, Option.fold ~none:agi ~some:(fun a -> min a agi) agilite_max
              | _ -> def, agi) (0, 8) perso.equipements in
            let perso = { perso with bonuses = perso.bonuses @ bonus_voies } in
            let perso = remplit_caracteristiques perso def_equipement agi_max in
            let ideaux = "" :: (List.map fst ideal_assoc) in
            let travers_options = "" :: (List.map fst travers_assoc) in
            let creation = Enregistrement {ideaux; travers_options; nom=""; label=""; ideal=""; travers=""} in
            edition_personnage ~creation app label perso;
            let p = Creation { perso; creation; label } in
            route app (page_to_jsoo p)
          | Some e -> alert app e
        end
      | Enregistrement {nom; label=lbl; ideal; travers; _} ->
        if nom = "" then alert app "nom vide" else
        let ideal = List.assoc_opt ideal ideal_assoc in
        let travers = List.assoc_opt travers travers_assoc in
        let perso = { perso with nom; ideal; travers } in
        let lbl = if lbl = "" then Format.sprintf "%s_%d" nom perso.niveau else lbl in
        ajout_personnage app lbl perso @@ fun app {perso; _} ->
        suppression_personnage app label;
        let p = Personnage {label=lbl; perso} in
        route app (page_to_jsoo p)
    end
  | _ -> init app

and capacite_choisie app vt rg =
  match page_of_jsoo app##.page with
  | Creation { creation; _ } ->
    begin match creation with
      | Voies { choix; _ } ->
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
  | Creation { creation; _ } ->
    begin match creation with
      | Voies { choix; _ } ->
        let c = (Unsafe.coerce app)##.page##.creation##.creation##.voies##.choix in
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
  telecharge (to_string label) s

and telecharge_raw_personnage _app label p =
  let s = to_string @@ _JSON##stringify p in
  telecharge (to_string label) s

and charge_modal_points app g = match page_of_jsoo app##.page with
  | Personnage { perso=p; _ } ->
    let genre = genre_points_of_jsoo g in
    let titre = genre_de_titre genre in
    let points, de = match genre with
      | `points_de_vigueur -> p.points_de_vigueur, None
      | `des_de_recuperation -> p.des_de_recuperation, Some (de_recuperation p.famille)
      | `points_de_chance -> p.points_de_chance, None
      | `points_de_mana -> p.points_de_mana, None in
    let points = { titre; genre; points; de; resultat=None } in
    app##.points := def (points_to_jsoo points);
    let cs : _ constr = Unsafe.global##.bootstrap##._Modal in
    let md = new%js cs (string "#points-modal") in
    ignore md##show
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"

and lance_de_recuperation app points = match page_of_jsoo app##.page, Optdef.to_option points##.de with
  | Personnage {label; perso}, Some de ->
    points##.resultat := undefined;
    lance_de "des-recuperation" (to_string de) 1 @@ fun r ->
    let p = points_of_jsoo points in
    let points_de_vigueur = { perso.points_de_vigueur with courant = min (perso.points_de_vigueur.courant + r) perso.points_de_vigueur.max } in
    let des_de_recuperation = { p.points with courant = p.points.courant - 1 } in
    let perso = { perso with points_de_vigueur; des_de_recuperation } in
    points##.points##.courant := des_de_recuperation.courant;
    points##.resultat := def r;
    edition_personnage app label perso;
    let p = Personnage {label; perso} in
    route app (page_to_jsoo p)
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"

and charge_modal_des app de bonus nombre titre =
  let des = { de=de_of_jsoo de; bonus; extra=0; nombre; titre=to_optdef to_string titre; resultat=None } in
  app##.des := def (des_to_jsoo des);
  let cs : _ constr = Unsafe.global##.bootstrap##._Modal in
  let md = new%js cs (string "#des-modal") in
  ignore md##show

and lance_de _app des =
  des##.resultat := undefined;
  let d = des_of_jsoo des in
  lance_de "des-container" (to_string des##.de) d.nombre @@ fun r ->
  des##.resultat := def r

and equipements _app l kind =
  let l = to_listf equipement_et_nombre_of_jsoo l in
  let kind = to_optdef to_string kind in
  of_list @@ List.filter_map (fun (e, nombre) -> match List.assoc_opt e !equipements, kind with
    | Some (Arme a), (None | Some "arme") ->
      let arme = List.map (function Distance {portee; _} -> Distance {portee; nombre} | a -> a) a.arme in
      Some (array [| Unsafe.inject (equipement_nom_to_jsoo e); Unsafe.inject (equipement_to_jsoo (Arme { a with arme })) |])
    | Some (Armure _ as eq), (None | Some "armure") ->
      Some (array [| Unsafe.inject (equipement_nom_to_jsoo e); Unsafe.inject (equipement_to_jsoo eq) |])
    | Some (Autre _ as eq), (None | Some "autre") ->
      Some (array [| Unsafe.inject (equipement_nom_to_jsoo e); Unsafe.inject (equipement_to_jsoo eq) |])
    | _ -> None) l

and edite app = match page_of_jsoo app##.page with
  | Edition e ->
    if not (verifie_caracteristiques e.choix.caracteristiques) then alert app "caracteristiques non valables" else
    let p = { e.perso with niveau = e.choix.niveau; equipements=e.choix.equipements;
                           bonuses=e.choix.bonuses } in
    begin match verifie_voies p e.choix.voies with
      | None ->
        let p = { p with voies=e.choix.voies } in
        let bonus_voies = bonus_voies @@ List.filter_map (fun (v, rg) -> Option.map (fun v -> v, rg) @@ List.assoc_opt v !voies) e.choix.voies in
        let bonuses = List.fold_left (fun acc (n, b) ->
          match List.find_opt (fun (n2, _) -> n = n2) acc with
          | None -> acc @ [ n, b ]
          | Some _ -> acc
        ) p.bonuses bonus_voies in
        let p = { p with bonuses } in
        let def_equipement, agi_max = List.fold_left (fun (def, agi) (e, _) ->
          match List.assoc_opt e !equipements with
          | Some Armure { defense; agilite_max; _ } ->
            def + defense, Option.fold ~none:agi ~some:(fun a -> min a agi) agilite_max
          | _ -> def, agi) (0, 8) p.equipements in
        let p = remplit_caracteristiques p def_equipement agi_max in
        edition_personnage app e.label p;
        let p = Personnage {label=e.label; perso=p} in
        route app (page_to_jsoo p)
      | Some e -> alert app e
    end
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"

and retire_equipement app e = match page_of_jsoo app##.page with
  | Edition { choix={equipements; _}; _} ->
    let e = equipement_nom_of_jsoo e in
    let equipements = List.filter (fun (x, _) -> x <> e) equipements in
    (Unsafe.coerce app)##.page##.edition##.choix##.equipements := of_listf equipement_et_nombre_to_jsoo equipements;
  | _ -> ()

and ajoute_equipement app = match page_of_jsoo app##.page with
  | Edition { choix = { equipement=e, nb; equipements; _ }; _ } ->
    let equipement = e, (if nb = 1 then None else Some nb) in
    let equipements = equipements @ [ equipement ] in
    (Unsafe.coerce app)##.page##.edition##.choix##.equipements := of_listf equipement_et_nombre_to_jsoo equipements
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"

and ajoute_bonus app = match page_of_jsoo app##.page with
  | Edition { choix = { bonuses; bonus; _ }; _ } ->
    let bonuses = bonuses @ [ bonus ] in
    (Unsafe.coerce app)##.page##.edition##.choix##.bonuses := of_listf avec_nom_to_jsoo bonuses
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"

and pp_equipement _app e =
  let s = equipement_to_str (equipement_nom_of_jsoo e) in
  string @@ String.capitalize_ascii @@ String.map (function '_' -> ' ' | c -> c) s

and pp_peuple _app p =
  let s = match Json_encoding.construct peuple_enc (peuple_of_jsoo p) with `String s -> s | _ -> assert false in
  let b = String.starts_with ~prefix:"demi_" s in
  string @@ String.capitalize_ascii @@ String.map (function '_' -> if b then '-' else ' ' | c -> c) s

and pp_voie _app v =
  let s = match Json_encoding.construct voie_type_enc (voie_type_of_jsoo v) with `String s -> s | _ -> assert false in
  let b = String.starts_with ~prefix:"demi_" s in
  string @@ String.capitalize_ascii @@ String.map (function '_' -> if b then '-' else ' ' | c -> c) s

[%%mounted fun app ->
  let elt_erreur = Dom_html.getElementById "erreur-modal" in
  let elt_des = Dom_html.getElementById "des-modal" in
  let elt_points = Dom_html.getElementById "points-modal" in
  ignore (Js_of_ocaml.Dom_events.listen elt_erreur (Js_of_ocaml.Dom_events.Typ.make "hide.bs.modal") @@ fun _ _ ->
          app##.modal_erreur_ := undefined; true);
  ignore (Js_of_ocaml.Dom_events.listen elt_des (Js_of_ocaml.Dom_events.Typ.make "hide.bs.modal") @@ fun _ _ ->
          app##.des := undefined; true);
  ignore (Js_of_ocaml.Dom_events.listen elt_points (Js_of_ocaml.Dom_events.Typ.make "hide.bs.modal") @@ fun _ _ ->
          begin match page_of_jsoo app##.page, to_optdef points_of_jsoo app##.points with
            | Personnage { label; perso }, Some p ->
              if p.points.max < p.points.courant then alert app "valeur supérieur au maximum" else
              let perso = match p.genre with
                | `points_de_vigueur -> { perso with points_de_vigueur = p.points }
                | `des_de_recuperation -> { perso with des_de_recuperation = p.points }
                | `points_de_chance -> { perso with points_de_chance = p.points }
                | `points_de_mana -> { perso with points_de_mana = p.points } in
              edition_personnage app label perso;
              let p = Personnage {label; perso} in
              app##.points := undefined;
              route app (page_to_jsoo p)
            | _ -> alert app "cette fonction n'est pas accessible sur cette page"
         end; true)
]

let () =
  open_db @@ fun db ->
  let%data db : Ezjs_idb.Types.iDBDatabase t = db [@@noconv] in
  let app = [%app {conv; mount; unhide; export}] in
  Dom_html.window##.onpopstate := Dom_html.handler (fun (e : Dom_html.popStateEvent t) ->
    route app (Unsafe.coerce e##.state); _false);
  try match Opt.to_option (Unsafe.coerce Dom_html.window##.history)##.state with
    | Some p ->
      begin match page_of_jsoo p with
        | Chargement -> init app
        | _ -> route app p
      end
    | None -> init app
  with exn ->
    log "initialisation erreur: %s" (Printexc.to_string exn);
    backup app
