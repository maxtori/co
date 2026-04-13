open Ezjs_min
open Ezjs_idb
open Co

type phase_creation =
  | Profil of { profils: (famille * profil list) list; choix: (famille * profil) option }
  | Peuple of { peuples: peuple list; choix: peuple option }
  | Caracteristiques of {
      bonuses: bonus_avec_nom list list;
      choix: caracteristiques; choix_bonus: (bonus_avec_nom list * int) option
    }
  | Equipements of {
      possibilites: equipement_et_nombre list list;
      choix: equipement_et_nombre list }
  | Voies of {
      voies: voie_et_rangs list; choix: voie_et_rangs list;
      points_de_maitrise: int * int; bonuses_optionnels: bonus_avec_nom list }
  | Competences of {
      maitrisees: (competence * int) list; possibilites: competence list; ajout_competence: competence option;
      possibilites_maitrise: competence list;
      choix: competence option; competences: (competence * int) list; points: int * int }
  | Enregistrement of {
      ideaux: string list; travers_options: string list;
      nom: string; label: string; ideal: string; travers: string;
      description: string; image: string option; image_url: string option }
[@@deriving jsoo {remove_undefined; snake}]

type avec_phase = {
  perso: personnage;
  creation: phase_creation option;
} [@@deriving jsoo]

type avec_label = {
  label: string;
  perso: personnage;
  creation: phase_creation option;
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
  bonus: int; [@mutable]
  extra: int;
  resultat: int option; [@mutable]
  choix: caracteristique_et_point list;
} [@@deriving jsoo]

type choix_edition = {
  niveau: int;
  caracteristiques: caracteristiques;
  bonuses: bonus_avec_nom list;
  bonus: bonus_avec_nom;
  voies: voie_et_rangs list;
  equipements: equipement_et_nombre list;
  equipement: equipement_nom * int;
  description: string;
  image: string option;
  image_url: string option;
  nom: string;
  ideal: string;
  travers: string;
  competences: competence_et_point list;
  ajout_competence: competence option;
} [@@deriving jsoo]

type edition = {
  label: string;
  perso: personnage;
  voies: voie_et_rangs list;
  equipements: equipement_et_nombre list;
  choix: choix_edition;
  ids: caracteristique_ou_bonus list;
  ideaux: string list;
  travers_options: string list;
  competences: competence list;
  points_de_maitrise: int * int;
  points_de_competences: int * int;
} [@@deriving jsoo]

type page =
  | Chargement
  | Personnages of avec_label list
  | Personnage of { label: string; perso: personnage }
  | Nouveau
  | Importation of string
  | Creation of { label: string; perso: personnage; phase: phase_creation }
  | Edition of edition
  | Backup of (Unsafe.any js_array t js_array t [@ignore])
[@@deriving jsoo {remove_undefined; snake}]

let voies : (voie_type * voie) list ref = ref []
let equipements : (equipement_nom * equipement) list ref = ref []

let%data page : page = Chargement
and modal_erreur : string option = None
and points : points option = None
and des : des option = None
and tsp : int = Int32.to_int (to_int32 date##now) / 1000
and image : string option = None
and has_storage : bool = Option.is_some (Optdef.to_option Unsafe.global##.navigator##.storage)

let genre_de_titre = function
  | `points_de_vigueur -> "Points de vigeur"
  | `des_de_recuperation -> "Dés de récupération"
  | `points_de_chance -> "Points de chance"
  | `points_de_mana -> "Points de mana"

let ouvre_db f =
  Personnages.set_name "personnages";
  let upgrade db e =
    if e.new_version = 1 && e.old_version = 0 then ignore (Personnages.create db)
    else if e.old_version = 1 && e.new_version = 0 then db##deleteObjectStore (string "personnages") in
  openDB "co" ~upgrade ~version:1 f

let ouverture_fichier fichier f =
  let reader = new%js File.fileReader in
  reader##.onloadend := Dom.handler (fun _evt ->
    if reader##.readyState = File.DONE then
      Opt.iter (File.CoerceTo.string (reader##.result)) (fun s -> f (to_string s));
    _true);
  reader##(readAsText fichier)

let ouverture_image fichier f =
  let reader = new%js File.fileReader in
  reader##.onloadend := Dom.handler (fun _evt ->
    if reader##.readyState = File.DONE then
      Opt.iter (File.CoerceTo.arrayBuffer (reader##.result)) (fun a -> f a);
    _true);
  reader##(readAsArrayBuffer fichier)

let backup app =
  let st = Personnages.store ~mode:READONLY app##.db in
  Personnages.Raw.fold st (fun label p acc ->
    (array [| Unsafe.inject label; Unsafe.inject p##.perso; Unsafe.inject p##.creation |]) :: acc
  ) [] @@ fun l ->
  app##.page := page_to_jsoo (Backup (of_list (List.rev l)))

let chargement_personnages app f =
  let st = Personnages.store ~mode:READONLY app##.db in
  let error _ = backup app in
  Personnages.fold ~error st (fun label {perso; creation} acc ->
    {label; perso; creation} :: acc) [] @@ fun l ->
  f app (List.rev l)

let ajout_personnage ?creation app label perso f =
  let st = Personnages.store ~mode:READWRITE app##.db in
  Personnages.add ~key:label ~callback:(fun _ -> f app {label; perso; creation}) st {perso; creation}

let edition_personnage ?creation app label perso =
  let st = Personnages.store ~mode:READWRITE app##.db in
  Personnages.put ~key:label st {perso; creation}

let edition_raw_personnage app label perso =
  let st = Personnages.store ~mode:READWRITE app##.db in
  Personnages.Raw.put ~key:label st (object%js val perso = perso val creation = undefined end)

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
  let rec aux acc = function
    | [] -> f (List.rev acc)
    | vt :: tl -> charge_voie vt (function None -> aux acc tl | Some v -> aux ((vt, v) :: acc) tl) in
  aux [] l

let chargement_voies_async l =
  List.iter (fun vt -> charge_voie vt (fun _ -> ())) l

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

let wrap app r f = match r with Ok x -> f x | Error e -> alert app e

let personnage_to_b64 p =
  let s = EzEncoding.construct personnage_enc p in
  let cs = Unsafe.global##._TextEncoder in
  let encoder = new%js cs in
  let uint8a = encoder##encode (string s) in
  let b = uint8a##toBase64 in
  to_string b

let personnage_of_b64 s =
  try
    let a = Unsafe.global##._Uint8Array##fromBase64 (string s) in
    let cs = Unsafe.global##._TextDecoder in
    let decoder = new%js cs in
    let s = decoder##decode a in
    Some (EzEncoding.destruct personnage_enc (to_string s))
  with _ -> None

let route ?(loading=true) ?path app p =
  app##.tsp := Int32.to_int (to_int32 date##now) / 1000;
  let p0, p1 = unproxy app##.page, unproxy p in
  if loading then (app##.page := page_to_jsoo Chargement);
  Firebug.console##log_3 p0 (string "-->") p1;
  let state p = some @@ Unsafe.coerce p  in
  let () = match page_of_jsoo p0 with
    | Chargement -> ()
    | _ -> Dom_html.window##.history##replaceState (state p0) (string "") null in
  let f app =
    app##.page := p1;
    let path = opt string path in
    Dom_html.window##.history##pushState (state p1) (string "") path in
  match page_of_jsoo p with
  | Personnages l ->
    let rec aux = function
      | [] -> f app
      | ({perso; _}: avec_label) :: tl ->
        chargement_voies_async (List.map fst perso.voies);
        chargement_equipements_async (List.map fst perso.equipements);
        aux tl in
    aux l
  | Personnage {perso; _} ->
    chargement_voies (List.map fst perso.voies) @@ fun _ ->
    chargement_equipements (List.map fst perso.equipements) @@ fun _ ->
    begin match perso.image with
      | None -> f app
      | Some nom_fichier ->
        let st = Unsafe.global##.navigator##.storage in
        Promise.jthen st##getDirectory @@ fun dir ->
        Promise.jthen (dir##getFileHandle (string nom_fichier)) @@ fun fh ->
        Promise.jthen fh##getFile @@ fun fi ->
        let url = Dom_html.window##._URL##createObjectURL fi in
        app##.image := def url;
        f app
    end
  | Creation { phase = Voies {voies; _}; _ } ->
    chargement_voies (List.map fst voies) @@ fun _ ->
    f app
  | Creation { phase = Equipements {possibilites; _}; perso = {equipements; _}; _ } ->
    chargement_equipements (List.map fst (equipements @ List.flatten possibilites)) @@ fun _ ->
    f app
  | Edition { voies; equipements=l; choix={equipements; _}; _ } ->
    chargement_voies (List.map fst voies) @@ fun _ ->
    chargement_equipements (List.map fst equipements) @@ fun _ ->
    chargement_equipements_async (List.map fst l);
    f app
  | _ -> f app

let init app =
  chargement_personnages app @@ fun app l ->
  match l with
  | [] -> route ~loading:false app (page_to_jsoo Nouveau)
  | _ -> route app (page_to_jsoo (Personnages l))

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

let telecharge name s =
  let blob = File.blob_from_string ~contentType:"application/json" s in
  let href = Dom_html.window##._URL##createObjectURL blob in
  let elt = Dom_html.createA Dom_html.document in
  elt##.href := href;
  elt##.download := string (Format.sprintf "%s.json" name);
  elt##click

let sauvergarde_fichier_persistent nom a f =
  let st = Unsafe.global##.navigator##.storage in
  Promise.jthen st##persist @@ fun b ->
  if not (to_bool b) then () else
    Promise.jthen st##getDirectory @@ fun dir ->
    Promise.jthen (dir##getFileHandle_1 (string nom) (object%js val create= _true end)) @@ fun fh ->
    Promise.jthen fh##createWritable @@ fun wr ->
    Promise.jthen (wr##write a) @@ fun () ->
    Promise.jthen wr##close @@ fun () ->
    Promise.jthen fh##getFile @@ fun fi ->
    let url = Dom_html.window##._URL##createObjectURL fi in
    f nom url

let capacites l =
  let voies, lc = List.split @@ List.filter_map (fun (vt, rgs) ->
    let v = List.assoc vt !voies in
    let v = List.filter (fun c -> List.mem c.rang rgs) v in
    match v with
    | [] -> None
    | _ -> Some ((vt, rgs), v)
  ) l in
  voies, List.flatten lc

(* methods *)

let%meth commence_creation app =
  let profils = profils () in
  let label = Format.sprintf "perso_%ld" (to_int32 date##now) in
  let p = Creation { label; perso=personnage_vide; phase=Profil {profils; choix=None} } in
  route app (page_to_jsoo p)

and personnage app p =
  let p = avec_label_of_jsoo p in
  let page = match p.creation with
    | Some phase -> Creation { label=p.label; perso=p.perso; phase }
    | None -> Personnage { label=p.label; perso=p.perso } in
  route app (page_to_jsoo page)

and dir app p = route app p

and home app = init app
and edition app = match page_of_jsoo app##.page with
  | Personnage { label; perso; _ } ->
    let ids = List.map snd caracteristique_assoc @ List.map snd bonus_type_assoc in
    let ideal = match perso.ideal with None -> "" | Some ideal -> ideal_to_str ideal in
    let travers = match perso.travers with None -> "" | Some t -> travers_to_str t in
    chargement_voies (List.map fst perso.voies) @@ fun l ->
    let l = List.map (fun (vt, v) -> vt, v, List.assoc vt perso.voies) l in
    let rg_mage = rang_max @@ Option.value ~default:[] @@ List.assoc_opt `Mage perso.voies in
    let voies =
      List.map (fun x -> x, if rg_mage >= 1 then [ 1 ] else [1; 2; 3; 4; 5]) (voies_peuple perso.peuple) @
      List.map (fun x -> x, [1; 2; 3; 4; 5]) (voies_profil perso.profil) @
      (voies_capacites ~famille:perso.famille l) in
    let voies_perso, capacites = capacites perso.voies in
    let _, _, _, _, pnv, pb, pc = rangs_et_points ~capacites perso in
    begin match points_de_competences perso with
      | Error e -> alert app e
      | Ok (points_niveau, points_capacites, points_utilises, _) ->
        let choix = {
          niveau=perso.niveau; caracteristiques=perso.caracteristiques_base;
          bonuses = perso.bonuses; bonus = ("", {id=`AGI; valeur=`int 0; opt=None});
          voies=voies_perso; equipements=perso.equipements; equipement=(`autre "", 1);
          description=perso.description; image_url=to_optdef to_string app##.image; image=perso.image;
          nom=perso.nom; ideal; travers; competences=perso.competences; ajout_competence=None;
        } in
        let competences = List.filter (fun c ->
          Option.is_none (List.assoc_opt c perso.competences)
        ) (List.map snd competence_assoc) in
        let edition = {
          label; perso; voies; choix; ids;
          equipements=List.map (fun (_, e) -> e, None) equipement_nom_assoc;
          ideaux = "" :: (List.map fst ideal_assoc);
          travers_options = "" :: (List.map fst travers_assoc);
          competences; points_de_maitrise=pc, pnv+pb;
          points_de_competences=points_utilises,points_niveau+points_capacites;
        } in
        route app (page_to_jsoo (Edition edition))
    end
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"

and [@noconv] importation app (ev: Dom_html.inputElement Dom.event t) =
  match Opt.to_option ev##.target, page_of_jsoo app##.page with
  | Some target, Importation key ->
    let f = List.hd @@ Dom.list_of_nodeList target##.files in
    importation_personnage app key f @@ fun app p ->
    route app (page_to_jsoo (Personnage {label=p.label; perso=p.perso}))
  | _ -> ()

and voie app vt rgs =
  match List.assoc_opt (voie_type_of_jsoo vt) !voies with
  | None -> log_str "voie non trouvée"; undefined
  | Some v ->
    let rgs = to_list rgs in
    let perso_bonuses = match page_of_jsoo app##.page with
      | Edition { choix = { bonuses; _ }; _ } -> bonuses
      | _ -> [] in
    let v = List.fold_left (fun acc (c: capacite) ->
      if not (List.mem c.rang rgs) then acc else
      let bonus = List.filter_map (fun b -> match b.opt with
        | None -> None
        | Some _ ->
          if List.exists (fun (n2, b2) -> n2 = c.nom && b.id = b2.id) perso_bonuses then
            Some { b with opt=Some (Some true) }
          else Some { b with opt=Some (Some false) }
      ) c.bonus in
      acc @ [ { c with bonus } ]
    ) [] v in
    def (voie_to_jsoo v)

and phase_suivante app =
  match page_of_jsoo app##.page with
  | Creation { perso; phase; label } ->
    begin match phase with
      | Profil {choix=None; _ } ->
        alert app "profil non choisi"
      | Profil {choix=Some (famille, profil); _ } ->
        let peuples = List.map snd peuple_assoc in
        let phase = Peuple { peuples; choix=None } in
        let perso = { perso with famille; profil } in
        let p = Creation { perso; phase; label } in
        ajout_personnage ~creation:phase app label perso @@ fun app _ ->
        route app (page_to_jsoo p)
      | Peuple {choix=None; _ } ->
        alert app "peuple non choisi"
      | Peuple {choix=Some peuple; _ } ->
        let choix = caracteristiques_par_defaut (Some peuple) in
        let bonuses = bonuses_peuple peuple choix in
        let phase = Caracteristiques { bonuses; choix; choix_bonus=None } in
        let perso = { perso with peuple } in
        edition_personnage ~creation:phase app label perso;
        let p = Creation { perso; phase; label } in
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
          let phase = match possibilites with
            | [] ->
              let voies = List.map (fun v -> v, [1; 2; 3; 4; 5]) @@
                voies_peuple perso.peuple @ voies_profil perso.profil in
              let _, _, _, _, _, _, pc = rangs_et_points ~capacites:[] perso in
              Voies { voies; choix=[]; points_de_maitrise=0, pc; bonuses_optionnels=[] }
            | _ ->
              let choix = List.map List.hd possibilites in
              Equipements { possibilites; choix } in
          edition_personnage ~creation:phase app label perso;
          let p = Creation { perso; phase; label } in
          route app (page_to_jsoo p)
        else alert app "caracteristiques non valables"
      | Equipements { choix; _ } ->
        let equipements = perso.equipements @ choix in
        let voies = List.map (fun v -> v, [1; 2; 3; 4; 5]) @@
          voies_peuple perso.peuple @ voies_profil perso.profil in
        let perso = { perso with equipements } in
        let _, _, _, _, pn, _, _ = rangs_et_points ~capacites:[] perso in
        let phase = Voies { voies; choix=[]; points_de_maitrise=0, pn; bonuses_optionnels=[] } in
        edition_personnage ~creation:phase app label perso;
        let p = Creation { perso; phase; label } in
        route app (page_to_jsoo p)
      | Voies { choix=l; bonuses_optionnels; _ } ->
        let l, capacites = capacites l in
        wrap app (verifie_voies ~capacites { perso with voies=l }) @@ fun _ ->
        let perso = { perso with voies=l } in
        let bonus_voies = bonus_capacites @@ List.filter_map (fun (v, rg) -> Option.map (fun v -> v, rg) @@ List.assoc_opt v !voies) l in
        let def_equipement, agi_max = List.fold_left (fun (def, agi) (e, _) ->
          match List.assoc_opt e !equipements with
          | Some Armure { defense; agilite_max; _ } ->
            def + defense, Option.fold ~none:agi ~some:(fun a -> min a agi) agilite_max
          | _ -> def, agi) (0, 8) perso.equipements in
        let perso = { perso with bonuses = perso.bonuses @ bonus_voies @ bonuses_optionnels } in
        let perso = remplit_caracteristiques perso def_equipement agi_max in
        wrap app (competences_maitrisees ~validation:false perso) @@ fun maitrisees ->
        let competences = List.filter (fun (_, n) -> n <> 0) maitrisees in
        let possibilites_maitrise = List.map snd competence_assoc in
        let possibilites = List.filter (fun c ->
          Option.is_none (List.assoc_opt c competences)
        ) possibilites_maitrise in
        wrap app (points_de_competences perso) @@ fun (points_niveau, points_capacites, _, _) ->
        let phase = Competences {
          maitrisees; choix=None; competences; ajout_competence=None;
          possibilites; points=0, (points_niveau+points_capacites);
          possibilites_maitrise;
        } in
        edition_personnage ~creation:phase app label perso;
        let p = Creation { perso; phase; label } in
        route app (page_to_jsoo p)
      | Competences { choix; competences; _ }  ->
        wrap app (competences_maitrisees ?choix perso) @@ fun competences_maitrisees ->
        let perso = { perso with competences_maitrisees; competences } in
        wrap app (points_de_competences perso) @@
        fun (points_niveau, points_capacites, points_utilises, points_maitrise_utilises) ->
        if points_capacites > points_maitrise_utilises then
          alert app (Format.sprintf "au moins %d points de compétence doivent être utilisés par des compétences du profil" points_capacites)
        else if points_niveau + points_capacites > points_utilises then
          alert app "points de compétences non utilisées"
        else if points_niveau + points_capacites < points_utilises then
          alert app "trop de points de compétences"
        else
        let ideaux = "" :: (List.map fst ideal_assoc) in
        let travers_options = "" :: (List.map fst travers_assoc) in
        let phase = Enregistrement {
          ideaux; travers_options; nom=""; label=""; ideal=""; travers="";
          description=""; image=None; image_url=None } in
        edition_personnage ~creation:phase app label perso;
        let p = Creation { perso; phase; label } in
        route app (page_to_jsoo p)
      | Enregistrement {nom; label=lbl; ideal; travers; description; image; _} ->
        if nom = "" then alert app "nom vide" else
        let ideal = List.assoc_opt ideal ideal_assoc in
        let travers = List.assoc_opt travers travers_assoc in
        let perso = { perso with nom; ideal; travers; description; image } in
        let lbl = if lbl = "" then Format.sprintf "%s_%d" nom perso.niveau else lbl in
        ajout_personnage app lbl perso @@ fun app {perso; _} ->
        suppression_personnage app label;
        let p = Personnage {label=lbl; perso} in
        route app (page_to_jsoo p)
    end
  | _ -> init app

and capacite_choisie app vt rg =
  match page_of_jsoo app##.page with
  | Creation { phase; _ } ->
    begin match phase with
      | Voies { choix; _ } ->
        List.exists (fun (c, rgs) -> c = voie_type_of_jsoo vt && List.mem rg rgs) choix
      | _ -> false
    end
  | Edition { choix={voies; _}; _ } ->
    List.exists (fun (c, rgs) -> c = voie_type_of_jsoo vt && List.mem rg rgs) voies
  | _ -> false

and choisit_capacite app vt rg =
  let change_capacites voies =
    let vt = voie_type_of_jsoo vt in
    let rec aux modified acc = function
      | [] when not modified -> List.rev ((vt, List.init rg (fun i -> i+1)) :: acc)
      | [] -> List.rev acc
      | (vt0, rgs0) :: tl ->
        let rg0 = rang_max rgs0 in
        let acc, modified =
          if vt <> vt0 then (vt0, rgs0) :: acc, modified
          else if rg0 < rg then (vt, rgs0 @ List.init (rg - rg0) (fun i -> i+rg0+1)) :: acc, true
          else if rg0 = rg && List.length rgs0 = 1 then acc, true
          else if rg0 = rg then (vt, List.filter (fun i -> i <> rg0) rgs0) :: acc, true
          else (vt, List.filter (fun i -> i <= rg) rgs0) :: acc, true in
        aux modified acc tl in
    aux false [] voies in
  let rafraichit_voies x peuple profil famille l1 =
    let rg_mage = rang_max @@ Option.value ~default:[] @@ List.assoc_opt `Mage l1 in
    chargement_voies (List.map fst l1) @@ fun l ->
    let l1 = List.map (fun (vt, v) -> vt, v, List.assoc vt l1) l in
    let voies =
      List.map (fun x -> x, if rg_mage >= 1 then [1] else [1; 2; 3; 4; 5]) (voies_peuple peuple) @
      List.map (fun x -> x, [1; 2; 3; 4; 5]) (voies_profil profil) @
      (voies_capacites ~famille l1) in
    chargement_voies (List.map fst voies) @@ fun _ ->
    x##.voies := of_listf voie_et_rangs_to_jsoo voies in
  match page_of_jsoo app##.page with
  | Creation { phase; perso; _ } ->
    begin match phase with
      | Voies { choix; _ } ->
        let voies = change_capacites choix in
        let voies, capacites = capacites voies in
        begin match verifie_voies ~validate:false ~capacites { perso with voies } with
          | Error e -> alert app e
          | Ok (pc, pn) ->
            (Unsafe.coerce app)##.page##.creation##.phase##.voies##.choix := of_listf voie_et_rangs_to_jsoo voies;
            (Unsafe.coerce app)##.page##.creation##.phase##.voies##.points_de_maitrise_ := array [|pc; pn|];
            rafraichit_voies (Unsafe.coerce app)##.page##.creation##.phase##.voies perso.peuple perso.profil perso.famille voies
        end
      | _ -> ()
    end
  | Edition { choix={voies; niveau; _}; perso; _} ->
    let voies = change_capacites voies in
    let voies, capacites = capacites voies in
    begin match verifie_voies ~validate:false ~capacites { perso with niveau; voies } with
      | Error e -> alert app e
      | Ok (pc, pn) ->
        (Unsafe.coerce app)##.page##.edition##.choix##.voies := of_listf voie_et_rangs_to_jsoo voies;
        (Unsafe.coerce app)##.page##.edition##.points_de_maitrise_ := array [|pc; pn|];
        rafraichit_voies (Unsafe.coerce app)##.page##.edition perso.peuple perso.profil perso.famille voies
    end
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

and sauvegarde_raw_personnage app label i =
  let elt = Dom_html.getElementById (Format.sprintf "backup-textarea-%d" i) in
  edition_raw_personnage app label (_JSON##parse (Unsafe.coerce elt)##.value);
  init app

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

and lance_de_recuperation app points = match page_of_jsoo app##.page, to_optdef de_of_jsoo points##.de with
  | Personnage {label; perso}, Some de ->
    points##.resultat := undefined;
    lance_de "des-recuperation" (de_str perso.niveau de) 1 @@ fun r ->
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
  let des = { de=de_of_jsoo de; bonus; extra=0; nombre; titre=to_optdef to_string titre; resultat=None; choix=[] } in
  app##.des := def (des_to_jsoo des);
  let cs : _ constr = Unsafe.global##.bootstrap##._Modal in
  let md = new%js cs (string "#des-modal") in
  ignore md##show

and charge_modal_des_competence app de (choix: caracteristique_et_point list) nombre (titre: string option) =
  let bonus, choix = match choix with
    | [ _, i ] -> i, []
    | _ -> 0, choix in
  let des = { de=de_of_jsoo de; bonus; extra=0; nombre; titre; resultat=None; choix } in
  app##.des := def (des_to_jsoo des);
  let cs : _ constr = Unsafe.global##.bootstrap##._Modal in
  let md = new%js cs (string "#des-modal") in
  ignore md##show

and lance_de app des =
  let niveau = match page_of_jsoo app##.page with
    | Personnage { perso={niveau; _}; _ } -> niveau
    | _ -> 0 in
  des##.resultat := undefined;
  let d = des_of_jsoo des in
  let de = de_str niveau d.de in
  lance_de "des-container" de d.nombre @@ fun r ->
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
    let ideal = if e.choix.ideal="" then None else Some (ideal_of_str e.choix.ideal) in
    let travers = if e.choix.travers="" then None else Some (travers_of_str e.choix.travers) in
    let perso = {
      e.perso with
      niveau = e.choix.niveau; equipements=e.choix.equipements;
      caracteristiques_base=e.choix.caracteristiques;
      bonuses=e.choix.bonuses; image=e.choix.image; description=e.choix.description;
      nom = e.choix.nom; ideal; travers } in
    let l, capacites = capacites e.choix.voies in
    begin match verifie_voies ~capacites { perso with voies=l } with
      | Ok _ ->
        let perso = { perso with voies=e.choix.voies } in
        let bonus_voies = bonus_capacites @@ List.filter_map (fun (v, rg) -> Option.map (fun v -> v, rg) @@ List.assoc_opt v !voies) l in
        let bonuses = List.fold_left (fun acc (n, b) ->
          match List.find_opt (fun (n2, _) -> n = n2) acc with
          | None -> acc @ [ n, b ]
          | Some _ -> acc
        ) perso.bonuses bonus_voies in
        let perso = { perso with bonuses } in
        let def_equipement, agi_max = List.fold_left (fun (def, agi) (e, _) ->
          match List.assoc_opt e !equipements with
          | Some Armure { defense; agilite_max; _ } ->
            def + defense, Option.fold ~none:agi ~some:(fun a -> min a agi) agilite_max
          | _ -> def, agi) (0, 8) perso.equipements in
        let perso = remplit_caracteristiques perso def_equipement agi_max in
        let perso = { perso with competences=e.choix.competences } in
        begin match points_de_competences perso with
          | Error e -> alert app e
          | Ok (points_niveau, points_capacites, points_utilises, points_maitrise_utilises) ->
            if points_niveau + points_capacites > points_utilises then
              alert app "points de compétences non utilisées"
            else if points_capacites > points_maitrise_utilises then
              alert app (Format.sprintf "au moins %d points de compétence doivent être utilisés par des compétences du profil" points_capacites)
            else if points_niveau + points_capacites < points_utilises then
              alert app "trop de points de compétences"
            else (
              edition_personnage app e.label perso;
              let p = Personnage {label=e.label; perso} in
              route app (page_to_jsoo p)
            )
        end
      | Error e -> alert app e
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
    (Unsafe.coerce app)##.page##.edition##.choix##.bonuses := of_listf bonus_avec_nom_to_jsoo bonuses
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

and [@noconv] charge_image app (ev: Dom_html.inputElement Dom.event t) =
  let aux ~nom ~label target f =
    let fichier = List.hd @@ Dom.list_of_nodeList target##.files in
    ouverture_image fichier @@ fun a ->
    let nom = if nom = "" then label else nom in
    let ext = Filename.extension (to_string fichier##.name) in
    let nom_fichier = String.lowercase_ascii (nom ^ ext) in
    sauvergarde_fichier_persistent nom_fichier a f in
  match Opt.to_option ev##.target, page_of_jsoo app##.page with
  | Some target, Creation { label; phase=Enregistrement {nom; _}; _ } ->
    aux ~nom ~label target @@ fun nom_fichier url ->
    (Unsafe.coerce app)##.page##.creation##.phase##.enregistrement##.image_url_ := def url;
    (Unsafe.coerce app)##.page##.creation##.phase##.enregistrement##.image := def (string nom_fichier)
  | Some target, Edition { label; perso={ nom; _ }; _ } ->
    aux ~nom ~label target @@ fun nom_fichier url ->
    (Unsafe.coerce app)##.page##.edition##.choix##.image_url_ := def url;
    (Unsafe.coerce app)##.page##.edition##.choix##.image := def (string nom_fichier)
  | _ -> ()

and [@noconv] choisit_bonus_capacite app (ev: Dom_html.inputElement Dom.event t) nom b =
  let aux target bonuses =
    let checked = to_bool target##.checked in
    let nom = to_string nom in
    let b = bonus_of_jsoo b in
    if checked then
      let b = { b with opt=Some (Some true) } in
      checked, bonuses @ [ nom, b ]
    else checked, List.filter (fun (n, b2) -> not (n = nom && b.id = b2.id)) bonuses in
  match Opt.to_option ev##.target, page_of_jsoo app##.page with
  | Some target, Creation {phase=Voies {bonuses_optionnels; _}; _} ->
    let _checked, bonuses = aux target bonuses_optionnels in
    (Unsafe.coerce app##.page)##.creation##.phase##.voies##.bonuses_optionnels_ := of_listf bonus_avec_nom_to_jsoo bonuses
  | Some target, Edition {perso; choix={competences; niveau; bonuses; _}; _} ->
    let checked, bonuses = aux target bonuses in
    wrap app (points_de_competences {perso with competences; niveau; bonuses}) @@
    fun (points_niveau, points_capacites, points_utilises, points_maitrise_utilises) ->
    if points_capacites > points_maitrise_utilises then (
      alert app (Format.sprintf "au moins %d points de compétence doivent être utilisés par des compétences du profil" points_capacites);
      target##.checked := bool (not checked))
    else if points_niveau + points_capacites < points_utilises then (
      alert app "trop de points de compétences";
      target##.checked := bool (not checked))
    else (
      (Unsafe.coerce app##.page)##.edition##.choix##.bonuses := of_listf bonus_avec_nom_to_jsoo bonuses;
      (Unsafe.coerce app##.page)##.edition##.points_de_competences_ :=
        array [| points_utilises; points_niveau + points_capacites |]
    )
  | Some target, _ ->
    let checked = to_bool target##.checked in
    alert app "cette fonction n'est pas accessible sur cette page";
    target##.checked := bool (not checked)
  | _ -> ()

and change_niveau app niveau = match page_of_jsoo app##.page with
  | Edition { choix={voies; competences; _}; perso; _} ->
    let voies, capacites = capacites voies in
    let perso = { perso with voies; competences; niveau } in
    let _, _, _, _, pnv, pb, pc = rangs_et_points ~capacites perso in
    wrap app (points_de_competences perso) @@ fun (points_niveau, points_capacites, points_utilises, _) ->
    if pc > pnv+pb then alert app "trop de capacités pour baisser le niveau"
    else if points_niveau + points_capacites < points_utilises then alert app "trop de points de compétences" else (
      (Unsafe.coerce app)##.page##.edition##.choix##.niveau := niveau;
      (Unsafe.coerce app)##.page##.edition##.points_de_maitrise_ := array [|pc; pnv+pb|];
      (Unsafe.coerce app##.page)##.edition##.points_de_competences_ :=
        array [| points_utilises; points_niveau + points_capacites |])
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"

and rang_max _app rgs =
  let l = to_list rgs in
  rang_max l

and copie_lien_personnage _app (p: personnage) =
  let b64 = personnage_to_b64 { p with image = None } in
  let origin = to_string Dom_html.window##.location##.origin in
  let pathname = to_string Dom_html.window##.location##.pathname in
  let pathname = match List.rev @@ String.split_on_char '/' pathname with
    | "index.html" :: tl -> "/" ^ String.concat "/" tl
    | _ -> pathname in
  let s = Format.sprintf "%s%s?perso=%s" origin pathname b64 in
  try
    Promise.jthen ((Unsafe.coerce Dom_html.window##.navigator)##share (object%js val url = string s val title = string "CO" end)) Fun.id
  with _ ->
    Promise.jthen ((Unsafe.coerce Dom_html.window##.navigator)##.clipboard##writeText (string s)) Fun.id

and wavy_cadre _app =
  string @@ Format.sprintf "clip-path:%s" @@
  Wavy.cadre [ `haut, (7, 15); `droite, (5, 10); `bas, (7, 15); `gauche, (5, 10) ]

and wavy_haut _app =
  string @@ Format.sprintf "clip-path:%s" @@ Wavy.cadre [ `haut, (4, 50) ]

and wavy_bas _app =
  string @@ Format.sprintf "clip-path:%s" @@ Wavy.cadre [ `bas, (4, 50) ]

and pp_competence _app (c: competence) =
  let s = match c with
    | `athletisme -> "athlétisme"
    | `discretion -> "discrétion"
    | `equitation -> "Équitation"
    | `medecine -> "médecine"
    | _ -> competence_to_str c in
  string (String.capitalize_ascii s)

and ajoute_competence app (arg: competence option) =
  let aux (o, competences, c) =
    let competences = competences @ [ c, 0 ] in
    o##.competences := of_listf competence_et_point_to_jsoo competences;
    o##.ajout_competence_ := undefined in
  let r = match page_of_jsoo app##.page with
    | Creation { phase=Competences {ajout_competence=None; _}; _ }
    | Edition { choix={ajout_competence=None; _}; _ } when Option.is_none arg ->
      Error "pas de compétence choisie"
    | Creation { phase=Competences { competences; ajout_competence; choix; possibilites;_ }; perso; _ } ->
      let a = match ajout_competence, arg with None, None -> assert false | _, Some a | Some a, _ -> a in
      begin match competences_maitrisees ?choix perso with
        | Ok _ ->
          (Unsafe.coerce app##.page)##.creation##.phase##.competences##.possibilites :=
            of_listf competence_to_jsoo (List.filter (fun c -> a <> c) possibilites);
          Ok ((Unsafe.coerce app##.page)##.creation##.phase##.competences, competences, a)
        | Error e ->
          (Unsafe.coerce app##.page)##.creation##.phase##.competences##.ajout_competence_ := undefined;
          Error e
      end
    | Edition { choix={competences; ajout_competence; _}; competences=l; _ } ->
      let a = match ajout_competence, arg with None, None -> assert false | _, Some a | Some a, _ -> a in
      (Unsafe.coerce app##.page)##.edition##.competences :=
        of_listf competence_to_jsoo (List.filter (fun c -> a <> c) l);
      Ok ((Unsafe.coerce app##.page)##.edition##.choix, competences, a)
    | _ -> Error "cette fonction n'est pas accessible sur cette page" in
  wrap app r aux

and ajoute_competence_maitrisee app =
  match page_of_jsoo app##.page with
  | Creation { phase=Competences { choix; _ }; perso; _ } ->
    wrap app (competences_maitrisees ?choix perso) @@ fun competences_maitrisees ->
    let competences = List.filter (fun (_, n) -> n <> 0) competences_maitrisees in
    let perso = { perso with competences_maitrisees; competences } in
    wrap app (points_de_competences perso) @@ fun (points_niveau, points_capacites, points_utilises, _) ->
    if points_niveau + points_capacites < points_utilises then
      alert app "trop de points de compétences"
    else (
      (Unsafe.coerce app##.page)##.creation##.phase##.competences##.competences :=
        of_listf competence_et_point_to_jsoo competences;
      (Unsafe.coerce app##.page)##.creation##.phase##.competences##.competences_maitrisees_ :=
        of_listf competence_et_point_to_jsoo competences_maitrisees;
      (Unsafe.coerce app##.page)##.creation##.phase##.competences##.points :=
        array [| points_utilises; points_niveau + points_capacites |]
    )
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"

and change_competence app (c: competence) i =
  match page_of_jsoo app##.page with
  | Creation { phase=Competences { competences; maitrisees; _ }; perso; _ } ->
    let competences = List.map (fun (c2, n) -> if c = c2 then c, i else c2, n) competences in
    let perso = { perso with competences_maitrisees=maitrisees; competences } in
    wrap app (points_de_competences perso) @@ fun (points_niveau, points_capacites, points_utilises, _) ->
    if points_niveau + points_capacites < points_utilises then
      alert app "trop de points de compétences"
    else (
      (Unsafe.coerce app##.page)##.creation##.phase##.competences##.competences :=
        of_listf competence_et_point_to_jsoo competences;
      (Unsafe.coerce app##.page)##.creation##.phase##.competences##.points :=
        array [| points_utilises; points_niveau + points_capacites |]
    )
  | Edition { choix = { competences; niveau; voies; _ }; perso; _ } ->
    let voies, _ = capacites voies in
    let competences = List.map (fun (c2, n) -> if c = c2 then c, i else c2, n) competences in
    let perso = { perso with voies; competences; niveau } in
    wrap app (points_de_competences perso) @@ fun (points_niveau, points_capacites, points_utilises, _) ->
    if points_niveau + points_capacites < points_utilises then
      alert app "trop de points de compétences"
    else (
      (Unsafe.coerce app##.page)##.edition##.choix##.competences :=
        of_listf competence_et_point_to_jsoo competences;
      (Unsafe.coerce app##.page)##.edition##.points_de_competences_ :=
        array [| points_utilises; points_niveau + points_capacites |]
    )
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"

and supprime_competence app (c: competence) =
  match page_of_jsoo app##.page with
  | Creation { phase=Competences { competences; choix; possibilites; _ }; perso; _ } ->
    let competences = List.remove_assoc c competences in
    wrap app (competences_maitrisees ?choix perso) @@ fun competences_maitrisees ->
    let perso = { perso with competences_maitrisees; competences } in
    wrap app (points_de_competences perso) @@ fun (points_niveau, points_capacites, points_utilises, _) ->
    (Unsafe.coerce app##.page)##.creation##.phase##.competences##.competences :=
      of_listf competence_et_point_to_jsoo competences;
    (Unsafe.coerce app##.page)##.creation##.phase##.competences##.points :=
      array [| points_utilises; points_niveau + points_capacites |];
    (Unsafe.coerce app##.page)##.creation##.phase##.competences##.possibilites :=
      of_listf competence_to_jsoo (possibilites @ [ c ])

  | Edition { choix = { competences; _ }; perso; competences=possibilites; _ } ->
    let competences = List.remove_assoc c competences in
    let perso = { perso with competences } in
    wrap app (points_de_competences perso) @@ fun (points_niveau, points_capacites, points_utilises, _) ->
    (Unsafe.coerce app##.page)##.edition##.choix##.competences :=
      of_listf competence_et_point_to_jsoo competences;
    (Unsafe.coerce app##.page)##.edition##.points_de_competences_ :=
      array [| points_utilises; points_niveau + points_capacites |];
    (Unsafe.coerce app##.page)##.edition##.competences :=
      of_listf competence_to_jsoo (possibilites @ [ c ])
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"

and competence_caracteristiques _app (c: competence) =
  let l = competence_caracteristiques c in
  of_listf caracteristique_to_jsoo l

and bonus_competence app c =
  match page_of_jsoo app##.page with
  | Personnage { perso; _ } ->
    let c, n = competence_et_point_of_jsoo c in
    let l = competence_caracteristiques c in
    let v = List.map (fun c -> c, valeur_caracteristique perso.caracteristiques c) l in
    def (of_listf (fun (c, v) -> caracteristique_et_point_to_jsoo (c, n + v)) v)
  | _ -> alert app "cette fonction n'est pas accessible sur cette page"; undefined

and choisit_bonus_des app b =
  (Unsafe.coerce app)##.des##.bonus := b;
  (Unsafe.coerce app)##.des##.choix := array [||]


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
  ouvre_db @@ fun db ->
  let%data db : Ezjs_idb.Types.iDBDatabase t = db [@@noconv] in
  let app = [%app {conv; mount; unhide; export}] in
  Dom_html.window##.onpopstate := Dom_html.handler (fun (e : Dom_html.popStateEvent t) ->
    (try route app (Unsafe.coerce e##.state) with _exn -> init app); _false);
  (Unsafe.coerce Dom_html.window)##.onfocus := Dom_html.handler (fun (_e : Dom_html.popStateEvent t) ->
    let now = Int32.to_int (to_int32 date##now) / 1000 in
    if now > app##.tsp + 3600 then route app app##.page;
    _false);
  let perso_param =
    let search = to_string Dom_html.window##.location##.search in
    if search  = "" then None else
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
      route app ~path:"/" (page_to_jsoo (Personnage {label; perso}))
    | _, Some p ->
      begin match page_of_jsoo p with
        | Chargement -> init app
        | _ -> route app p
      end
    | _ -> init app
  with exn ->
    log "initialisation erreur: %s" (Printexc.to_string exn);
    backup app
