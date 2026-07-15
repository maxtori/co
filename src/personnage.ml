open Co
open Ezjs_min
open Common

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

type liste_equipements = (string * equipement_avec_nom list) list
[@@@jsoo
  class type liste_equipements_jsoo = [liste_equipements_jsoo Ezjs_min.t] Ezjs_min.Table.ct
  let liste_equipements_to_jsoo l : liste_equipements_jsoo Ezjs_min.t =
    Ezjs_min.Table.makef (of_listf equipement_avec_nom_to_jsoo) l
  let liste_equipements_of_jsoo t =
    Ezjs_min.Table.itemsf (to_listf equipement_avec_nom_of_jsoo) t
]

type pieces = {
  metal: piece;
  titre: string;
  quantite: int;
} [@@deriving jsoo]

let genre_de_titre = function
  | `points_de_vigueur -> "Points de vigeur"
  | `des_de_recuperation -> "Dés de récupération"
  | `points_de_chance -> "Points de chance"
  | `points_de_mana -> "Points de mana"

let piece_titre = function
  | `pp -> "Pièces de platine"
  | `po -> "Pièces d'or"
  | `pa -> "Pièces d'argent"
  | `pc -> "Pièces de cuivre"

let%file _ = "./personnage.html"
let%prop p : personnage = { req }
and lbl : string = { req }

let%data points : points option = None
and image : string option = None
and equipements : liste_equipements = ["arme", []; "armure", []; "autre", []]
and voies : voies = []
and hide_modal = "hide.bs.modal"
and pieces : pieces option = None

let prepare app =
  let p = personnage_of_jsoo app##.p in
  let@ equipements = Equipements.charge_equipements p.equipements in
  let armes, armures, autre = List.fold_left (fun (armes, armures, autre) (en, e) ->
    match e with
    | Arme _ -> (en, e) :: armes, armures, autre
    | Armure _ -> armes, (en, e) :: armures, autre
    | Autre _ -> armes, armures, (en, e) :: autre) ([], [], []) equipements in
  Table.add app##.equipements "arme" (of_listf equipement_avec_nom_to_jsoo (List.rev armes));
  Table.add app##.equipements "armure" (of_listf equipement_avec_nom_to_jsoo (List.rev armures));
  Table.add app##.equipements "autre" (of_listf equipement_avec_nom_to_jsoo (List.rev autre));
  let@ voies = Voies.charge_voies ~caracteristiques:p.caracteristiques (List.map fst p.voies) in
  let l = List.filter_map (fun (vt, v) -> match List.assoc_opt vt p.voies with
    | None -> None
    | Some rangs -> Some (vt, Voies.voie_avec_bonus ~bonuses:p.bonuses ~rangs v)) voies in
  app##.voies := voies_to_jsoo l;
  Option.iter (fun nom_fichier ->
    let st = Unsafe.global##.navigator##.storage in
    let@ dir = Promise.jthen st##getDirectory in
    let@ fh = Promise.jthen (dir##getFileHandle (string nom_fichier)) in
    let@ fi = Promise.jthen fh##getFile in
    let url = Dom_html.window##._URL##createObjectURL fi in
    app##.image := def url
  ) p.image

let%meth wavy_cadre _app (i: int) = wavy_cadre i

and wavy_haut _app i =
  let s = match Hashtbl.find_opt wavy_haut i with
    | None ->
      let s = Wavy.cadre [ `haut, (4, 50) ] in
      Hashtbl.add wavy_haut i s;
      s
    | Some s -> s in
  string @@ Format.sprintf "clip-path:%s" s

and wavy_bas _app i =
  let s = match Hashtbl.find_opt wavy_bas i with
    | None ->
      let s = Wavy.cadre [ `bas, (4, 50) ] in
      Hashtbl.add wavy_bas i s;
      s
    | Some s -> s in
  string @@ Format.sprintf "clip-path:%s" s

and charge_modal_des app (de: de) (bonus: int) (nombre: int) (titre: string option) =
  let des = { de; bonus; extra=0; nombre; titre; resultat=None; choix=[] } in
  [%emit "charge_modal_des" app (des_to_jsoo des)]

and charge_modal_des_competence app de (choix: caracteristique_et_point list) nombre (titre: string option) =
  let bonus, choix = match choix with
    | [ _, i ] -> i, []
    | _ -> 0, choix in
  let des = { de=de_of_jsoo de; bonus; extra=0; nombre; titre; resultat=None; choix } in
  [%emit "charge_modal_des" app (des_to_jsoo des)]

and charge_modal_points app g =
  let p = personnage_of_jsoo app##.p in
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

and charge_modal_pieces app k =
  let p = personnage_of_jsoo app##.p in
  let metal = piece_of_jsoo k in
  let titre = piece_titre metal in
  let quantite = match metal with
    | `pp -> p.bourse.pp
    | `po -> p.bourse.po
    | `pa -> p.bourse.pa
    | `pc -> p.bourse.pc in
  app##.pieces := def (pieces_to_jsoo { titre; metal; quantite });
  let cs : _ constr = Unsafe.global##.bootstrap##._Modal in
  let md = new%js cs (string "#pieces-modal") in
  ignore md##show

and pp_equipement _app e = Equipements.pp_equipement (Unsafe.obj [||]) e
and pp_competence _app c = Competences.pp_competence (Unsafe.obj [||]) c

and bonus_competence app c =
  let perso = personnage_of_jsoo app##.p in
  let c, n = competence_et_point_of_jsoo c in
  let l = competence_caracteristiques c in
  let v = List.map (fun c -> c, valeur_caracteristique perso.caracteristiques c) l in
  of_listf (fun (c, v) -> caracteristique_et_point_to_jsoo (c, n + v)) v

and pp_voie _app vt = Voies.pp_voie (Unsafe.obj [||]) vt

and rang_max _app rgs =
  let l = to_list rgs in
  rang_max l

and lance_de_recuperation app points = match to_optdef de_of_jsoo points##.de with
  | Some de ->
    points##.resultat := undefined;
    let perso = personnage_of_jsoo app##.p in
    let label = to_string app##.lbl in
    let@ r = Common.lance_de "des-recuperation" (de_str perso.niveau de) 1 in
    let p = points_of_jsoo points in
    let points_de_vigueur = { perso.points_de_vigueur with courant = min (perso.points_de_vigueur.courant + r) perso.points_de_vigueur.max } in
    let des_de_recuperation = { p.points with courant = p.points.courant - 1 } in
    let perso = { perso with points_de_vigueur; des_de_recuperation } in
    points##.points##.courant := des_de_recuperation.courant;
    points##.resultat := def r;
    let@ () = edition_personnage label perso in
    [%emit "perso" app (avec_label_et_phase_to_jsoo {perso; label; creation=None})]
  | _ -> ()

and vide_points app =
  match to_optdef points_of_jsoo app##.points with
  | Some p ->
    let perso = personnage_of_jsoo app##.p in
    let label = to_string app##.lbl in
    if p.points.max < p.points.courant then Common.alert app "valeur supérieur au maximum" else
    let perso = match p.genre with
      | `points_de_vigueur -> { perso with points_de_vigueur = p.points }
      | `des_de_recuperation -> { perso with des_de_recuperation = p.points }
      | `points_de_chance -> { perso with points_de_chance = p.points }
      | `points_de_mana -> { perso with points_de_mana = p.points } in
    let@ () = Common.edition_personnage label perso in
    app##.points := undefined;
    [%emit "perso" app (avec_label_et_phase_to_jsoo {perso; label; creation=None})]
  | _ -> ()

and vide_pieces app =
  match to_optdef pieces_of_jsoo app##.pieces with
  | Some p ->
    let perso = personnage_of_jsoo app##.p in
    let label = to_string app##.lbl in
    let bourse = match p.metal with
      | `pp -> { perso.bourse with pp = p.quantite }
      | `po -> { perso.bourse with po = p.quantite }
      | `pa -> { perso.bourse with pa = p.quantite }
      | `pc -> { perso.bourse with pc = p.quantite } in
    let perso = { perso with bourse } in
    let@ () = Common.edition_personnage label perso in
    app##.pieces := undefined;
    [%emit "perso" app (avec_label_et_phase_to_jsoo {perso; label; creation=None})]
  | _ -> ()

[%%created fun app -> prepare app]

[%%comp {name="personnage"; conv}]
