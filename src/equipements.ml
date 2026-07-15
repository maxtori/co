open Ezjs_min
open Co

let%file _ = "./equipements.html"

let%prop p : personnage = {req}

let%data possibilites: equipement_avec_nombre list list = []
and choix: equipement_avec_nombre list = []
and equipements: equipement_avec_nom list = []
and ajout: equipement_avec_nombre = `autre "", Some 1
and ajout_custom: equipement_avec_nom option = None
and tous: equipement_nom list = List.map snd equipement_nom_assoc

let equipements : (equipement_nom * equipement) list ref = ref []
let equipement_promesses : (string, (equipement -> unit) list) Hashtbl.t = Hashtbl.create 10

let charge_equipement (e, n) f =
  let aux e n = match e, n with
    | Arme a, (Some _ as nombre) ->
      let arme = List.map (function Distance {portee; _} -> Distance {portee; nombre} | a -> a) a.arme in
      Arme { a with arme }
    | _ -> e in
  match List.assoc_opt e !equipements with
  | Some e -> f (aux e n)
  | None ->
    match e with
    | `autre _ ->
      let equipement = Autre { description=""; prix=None } in
      equipements := (e, equipement) :: !equipements;
      f equipement
    | _ ->
      let e_str = equipement_to_str e in
      let url = Format.sprintf "data/%s.json" e_str in
      match Hashtbl.find_opt equipement_promesses e_str with
      | Some l -> Hashtbl.replace equipement_promesses e_str (f :: l)
      | None ->
        Format.printf "chargement equipement %s@." e_str;
        Hashtbl.add equipement_promesses e_str [ f ];
        let@ s = Common.charge_fichier url in
        let equipement = EzEncoding.destruct equipement_enc s in
        equipements := (e, equipement) :: !equipements;
        Option.iter (fun l ->
          List.iter (fun f -> f (aux equipement n)) @@ List.rev l;
          Hashtbl.remove equipement_promesses e_str) @@
        Hashtbl.find_opt equipement_promesses e_str

let charge_equipements l f =
  let@ acc = Common.fold (fun acc x cb ->
    match x with
    | `custom (en, eq) -> cb ((`autre en, eq) :: acc)
    | `connu (e, n) ->
      charge_equipement (e, n) @@ fun eq -> cb ((e, eq) :: acc)) [] l in
  f (List.rev acc)

let defense_agilite equipements f =
  Common.fold (fun (def, agi) e cb ->
    let f = function
      | Armure { defense; agilite_max; _ } ->
        cb (def + defense, Option.fold ~none:agi ~some:(fun a -> min a agi) agilite_max)
      | _ -> cb (def, agi) in
    match e with `custom (_, e) -> f e | `connu (e, n) -> charge_equipement (e, n) f
  ) (0, 8) equipements f

let prepare app p =
  match p.equipements with
  | [] ->
    let l = equipements_profil p.profil in
    let leq, possibilites = List.partition (function [ _ ] -> true | _ -> false) l in
    let choix = List.map List.hd possibilites in
    let leq = List.map (fun l -> `connu (List.hd l)) leq in
    let@ equipements = charge_equipements leq in
    app##.possibilites := of_listf (of_listf equipement_avec_nombre_to_jsoo) possibilites;
    app##.choix := of_listf equipement_avec_nombre_to_jsoo choix;
    app##.equipements := of_listf equipement_avec_nom_to_jsoo equipements
  | l ->
    let@ equipements = charge_equipements l in
    app##.equipements := of_listf equipement_avec_nom_to_jsoo equipements

let%meth pp_equipement _app e =
  let s = equipement_to_str (equipement_nom_of_jsoo e) in
  string @@ String.capitalize_ascii @@ String.map (function '_' -> ' ' | c -> c) s

and retire_equipement app e =
  let e = equipement_nom_of_jsoo e in
  let equipements = to_listf equipement_avec_nom_of_jsoo app##.equipements in
  let equipements = List.filter (function (en, _) -> en <> e) equipements in
  app##.equipements := of_listf equipement_avec_nom_to_jsoo equipements

and ajoute_equipement app =
  let e, nb = equipement_avec_nombre_of_jsoo app##.ajout in
  let ajout_custom = to_optdef equipement_avec_nom_of_jsoo app##.ajout_custom_ in
  let equipements = to_listf equipement_avec_nom_of_jsoo app##.equipements in
  let aux cb = match e, ajout_custom with
    | `autre ("arme_contact_custom" | "arme_distance_custom" | "armure_custom" | "autre_custom"), Some (en, eq) ->
      let eq = match eq with
        | Autre { description; prix } -> Autre { description; prix=if prix = Some "" then None else prix }
        | Armure { defense; agilite_max; prix; notes } ->
          let prix = if prix = Some "" then None else prix in
          let notes = if prix = Some "" then None else notes in
          Armure { defense; agilite_max; prix; notes }
        | Arme { arme; dommage; prix; typ; notes } ->
          let prix = if prix = Some "" then None else prix in
          let notes = if prix = Some "" then None else notes in
          Arme { arme; dommage; prix; typ; notes } in
      cb (en, eq)
    | _ ->
      let@ eq = charge_equipement (e, nb) in
      cb (e, eq) in
  let@ equipement = aux in
  let equipements = equipements @ [ equipement ] in
  app##.equipements := of_listf equipement_avec_nom_to_jsoo equipements;
  (Unsafe.coerce app)##.ajout :=
    array [| Unsafe.inject (equipement_nom_to_jsoo (`autre "")); Unsafe.inject 1 |];
  app##.ajout_custom_ := undefined

and [@noconv] charge_custom_equipement app (ev: Dom_html.inputElement Dom.event t) =
  match Opt.to_option ev##.target with
  | None -> ()
  | Some target ->
    let kind = to_string target##.value in
    match kind with
    | "arme_contact_custom" -> (Unsafe.coerce app)##.ajout_custom :=
        def @@ array [| Unsafe.inject (string ""); Unsafe.inject @@ equipement_to_jsoo (Arme {
          arme = [Contact {deux_mains=None}]; dommage=(1, `d6);
          prix=None; typ=`contondants; notes=None }) |]
    | "arme_distance_custom" -> (Unsafe.coerce app)##.ajout_custom_ :=
        def @@ array [| Unsafe.inject (string ""); Unsafe.inject @@ equipement_to_jsoo (Arme {
          arme = [Distance {portee=20; nombre=None}]; dommage=(1, `d6);
          prix=None; typ=`perforants; notes=None }) |]
    | "armure_custom" -> (Unsafe.coerce app)##.ajout_custom_ :=
        def @@ array [| Unsafe.inject (string ""); Unsafe.inject @@ equipement_to_jsoo (Armure {
          defense=1; agilite_max=None; prix=None; notes=None }) |]
    | "autre_custom" -> (Unsafe.coerce app)##.ajout_custom_ :=
        def @@ array [|Unsafe.inject (string ""); Unsafe.inject @@ equipement_to_jsoo (Autre { description=""; prix=None }) |]
    | _ -> (Unsafe.coerce app)##.ajout_custom_ := undefined

and [@noconv] maj app : personnage_jsoo t Promise.promise t =
  let p = personnage_of_jsoo app##.p in
  let equipements = to_listf equipement_avec_nom_of_jsoo app##.equipements in
  let choix = to_listf equipement_avec_nombre_of_jsoo app##.choix in
  let equipements = List.map (fun (en, eq) -> match en with
    | `autre s -> `custom (s, eq)
    | _ -> match eq with
      | Arme a ->
        let nombre = List.find_map (function Distance {nombre; _} -> nombre | _ -> None) a.arme in
        `connu (en, nombre)
      | _ -> `connu (en, None)) equipements in
  let equipements = equipements @ List.map (fun (e, n) -> `connu (e, n)) choix in
  let f resolve _reject =
    let@ defense_equipement, agilite_max = defense_agilite equipements in
    resolve (personnage_to_jsoo { p with equipements; defense_equipement; agilite_max }) in
  Promise.promise f

let%watch p app (perso: personnage) _old = prepare app perso

[%%created fun app -> prepare app (personnage_of_jsoo app##.p)]

[%%comp {name="equipements"; conv}]
