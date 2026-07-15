open Co
open Ezjs_min

type points_de_maitrise = int * int [@@deriving jsoo]

let%file _ = "./voies.html"

let%prop p : personnage = {req}
and edition = false

let%data voies: voie_et_rangs list = []
and choix: voie_et_rangs list = []
and points_de_maitrise: points_de_maitrise = 0, 0
and bonuses: bonus_avec_nom list = []
and liste_voies: voies = []

let voies : (voie_type * voie) list ref = ref []
let voie_promesses : (string, (voie -> unit) list) Hashtbl.t = Hashtbl.create 10

let charge_voie ~(caracteristiques: caracteristiques) v f =
  match List.assoc_opt v !voies with
  | Some v -> f v
  | None ->
    let aux s =
      let voie = EzEncoding.destruct voie_enc s in
      List.map (fun (c: capacite) ->
        let bonus = List.flatten @@ List.map (fun b ->
          let aux = function
            | [ id ] -> [ { b with id } ]
            | l -> List.map (fun id -> { b with id; opt=Some None }) l in
          match b.id with
          | `FAI -> aux @@ plus_faibles_caracteristiques caracteristiques
          | `HAU -> aux @@ plus_hautes_caracteristiques caracteristiques
          | _ -> [ b ]) c.bonus in
        { c with bonus }) voie in
    let v_str = voie_type_to_str v in
    let url = Format.sprintf "data/%s.json" v_str in
    match Hashtbl.find_opt voie_promesses v_str with
    | Some l -> Hashtbl.replace voie_promesses v_str (f :: l)
    | None ->
      Format.printf "chargement voie %s@." v_str;
      Hashtbl.add voie_promesses v_str [ f ];
      let@ s = Common.charge_fichier url in
      let voie = aux s in
      voies := (v, voie) :: !voies;
      Option.iter (fun l ->
        List.iter (fun f -> f voie) @@ List.rev l;
        Hashtbl.remove voie_promesses v_str) @@
      Hashtbl.find_opt voie_promesses v_str

let charge_voies ~caracteristiques l f =
  let rec aux acc = function
    | [] -> f (List.rev acc)
    | vt :: tl -> charge_voie ~caracteristiques vt (fun v -> aux ((vt, v) :: acc) tl) in
  aux [] l

let voies_possibles ?voies (perso: personnage) =
  let rg_mage = rang_max @@ Option.value ~default:[] @@ List.assoc_opt `Mage perso.voies in
  List.map (fun x -> x, if (rg_mage >= 1 && perso.famille = `Mages) then [ 1 ] else [1; 2; 3; 4; 5]) (voies_peuple perso.peuple) @
  List.map (fun x -> x, [1; 2; 3; 4; 5]) (voies_profil perso.profil) @
  voies_prestige ~niveau:perso.niveau perso.famille @
  (match voies with None -> [] | Some l -> voies_capacites ~famille:perso.famille l)

let capacites ~caracteristiques l f =
  Common.fold (fun (acc_voies, acc_c) (vt, rgs) f ->
    let@ v = charge_voie ~caracteristiques vt in
    match List.filter (fun c -> List.mem c.rang rgs) v with
    | [] -> f (acc_voies, acc_c)
    | v -> f (acc_voies @ [ vt, rgs ], acc_c @ v)
  ) ([], []) l f

let voie_avec_bonus ~bonuses ~rangs v =
  List.fold_left (fun acc (c: capacite) ->
    if not (List.mem c.rang rangs) then acc else
    let bonus = List.filter_map (fun b -> match b.opt with
      | None -> None
      | Some _ ->
        if List.exists (fun (n2, b2) -> n2 = c.nom && b.id = b2.id) bonuses then
          Some { b with opt=Some (Some true) }
        else Some { b with opt=Some (Some false) }
    ) c.bonus in
    acc @ [ { c with bonus } ]
  ) [] v

let prepare app p =
  let aux ?(choix=[]) ?(capacites=[]) voies =
    let@ l = charge_voies ~caracteristiques:p.caracteristiques (List.map fst voies) in
    let _, _, _, _, pnv, pb, pc = rangs_et_points ~capacites p in
    app##.voies := of_listf voie_et_rangs_to_jsoo voies;
    app##.choix := of_listf voie_et_rangs_to_jsoo choix;
    let points_de_maitrise = points_de_maitrise_to_jsoo (pc, pnv+pb) in
    app##.points_de_maitrise_ := points_de_maitrise;
    if to_bool app##.edition then [%emit "points_maitrise" app points_de_maitrise];
    app##.bonuses := of_listf bonus_avec_nom_to_jsoo p.bonuses;
    let l = List.filter_map (fun (vt, v) -> match List.assoc_opt vt voies with
      | None -> None
      | Some rangs -> Some (vt, voie_avec_bonus ~bonuses:p.bonuses ~rangs v)) l in
    app##.liste_voies_ := voies_to_jsoo l in
  match p.voies with
  | [] -> aux (voies_possibles p)
  | _ ->
    let@ l = charge_voies ~caracteristiques:p.caracteristiques (List.map fst p.voies) in
    let voies = List.map (fun (vt, v) -> vt, v, List.assoc vt p.voies) l in
    let@ choix, capacites = capacites ~caracteristiques:p.caracteristiques p.voies in
    let voies = voies_possibles ~voies p in
    aux ~choix ~capacites voies

let%meth capacite_choisie app vt rg =
  let choix = to_listf voie_et_rangs_of_jsoo app##.choix in
  List.exists (fun (c, rgs) -> c = voie_type_of_jsoo vt && List.mem rg rgs) choix

and choisit_capacite app vt rg =
  let perso = personnage_of_jsoo app##.p in
  let change_capacites lv bonuses f =
    let vt = voie_type_of_jsoo vt in
    let@ v = charge_voie ~caracteristiques:perso.caracteristiques vt in
    let rec aux modifie acc bonuses bonuses_supprimes = function
      | [] when not modifie -> List.rev ((vt, [ rg ]) :: acc), bonuses, bonuses_supprimes
      | [] -> List.rev acc, bonuses, bonuses_supprimes
      | (vt0, rgs0) :: tl ->
        let acc, modifie, supprime =
          if vt <> vt0 then (vt0, rgs0) :: acc, modifie, false else
          let rgs, supprime =
            if List.mem rg rgs0 then List.filter (fun rg0 -> rg <> rg0) rgs0, true
            else rgs0 @ [ rg ], false in
          (vt, rgs) :: acc, true, supprime in
        if not supprime then aux modifie acc bonuses bonuses_supprimes tl else
        let capacite_supprimee = (List.find (fun c -> c.rang = rg) v) in
        let bonuses = List.filter (fun (n, _) -> not (n = capacite_supprimee.nom)) bonuses in
        let bonuses_supprimes = bonuses_supprimes @ List.map (fun b -> capacite_supprimee.rang, b.id) capacite_supprimee.bonus in
        aux modifie acc bonuses bonuses_supprimes tl in
    f (aux false [] bonuses [] lv) in
  let rafraichit_voies perso =
    let@ l = charge_voies ~caracteristiques:perso.caracteristiques (List.map fst perso.voies) in
    let voies = List.map (fun (vt, v) -> vt, v, List.assoc vt perso.voies) l in
    let voies = voies_possibles ~voies perso in
    let@ _ = charge_voies ~caracteristiques:perso.caracteristiques (List.map fst voies) in
    app##.voies := of_listf voie_et_rangs_to_jsoo voies in
  let choix = to_listf voie_et_rangs_of_jsoo app##.choix in
  let bonuses = to_listf bonus_avec_nom_of_jsoo app##.bonuses in
  let@ voies, bonuses, bonus_supprimes = change_capacites choix bonuses in
  let@ voies, capacites = capacites ~caracteristiques:perso.caracteristiques voies in
  let perso = { perso with voies } in
  match verifie_voies ~validate:false ~capacites perso with
  | Error e -> Common.alert app e
  | Ok (pc, pn) ->
    app##.choix := of_listf voie_et_rangs_to_jsoo voies;
    let points_de_maitrise = points_de_maitrise_to_jsoo (pc, pn) in
    app##.points_de_maitrise_ := points_de_maitrise;
    if to_bool app##.edition then [%emit "points_maitrise" app points_de_maitrise];
    app##.bonuses := of_listf bonus_avec_nom_to_jsoo bonuses;
    List.iter (fun (rg, id) ->
      match Dom_html.getElementById_opt (Format.sprintf "voies-bonus-%d-%s" rg (caracteristique_ou_bonus_to_str id)) with
      | None -> ()
      | Some elt -> (Unsafe.coerce elt)##.checked := _false
    ) bonus_supprimes;
    rafraichit_voies perso

and pp_voie _app v =
  let s = voie_type_to_str (voie_type_of_jsoo v) in
  let b = String.starts_with ~prefix:"demi_" s in
  string @@ String.capitalize_ascii @@ String.map (function '_' -> if b then '-' else ' ' | c -> c) s

and [@noconv] choisit_bonus_capacite app (ev: Dom_html.inputElement Dom.event t) vt rg nom b =
  let aux checked bonuses voies =
    let vt = voie_type_of_jsoo vt in
    let nom = to_string nom in
    let b = bonus_of_jsoo b in
    if checked then
      match List.assoc_opt vt voies with
      | Some rgs when List.mem rg rgs ->
        let b = { b with opt=Some (Some true) } in
        Ok (bonuses @ [ nom, b ])
      | _ -> Error "capacité non apprise"
    else Ok (List.filter (fun (n, b2) -> not (n = nom && b.id = b2.id)) bonuses) in
  let choix = to_listf voie_et_rangs_of_jsoo app##.choix in
  let bonuses = to_listf bonus_avec_nom_of_jsoo app##.bonuses in
  match Opt.to_option ev##.target with
  | None -> ()
  | Some target ->
    let checked = to_bool target##.checked in
    match aux checked bonuses choix with
    | Error e ->
      ignore (Common.alert app e);
      target##.checked := bool (not checked)
    | Ok bonuses ->
      app##.bonuses := of_listf bonus_avec_nom_to_jsoo bonuses

and [@noconv] maj app : personnage_jsoo t Promise.promise t =
  let p = personnage_of_jsoo app##.p in
  let choix = to_listf voie_et_rangs_of_jsoo app##.choix in
  Promise.promise @@ fun resolve reject ->
  let@ voies, capacites = capacites ~caracteristiques:p.caracteristiques choix in
  let p = { p with voies } in
  match verifie_voies ~capacites p with
  | Error e ->
    let fail s = reject (Common.js_error s) in
    Common.alert ~fail app e
  | Ok _ ->
    let@ l = Common.fold (fun acc (vt, rg) f ->
      let@ v = charge_voie ~caracteristiques:p.caracteristiques vt in
      f (acc @ [ vt, v, rg ])
    ) [] voies in
    let bonus_voies = bonus_capacites l in
    let bonuses = to_listf bonus_avec_nom_of_jsoo app##.bonuses in
    let bonuses = List.fold_left (fun acc (n, b) ->
      match List.find_opt (fun (n2, b2) -> n = n2 && b.id = b2.id) acc with
      | None -> acc @ [ n, b ]
      | Some _ -> acc
    ) bonuses bonus_voies in
    let p = { p with bonuses } in
    let p = remplit_caracteristiques p in
    resolve (personnage_to_jsoo p)

let%watch p app (perso: personnage) _old = prepare app perso

[%%created fun app -> prepare app (personnage_of_jsoo app##.p)]

[%%comp {name="voies"; conv}]
