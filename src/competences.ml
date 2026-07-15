open Co
open Ezjs_min

type points_competences = int * int [@@deriving jsoo]

let%file _ = "./competences.html"

let%prop p : personnage = {req}
and edition = false

let%data maitrisees: competence_et_point list = []
and possibilites: competence list = []
and ajout_competence: competence option = None
and possibilites_maitrisees: competence list = []
and choix: competence option = None
and competences: competence_et_point list = []
and points: points_competences = 0, 0

let prepare app perso =
  let maitrisees, competences = match perso.competences_maitrisees with
    | [] ->
      let maitrisees = Result.get_ok (competences_maitrisees ~validation:false perso) in
      maitrisees, List.filter (fun (_, n) -> n <> 0) maitrisees
    | _ -> perso.competences_maitrisees, perso.competences in
  let possibilites_maitrisees = List.map snd competence_assoc in
  let possibilites = List.filter (fun c -> Option.is_none (List.assoc_opt c competences)) possibilites_maitrisees in
  let points_niveau, points_capacites, points_utilises, _ = Result.get_ok (points_de_competences perso) in
  app##.maitrisees := of_listf competence_et_point_to_jsoo maitrisees;
  app##.possibilites := of_listf competence_to_jsoo possibilites;
  app##.possibilites_maitrisees_ := of_listf competence_to_jsoo possibilites_maitrisees;
  app##.competences := of_listf competence_et_point_to_jsoo competences;
  let points = points_competences_to_jsoo (points_utilises, points_niveau + points_capacites) in
  app##.points := points;
  if to_bool app##.edition then [%emit "points_competences" app points]

let%meth pp_competence _app c =
  let s = match competence_of_jsoo c with
    | `athletisme -> "athlétisme"
    | `discretion -> "discrétion"
    | `equitation -> "Équitation"
    | `medecine -> "médecine"
    | c -> competence_to_str c in
  string (String.capitalize_ascii s)

and ajoute_competence app (arg: competence option) =
  match to_optdef competence_of_jsoo app##.ajout_competence_, arg with
  | None, None -> Common.alert app "pas de compétence choisie"
  | _, Some a | Some a, _ ->
    let perso = personnage_of_jsoo app##.p in
    let competences = to_listf competence_et_point_of_jsoo app##.competences in
    let choix = to_optdef competence_of_jsoo app##.choix in
    let possibilites = to_listf competence_of_jsoo app##.possibilites in
    match competences_maitrisees ?choix perso with
    | Error e ->
      app##.ajout_competence_ := undefined;
      Common.alert app e
    | Ok maitrisees ->
      let competences = competences @ [ a, 1 ] in
      match points_de_competences { perso with competences_maitrisees=maitrisees; competences } with
      | Error e ->
        app##.ajout_competence_ := undefined;
        Common.alert app e
      | Ok (points_niveau, points_capacites, points_utilises, _) ->
        app##.possibilites :=
          of_listf competence_to_jsoo (List.filter (fun c -> a <> c) possibilites);
        app##.competences := of_listf competence_et_point_to_jsoo competences;
        let points = points_competences_to_jsoo
            (points_utilises, points_niveau + points_capacites) in
        app##.points := points;
        if to_bool app##.edition then [%emit "points_competences" app points]

and ajoute_competence_maitrisee app =
  let perso = personnage_of_jsoo app##.p in
  let choix = to_optdef competence_of_jsoo app##.choix in
  match competences_maitrisees ?choix perso with
  | Error e -> Common.alert app e
  | Ok maitrisees ->
    let competences = List.filter (fun (_, n) -> n <> 0) maitrisees in
    match points_de_competences { perso with competences_maitrisees=maitrisees; competences } with
    | Error e -> Common.alert app e
    | Ok (points_niveau, points_capacites, points_utilises, _) ->
      if points_niveau + points_capacites < points_utilises then
        Common.alert app "trop de points de compétences"
      else
      let points = points_competences_to_jsoo
          (points_utilises, points_niveau + points_capacites) in
      app##.competences := of_listf competence_et_point_to_jsoo competences;
      app##.maitrisees := of_listf competence_et_point_to_jsoo maitrisees;
      app##.points := points;
      if to_bool app##.edition then [%emit "points_competences" app points]

and change_competence app (c: competence) i =
  let perso = personnage_of_jsoo app##.p in
  let maitrisees = to_listf competence_et_point_of_jsoo app##.maitrisees in
  let competences = to_listf competence_et_point_of_jsoo app##.competences in
  let competences = List.map (fun (c2, n) -> if c = c2 then c, i else c2, n) competences in
  let perso = { perso with competences_maitrisees=maitrisees; competences } in
  match points_de_competences perso with
  | Error e -> Common.alert app e
  |Ok (points_niveau, points_capacites, points_utilises, _) ->
    if points_niveau + points_capacites < points_utilises then
      Common.alert app "trop de points de compétences"
    else
    let points = points_competences_to_jsoo (points_utilises, points_niveau + points_capacites) in
    app##.competences := of_listf competence_et_point_to_jsoo competences;
    app##.points := points;
    if to_bool app##.edition then [%emit "points_competences" app points]

and supprime_competence app (c: competence) =
  let perso = personnage_of_jsoo app##.p in
  let choix = to_optdef competence_of_jsoo app##.choix in
  let competences = to_listf competence_et_point_of_jsoo app##.competences in
  let possibilites = to_listf competence_of_jsoo app##.possibilites in
  let competences = List.remove_assoc c competences in
  match competences_maitrisees ?choix perso with
  | Error e -> Common.alert app e
  | Ok maitrisees ->
    let perso = { perso with competences_maitrisees=maitrisees; competences } in
    match points_de_competences perso with
    | Error e -> Common.alert app e
    | Ok (points_niveau, points_capacites, points_utilises, _) ->
      let points = points_competences_to_jsoo (points_utilises, points_niveau + points_capacites) in
      app##.competences := of_listf competence_et_point_to_jsoo competences;
      app##.points := points;
      app##.possibilites := of_listf competence_to_jsoo (possibilites @ [ c ]);
      if to_bool app##.edition then [%emit "points_competences" app points]

and competence_caracteristiques _app (c: competence) =
  of_listf caracteristique_to_jsoo @@ competence_caracteristiques c

and [@noconv] maj app : personnage_jsoo t Promise.promise t =
  let perso = personnage_of_jsoo app##.p in
  let choix = to_optdef competence_of_jsoo app##.choix in
  Promise.promise @@ fun resolve reject ->
  let fail s = reject (Common.js_error s) in
  match competences_maitrisees ?choix perso with
  | Error e -> Common.alert ~fail app e
  | Ok competences_maitrisees ->
    let competences = to_listf competence_et_point_of_jsoo app##.competences in
    let competences = List.filter (fun (_, n) -> n > 0) competences in
    let perso = { perso with competences_maitrisees; competences } in
    match points_de_competences perso with
    | Error e -> Common.alert ~fail app e
    | Ok (points_niveau, points_capacites, points_utilises, points_maitrise_utilises) ->
      if points_utilises - points_maitrise_utilises > points_niveau then
        Common.alert ~fail app (Format.sprintf "au moins %d points de compétence doivent être utilisés par des compétences du profil" points_capacites)
      else if points_niveau + points_capacites < points_utilises then
        Common.alert ~fail app "trop de points de compétences"
      else resolve (personnage_to_jsoo perso)

let%watch p app (perso: personnage) _old = prepare app perso

[%%created fun app -> prepare app (personnage_of_jsoo app##.p)]

[%%comp {name="competences"; conv}]
