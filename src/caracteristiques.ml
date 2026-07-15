open Ezjs_min
open Co

type bonus_avec_index = bonus_avec_nom list * int [@@deriving jsoo]

let%file _ = "./caracteristiques.html"

let%prop p : personnage = {req}

let%data choix: caracteristiques = caracteristiques_par_defaut None
and choix_bonus: bonus_avec_index option = None
and bonuses: bonus_avec_nom list list = []
and resultat: caracteristiques = caracteristiques_par_defaut None

let calcul ?bonus choix = match bonus with
  | None -> Error "caracteristiques bonus non choisies"
  | Some bonus -> Ok (bonus, ajoute_caracteristiques choix bonus)

let prepare app p =
  let bonuses = bonuses_peuple p.peuple p.caracteristiques_base in
  app##.choix := caracteristiques_to_jsoo p.caracteristiques_base;
  app##.bonuses := of_listf (of_listf bonus_avec_nom_to_jsoo) bonuses;
  let bonus_peuple = match p.bonus_peuple with
    | [] -> extrait_bonus_peuple p.peuple p.bonuses
    | _ -> p.bonus_peuple in
  match bonus_peuple with
  | [] -> ()
  | _ ->
    let acc, _ = List.fold_left (fun (acc, i) b -> match acc with
      | Some acc -> Some acc, i+1
      | None -> if b = bonus_peuple then Some (i, b), i+1 else None, i+1
    ) (None, 0) bonuses in
    match acc with
    | None -> ()
    | Some (i, bonus) ->
      app##.choix_bonus_ := def (bonus_avec_index_to_jsoo (p.bonus_peuple, i));
      match calcul ~bonus p.caracteristiques_base with
      | Ok (_, caracteristiques) -> app##.resultat := caracteristiques_to_jsoo caracteristiques;
      | _ -> ()

let%meth choisit app (b: bonus_avec_nom list) (i: int) =
  app##.choix_bonus_ := def (bonus_avec_index_to_jsoo (b, i));
  Promise.jthen app##maj (fun _ -> ())

and [@noconv] maj app : personnage_jsoo t Promise.promise t =
  let p = personnage_of_jsoo app##.p in
  let caracteristiques_base = caracteristiques_of_jsoo app##.choix in
  Promise.promise @@ fun resolve reject ->
  let fail s = reject (Common.js_error s) in
  if verifie_caracteristiques caracteristiques_base  then
    let x = to_optdef bonus_avec_index_of_jsoo app##.choix_bonus_ in
    let bonus = Option.map fst x in
    match calcul ?bonus caracteristiques_base with
    | Error e ->
      app##.resultat := caracteristiques_to_jsoo caracteristiques_base;
      Common.alert ~fail app e
    | Ok (bonus_peuple, caracteristiques) ->
      app##.resultat := caracteristiques_to_jsoo caracteristiques;
      resolve (personnage_to_jsoo { p with bonus_peuple; caracteristiques_base; caracteristiques })
  else Common.alert ~fail app "caracteristiques non valables"

let%watch p app (perso: personnage) _old = prepare app perso

[%%created fun app -> prepare app (personnage_of_jsoo app##.p)]

[%%comp {name="caracteristiques"; conv}]
