open Co
open Ezjs_min

let%file _ = "./bonuses.html"

let%prop p : personnage = { req }
let%data bonuses : bonus_avec_nom list = []
and bonus : bonus_avec_nom = "", {id=`AGI; valeur=`int 0; opt=None}
and bonus_ids : caracteristique_ou_bonus list = List.map snd caracteristique_assoc @ List.map snd bonus_type_assoc

let prepare app p = app##.bonuses := of_listf bonus_avec_nom_to_jsoo p.bonuses

let%meth ajoute_bonus app =
  let bonuses = to_listf bonus_avec_nom_of_jsoo app##.bonuses in
  let bonus = bonus_avec_nom_of_jsoo app##.bonus in
  let bonuses = bonuses @ [ bonus ] in
  app##.bonuses := of_listf bonus_avec_nom_to_jsoo bonuses

and [@noconv] maj app : personnage_jsoo t Promise.promise t =
  let p = personnage_of_jsoo app##.p in
  let bonuses = to_listf bonus_avec_nom_of_jsoo app##.bonuses in
  Promise.promise @@ fun resolve _reject -> resolve (personnage_to_jsoo { p with bonuses })

let%watch p app (perso: personnage) _old = prepare app perso

[%%created fun app -> prepare app (personnage_of_jsoo app##.p)]

[%%comp {name="bonuses"; conv}]
