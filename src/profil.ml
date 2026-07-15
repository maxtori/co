open Ezjs_min
open Co

type famille_et_profils = famille * profil list [@@deriving jsoo]
type famille_et_profil = famille * profil [@@deriving jsoo]

let%file _ = "./profil.html"

let%prop p : personnage = { req }

let%data profils : famille_et_profils list = profils ()
and choix : famille_et_profil option = None

let%meth choisit app (f: famille) (p: profil) =
  app##.choix := Ezjs_min.def (famille_et_profil_to_jsoo (f, p))

and [@noconv] maj app : personnage_jsoo t Promise.promise t =
  Promise.promise @@ fun resolve reject ->
  match to_optdef famille_et_profil_of_jsoo app##.choix with
  | None ->
    let fail s = reject (Common.js_error s) in
    Common.alert ~fail app "profil non choisi"
  | Some (famille, profil) ->
    let p = personnage_of_jsoo app##.p in
    resolve (personnage_to_jsoo { p with famille; profil })

[%%comp {name="profil"; conv}]
