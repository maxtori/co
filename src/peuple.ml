open Ezjs_min
open Co

let%file _ = "./peuple.html"

let%prop p : personnage = { req }

let%data peuples : peuple list = List.map snd peuple_assoc
and choix : peuple option = None

let%meth choisit app (p: peuple) = app##.choix := Ezjs_min.def (peuple_to_jsoo p)

and pp_peuple _app (p: peuple) : string = Common.pp_peuple p

and [@noconv] maj app : personnage_jsoo t Promise.promise t =
  Promise.promise @@ fun resolve reject ->
  match to_optdef peuple_of_jsoo app##.choix with
  | None ->
    let fail s = reject (Common.js_error s) in
    Common.alert ~fail app "peuple non choisi"
  | Some peuple ->
    let p = personnage_of_jsoo app##.p in
    resolve (personnage_to_jsoo { p with peuple })

[%%comp {name="peuple"; conv}]
