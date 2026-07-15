open Ezjs_min
open Co

let%file _ = "./niveau.html"

let%prop p : personnage = {req}

let%data niveau app = app##.p##.niveau

let%meth change app (niveau: int) =
  if niveau <= 0 then Common.alert app "le niveau doit rester positif"
  else app##.niveau := niveau

and [@noconv] maj app : personnage_jsoo t Promise.promise t =
  let p = personnage_of_jsoo app##.p in
  Promise.promise @@ fun resolve _reject ->
  resolve (personnage_to_jsoo { p with niveau = app##.niveau })

let%watch [@noconv] p app (perso: personnage_jsoo t) _old = app##.niveau := perso##.niveau

[%%comp {name="niveau"; conv}]
