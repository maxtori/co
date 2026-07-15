open Co
open Ezjs_min

let%file _ = "./backup.html"

let%prop [@noconv] list : Unsafe.any js_array t js_array t = {req}

let%meth telecharge_raw_personnage _app (label: string) (p: Unsafe.any) =
  let s = to_string @@ _JSON##stringify p in
  Common.telecharge label s

and detruit_personnage app (n: string) =
  let@ () = Common.suppression_personnage n in
  [%emit "init" app]

and sauvegarde_raw_personnage app label i =
  let elt = Dom_html.getElementById (Format.sprintf "backup-textarea-%d" i) in
  let st = Common.Store.store ~mode:Ezjs_idb.READWRITE !Common.db in
  let perso = _JSON##parse (Unsafe.coerce elt)##.value in
  let callback _ = [%emit "init" app] in
  Common.Store.Raw.put ~key:label ~callback st
    (object%js val perso = perso val creation = undefined end);

[%%comp {name="backup"; conv}]
