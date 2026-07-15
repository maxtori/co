open Co
open Ezjs_min
open Common

type creation = {
  label: string;
  perso: personnage;
  phase: phase;
} [@@deriving jsoo]

type maj = {
  ancien: creation;
  nouveau: creation;
} [@@deriving jsoo]

let%file _ = "./creation.html"

let%prop p : personnage = { req }
and label : string = { req }
and phase : phase = { req; cons = (Unsafe.global##._String : js_string t constr) }

let%meth alert app (s: string) = Common.alert app s

and [@noconv] maj app : maj_jsoo t Promise.promise t =
  let phase = phase_of_jsoo app##.phase in
  let label = to_string app##.label in
  Promise.promise @@ fun resolve reject ->
  let fail s = reject (js_error s) in
  let aux ph cb =
    let name = Ezjs_min.to_string @@ phase_to_jsoo phase in
    match Optdef.to_option [%ref app ("creation-" ^ name)], phase with
    | Some elt, Sauvegarde ->
      let@ x = Promise.jthen (Unsafe.coerce elt)##maj in
      let { label; perso } : avec_label = avec_label_of_jsoo x in
      let ancien = { label; perso; phase } in
      let nouveau = { label; perso; phase=ph } in
      cb { ancien; nouveau }
    | Some elt, _ ->
      let@ x = Promise.jthen (Unsafe.coerce elt)##maj in
      let perso = personnage_of_jsoo x in
      let ancien = { label; perso; phase } in
      let nouveau = { label; perso; phase=ph } in
      cb { ancien; nouveau }
    | None, _ -> alert ~fail app (name ^ " child non trouvé") in
  let cb m = resolve (maj_to_jsoo m) in
  match phase with
  | Profil -> aux Peuple cb
  | Peuple -> aux Caracteristiques cb
  | Caracteristiques -> aux Equipements cb
  | Equipements -> aux Niveau cb
  | Niveau -> aux Voies cb
  | Voies -> aux Competences cb
  | Competences -> aux Sauvegarde cb
  | Sauvegarde -> aux Fin cb
  | Bonuses | Fin -> alert ~fail app "phase non attendue"

[%%comp {
  name="creation"; conv;
  components=[Profil; Peuple; Caracteristiques; Equipements; Niveau; Voies; Competences; Sauvegarde]
}]
