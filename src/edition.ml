open Co
open Ezjs_min
open Common

let%file _ = "./edition.html"

let%prop p : personnage = { req }
and lbl : string = { req }

let phases = [ Niveau; Caracteristiques; Voies; Equipements; Competences; Bonuses; Sauvegarde ]

let%data [@noconv] perso app : personnage_jsoo t = app##.p
and [@noconv] label app : js_string t = app##.lbl
and hide_collapse = "hide.bs.collapse"
and shown_collapse = "shown.bs.collapse"
and phase_courante : phase option = None
and points_de_maitrise : Voies.points_de_maitrise = 0, 0
and points_de_competences: Competences.points_competences = 0, 0

let%meth alert app (e: string) = Common.alert app e
and points_maitrise app pts = app##.points_de_maitrise_ := pts
and points_competences app pts = app##.points_de_competences_ := pts

and [@noconv] check app (ph: phase_jsoo t) =
  let name = Ezjs_min.to_string ph in
  match Optdef.to_option [%ref app ("edition-" ^ name)], phase_of_jsoo ph with
  | Some elt, Sauvegarde ->
    let@ s : avec_label_jsoo t = Promise.jthen (Unsafe.coerce elt)##maj in
    app##.perso := s##.perso;
    app##.label := s##.label
  | Some elt, _ ->
    let@ p = Promise.jthen (Unsafe.coerce elt)##maj in
    app##.perso := p
  | _ -> alert app (name ^ " child non trouvé")

and [@noconv] maj app : avec_label_jsoo t Promise.promise t =
  let perso = personnage_of_jsoo app##.perso in
  let label = to_string app##.label in
  let phases = match to_optdef phase_of_jsoo app##.phase_courante_ with
    | None -> phases
    | Some ph -> ph :: phases in
  Promise.promise @@ fun resolve reject ->
  let fail s = reject (js_error s) in
  let@ acc = fold (fun e ph cb ->
    let name = Ezjs_min.to_string @@ phase_to_jsoo ph in
    match Optdef.to_option [%ref app ("edition-" ^ name)], ph with
    | Some elt, Sauvegarde ->
      let@ x = Promise.jthen (Unsafe.coerce elt)##maj in
      cb (avec_label_of_jsoo x)
    | Some elt, _ ->
      let@ p = Promise.jthen (Unsafe.coerce elt)##maj in
      app##.perso := p;
      let perso = personnage_of_jsoo p in
      [%next app (fun _app -> cb { e with perso })]
    | _ -> alert ~fail app (name ^ " child non trouvé")) { perso; label } phases in
  resolve (avec_label_to_jsoo acc)

and ouvre app ph = app##.phase_courante_ := def ph

[%%comp {name="edition"; conv; components=[Caracteristiques; Equipements; Niveau; Voies; Competences; Bonuses; Sauvegarde]}]
