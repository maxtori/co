let courbe dst control = Format.sprintf "curve to %s with %s" dst control

let haut ?connexion e pct =
  let pct = match connexion with
    | None -> Format.sprintf "%.5g%%" pct
    | Some x -> Format.sprintf "calc(%.5g%% - %.5gpx)" pct x in
  Format.sprintf "%s %.5gpx" pct e
let droite ?connexion e pct =
  let pct = match connexion with
    | None -> Format.sprintf "%.5g%%" pct
    | Some x -> Format.sprintf "calc(%.5g%% - %.5gpx)" pct x in
  Format.sprintf "calc(100%% - %.5gpx) %s" e pct
let bas ?connexion e pct =
  let pct = match connexion with
    | None -> Format.sprintf "%.5g%%" (100. -. pct)
    | Some x -> Format.sprintf "calc(%.5g%% + %.5gpx)" (100. -. pct) x in
  Format.sprintf "%s calc(100%% - %.5gpx)" pct e
let gauche ?connexion e pct =
  let pct = match connexion with
    | None -> Format.sprintf "%.5g%%" (100. -. pct)
    | Some x -> Format.sprintf "calc(%.5g%% + %.5gpx)" (100. -. pct) x in
  Format.sprintf "%.5gpx %s" e pct

let epaisseur_aleatoire e = Float.of_int (Random.int (2 * e)) /. 2.
let demi e = (Float.of_int e) /. 2.

let courbes ?connexion f e n =
  let ctrl = List.init n (fun _ -> epaisseur_aleatoire e) in
  let rec aux i acc = function
    | [] -> List.rev acc
    | [ ctrl1; _ ] -> aux i acc [ctrl1]
    | [ ctrl1 ] ->
      let dst = demi e in
      let pct_dst = 100. in
      let pct_ctrl = Float.round (10000. *. (Float.of_int i +. 0.5) /. Float.of_int n) /. 100. in
      let dst, ctrl = f ?connexion dst pct_dst, f ?connexion:None ctrl1 pct_ctrl in
      let acc = (courbe dst ctrl) :: acc in
      List.rev acc
    | ctrl1 :: ctrl2 :: tl ->
      if i=0 then aux (i+1) acc (ctrl2 :: tl) else
      let dst = Float.abs (ctrl1 -. ctrl2) /. 2. +. min ctrl1 ctrl2 in
      let pct_dst = Float.round (Float.of_int (10000 * (i+1)) /. Float.of_int n) /. 100. in
      let pct_ctrl = Float.round (10000. *. (Float.of_int i +. 0.5) /. Float.of_int n) /. 100. in
      let dst, ctrl = f ?connexion:None dst pct_dst, f ?connexion:None ctrl1 pct_ctrl in
      aux (i+1) ((courbe dst ctrl) :: acc) (ctrl2 :: tl) in
  aux 0 [] ctrl

let cadre l =
  let haut = match List.assoc_opt `haut l with
    | None ->
      let connexion = match List.assoc_opt `droite l with
        | None -> "hline to 100%"
        | Some (e, _) -> Format.sprintf "hline to calc(100%% - %.5gpx)" (demi e) in
      [ "from 0 0"; connexion ]
    | Some (e, n) ->
      let debut = Format.sprintf "from 0 %.5gpx" (demi e) in
      let connexion = Option.map (fun (e, _) -> demi e) (List.assoc_opt `droite l) in
      debut :: courbes ?connexion haut e n in
  let droite = match List.assoc_opt `droite l with
    | None ->
      let connexion = match List.assoc_opt `bas l with
        | None -> "vline to 100%"
        | Some (e, _) -> Format.sprintf "vline to calc(100%% - %.5gpx)" (demi e) in
      [ connexion ]
    | Some (e, n) ->
      let connexion = Option.map (fun (e, _) -> demi e) (List.assoc_opt `bas l) in
      courbes ?connexion droite e n in
  let bas = match List.assoc_opt `bas l with
    | None ->
      let connexion = match List.assoc_opt `gauche l with
        | None -> "hline to 0"
        | Some (e, _) -> Format.sprintf "hline to %.5gpx" (demi e) in
      [ connexion ]
    | Some (e, n) ->
      let connexion = Option.map (fun (e, _) -> demi e) (List.assoc_opt `gauche l) in
      courbes ?connexion bas e n in
  let gauche = match List.assoc_opt `bas l with
    | None ->
      let connexion = match List.assoc_opt `haut l with
        | None -> "vline to 0"
        | Some (e, _) -> Format.sprintf "vline to %.5gpx" (demi e) in
      [ connexion ]
    | Some (e, n) ->
      let connexion = Option.map (fun (e, _) -> demi e) (List.assoc_opt `haut l) in
      courbes ?connexion gauche e n in
  Format.sprintf "shape(%s)" @@ String.concat "," (haut @ droite @ bas @ gauche)

let () = Random.self_init ()
