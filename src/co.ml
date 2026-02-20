type famille =
  | Aventuriers
  | Combattants
  | Mages
  | Mystiques
[@@deriving encoding {assoc}, jsoo]

type profil_aventurier = [
  | `Arquebusier
  | `Barde
  | `Rodeur
  | `Voleur
] [@@deriving encoding {assoc; lower}, jsoo]

type profil_combattant = [
  | `Barbare
  | `Chevalier
  | `Guerrier
] [@@deriving encoding {assoc; lower}, jsoo]

type profil_mage = [
  | `Ensorceleur
  | `Forgesort
  | `Magicien
  | `Sorcier
] [@@deriving encoding {assoc; lower}, jsoo]

type profil_mystique = [
  | `Druide
  | `Moine
  | `Pretre
] [@@deriving encoding {assoc; lower}, jsoo]

type profil = [
  | profil_aventurier
  | profil_combattant
  | profil_mage
  | profil_mystique
] [@@deriving encoding]

[@@@jsoo
  class type profil_jsoo = Ezjs_min.js_string
  let profil_to_jsoo : profil -> profil_jsoo Ezjs_min.t = function
    | #profil_aventurier as p -> profil_aventurier_to_jsoo p
    | #profil_combattant as p -> profil_combattant_to_jsoo p
    | #profil_mage as p -> profil_mage_to_jsoo p
    | #profil_mystique as p -> profil_mystique_to_jsoo p
  let profil_of_jsoo : profil_jsoo Ezjs_min.t -> profil = fun p ->
    try (profil_aventurier_of_jsoo p :> profil) with _ ->
    try (profil_combattant_of_jsoo p :> profil) with _ ->
    try (profil_mage_of_jsoo p :> profil) with _ ->
      (profil_mystique_of_jsoo p :> profil)
  let profil_jsoo_conv = profil_to_jsoo, profil_of_jsoo
]

type peuple =
  | Demi_elfe
  | Demi_orc
  | Elfe_haut
  | Elfe_sylvain
  | Gnome
  | Halfelin
  | Humain
  | Nain
[@@deriving encoding {assoc}, jsoo]

type caracteristique =
  | AGI
  | CON
  | FOR
  | PER
  | CHA
  | INT
  | VOL
[@@deriving encoding, jsoo]

type caracteristique_avec_valeur = { caracteristique: caracteristique; valeur: int } [@@deriving encoding, jsoo]

type caracteristiques = {
  agilite: int;
  constitution: int;
  force: int;
  perception: int;
  charisme: int;
  intelligence: int;
  volonte: int;
} [@@deriving encoding, jsoo]

type de = [ `d3 | `d4 | `d6 | `d8 | `d10 | `d12 | `d20 ] [@@deriving encoding, jsoo]

type dommage_type = [
  | `contondants
  | `perforants
  | `tranchants
] [@@deriving encoding, jsoo]

type equipement =
  | Arme_au_contact of {
    dommage: int * de; deux_mains: (int * de) option;
    prix: string option; typ: dommage_type [@key "type"]; notes: string option }
  | Arme_a_distance of {
      dommage: int * de; portee: int; prix: string option;
      typ: dommage_type [@key "type"]; notes: string option }
  | Armure of { defense: int; agilite_max: int option; prix: string option }
  | Autre of { description: string; prix: string option }
[@@deriving encoding, jsoo {snake; remove_prefix=false; remove_undefined}]

type equipement_nom = [
  | `mains_nues
  | `baton
  | `baton_ferre
  | `dague
  | `epee_a_deux_mains
  | `epee_batarde
  | `epee_courte
  | `epee_longue
  | `epieu
  | `fleau
  | `fleau_a_deux_mains
  | `gourdin
  | `hache
  | `hache_a_deux_mains
  | `lance
  | `lance_de_cavalerie
  | `marteau
  | `masse
  | `pique
  | `rapiere
  | `stylet
  | `vivelame
  | `arbalete_de_poing
  | `arbalete_legere
  | `arbalete_lourde
  | `arc_court
  | `arc_long
  | `couteaux_de_lancer
  | `fronde
  | `hachette
  | `javelot
  | `lance_pierre
  | `petoire
  | `mousquet
  | `tissus_matelasses
  | `fourrures
  | `cuir_simple
  | `cuir_renforce
  | `broigne
  | `chemise_de_mailles
  | `cotte_de_mailles
  | `armure_de_plaques
  | `plaque_complete
  | `petit_bouclier
  | `grand_bouclier
  | `autre of string
] [@@deriving encoding {assoc}, jsoo]

let equiepement_of_str s = Json_encoding.destruct equipement_nom_enc (`String s)
let equipement_to_str e = match Json_encoding.construct equipement_nom_enc e with `String s -> s | _ -> assert false

type ideal =
  | Abnegation
  | Clemence
  | Compassion
  | Courage
  | Egalite
  | Education
  | Fraternite
  | Frugalite
  | Generosite
  | Honnetete
  | Honneur
  | Humilite
  | Justice
  | Liberte
  | Loyaute
  | Pacifisme
  | Protection
  | SensDuSacrifice
  | Solidarite
  | Verite
[@@deriving encoding {assoc}, jsoo]

type travers =
  | Alcoolique
  | Couard
  | Credule
  | Cupide
  | Colerique
  | Distrait
  | Dragueur
  | Fanfaron
  | Gourmand
  | Grossier
  | Impatient
  | Indecis
  | Menteur
  | Orgueilleux
  | Paranoiaque
  | Paresseux
  | Phobie
  | Timide
  | Violent
  | Voleur
[@@deriving encoding {assoc}, jsoo]

type description = Json_repr.ezjsonm
[@@deriving encoding]

[@@@jsoo
  type description_jsoo = Ezjs_min.Unsafe.top
  let description_to_jsoo = Js_json.js_of_json
  let description_of_jsoo = Js_json.json_of_js
  let description_jsoo_conv = description_to_jsoo, description_of_jsoo
]

type voie_arquebusier = [
  | `Artilleur
  | `Explosifs
  | `Mercenaire
  | `Precision
  | `Pistolero
] [@@deriving encoding {lower; assoc}, jsoo]

type voie_barde = [
  | `Escrime
  | `Musicien
  | `Saltimbanque
  | `Seduction
  | `Vagabond
] [@@deriving encoding {lower; assoc}, jsoo]

type voie_rodeur = [
  | `Archer
  | `Compagnon_animal
  | `Survie
  | `Traqueur
  | `Combat_a_deux_armes
] [@@deriving encoding {lower; assoc}, jsoo]

type voie_voleur = [
  | `Assassin
  | `Aventurier
  | `Deplacement
  | `Roublard
  | `Spadassin
] [@@deriving encoding {lower; assoc}, jsoo]

type voie_aventurier = [
  | voie_arquebusier
  | voie_barde
  | voie_rodeur
  | voie_voleur
] [@@deriving encoding]

[@@@jsoo
  class type voie_aventurier_jsoo = Ezjs_min.js_string
  let voie_aventurier_to_jsoo : voie_aventurier -> voie_aventurier_jsoo Ezjs_min.t = function
    | #voie_arquebusier as v -> voie_arquebusier_to_jsoo v
    | #voie_barde as v -> voie_barde_to_jsoo v
    | #voie_rodeur as v -> voie_rodeur_to_jsoo v
    | #voie_voleur as v -> voie_voleur_to_jsoo v
  let voie_aventurier_of_jsoo : voie_aventurier_jsoo Ezjs_min.t -> voie_aventurier = fun v ->
    try (voie_arquebusier_of_jsoo v :> voie_aventurier) with _ ->
    try (voie_barde_of_jsoo v :> voie_aventurier) with _ ->
    try (voie_rodeur_of_jsoo v :> voie_aventurier) with _ ->
      (voie_voleur_of_jsoo v :> voie_aventurier)
  let voie_aventurier_jsoo_conv = voie_aventurier_to_jsoo, voie_aventurier_of_jsoo
]

type voie_barbare = [
  | `Brute
  | `Pagne
  | `Pourfendeur
  | `Primitif
  | `Rage
] [@@deriving encoding {lower; assoc}, jsoo]

type voie_chevalier = [
  | `Cavalier
  | `Guerre
  | `Preux
  | `Meneur_d_hommes
  | `Noblesse
] [@@deriving encoding {lower; assoc}, jsoo]

type voie_guerrier = [
  | `Bouclier
  | `Combat
  | `Maitre_d_armes
  | `Resistance
  | `Soldat
] [@@deriving encoding {lower; assoc}, jsoo]

type voie_combattant = [
  | voie_barbare
  | voie_chevalier
  | voie_guerrier
] [@@deriving encoding]

[@@@jsoo
  class type voie_combattant_jsoo = Ezjs_min.js_string
  let voie_combattant_to_jsoo : voie_combattant -> voie_combattant_jsoo Ezjs_min.t = function
    | #voie_barbare as v -> voie_barbare_to_jsoo v
    | #voie_chevalier as v -> voie_chevalier_to_jsoo v
    | #voie_guerrier as v -> voie_guerrier_to_jsoo v
  let voie_combattant_of_jsoo : voie_combattant_jsoo Ezjs_min.t -> voie_combattant = fun v ->
    try (voie_barbare_of_jsoo v :> voie_combattant) with _ ->
    try (voie_chevalier_of_jsoo v :> voie_combattant) with _ ->
      (voie_guerrier_of_jsoo v :> voie_combattant)
  let voie_combattant_jsoo_conv = voie_combattant_to_jsoo, voie_combattant_of_jsoo
]

type voie_ensorceleur = [
  | `Air
  | `Divination
  | `Envouteur
  | `Illusions
  | `Invocation
] [@@deriving encoding {lower; assoc}, jsoo]

type voie_forgesort = [
  | `Artefacts
  | `Elixirs
  | `Metal
  | `Golem
  | `Runes
] [@@deriving encoding {lower; assoc}, jsoo]

type voie_magicien = [
  | `Magie_des_arcanes
  | `Magie_destructrice
  | `Magie_elementaire
  | `Magie_protectrice
  | `Magie_universelle
] [@@deriving encoding {lower; assoc}, jsoo]

type voie_sorcier = [
  | `Demon
  | `Mort
  | `Outre_tombe
  | `Sang
  | `Sombre_magie
] [@@deriving encoding {lower; assoc}, jsoo]

type voie_mage = [
  | `Mage
  | voie_ensorceleur
  | voie_forgesort
  | voie_magicien
  | voie_sorcier
] [@@deriving encoding {lower}]

[@@@jsoo
  class type voie_mage_jsoo = Ezjs_min.js_string
  let voie_mage_to_jsoo : voie_mage -> voie_mage_jsoo Ezjs_min.t = function
    | `Mage -> Ezjs_min.string "Mage"
    | #voie_ensorceleur as v -> voie_ensorceleur_to_jsoo v
    | #voie_forgesort as v -> voie_forgesort_to_jsoo v
    | #voie_magicien as v -> voie_magicien_to_jsoo v
    | #voie_sorcier as v -> voie_sorcier_to_jsoo v
  let voie_mage_of_jsoo : voie_mage_jsoo Ezjs_min.t -> voie_mage = fun v ->
    if Ezjs_min.to_string v = "Mage" then `Mage else
    try (voie_ensorceleur_of_jsoo v :> voie_mage) with _ ->
    try (voie_forgesort_of_jsoo v :> voie_mage) with _ ->
    try (voie_magicien_of_jsoo v :> voie_mage) with _ ->
      (voie_sorcier_of_jsoo v :> voie_mage)
  let voie_mage_jsoo_conv = voie_mage_to_jsoo, voie_mage_of_jsoo
]

type voie_druide = [
  | `Animaux
  | `Fauve
  | `Nature
  | `Protecteur
  | `Vegetaux
] [@@deriving encoding {lower; assoc}, jsoo]

type voie_moine = [
  | `Energie_vitale
  | `Maitrise
  | `Meditation
  | `Poing
  | `Vent
] [@@deriving encoding {lower; assoc}, jsoo]

type voie_pretre = [
  | `Foi
  | `Guerre_sainte
  | `Priere
  | `Soins
  | `Spiritualite
] [@@deriving encoding {lower; assoc}, jsoo]

type voie_mystique = [
  | voie_druide
  | voie_moine
  | voie_pretre
] [@@deriving encoding]

[@@@jsoo
  class type voie_mystique_jsoo = Ezjs_min.js_string
  let voie_mystique_to_jsoo : voie_mystique -> voie_mystique_jsoo Ezjs_min.t = function
    | #voie_druide as v -> voie_druide_to_jsoo v
    | #voie_moine as v -> voie_moine_to_jsoo v
    | #voie_pretre as v -> voie_pretre_to_jsoo v
  let voie_mystique_of_jsoo : voie_mystique_jsoo Ezjs_min.t -> voie_mystique = fun v ->
    try (voie_druide_of_jsoo v :> voie_mystique) with _ ->
    try (voie_moine_of_jsoo v :> voie_mystique) with _ ->
      (voie_pretre_of_jsoo v :> voie_mystique)
  let voie_mystique_jsoo_conv = voie_mystique_to_jsoo, voie_mystique_of_jsoo
]

type voie_peuple = [
  | `Demi_orc
  | `Elfe_haut
  | `Elfe_sylvain
  | `Gnome
  | `Halfelin
  | `Humain
  | `Nain
] [@@deriving encoding {lower}, jsoo]

type voie_type = [
  | voie_aventurier
  | voie_combattant
  | voie_mage
  | voie_mystique
  | voie_peuple
] [@@deriving encoding]

let voie_type_of_str s = Json_encoding.destruct voie_type_enc (`String s)
let voie_type_to_str v = match Json_encoding.construct voie_type_enc v with `String s -> s | _ -> assert false

[@@@jsoo
  class type voie_type_jsoo = Ezjs_min.js_string
  let voie_type_to_jsoo : voie_type -> voie_type_jsoo Ezjs_min.t = function
    | #voie_aventurier as v -> voie_aventurier_to_jsoo v
    | #voie_combattant as v -> voie_combattant_to_jsoo v
    | #voie_mage as v -> voie_mage_to_jsoo v
    | #voie_mystique as v -> voie_mystique_to_jsoo v
    | #voie_peuple as v -> voie_peuple_to_jsoo v
  let voie_type_of_jsoo : voie_type_jsoo Ezjs_min.t -> voie_type = fun v ->
    try (voie_aventurier_of_jsoo v :> voie_type) with _ ->
    try (voie_combattant_of_jsoo v :> voie_type) with _ ->
    try (voie_mage_of_jsoo v :> voie_type) with _ ->
    try (voie_mystique_of_jsoo v :> voie_type) with _ ->
      (voie_peuple_of_jsoo v :> voie_type)
  let voie_type_jsoo_conv = voie_type_to_jsoo, voie_type_of_jsoo
]

type action =
  | Limitee [@enc.key "limitée"]
  | Attaque
  | Gratuite
  | Mouvement
[@@deriving encoding, jsoo]

let actions_enc = Json_encoding.union [
  Json_encoding.case action_enc (function [ x ] -> Some x | _ -> None) (fun x -> [x]);
  Json_encoding.case (Json_encoding.list action_enc) Option.some Fun.id;
]

type capacite = {
  nom: string;
  description: string;
  action: action list; [@dft []] [@encoding actions_enc]
  sort: bool; [@dft false]
} [@@deriving encoding, jsoo]

type voie = capacite list [@@deriving encoding, jsoo]

type voies = (voie_type * voie) list [@assoc (voie_type_to_str, voie_type_of_str)]
[@@deriving encoding]

[@@@jsoo
  class type voies_jsoo = [voie_jsoo Ezjs_min.t] Ezjs_min.Table.ct
  let voies_to_jsoo l : voies_jsoo Ezjs_min.t =
    Ezjs_min.Table.makef voie_to_jsoo @@ List.map (fun (k, v) -> voie_type_to_str k, v) l
  let voies_of_jsoo t =
    List.map (fun (k, v) -> voie_type_of_str k, v) @@
    Ezjs_min.Table.itemsf voie_of_jsoo t
]

type points_avec_max = {
  courant: int;
  max: int;
} [@@deriving encoding, jsoo]

type phase =
  | Profil of { profils: (famille * profil list) list; choix: (famille * profil) option }
  | Peuple of { peuples: peuple list; choix: peuple option }
  | Caracteristiques of {
      bonuses: caracteristique_avec_valeur list list;
      choix: caracteristiques; choix_bonus: (caracteristique_avec_valeur list * int) option
    }
  | Voies of { voies: voie_type list; choix: (voie_type * int) list }
  | Enregistrement of {
      ideaux: string list; travers_options: string list;
      nom: string; label: string; ideal: string; travers: string;
    }
[@@deriving encoding, jsoo {remove_undefined; snake}]

type personnage = {
  nom: string;
  niveau: int;
  famille: famille;
  profil: profil;
  peuple: peuple;
  caracteristiques: caracteristiques;
  points_de_vigueur: points_avec_max;
  des_de_recuperation: points_avec_max;
  points_de_chance: points_avec_max;
  points_de_mana: points_avec_max;
  initiative: int;
  defense: int;
  equipements: equipement_nom list; [@dft []]
  ideal: ideal option;
  travers: travers option;
  description: description;
  voies: (voie_type * int) list; [@dft []]
  creation: phase option;
  caracteristiques_bonus: caracteristique_avec_valeur list;
} [@@deriving encoding, jsoo]

let caracteristiques_par_defaut peuple = match peuple with
  | None ->
    { agilite=2; constitution=0; force=3; perception=1; charisme=(-1);
      intelligence=0; volonte=1 }
  | Some p -> match p with
    | Demi_elfe ->
      { agilite=2; constitution=0; force=(-1); perception=3; charisme=1;
        intelligence=0; volonte=1 }
    | Demi_orc ->
      { agilite=1; constitution=2; force=3; perception=0; charisme=0;
        intelligence=(-1); volonte=1 }
    | Elfe_haut ->
      { agilite=2; constitution=(-1); force=0; perception=0; charisme=1;
        intelligence=3; volonte=1 }
    | Elfe_sylvain ->
      { agilite=3; constitution=(-1); force=0; perception=2; charisme=1;
        intelligence=0; volonte=1 }
    | Gnome ->
      { agilite=(-1); constitution=1; force=0; perception=2; charisme=0;
        intelligence=3; volonte=1 }
    | Halfelin ->
      { agilite=3; constitution=1; force=0; perception=1; charisme=0;
        intelligence=(-1); volonte=2 }
    | Humain ->
      { agilite=2; constitution=0; force=3; perception=1; charisme=(-1);
        intelligence=0; volonte=1 }
    | Nain ->
      { agilite=0; constitution=3; force=2; perception=0; charisme=(-1);
        intelligence=1; volonte=1 }

let points_vide = { courant = 0; max = 0 }

let personnage_vide = {
  nom=""; niveau=1; famille=Aventuriers; profil=`Arquebusier; peuple=Demi_elfe;
  caracteristiques=caracteristiques_par_defaut (Some Demi_elfe); points_de_vigueur=points_vide;
  des_de_recuperation=points_vide; points_de_chance=points_vide; points_de_mana=points_vide;
  initiative=0; defense=0; equipements=[]; ideal=None; travers=None; description=`Null;
  voies=[]; creation=None; caracteristiques_bonus=[];
}

let profils () =
  List.map (fun (_, f) -> match f with
    | Aventuriers -> f, List.map snd profil_aventurier_assoc
    | Combattants -> f, List.map snd profil_combattant_assoc
    | Mages -> f, List.map snd profil_mage_assoc
    | Mystiques -> f, List.map snd profil_mystique_assoc
  ) famille_assoc

let bonuses_peuple p c =
  let l = match p with
    | Demi_elfe -> [
        [ PER, 1; FOR, -1 ]; [ PER, 1; CON, -1 ];
        [ CHA, 1; FOR, -1 ]; [ CHA, 1; CON, -1 ];
      ]
    | Demi_orc -> [
        [ FOR, 1; CHA, -1 ]; [ FOR, 1; INT, -1 ];
        [ CON, 1; CHA, -1 ]; [ CON, 1; INT, -1 ];
      ]
    | Elfe_haut -> [
        [ INT, 1; FOR, -1 ]; [ CHA, 1; FOR, -1 ];
      ]
    | Elfe_sylvain -> [
        [ AGI, 1; FOR, -1 ]; [ PER, 1; FOR, -1 ];
      ]
    | Gnome -> [
        [ INT, 1; FOR, -1 ]; [ PER, 1; FOR, -1 ];
      ]
    | Halfelin -> [
        [ AGI, 1; FOR, -1 ]; [ VOL, 1; FOR, -1 ];
      ]
    | Humain ->
      let aux acc i =
        let acc = if c.agilite = i then AGI :: acc else acc in
        let acc = if c.constitution = i then CON :: acc else acc in
        let acc = if c.force = i then FOR :: acc else acc in
        let acc = if c.perception = i then PER :: acc else acc in
        let acc = if c.charisme = i then CHA :: acc else acc in
        let acc = if c.intelligence = i then INT :: acc else acc in
        let acc = if c.volonte = i then VOL :: acc else acc in
        acc in
      let l = aux [] (-1) in
      let l = if List.length l >= 2 then l else aux l 0 in
      List.map (fun c -> [c, 1]) l
    | Nain -> [
        [ CON, 1; AGI, -1 ]; [ VOL, 1; AGI, -1 ];
      ] in
  List.map (fun l -> List.map (fun (caracteristique, valeur) -> {caracteristique; valeur}) l) l

let verifie_caracteristiques c =
  let acc = [] in
  let aux acc x = match List.assoc_opt x acc with
    | None -> (x, 1) :: acc
    | Some v -> (x, v+1) :: List.remove_assoc x acc in
  let acc = aux acc c.agilite in
  let acc = aux acc c.constitution in
  let acc = aux acc c.force in
  let acc = aux acc c.perception in
  let acc = aux acc c.charisme in
  let acc = aux acc c.intelligence in
  let acc = aux acc c.volonte in
  match
    List.assoc_opt 4 acc, List.assoc_opt 3 acc, List.assoc_opt 2 acc,
    List.assoc_opt 1 acc, List.assoc_opt 0 acc, List.assoc_opt (-1) acc with
  | None, None, Some 3, Some 2, Some 1, Some 1
  | None, Some 1, Some 1, Some 2, Some 2, Some 1
  | Some 1, None, Some 1, Some 1, Some 2, Some 2 -> true
  | _ -> false

let ajoute_caracteristiques ?(factor=1) c l =
  List.fold_left (fun acc {caracteristique; valeur} -> match caracteristique with
    | AGI -> { acc with agilite = acc.agilite + factor * valeur }
    | CON -> { acc with constitution = acc.constitution + factor * valeur }
    | FOR -> { acc with force = acc.force + factor * valeur }
    | PER -> { acc with perception = acc.perception + factor * valeur }
    | CHA -> { acc with charisme = acc.charisme + factor * valeur }
    | INT -> { acc with intelligence = acc.intelligence + factor * valeur }
    | VOL -> { acc with volonte = acc.volonte + factor * valeur }) c l

let voies_peuple = function
  | Demi_elfe -> [ `Elfe_haut; `Elfe_sylvain; `Humain ]
  | Demi_orc -> [ `Demi_orc ]
  | Elfe_haut -> [ `Elfe_haut ]
  | Elfe_sylvain -> [ `Elfe_sylvain ]
  | Gnome -> [ `Gnome ]
  | Halfelin -> [ `Halfelin ]
  | Humain -> [ `Humain ]
  | Nain -> [ `Nain ]

let voies_profil = function
  | `Arquebusier -> List.map snd voie_arquebusier_assoc
  | `Barde -> List.map snd voie_barde_assoc
  | `Rodeur -> List.map snd voie_rodeur_assoc
  | `Voleur -> List.map snd voie_voleur_assoc
  | `Barbare -> List.map snd voie_barbare_assoc
  | `Chevalier -> List.map snd voie_chevalier_assoc
  | `Guerrier -> List.map snd voie_guerrier_assoc
  | `Ensorceleur -> `Mage :: (List.map snd voie_ensorceleur_assoc)
  | `Forgesort -> `Mage :: (List.map snd voie_forgesort_assoc)
  | `Magicien -> `Mage :: (List.map snd voie_magicien_assoc)
  | `Sorcier -> `Mage :: (List.map snd voie_sorcier_assoc)
  | `Druide -> List.map snd voie_druide_assoc
  | `Moine -> List.map snd voie_moine_assoc
  | `Pretre -> List.map snd voie_pretre_assoc

let verifie_voies p voies =
  let rg_peuple = List.fold_left (fun acc (v, rg) -> match v with
    | #voie_peuple -> rg
    | _ -> acc) 0 voies in
  let rg_mage = match List.find_map (fun (v, rg) -> match v with
    | `Mage -> Some rg
    | _ -> None) voies with None -> 0 | Some rg -> rg in
  let points_capacite, rg_max_sans_mage = List.fold_left (fun (acc, m) (v, rg) -> match v with
    | `Mage -> acc + (if rg < 3 then 1 else 2), m
    | _ -> acc + (if rg < 3 then 1 else 2), max m rg
  ) (0, 0) voies in
  let rg_max = max rg_mage rg_max_sans_mage in
  let points_niveau = if rg_mage = 0 then 3 + 2 * p.niveau else 5 + 2 * p.niveau in
  let niveau_capacite_check =
    (p.niveau = 1 && (rg_max_sans_mage = 1 && rg_mage = 2 || rg_max_sans_mage = 2 && rg_mage = 1)) ||
    (rg_max <= 3 && rg_max <= p.niveau) || (rg_max = 4 && p.niveau >= 5) ||
    (rg_max = 5 && p.niveau >= 7) || (rg_max = 6 && p.niveau >= 9) ||
    (rg_max = 7 && p.niveau >= 11) || (rg_max = 8 && p.niveau >= 13) in
  let profil_check = List.fold_left (fun acc (v, _) ->
    if not acc then acc else
    match p.profil, v with
    | _, #voie_peuple
    | `Arquebusier, #voie_arquebusier
    | `Barde, #voie_barde
    | `Rodeur, #voie_rodeur
    | `Voleur, #voie_voleur
    | `Barbare, #voie_barbare
    | `Chevalier, #voie_chevalier
    | `Guerrier, #voie_guerrier
    | `Ensorceleur, (#voie_ensorceleur | #voie_mage)
    | `Forgesort, (#voie_forgesort | #voie_mage)
    | `Magicien, (#voie_magicien | #voie_mage)
    | `Sorcier, (#voie_sorcier | #voie_mage)
    | `Druide, #voie_druide
    | `Moine, #voie_moine -> true
    | _ -> false) true voies in
  let peuple_mage_check = (rg_mage = 0 && rg_peuple >= 1) || (rg_mage >= 1 && rg_peuple = 1) in
  if not profil_check then Some "voie pas dans le profil" else
  if not niveau_capacite_check then Some "rang de capacite trop haut" else
  if not peuple_mage_check then Some "voie de peuple et mage ne peuvent pas être suivies ensemble" else
  if points_capacite > points_niveau then Some "trop de capacites" else
  None

let remplit_caracteristiques p nb_sorts def_equipement agi_max  =
  let aux p = { courant=p; max=p } in
  let points_de_vigueur = aux @@ p.caracteristiques.constitution + p.niveau * (match p.famille with
    | Aventuriers -> 8
    | Combattants -> 10
    | Mages -> 6
    | Mystiques -> 8) in
  let des_de_recuperation = aux @@ 2 + p.caracteristiques.constitution + (match p.famille with
    | Mystiques -> 1 | _ -> 0) in
  let points_de_chance = aux @@ 2 + p.caracteristiques.charisme + (match p.famille with
    | Aventuriers -> 1 | _ -> 0) in
  let points_de_mana = aux (if nb_sorts = 0 then 0 else p.caracteristiques.volonte + nb_sorts) in
  let initiative = 10 + p.caracteristiques.perception in
  let defense = 10 + min agi_max p.caracteristiques.agilite + def_equipement in
  { p with points_de_vigueur; des_de_recuperation; points_de_chance; points_de_mana;
           initiative; defense }

let equipements_profil : profil -> equipement_nom list = function
  | `Arquebusier -> [ `petoire; `epee_longue; `dague; `cuir_renforce ]
  | `Barde -> [ `rapiere; `dague; `autre "instrument_de_musique"; `cuir_simple ]
  | `Rodeur -> [ `epee_longue; `arc_court; `dague; `cuir_renforce ]
  | `Voleur -> [ `rapiere; `dague; `autre "outils de crochetage"; `cuir_simple; `autre "corde" ]
  | `Barbare -> [ `hache_a_deux_mains; `javelot; `dague; `cuir_simple ]
  | `Chevalier -> [ `epee_longue; `grand_bouclier; `lance_de_cavalerie; `cotte_de_mailles ]
  | `Guerrier -> [ `epee_longue; `hache_a_deux_mains; `hachette; `grand_bouclier; `chemise_de_mailles ]
  | `Ensorceleur -> [ `baton_ferre; `dague ]
  | `Forgesort -> [ `dague; `baton_ferre; `marteau ]
  | `Magicien | `Sorcier -> [ `dague; `baton_ferre; `autre "grimoire de sorts" ]
  | `Druide -> [ `epieu; `dague; `arc_court; `cuir_simple ]
  | `Moine -> [ `baton ]
  | `Pretre -> [ `masse; `petit_bouclier; `chemise_de_mailles ]

let nombre_sorts voies =
  List.fold_left (fun acc (l, rg) ->
    let rec aux acc i l =
      if i > rg then acc else
      match l with
      | [] -> acc
      | c :: tl ->
        let acc = if c.sort then acc + 1 else acc in
        aux acc (i+1) tl in
    aux acc 0 l
  ) 0 voies
