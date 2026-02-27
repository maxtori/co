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

type caracteristique = [
  | `AGI
  | `CON
  | `FOR
  | `PER
  | `CHA
  | `INT
  | `VOL
] [@@deriving encoding {assoc}, jsoo]

type bonus = [
  | `DEF
  | `INI
  | `WEA
  | `PV
  | `PC
  | `PR
  | `PM
] [@@deriving encoding {assoc}, jsoo]

type caracteristique_ou_bonus = [ caracteristique | bonus ] [@@deriving encoding]

[@@@jsoo
  class type caracteristique_ou_bonus_jsoo = Ezjs_min.js_string
  let caracteristique_ou_bonus_to_jsoo : caracteristique_ou_bonus -> caracteristique_ou_bonus_jsoo Ezjs_min.t = function
    | #caracteristique as v -> caracteristique_to_jsoo v
    | #bonus as v -> bonus_to_jsoo v
  let caracteristique_ou_bonus_of_jsoo : caracteristique_ou_bonus_jsoo Ezjs_min.t -> caracteristique_ou_bonus = fun v ->
    try (caracteristique_of_jsoo v :> caracteristique_ou_bonus) with _ ->
    (bonus_of_jsoo v :> caracteristique_ou_bonus)
  let caracteristique_ou_bonus_jsoo_conv = caracteristique_ou_bonus_to_jsoo, caracteristique_ou_bonus_of_jsoo
]

type valeur = [
  | `int of int
  | `car of caracteristique
] [@@deriving encoding]

[@@@jsoo
  type valeur_jsoo = Ezjs_min.Unsafe.top
  let valeur_to_jsoo : valeur -> valeur_jsoo Ezjs_min.t = function
    | `int i -> Ezjs_min.Unsafe.inject i
    | `car c -> Ezjs_min.Unsafe.inject (caracteristique_to_jsoo c)
  let valeur_of_jsoo : valeur_jsoo Ezjs_min.t -> valeur = fun v ->
    try (`int (Float.to_int @@ Ezjs_min.float_of_number @@ Ezjs_min.Unsafe.coerce v))
    with _ -> `car (caracteristique_of_jsoo (Ezjs_min.Unsafe.coerce v))
  let valeur_jsoo_conv = valeur_to_jsoo, valeur_of_jsoo
]

type avec_valeur = { id: caracteristique_ou_bonus; valeur: valeur; opt: bool option option } [@@deriving encoding, jsoo]
type avec_nom = string * avec_valeur [@@deriving encoding, jsoo]

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

type genre_points = [
  | `points_de_vigueur
  | `des_de_recuperation
  | `points_de_chance
  | `points_de_mana
] [@@deriving encoding, jsoo]

type dommage_type = [
  | `contondants
  | `perforants
  | `tranchants
] [@@deriving encoding, jsoo]

type arme_type =
  | Contact of { deux_mains: (int * de) option }
  | Distance of { portee: int; nombre: int option }
[@@deriving encoding, jsoo]

type equipement =
  | Arme of {
    arme: arme_type list; [@dft [Contact {deux_mains=None}]] dommage: int * de;
    prix: string option; typ: dommage_type [@key "type"]; notes: string option }
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
  | Limitee [@key "limitée"]
  | Attaque [@key "attaque"]
  | Gratuite [@key "gratuite"]
  | Mouvement [@key "mouvement"]
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
  bonus: avec_valeur list; [@dft []]
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
  courant: int; [@mutable]
  max: int;
} [@@deriving encoding, jsoo]

type equipement_et_nombre = equipement_nom * int option [@@deriving encoding, jsoo]

type personnage = {
  nom: string;
  niveau: int;
  famille: famille;
  profil: profil;
  peuple: peuple;
  caracteristiques_base: caracteristiques;
  caracteristiques: caracteristiques;
  points_de_vigueur: points_avec_max;
  des_de_recuperation: points_avec_max;
  points_de_chance: points_avec_max;
  points_de_mana: points_avec_max;
  initiative: int;
  defense: int;
  equipements: equipement_et_nombre list; [@dft []]
  ideal: ideal option;
  travers: travers option;
  description: description;
  voies: (voie_type * int) list; [@dft []]
  bonuses: avec_nom list;
} [@@deriving encoding, jsoo]

let caracteristiques_par_defaut peuple = match peuple with
  | None ->
    { agilite=0; constitution=0; force=0; perception=0; charisme=0;
      intelligence=0; volonte=0 }
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
  caracteristiques=caracteristiques_par_defaut None;
  caracteristiques_base=caracteristiques_par_defaut (Some Demi_elfe);
  points_de_vigueur=points_vide;
  des_de_recuperation=points_vide; points_de_chance=points_vide; points_de_mana=points_vide;
  initiative=0; defense=0; equipements=[]; ideal=None; travers=None; description=`Null;
  voies=[]; bonuses=[];
}

let profils () =
  List.map (fun (_, f) -> match f with
    | Aventuriers -> f, List.map snd profil_aventurier_assoc
    | Combattants -> f, List.map snd profil_combattant_assoc
    | Mages -> f, List.map snd profil_mage_assoc
    | Mystiques -> f, List.map snd profil_mystique_assoc
  ) famille_assoc

let bonuses_peuple p c =
  let nom, l = match p with
    | Demi_elfe -> "demi_elfe", [
      [ `PER, 1; `FOR, -1 ]; [ `PER, 1; `CON, -1 ];
      [ `CHA, 1; `FOR, -1 ]; [ `CHA, 1; `CON, -1 ];
    ]
    | Demi_orc -> "demi_orc", [
      [ `FOR, 1; `CHA, -1 ]; [ `FOR, 1; `INT, -1 ];
      [ `CON, 1; `CHA, -1 ]; [ `CON, 1; `INT, -1 ];
    ]
    | Elfe_haut -> "elfe_haut", [
      [ `INT, 1; `FOR, -1 ]; [ `CHA, 1; `FOR, -1 ];
    ]
    | Elfe_sylvain -> "elfe_sylvain", [
      [ `AGI, 1; `FOR, -1 ]; [ `PER, 1; `FOR, -1 ];
    ]
    | Gnome -> "gnome", [
      [ `INT, 1; `FOR, -1 ]; [ `PER, 1; `FOR, -1 ];
    ]
    | Halfelin -> "halfelin", [
      [ `AGI, 1; `FOR, -1 ]; [ `VOL, 1; `FOR, -1 ];
    ]
    | Humain ->
      let aux acc i =
        let acc = if c.agilite = i then `AGI :: acc else acc in
        let acc = if c.constitution = i then `CON :: acc else acc in
        let acc = if c.force = i then `FOR :: acc else acc in
        let acc = if c.perception = i then `PER :: acc else acc in
        let acc = if c.charisme = i then `CHA :: acc else acc in
        let acc = if c.intelligence = i then `INT :: acc else acc in
        let acc = if c.volonte = i then `VOL :: acc else acc in
        acc in
      let l = aux [] (-1) in
      let l = if List.length l >= 2 then l else aux l 0 in
      "humain", List.map (fun c -> [c, 1]) l
    | Nain -> "nain", [
        [ `CON, 1; `AGI, -1 ]; [ `VOL, 1; `AGI, -1 ];
      ] in
  List.map (fun l -> List.map (fun (id, v) -> nom, {id; valeur=`int v; opt=None}) l) l

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
  List.fold_left (fun acc (_, {id; valeur; _}) ->
    match valeur with
    | `car _ -> acc
    | `int v ->
      let v = factor * v in
      match id with
      | `AGI -> { acc with agilite = acc.agilite + v }
      | `CON -> { acc with constitution = acc.constitution + v }
      | `FOR -> { acc with force = acc.force + v }
      | `PER -> { acc with perception = acc.perception + v }
      | `CHA -> { acc with charisme = acc.charisme + v }
      | `INT -> { acc with intelligence = acc.intelligence + v }
      | `VOL -> { acc with volonte = acc.volonte + v }
      | _ -> acc
  ) c l

let ajoute_bonus ?(factor=1) p l =
  List.fold_left (fun acc (_, {id; valeur; _}) ->
    let v = factor * (match valeur with
      | `int i -> i
      | `car c -> match c with
        | `AGI -> acc.caracteristiques.agilite
        | `CON -> acc.caracteristiques.constitution
        | `FOR -> acc.caracteristiques.force
        | `PER -> acc.caracteristiques.perception
        | `CHA -> acc.caracteristiques.charisme
        | `INT -> acc.caracteristiques.intelligence
        | `VOL -> acc.caracteristiques.volonte) in
    match id with
    | `DEF -> { acc with defense = acc.defense + v }
    | `INI -> { acc with initiative = acc.initiative + v }
    | `WEA -> acc (* todo *)
    | `PV -> { acc with points_de_vigueur = { max = acc.points_de_vigueur.max + v; courant = acc.points_de_vigueur.courant + v } }
    | `PC -> { acc with points_de_chance = { max = acc.points_de_chance.max + v; courant = acc.points_de_chance.courant + v } }
    | `PR -> { acc with des_de_recuperation = { max = acc.des_de_recuperation.max + v; courant = acc.des_de_recuperation.courant + v } }
    | `PM -> { acc with points_de_mana = { max = acc.points_de_mana.max + v; courant = acc.points_de_mana.courant + v } }
    | _ -> acc
  ) p l

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
    match p.profil, p.peuple, v with
    | _, Demi_elfe, (`Elfe_haut | `Elfe_sylvain | `Humain)
    | _, Demi_orc, `Demi_orc
    | _, Elfe_haut, `Elfe_haut
    | _, Elfe_sylvain, `Elfe_sylvain
    | _, Gnome, `Gnome
    | _, Halfelin, `Halfelin
    | _, Humain, `Humain
    | _, Nain, `Nain
    | `Arquebusier, _, #voie_arquebusier
    | `Barde, _, #voie_barde
    | `Rodeur, _, #voie_rodeur
    | `Voleur, _, #voie_voleur
    | `Barbare, _, #voie_barbare
    | `Chevalier, _, #voie_chevalier
    | `Guerrier, _, #voie_guerrier
    | `Ensorceleur, _, (#voie_ensorceleur | #voie_mage)
    | `Forgesort, _, (#voie_forgesort | #voie_mage)
    | `Magicien, _, (#voie_magicien | #voie_mage)
    | `Sorcier, _, (#voie_sorcier | #voie_mage)
    | `Druide, _, #voie_druide
    | `Moine, _, #voie_moine
    | `Pretre, _, #voie_pretre -> true
    | _ -> false) true voies in
  let peuple_mage_check = (rg_mage = 0 && rg_peuple >= 1) || (rg_mage >= 1 && rg_peuple = 1) in
  if not profil_check then Some "voie pas dans le profil" else
  if not niveau_capacite_check then Some "rang de capacite trop haut" else
  if not peuple_mage_check then Some "voie de peuple et mage ne peuvent pas être suivies ensemble" else
  if points_capacite > points_niveau then Some "trop de capacites" else
  None

let remplit_caracteristiques p def_equipement agi_max  =
  let caracteristiques = ajoute_caracteristiques p.caracteristiques_base p.bonuses in
  let p = { p with caracteristiques } in
  let aux p max = let courant = if p.courant = 0 || p.courant = p.max then max else p.courant in { courant; max } in
  let points_de_vigueur = aux p.points_de_vigueur @@ p.caracteristiques.constitution + p.niveau * (match p.famille with
    | Aventuriers -> 8
    | Combattants -> 10
    | Mages -> 6
    | Mystiques -> 8) in
  let des_de_recuperation = aux p.des_de_recuperation @@ 2 + p.caracteristiques.constitution + (match p.famille with
    | Mystiques -> 1 | _ -> 0) in
  let points_de_chance = aux p.points_de_chance @@ 2 + p.caracteristiques.charisme + (match p.famille with
    | Aventuriers -> 1 | _ -> 0) in
  let initiative = 10 + p.caracteristiques.perception in
  let defense = 10 + min agi_max p.caracteristiques.agilite + def_equipement in
  let points_de_mana0 = p.points_de_mana in
  let p = { p with points_de_vigueur; des_de_recuperation; points_de_chance;
                   initiative; defense; points_de_mana={courant=0; max=0} } in
  let p = ajoute_bonus p p.bonuses in
  let points_de_mana = aux points_de_mana0 @@ if p.points_de_mana.max = 0 then 0 else p.points_de_mana.max + p.caracteristiques.volonte in
  { p with points_de_mana }

let equipements_profil : profil -> (equipement_nom * int option) list list = function
  | `Arquebusier ->
    [ [`petoire, None]; [`epee_longue, None]; [`dague, None]; [`cuir_renforce, None] ]
  | `Barde ->
    [ [`rapiere, None]; [`dague, None]; [`autre "instrument_de_musique", None]; [`cuir_simple, None] ]
  | `Rodeur ->
    [ [`epee_longue, None]; [`dague, None]; [`cuir_renforce, None];
      [`arc_court, None; `epee_courte, None; `hachette, None; `lance, None] ]
  | `Voleur ->
    [ [`rapiere, None]; [`dague, Some 5 ]; [`autre "outils de crochetage", None];
      [`cuir_simple, None]; [`autre "corde", None] ]
  | `Barbare ->
    [ [`hache_a_deux_mains, None; `hache, None; `epee_longue, None; `epee_batarde, None ];
      [`javelot, Some 2]; [`dague, None]; [`cuir_simple, None] ]
  | `Chevalier ->
    [ [`epee_longue, None]; [`grand_bouclier, None]; [`lance_de_cavalerie, None];
      [`cotte_de_mailles, None] ]
  | `Guerrier ->
    [ [`epee_longue, None; `epee_a_deux_mains, None; `hache_a_deux_mains, None];
      [`dague, None; `hachette, None]; [`grand_bouclier, None]; [`chemise_de_mailles, None] ]
  | `Ensorceleur ->
    [ [`baton_ferre, None];
      [`dague, None; `epee_courte, None; `fleau, None; `marteau, None;
       `masse, None; `gourdin, None; `stylet, None; `arbalete_de_poing, None;
       `arbalete_legere, None; `arc_court, None; `couteaux_de_lancer, None;
       `fronde, None; `hachette, None; `javelot, None; `lance, None; `lance_pierre, None] ]
  | `Forgesort ->
    [ [`dague, None]; [`baton_ferre, None]; [`marteau, None] ]
  | `Magicien ->
    [ [`dague, None]; [`baton_ferre, None]; [`autre "grimoire de sorts", None] ]
  | `Sorcier ->
    [ [`dague, None]; [`baton_ferre, None]; [`autre "grimoire de sorts", None; `autre "parchemins anciens", None] ]
  | `Druide ->
    [ [`baton_ferre, None; `epieu, None]; [`dague, None]; [`arc_court, None]; [`cuir_simple, None] ]
  | `Moine -> [ [`baton_ferre, None] ]
  | `Pretre -> [ [`masse, None; `marteau, None; `baton_ferre, None]; [`petit_bouclier, None]; [`chemise_de_mailles, None] ]

let bonus_voies voies =
  let rec aux acc i rg l =
    if i > rg then acc else
    match l with
    | [] -> acc
    | (c: capacite) :: tl ->
      let acc = acc @ (List.map (fun x -> c.nom, x) c.bonus) in
      let acc = if c.sort then acc @ [ c.nom, { id=`PM; valeur=`int 1; opt=None } ] else acc in
      aux acc (i+1) rg tl in
   List.fold_left (fun acc (l, rg) ->
     aux acc 1 rg l
  ) [] voies

let de_recuperation = function
  | Aventuriers -> `d8
  | Combattants -> `d10
  | Mages -> `d6
  | Mystiques -> `d8
