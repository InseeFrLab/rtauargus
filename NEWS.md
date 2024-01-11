---
title: ‘rtauargus’ package / Package ‘rtauargus’
subtitle: History of changes / Historique des modifications
output: rmarkdown::html_vignette
---


## rtauargus 1.2.0

[01/2024]

* Implementation of a method to tackle some tables of 4/5 dimensions. 

> The method is quickly explained and its use is shown in a specific vignette.
> A paper explaining more deeply the idea and the modus operandi is available 
here: "https://github.com/InseeFrLab/dims_reduction_tables_workshop_20231215".

* Implementation of the function `tabulate_micro_data()` to compute tabular data from 
a microdata file.

> The function can create frequency and magnitude tabular data with hierarchical variables. 
The tabular data computed contains the information to compute primary secret 
according to frequency rule and (1,k)-dominance rule.

* Resolution of a malfunction while dealing with costs.

* **rtauargus()** function has been renamed more properly as **micro_rtauargus()**.
Its arguments remain the same.

## rtauargus 1.1.2

[01/02/2023]

* Improvement in handling Interval Protection Level while using **tab_multi_manager()**:

> The IP levels are now set automatically into the apriori files for actual Primary Suppressed Cells 
and only them. This ensures IP to be applied only for the actual primary secret and limits the 
propagation of suppressions due to IP manipulation. In some very particular cases, this can lead to 
significantly reduce  the suppressions.

## rtauargus 1.1.0

[01/10/2022]

* **tab_multi_manager()**: argument *alt_hrc* and *alt_totcode* to handle non-nested hierarchies.

## rtauargus 1.0.0

[01/09/2022]

* English documentation  
* **tab_multi_manager()**: function to protect several tables at once.  
* **tab_arb()** : argument *value* is now called *response_var* as in the **tab_rda()** function.  
* **tab_arb()** : argument *apriori* is now called *hst_filename* as in the **tab_rda()** function.  
* **write_hrc2()** : new function to creat a hrc file from a correspondence table  

## rtauargus 0.5.0

[18/04/2022]

* Ajout de fonctions pour gérer le secret directement sur des données tabulées.  
Addition of functions to manage confidentiality directly on tabular data.

## rtauargus 0.4.3

[13/10/2021]

* projet transféré dans le groupe `outilsconfidentialite`

## rtauargus 0.4.2

[17/12/2020]

* projet migré vers gitlab.insee.fr

## rtauargus 0.4.1

[18/10/2019]

* **rtauargus_plus()** : extension de la fonction `rtauargus` pour un grand
  nombre de croisements (ayant tous les mêmes caractéristiques). Réduit le temps
  d'exécution par rapport à la version "normale".

## rtauargus 0.4.0

[04/09/2019]

* **rtauargus()** peut désormais prendre en entrée un couple de fichiers asc et
  rda (au lieu d'un data.frame). Permet de lancer un traitement dans le cas
  où ces fichiers texte de microdonnées existent déjà.
* **run_arb()** :
    - nouveau nom de <font color="red">run_tauargus</font> (nom plus explicite,
      principalement pour éviter la confusion avec la fonction _rtauargus_).
      L'ancien nom reste pour l'instant utilisable : un message avertit du
      changement. Il sera remplacé par un message d'erreur dans une prochaine
      version.
    - vérifications préalables à l'exécution de &tau;-Argus :
        * existence du logiciel sur le poste ;
        * existence des fichiers asc et rda ;
        * présence des variables à utiliser (croisements, variable de réponse, 
          ...) dans les métadonnées (fichier rda) ;
        * existence des dossiers où vont être écrits les résultats. Si absents,
          possibilité de les créer automatiquement (paramètre `missing_dir` avec 
          option associée `rtauargus.missing_dir`).
* **import()** : si des fichiers _apriori_ ont été utilisés, stockage du nom de
  ces fichiers dans les métadonnées (attributs) de l'objet R créé
* améliorations de la documentation (rubriques d'aide et vignette)

## rtauargus 0.3.1

[11/07/2019]

* informe de l'absence de tauargus.exe au chargement du package
* diverses améliorations de la documentation (rubriques d'aide et vignette)
* utilise valeurs par défaut des options en cas d'effacement accidentel par
  l'utilisateur

## rtauargus 0.3.0

[29/03/2019]

* **micro_arb()** :
    - ajoute paramètre apriori (fichiers hst)
    - conserve les noms de tabulations en sortie si précisés dans
      `explanatory_vars`
* vérification plus rigoureuse de certains paramètres

## rtauargus 0.2.1

[03/12/2018]

* **write_hrc()** : prise en compte de davantage de cas particuliers
* améliore vignette

## rtauargus 0.2.0

[14/08/2018]

* **micro_arb()** :
    - implémente tableaux liés (paramètre **linked**)
    - corrige extensions de fichiers pour `"5"` (.tab) et `"6"` (.jj)
    - corrige bug quand un seul paramètre à suppress, par exemple `MOD(1)`
* **write_hrc()** : nouvelle fonction  créant un fichier .hrc à partir de 
  variables hiérarchiques présentes dans les microdonnées 
* **micro_asc_rda()** :
    - implémente paramètres **missing**, **totcode** et **codelist**
    - simplifie syntaxe pour paramètre **hrc** (cohérente avec nouveaux
      paramètres)
    - nouveau paramètre **hierleadstring** (valeur par défaut dans l'option de
      package **rtauargus.hierleadstring**)
* efface les options du package à son déchargement
* corrections diverses documentation

## rtauargus 0.1.0

[01/08/2018]

* Première version stable.
