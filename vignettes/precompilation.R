# Précompilation en local pour Vignette nécessitant Tau-Argus
# Impossible à faire tourner dans l'intégration continue

knitr::knit(
  "vignettes/rtauargus.Rmd.orig",
  "vignettes/rtauargus.Rmd",
  encoding = "UTF-8"
)

knitr::knit(
  "vignettes/protect_multi_tables.Rmd.orig",
  "vignettes/protect_multi_tables.Rmd",
  encoding = "UTF-8"
)

knitr::knit(
  "vignettes/rtauargus_micro.Rmd.orig",
  "vignettes/rtauargus_micro.Rmd",
  encoding = "UTF-8"
)

knitr::knit(
  "vignettes/options_safety_rules.Rmd.orig",
  "vignettes/options_safety_rules.Rmd",
  encoding = "UTF-8"
)

# knitr::knit(
#   "vignettes/split_tab.Rmd.orig",
#   "vignettes/split_tab.Rmd",
#   encoding = "UTF-8"
# )
# car plus de fichier

### Versions françaises

knitr::knit(
  "vignettes/rtauargus_fr.Rmd.orig",
  "vignettes/rtauargus_fr.Rmd",
  encoding = "UTF-8"
)

knitr::knit(
  "vignettes/protect_multi_tables_fr.Rmd.orig",
  "vignettes/protect_multi_tables_fr.Rmd",
  encoding = "UTF-8"
)

knitr::knit(
  "vignettes/rtauargus_micro_fr.Rmd.orig",
  "vignettes/rtauargus_micro_fr.Rmd",
  encoding = "UTF-8"
)

knitr::knit(
  "vignettes/options_safety_rules_fr.Rmd.orig",
  "vignettes/options_safety_rules_fr.Rmd",
  encoding = "UTF-8"
)

# Actuellement, la vignette necessite d'avoir crée des fichiers hrc sur son ordinateur
# il s'agit du jeu de données au début du bench 4D
# je mets donc en commentaire pour éviter de le lancer par erreur :
# le knit fonctionnerait, mais l'output ne seraitn pas le bon (bug silencieux)
# knitr::knit(
#   "vignettes/split_tab_fr.Rmd.orig",
#   "vignettes/split_tab_fr.Rmd",
#   encoding = "UTF-8"
# )

knitr::knit(
  "vignettes/auto_metadata_fr.Rmd.orig",
  "vignettes/auto_metadata_fr.Rmd",
  encoding = "UTF-8"
)

knitr::knit(
  "vignettes/auto_metadata.Rmd.orig",
  "vignettes/auto_metadata.Rmd",
  encoding = "UTF-8"
)
