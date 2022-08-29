# Précompilation en local pour Vignette nécessitant Tau-Argus
# Impossible à faire tourner dans l'intégration continue

knitr::knit(
  "vignettes/rtauargus.Rmd.orig",
  "vignettes/rtauargus.Rmd"
)

knitr::knit(
  "vignettes/rtauargus_micro.Rmd.orig",
  "vignettes/rtauargus_micro.Rmd"
)

knitr::knit(
  "vignettes/options_safety_rules.Rmd.orig",
  "vignettes/options_safety_rules.Rmd"
)
