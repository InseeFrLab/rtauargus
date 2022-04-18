# Précompilation en local pour Vignette nécessitant Tau-Argus
# Impossible à faire tourner dans l'intégration continue

knitr::knit(
  "vignettes/rtauargus.Rmd.orig",
  "vignettes/rtauargus.Rmd"
)
