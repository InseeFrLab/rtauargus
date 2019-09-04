\docType{package}
\name{rtauargus-package}
\alias{rtauargus-package}
\title{Package 'rtauargus' : Tau-Argus depuis R}

\description{

Le package \emph{rtauargus} offre une interface \strong{R} pour
\strong{Tau-Argus}.

}

\details{

Le package permet d'intégrer à une chaîne de traitement R les tâches réalisées
par Tau-Argus. Ses fonctionnalités principales sont de :

\itemize{
  \item{créer les inputs (fichiers asc et rda) à partir de données au format
     R ;}
  \item{générer la séquence d'instructions à exécuter en mode batch
    (fichier arb) ;}
  \item{lancer le batch Tau-Argus en ligne de commande ;}
  \item{récupérer les résultats dans R.}
}

Ces différentes opérations peuvent être exécutées en une seule fois, mais aussi
de manière modulaire.

La syntaxe de certains arguments colle au plus près à la syntaxe batch de
Tau-Argus. Ceci permet d’utiliser un grand nombre de fonctionnalités sans
multiplier les arguments des fonctions. Le package pourra en outre s’adapter
plus facilement à d’éventuelles modifications du logiciel (nouvelles méthodes
disponibles, options supplémentaires…). Les règles de syntaxe pour l’écriture de
batch figurent dans le manuel de référence de Tau-Argus.

}

\section{Précautions d'usage}{

Le package a été développé sur la base des versions open source de Tau-Argus
(versions 4.1 et supérieures), en particulier la dernière version disponible
lors du développement (4.1.7). \strong{Il n'est pas compatible avec la version
3.5} (du fait notamment de l'utilisation d'options supplémentaires dans
<WRITETABLE> en mode batch).

Pour l'instant, \strong{seules les microdonnées sont acceptées} en entrée
(pas les données déjà tabulées).

}

\section{Emplacement de TauArgus.exe}{

La fonction \code{\link{run_arb}} doit connaître l'emplacement du
programme Tau-Argus sur le disque. Cet emplacement est renseigné de manière
globale via l'option \code{rtauargus.tauargus_exe}. Il perdure ainsi pour la
session R courante.

Au chargement du package, si \code{rtauargus.tauargus_exe} n'est pas déjà
renseigné, une valeur par défaut lui est attribuée. Pour changer cet
emplacement, exécuter l'instruction :

\code{options(rtauargus.tauargus_exe = "chemin/vers/TauArgus.exe")}

Placer cette instruction dans un programme pour que l'emplacement soit conservé
uniquement pour la session. Pour qu'il soit conservé de manière permanente pour
un utilisateur, placer l'instruction dans un fichier de configuration de R
(.Rprofile...).

La fonction (expérimentale) \code{\link{tauargus_info}} tente de récupérer
le numéro de version et la date de parution du programme Tau-Argus utilisé par
rtauargus.

}

\section{Autres options}{

En plus de \code{rtauargus.tauargus_exe}, d'autres options permettent de
paramétrer des comportements par défaut.

Leurs noms commencent par \code{"rtauargus."}. La partie après le préfixe
correspond à un nom de paramètre d'une fonction. Par exemple,
\code{rtauargus.decimals} sera la valeur utilisée si le paramètre
\code{decimals} de la fonction \code{micro_asc_rda} n'est pas renseigné par
l'utilisateur.

Voir \code{\link{rtauargus_options}} pour plus de détails.
}

\references{

Tau-Argus : \itemize{

  \item{\href{http://neon.vb.cbs.nl/casc/tau.htm}{téléchargement et documentation}
  (sur le site de l'office néerlandais de statistiques)}

  \item{\href{https://github.com/sdcTools}{code source et support}
  (sur github.com)}

}

}
