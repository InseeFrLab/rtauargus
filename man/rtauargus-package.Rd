\docType{package}
\name{rtauargus-package}
\alias{rtauargus-package}
\title{Package 'rtauargus' : Tau Argus depuis R}

\description{

Le package \strong{rtauargus} permet de secrétiser des données tabulées avec
Tau Argus depuis R.

}

\details{

Le package a été développé sur la base des versions open source de Tau Argus
(versions 4.1 et supérieures).

Ses fonctionnalités principales sont les suivantes :\itemize{
 \item{créer les inputs (fichiers asc et rda) ;}
 \item{générer la séquence d'instructions à exécuter en mode batch
   (fichier arb) ;}
 \item{lancer le batch Tau Argus en ligne de commande ;}
 \item{récupérer les résultats dans R.}
}
Ces différentes étapes peuvent être appelées de manière modulaire ou en une
seule fois.

}

\section{TauArgus.exe}{

La fonction \code{\link{run_tauargus}} doit connaître l'emplacement du
programme Tau Argus sur le disque. Cet emplacement est renseigné de manière
globale via l'option \code{rtauargus_exe}. Il perdure ainsi pour la session R
courante.

Au chargement du package, si \code{rtauargus_exe} n'est pas déjà renseigné, une
valeur par défaut lui est attribuée: l'emplacement de la version portable open
source sur AUS (\code{Y:/Logiciels/Tau/TauArgus.exe}). Utiliser
\code{\link{set_tauargus_exe}} pour changer cet emplacement (pour la durée de la
session R).

Pour définir un emplacement qui sera conservé pour un utilisateur, il est
possible de placer une instruction
\code{options(rtauargus_exe = "chemin/vers/TauArgus.exe")}
dans un fichier de configuration de R (.Rprofile...).

La fonction (expérimentatle) \code{\link{tauargus_info}} tente de récupérer
certaines infos associées au programme Tau Argus.

}

\references{

Tau Argus : \itemize{

  \item{\href{http://neon.vb.cbs.nl/casc/tau.htm}{téléchargement et documentation}
  (sur le site de l'office néerlandais de statistiques)}

  \item{\href{https://github.com/sdcTools}{code source et support}
  (sur github.com)}

}

}
