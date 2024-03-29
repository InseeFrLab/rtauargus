% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_rtauargus.R
\name{tab_rtauargus}
\alias{tab_rtauargus}
\title{Protect one table by suppressing cells with Tau-Argus}
\usage{
tab_rtauargus(
  tabular,
  explanatory_vars,
  files_name = NULL,
  dir_name = NULL,
  totcode = getOption("rtauargus.totcode"),
  hrc = NULL,
  secret_var = NULL,
  secret_no_pl = NULL,
  cost_var = NULL,
  value = "value",
  freq = "freq",
  ip = 10,
  maxscore = NULL,
  suppress = "MOD(1,5,1,0,0)",
  safety_rules = paste0("MAN(", ip, ")"),
  show_batch_console = FALSE,
  output_type = 4,
  output_options = "",
  unif_labels = TRUE,
  split_tab = FALSE,
  nb_tab_option = "smart",
  limit = 14700,
  ...
)
}
\arguments{
\item{tabular}{data.frame which contains the tabulated data and
an additional boolean variable that indicates the primary secret of type boolean \cr
( data.frame contenant les données tabulées et
une variable supplémentaire indiquant le secret primaire de type booléen.)}

\item{explanatory_vars}{Vector of explanatory variables \cr
Variables catégorielles, sous forme  de vecteurs \cr
Example : \code{c("A21", "TREFF", "REG")} for a table crossing
\code{A21} x \code{TREFF} x \code{REG}
(Variable indiquant le secret primaire de type booléen:
prend la valeur "TRUE" quand les cellules du tableau doivent être masquées
par le secret primaire, "FALSE" sinon. Permet de créer un fichier d'apriori)}

\item{files_name}{string used to name all the files needed to process.
All files will have the same name, only their extension will be different.}

\item{dir_name}{string indicated the path of the directory in which to save
all the files (.rda, .hst, .txt, .arb, .csv) generated by the function.}

\item{totcode}{Code(s) which represent the total of a categorical variable
(see section 'Specific parameters' for this parameter's syntax).
If unspecified for a variable(neither by default nor explicitly)
it will be set to  \code{rtauargus.totcode}. \cr
(Code(s) pour le total d'une variable catégorielle (voir
section 'Specific parameters' pour la syntaxe de ce paramètre). Les
variables non spécifiées (ni par défaut, ni explicitement) se verront
attribuer la valeur de \code{rtauargus.totcode}.)}

\item{hrc}{Informations of hierarchical variables (see section
'Hierarchical variables'). \cr
(Informations sur les variables hiérarchiques (voir section
'Hierarchical variables').)
(Caractère qui, répété n fois, indique que la valeur est
à n niveaux de profondeur dans la hiérarchie.)}

\item{secret_var}{Nae of the boolean variable which specifies the secret, primary or not :
equal to "TRUE" if a cell is concerned by the secret,"FALSE" otherwise.
will  be exported in the apriori file. \cr
(Variable indiquant le secret  de type booléen:
prend la valeur "TRUE" quand les cellules du tableau doivent être masquées
"FALSE" sinon. Permet de créer un fichier d'apriori)}

\item{secret_no_pl}{name of a boolean variable which indicates the cells
on which the protection levels won't be applied. If \code{secret_no_pl = NULL}
(default), the protection levels are applied on each cell which gets a \code{TRUE}
status for the \code{secret_var}.\cr}

\item{cost_var}{Numeric variable allow to change the cost suppression of a cell
for secondary suppression, it's the value of the cell by default, can be
specified for each cell, fill with NA if the cost doesn't need to be changed
for all cells \cr
(Variable numeric qui permet de changer la coût de suppression d'une cellule,
pris en compte dans les algorithmes de secret secondaire.Par défaut le coût
correspond à la valeur de la cellule.  peut être spécifié pour chacune des cellules,
peut contenir des NA pour les coûts que l'on ne souhaite pas modifier.)
(nombre minimal de décimales à afficher (voir section 'Number of decimals').)}

\item{value}{Name of the column containing the value of the cells. \cr
(Nom de la colonne contenant la valeur des cellules)}

\item{freq}{Name of the column containing the cell frequency. \cr
(Nom de la colonne contenant les effectifs pour une cellule)}

\item{ip}{Value of the safety margin in \% (must be an integer).
(Valeur pour les intervalles de protection en \%, doit être entier )}

\item{maxscore}{Name of the column containing, the value of the largest
contributor of a cell. \cr
(Nom de la colonne contenant la valeur du plus gros contributeur
d'une cellule)}

\item{suppress}{Algortihm for secondary suppression (Tau-Argus batch syntax), and the
parameters for it.\cr
( Algorithme de gestion du secret secondaire
(syntaxe batch de Tau-Argus), ainsi que les potentiels paramètres associés)}

\item{safety_rules}{Rules for primary suppression with Argus syntax, if the primary suppression
has been dealt with an apriori file specify manual safety range :"MAN(10)"
for example.\cr
( Règle(s) de secret primaire.
Chaîne de caractères en syntaxe batch Tau-Argus. Si le secret primaire
a été traité dans un fichier d'apriori : utiliser "MAN(10)")}

\item{show_batch_console}{to display the batch progress in the
console. \cr
(pour afficher le déroulement du batch dans la
console.)}

\item{output_type}{Type of the output file (Argus codification)
By default \code{"2"} (csv for pivot-table).
For SBS files use \code{"4"}\cr
(Format des fichiers en sortie (codification Tau-Argus).
Valeur par défaut du package : \code{"2"} (csv for pivot-table).
Pour le format SBS utiliser \code{"4"})}

\item{output_options}{Additionnal parameter for the output,
by default : code{"AS+"} (print Status). To specify no options : \code{""}.\cr
(Options supplémentaires des fichiers en sortie. Valeur
par défaut du package : \code{"AS+"} (affichage du statut). Pour ne
spécifier aucune option, \code{""}.)}

\item{unif_labels}{boolean, if explanatory variables have to be standardized}

\item{split_tab}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} boolean,
whether to reduce dimension to 3 while treating a table of dimension 4 or 5
(default to \code{FALSE})}

\item{nb_tab_option}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} strategy to follow
to choose variables automatically while splitting:
\itemize{
\item{\code{"min"}: minimize the number of tables;}
\item{\code{"max"}: maximize the number of tables;}
\item{\code{"smart"}: minimize the number of tables under the constraint
of their row count.}
}}

\item{limit}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} numeric, used to choose
which variable to merge (if nb_tab_option = 'smart')
and split table with a number of row above this limit in order to avoid
tauargus failures}

\item{...}{any parameter of the tab_rda, tab_arb or run_arb functions, relevant
for the treatment of tabular.}
}
\value{
If output_type equals to 4 and split_tab = FALSE,
then the original tabular is returned with a new
column called Status, indicating the status of the cell coming from Tau-Argus :
"A" for a primary secret due to frequency rule, "B" for a primary secret due
to dominance rule, "D" for secondary secret and "V" for no secret cell.

If split_tab = TRUE,
then the original tabular is returned with some new columns which are boolean
variables indicating the status of a cell at each iteration of the protection
process as we get with \code{tab_multi_manager()} function. \code{TRUE}
denotes a cell that have to be suppressed. The last column is then the
final status of the suppression process of the original table.

If \code{split_tab = FALSE} and  \code{output_type} doesn't equal to \code{4},
then the raw result from tau-argus is returned.
}
\description{
The function prepares all the files needed by Tau-Argus and launches the
software with the good settings and gets back the result.
}
\section{Standardization of explanatory variables and hierarchies}{


The boolean argument \code{unif_labels} is useful to
prevent some common errors in using Tau-Argus. Indeed, Tau-Argus needs that,
within a same level of a hierarchy, the labels have the same number of
characters. When the argument is set to TRUE, \code{tab_rtauargus}
standardizes the explanatory variables to prevent this issue.
Hierarchical explanatory variables (explanatory variables associated to
a hrc file) are then modified in the tabular data and an another hrc file is
created to be relevant with the tabular. In the output, these modifications
are removed.
}

\examples{
\dontrun{
library(dplyr)
data(turnover_act_size)

# Prepare data with primary secret ----
turnover_act_size <- turnover_act_size \%>\%
  mutate(
    is_secret_freq = N_OBS > 0 & N_OBS < 3,
    is_secret_dom = ifelse(MAX == 0, FALSE, MAX/TOT>0.85),
    is_secret_prim = is_secret_freq | is_secret_dom
  )

# Make hrc file of business sectors ----
data(activity_corr_table)
hrc_file_activity <- activity_corr_table \%>\%
  write_hrc2(file_name = "hrc/activity")

# Compute the secondary secret ----
options(
  rtauargus.tauargus_exe =
    "Y:/Logiciels/TauArgus/TauArgus4.2.3/TauArgus.exe"
)

res <- tab_rtauargus(
  tabular = turnover_act_size,
  files_name = "turn_act_size",
  dir_name = "tauargus_files",
  explanatory_vars = c("ACTIVITY", "SIZE"),
  hrc = c(ACTIVITY = hrc_file_activity),
  totcode = c(ACTIVITY = "Total", SIZE = "Total"),
  secret_var = "is_secret_prim",
  value = "TOT",
  freq = "N_OBS",
  verbose = FALSE
)

# Reduce dims feature

data(datatest1)
res_dim4 <- tab_rtauargus(
  tabular = datatest1,
  dir_name = "tauargus_files",
  explanatory_vars = c("A10", "treff","type_distrib","cj"),
  totcode = rep("Total", 4),
  secret_var = "is_secret_prim",
  value = "pizzas_tot_abs",
  freq = "nb_obs_rnd",
  split_tab = TRUE
)
}
}
