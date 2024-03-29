% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sp_var_to_merge.R
\name{nb_tab_generated}
\alias{nb_tab_generated}
\title{Calculate the number of tables generated when merging 3 variables
in the transition from 5 to 3 dimensions}
\usage{
nb_tab_generated(v1, v2, v3 = NULL, v4 = NULL, hrcfiles = NULL, data = NULL)
}
\arguments{
\item{v1}{first variable to be merged}

\item{v2}{second variable to be merged}

\item{v3}{third variable to be merged (
variable that will be merged with v1 and v2 if v4 is not specified)}

\item{v4}{fourth variable to be merged (with v3)}

\item{hrcfiles}{named list of hrc files}

\item{data}{data.frame (used only in the case where a trio is formed)}
}
\value{
an integer representing the number of tables generated
}
\description{
Calculate the number of tables generated when merging 3 variables
in the transition from 5 to 3 dimensions
}
\examples{
# Dimension 4
library(dplyr)
data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
  GEO = c("Total", "G1", "G2"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2"),
  stringsAsFactors = FALSE
) \%>\%
  as.data.frame()

data <- data \%>\% mutate(VALUE = 1)

hrc_act <- "hrc_ACT.hrc"

sdcHierarchies::hier_create(root = "Total", nodes = c("A", "B")) \%>\%
  sdcHierarchies::hier_add(root = "A", nodes = c("A1", "A2")) \%>\%
  sdcHierarchies::hier_add(root = "B", nodes = c("B1", "B2")) \%>\%
  sdcHierarchies::hier_convert(as = "argus") \%>\%
  slice(-1) \%>\%
  mutate(levels = substring(paste0(level, name), 3)) \%>\%
  select(levels) \%>\%
  write.table(file = hrc_act, row.names = FALSE, col.names = FALSE, quote = FALSE)

# 1 pair created
nb_tab_generated(v1 = "ACT", v2 = "GEO",
                hrcfiles = c(ACT = hrc_act))

# Dimension 5
data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
  GEO = c("Total", "GA", "GB", "GA1", "GA2", "GB1", "GB2"),
  SEX = c("Total", "F", "M", "F1", "F2", "M1", "M2"),
  AGE = c("Total", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"),
  ECO = c("PIB", "Ménages", "Entreprises"),
  stringsAsFactors = FALSE,
  KEEP.OUT.ATTRS = FALSE
) \%>\%
  as.data.frame()

data <- data \%>\% mutate(VALUE = 1:n())

hrc_act <- "hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("A", "B")) \%>\%
  sdcHierarchies::hier_add(root = "A", nodes = c("A1", "A2")) \%>\%
  sdcHierarchies::hier_convert(as = "argus") \%>\%
  slice(-1) \%>\%
  mutate(levels = substring(paste0(level, name), 3)) \%>\%
  select(levels) \%>\%
  write.table(file = hrc_act, row.names = FALSE, col.names = FALSE, quote = FALSE)

hrc_geo <- "hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("GA", "GB")) \%>\%
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1", "GA2")) \%>\%
  sdcHierarchies::hier_add(root = "GB", nodes = c("GB1", "GB2")) \%>\%
  sdcHierarchies::hier_convert(as = "argus") \%>\%
  slice(-1) \%>\%
  mutate(levels = substring(paste0(level, name), 3)) \%>\%
  select(levels) \%>\%
  write.table(file = hrc_geo, row.names = FALSE, col.names = FALSE, quote = FALSE)

hrc_sex <- "hrc_SEX.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("F", "M")) \%>\%
  sdcHierarchies::hier_add(root = "F", nodes = c("F1", "F2")) \%>\%
  sdcHierarchies::hier_add(root = "M", nodes = c("M1", "M2")) \%>\%
  sdcHierarchies::hier_convert(as = "argus") \%>\%
  slice(-1) \%>\%
  mutate(levels = substring(paste0(level, name), 3)) \%>\%
  select(levels) \%>\%
  write.table(file = hrc_sex, row.names = FALSE, col.names = FALSE, quote = FALSE)

# Trio merged
nb_tab_generated(data = data,
                v1 = "ACT", v2 = "GEO", v3 = "SEX",
                hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, SEX = hrc_sex))

# 2 pairs created
nb_tab_generated(v1 = "ACT", v2 = "GEO",
                v3 = "SEX", v4 = "EXO",
                hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, SEX = hrc_sex))
}
\keyword{internal}
