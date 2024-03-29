% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{turnover_nuts_cj}
\alias{turnover_nuts_cj}
\title{data - Turnover broken down by NUTS and size of French companies (fake values).}
\format{
A tibble/data frame with 452 rows and 5 variables:
\describe{
\item{NUTS}{nuts - european denomination of administrative levels.
Hierarchical variables with two levels (nuts2 and nuts3) described
in the nuts23_fr_corr_table dataset. The root is noted "Total"}
\item{CJ}{Type of companies (3 categories + overall category "Total")}
\item{N_OBS}{Frequency, number of companies}
\item{TOT}{turnover value in euros}
\item{MAX}{turnover of the company which contributes the most to the cell.}
}
}
\usage{
turnover_nuts_cj
}
\description{
A tabular dataset containing the turnover broken down by NUTS geographical localisation
and Type of companies. Useful for playing with tab_ functions.
}
\seealso{
nuts23_fr_corr_table
}
\keyword{datasets}
