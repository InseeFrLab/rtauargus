% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_in_clusters.R
\name{split_dataframe}
\alias{split_dataframe}
\title{Split a Data Frame Based on a Chosen Variable}
\usage{
split_dataframe(tab, var, hrc_var)
}
\arguments{
\item{tab}{A data frame to be split.}

\item{var}{The variable to base the split on.}

\item{hrc_var}{The hierarchical variable associated with \code{var}. If \code{hrc_var}
is present, the data frame will first be split by its values; otherwise,
it will be split directly by \code{var}.}
}
\value{
A list of data frames (\code{list_diff}), where each element corresponds
to a grouping determined by \code{var} and/or \code{hrc_var}.
}
\description{
This function splits a data frame into a list of smaller data frames based on
a specified variable (\code{var}) and its associated hierarchical variable (\code{hrc_var}).
The function handles cases where \code{hrc_var} contains character values or is missing,
ensuring that independent and hierarchical groupings are managed appropriately.
}
\examples{
\dontrun{
# Example data
tab <- data.frame(
  id = 1:6,
  var = c("A", "A", "B", "B", "C", "C"),
  hrc_var = c(NA, "H1", "H1", "H2", NA, NA),
  value = c(10, 20, 30, 40, 50, 60)
)

# Split by 'var' and 'hrc_var'
list_diff <- split_dataframe(tab, "var", "hrc_var")

# View the structure of the result
str(list_diff)
}
}
