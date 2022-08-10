#' Fake values of production of red vegetables by sector and size of companies.
#'
#' A tabluar dataset containing the turnover broken down by Activity
#' and Size of companies. Useful for playing with tab_ functions.
#'
#' @format A data frame with 1242 rows and 5 variables:
#' \describe{
#'   \item{ACTIVITY}{activity, hierarchical variables}
#'   \item{treff}{size of the companies (3 classes of number of employees)}
#'   \item{n_obs}{Frequency, number of companies}
#'   \item{tot}{turnover}
#'   \item{max}{turnover of the company which contributes the most to the cell.}
#' }
"red_vegetables"
