# fonction transformant une liste de parametres associes a des
# variables en un data.frame

df_param <- function(list_param) {

  res <-
    data.frame(
      colname = names(list_param),
      stringsAsFactors = FALSE,
      row.names = names(list_param)
    )

  for (i in seq_along(list_param)) {
    curvar <- names(list_param)[i]
    curobj <- list_param[[i]]
    for (j in seq_along(curobj)) {
      curpar <- names(list_param[[i]])[j]
      res[curvar, curpar] <- curobj[j]
    }
  }

  res

}

# fonction normalizePath sans warning
normPath2 <- function(path, ...) suppressWarnings(normalizePath(path, ...))

# lien parametre extension
output_extensions <- c(
  "1" = ".csv",
  "2" = ".csv",
  "3" = ".txt",
  "4" = ".sbs",
  "5" = ".txt",
  "6" = ".txt"
)
