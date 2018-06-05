### --------- ###
# - FUNCTIONS - #
# ------------- #

findMissingValues <- function(df, verbose = FALSE, days = FALSE) {
  # Checks for NA values in dataframe and prints information. Returns a list of
  # indices of the NA entries in dataframe.
  #
  # Args:
  #   df: The dataframe that will be checked for missing values.
  #   verbose: If TRUE, prints information on the missing values such as num-
  #            ber and percentage of missing values, as well as number of days
  #            affected.
  #   days: If TRUE, returns a list of days with at least one missing value.
  #
  # Returns:
  #   A list of indices that correspond to the possiton of missing values in df.

  indices <- which(is.na.data.frame(df))

  if (verbose == TRUE) {

    name <- deparse(substitute(df))  # get name of dataframe
    no_na <- length(indices)  # number of na in df
    entries <- nrow(df)  # number of values in df

    print(sprintf("Checking for missing values in dataframe '%s'.", name))
    print(sprintf("%.0f of %.0f (%.3f%%) values are NA.",
                  no_na, entries, no_na/entries))

  }

  if (days == TRUE) {

    naDays <- 0
    return(naDays)

  } else {

    return(indices)

  }

}
