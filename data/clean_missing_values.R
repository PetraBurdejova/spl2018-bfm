
DiagMissingValues <- function(df, verbose = FALSE) {
  # Checks for NA values in dataframe and prints information.
  #
  # Args:
  #   df: The dataframe that will be checked for missing values.
  #   verbose: Print more detailed information.
  #
  # Returns:
  #   A list of indices that correspond to the possiton of missing values in df.

  name <- deparse(substitute(df))  # get name of dataframe

  r <- is.na(df)
  cc <- complete.cases(df)

  # Diagnostics Output
  cat(sprintf("---- NA Diagnostics for '%s' ----\n", name))
  cat(sprintf("Number of complete cases: %i of %i.\n", 
              sum(cc), nrow(df) ))
  cat(sprintf("Number of incomplete cases: %i (%.3f%%).\n",
              (nrow(df)-sum(cc)), (1-sum(cc)/nrow(df))*100) )

  if (verbose == TRUE) {
    cat(sprintf("NA's per column:\n\n"))
    print(apply(r, 2, sum))
    cat("\n")

    i <- 1
    for (col in names(df)){
      cat(sprintf("NA's in at least %i columns: %i\n",
                  i, sum(apply(r, 1, sum) >= i) ))
      i <- i+1
    }

  }

}
