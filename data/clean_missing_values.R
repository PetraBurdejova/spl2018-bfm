
DiagMissingValues <- function(df, verbose = FALSE, days = FALSE) {
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

  name <- deparse(substitute(df))  # get name of dataframe

  r <- is.na(df)
  cc <- complete.cases(df)
  
  # Diagnostics Output
  cat(sprintf("\n---- NA Diagnostics for '%s' ----\n", name))
  cat(sprintf("Number of complete cases: %i of %i.\n", 
              sum(cc), nrow(df) ))
  cat(sprintf("Number of incomplete cases: %i (%.3f%%).\n\n\n",
              (nrow(df)-sum(cc)), (1-sum(cc)/nrow(df)) ))
  cat(sprintf("NA's per column:\n\n"))
  print(apply(r, 2, sum))
  cat("\n")
  
  i <- 1
  for (col in names(df)){
    cat(sprintf("NA's in at least %i columns: %i\n",
                i, sum(apply(r, 1, sum) >= i) ))
    i <- i+1
  }

  # TODO: Add regression tests.

}
