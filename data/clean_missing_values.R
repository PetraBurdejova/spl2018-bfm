
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

}


# Function for Time Frame
time.FRAME <- function(x) {
  # Chooses Time frame for all variables
  #
  # Args:
  #   x: Imported dataframe
  #
  # Returns:
  #   y: Dataframe with right time frame
  start.d <- ymd_hm("2015-01-01 00:00")
  stop.d <- ymd_hm("2017-12-31 23:00")
  ind.start <- which(x$TIME == start.d)
  ind.stop <- which(x$TIME == stop.d)
  ind <- (ind.start: ind.stop)
  y <- x[ind, ]
}
