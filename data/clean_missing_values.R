
DiagMissingValues <- function(df, dlevel = 0) {
  # Checks for NA values in dataframe and prints information.
  #
  # Args:
  #   df: The dataframe that will be checked for missing values.
  #   dlevel: Print more detailed information. 0: No information.
  #           1: General information. 2: Information on columns.
  #
  # Returns:
  #   Returns a list of timepoints of incomplete cases in df.

  name <- deparse(substitute(df))  # get name of dataframe

  r <- is.na(df)
  cc <- complete.cases(df)

  if (dlevel >= 1) {
    # dlevel 1 diagnostics: general information.
    cat(sprintf("------ NA Diagnostics for '%s' ------\n", name))
    cat(sprintf("Number of complete cases: %i of %i.\n", 
                sum(cc), nrow(df) ))
    cat(sprintf("Number of incomplete cases: %i (%.3f%%).\n",
                (nrow(df)-sum(cc)), (1-sum(cc)/nrow(df))*100) )
  }

  if (dlevel >= 2) {
    # dlevel 2 diagnostics: information per column.
    print(apply(r, 2, sum))
    i <- 1
    for (col in names(df)){
      cat(sprintf("NA's in at least %i columns: %i\n",
                  i, sum(apply(r, 1, sum) >= i) ))
      i <- i+1
    }

  }

  tp <- subset(df$TIME, complete.cases(df) == FALSE)
  # Get incomplete timepoints.

  return(tp)
}
