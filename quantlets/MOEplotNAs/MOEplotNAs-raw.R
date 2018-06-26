library(ggplot2)

source("MOErawdata/MOErawdata.R")

DiagMissingValues <- function(df, dlevel = 0) {
  # Checks for NA values in dataframe and prints information.
  #
  # Args:
  #   df: The dataframe that will be checked for missing values.
  #   dlevel: Print more detailed information. 0: No information.
  #           1: General information. 2: Information on columns.
  #
  # Returns:
  #   A <dataframe> of timepoints of incomplete cases in df.

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
    print(apply(r, 2, sum))
  }

  if (dlevel >= 2) {
    # dlevel 2 diagnostics: information per column.
    i <- 1
    for (col in names(df)){
      cat(sprintf("NA's in at least %i columns: %i\n",
                  i, sum(apply(r, 1, sum) >= i) ))
      i <- i+1
    }

  }

  # Get timepoints of incomplete cases and output dataframe
  tp <- subset(df$TIME, complete.cases(df) == FALSE)
  tp <- as.data.frame(tp)
  names(tp) <- "TIME"

  return(tp)
}

df.na.dm <- data.frame(DiagMissingValues(df.dm), "DEM")
names(df.na.dm) <- c("TIME", "SOURCE")
df.na.solar <- data.frame(DiagMissingValues(df.solar), "SOLAR")
names(df.na.solar) <- c("TIME", "SOURCE")
df.na.wind <- data.frame(DiagMissingValues(df.wind), "WIND")
names(df.na.wind) <- c("TIME", "SOURCE")
df.na <- rbind(df.na.dm,df.na.solar,df.na.wind)

gg <- ggplot(df.na) +
  geom_histogram(aes(x=TIME, col=SOURCE, fill=SOURCE), alpha = 0.8, binwidth = 24*3600) +
  labs(title = "Histogram of NAs per day in raw datasets",
       x = "Date", y = "NAs per day")  
