library(ggplot2)
library(lubridate)

load("MOEmergedata/energy_market.Rdata")
load("MOErawdata/energy_market_raw.Rdata")

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

# Choose time frame to analyze 
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

#df.dm       <- time.FRAME(df.dm)
#df.pun      <- time.FRAME(df.pun)
#df.solar    <- time.FRAME(df.solar)
#df.wind     <- time.FRAME(df.wind)
#df.solar.AT <- time.FRAME(df.solar.AT)
#df.wind.AT  <- time.FRAME(df.wind.AT)

df.na.clean <- data.frame(DiagMissingValues(df), "CLEAN")
names(df.na.clean) <- c("TIME", "SOURCE")

df.na.dm <- data.frame(DiagMissingValues(df.dm), "DEM")
names(df.na.dm) <- c("TIME", "SOURCE")
df.na.solar <- data.frame(DiagMissingValues(df.solar), "SOLAR")
names(df.na.solar) <- c("TIME", "SOURCE")
df.na.wind <- data.frame(DiagMissingValues(df.wind), "WIND")
names(df.na.wind) <- c("TIME", "SOURCE")

df.na.raw <- rbind(df.na.dm,df.na.solar,df.na.wind)

ggplot(df.na.raw) +
  geom_histogram(aes(x=TIME, col=SOURCE, fill=SOURCE),
                 alpha = 0.8, binwidth = 24*3600) +
  labs(x = "Date", y = "NAs per day") +
  theme_bw()

ggsave("MOEplotNAs/na_raw_v2.pdf", plot = last_plot(), device = "pdf", 
       path =  NULL, scale = 1.5, width = 7.5 , height = 3,  
       units = c("in", "cm", "mm"), dpi = 800, limitsize = TRUE)

ggplot(df.na.clean) +
  geom_histogram(aes(x=TIME, col=SOURCE, fill=SOURCE),
                 alpha = 0.8, binwidth=1) +
  labs(x = "Date", y = "is NA?") +
  theme_bw()

ggsave("MOEplotNAs/na_clean.pdf", plot = last_plot(), device = "pdf", 
       path =  NULL, scale = 1.5, width = 7.5 , height = 1,  
       units = c("in", "cm", "mm"), dpi = 800, limitsize = TRUE)
