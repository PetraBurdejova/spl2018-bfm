### 0.   EXPLANATION            ------   
#
# data_merger.R
#
# Data from raw_data is formatted and merged in a single dataframe
#
# Input:  Variables from raw_data.R
#
# Ouput:  A dataframe containing the variables
#             TIME
#             PUN
#             SOLAR
#             WIND
#             DEM


### 1.   LOAD RAW DATA          ----

# Importing Data
source("raw_data.R")


### 2a.  PUN                    ----

# Function for time frame
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

# Choose time frame to analyze 
df.pun <- time.FRAME(df.pun)

# Calculate the mean price per hour/ day
df.pun <- aggregate(list("PUN" = df.pun$PUN), 
                         list("TIME" = cut(df.pun$TIME, "1 day")), FUN = mean)

# Adding names and POSIXct time 
names(df.pun) <- c("TIME", "DAY-AHEAD.MW")
df.pun$TIME <- ymd(df.pun$TIME)


### 2b.1 SOLAR DE               ----

# Choose time-frame to analyze 
df.solar <- time.FRAME(df.solar)

# Function for computing MW/h from smaller intervalls
hour.MW.comp <- function(x){
  # Computes MW per hour from 15 minute intervalls
  # 
  # Args:
  #   x =...
  y <- sum(x)*0.25
  return(y)
}

# Daily average of MW produced per firm
df.solar <- aggregate(list(df.solar$`50Hertz`, 
                           df.solar$`Amprion`, df.solar$`TenneT.TSO`, 
                           df.solar$`Transnet.BW`), 
                      list("TIME" = cut(df.solar$TIME, "day")), FUN = hour.MW.comp)

# Adding names and POSIXct time 
names(df.solar) <- c("TIME", "50Hertz", "Amprion", "TenneT.TSO", 
                     "Transnet.BW")
df.solar$TIME <- ymd(df.solar$TIME)

# Sum of the MW per day produced by the different firms
MW.per.Day <- rowSums(df.solar[ ,-1])
df.solar <- data.frame(df.solar$TIME, MW.per.Day)

names(df.solar) <- c("TIME", "MW.per.Day")


### 2b.2 SOLAR AT               ----

# Choose time frame to analyze 
df.solar.AT <- time.FRAME(df.solar.AT)

# Calculate the sum MW per hour/ day
df.solar.AT <- aggregate(list("SOLAR.MW.AT" = df.solar.AT$`SOLAR.MW.AT`), 
                        list("TIME" = cut(df.solar.AT$TIME, "1 day")), FUN = sum)

# Adding names and POSIXct time 
names(df.solar.AT) <- c("TIME", "MW.per.Day")
df.solar.AT$TIME <- ymd(df.solar.AT$TIME)


### 2c.  WIND DE                ----

# Choose time frame to analyze 
df.wind <- time.FRAME(df.wind)


# Daily average of MW produced per firm
df.wind <- aggregate(list(df.wind$`50Hertz`, 
                          df.wind$`Amprion`, df.wind$`TenneT.TSO`, 
                          df.wind$`Transnet.BW`), 
                      list("TIME" = cut(df.wind$TIME, "day")), FUN = hour.MW.comp)

# Adding names and POSIXct time 
names(df.wind) <- c("TIME", "50Hertz", "Amprion", "TenneT.TSO", 
                     "Transnet.BW")
df.wind$TIME <- ymd(df.wind$TIME)

# Sum of the MW per Day produced by the different Firms
MW.per.Day <- rowSums(df.wind[ ,-1])
df.wind <- data.frame(df.wind$TIME, MW.per.Day)

# Adding names and POSIXct time 
names(df.wind) <- c("TIME", "MW.per.Day")


### 2c.  WIND AT                -----

# Choose time frame to analyze 
df.wind.AT <- time.FRAME(df.wind.AT)

# Calculate the mean MW per hour/ day
df.wind.AT <- aggregate(list("WIND.MW.AT" = df.wind.AT$`WIND.MW.AT`), 
                   list("TIME" = cut(df.wind.AT$TIME, "1 day")), FUN = sum)

# Adding names and POSIXct time 
names(df.wind.AT) <- c("TIME", "MW.per.Day")
df.wind.AT$TIME <- ymd(df.wind.AT$TIME)


### 2d.  DEMAND                 ----

# Choose time frame to analyze 
df.dm <- time.FRAME(df.dm)

# Function for checking for NAs
FindMissingValues <- function(df, verbose = FALSE, days = FALSE) {
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
    no.na <- length(indices)  # number of na in df
    entries <- nrow(df)  # number of values in df
    
    print(sprintf("Checking for missing values in dataframe '%s'.", name))
    print(sprintf("%.0f of %.0f (%.3f%%) values are NA.",
                  no.na, entries, no.na/entries))
    
  }
  
  if (days == TRUE) {
    
    naDays <- 0
    return(naDays)
    
  } else {
    
    return(indices)
    
  }
  
}

# Removing said NAs
## Comment: Just first "dirty" removing. Will be more advanced in future
ind <- FindMissingValues(df.dm$`DAY-AHEAD.MW`, verbose = F, days = F)

for (i in ind) {
df.dm$`DAY-AHEAD.MW`[i] <- mean(df.dm$`DAY-AHEAD.MW`[(i-1):(i+1)], 
                                  na.rm = T)
}

# Calculate the sum MW per hour/ day
df.dm <- aggregate(list("DAY-AHEAD.MW" = df.dm$`DAY-AHEAD.MW`), 
                  list("TIME" = cut(df.dm$TIME, "1 day")), FUN = sum)

# Adding names and POSIXct time 
names(df.dm) <- c("TIME", "DAY-AHEAD.MW")
df.dm$TIME <- ymd(df.dm$TIME)


### 2e.  MERGE OF AT & DE DATA  ------

# Function for merging AT & DE
sum.f <- function(x, y) {
  # Calculates the *rowsum* for solar and wind variables in DE & AT
  #
  # Args: 
  #   x, y = Input Dataframes
  #
  # Output: 
  #   z = Vector of values
    z <- x$`MW.per.Day` + y$`MW.per.Day`
    return(z)
}

# Merging AT & DE data  
df.solar <- sum.f(df.solar, df.solar.AT)
df.wind  <- sum.f(df.wind, df.wind.AT)


### 3.   FINAL DATAFRAME        ----

# Bind final Dataframe
df <- cbind(df.pun, df.dm, df.solar, df.wind)
df <- df[ -c(3) ]

# Adding names
names(df) <- c("TIME", "PUN", "DEM", 
               "SOLAR", "WIND")


# Removing leftover NAs
## Comment: Just first "dirty" removing. Will be more advanced in future
#Solar
ind <- FindMissingValues(df$`SOLAR`, verbose = F, days = F)
for (i in ind) {
  df$`SOLAR`[i] <- mean(df$`SOLAR`[(i-1):(i+1)], 
                                na.rm = T)
}

# Wind
ind <- FindMissingValues(df$`WIND`, verbose = F, days = F)
for (i in ind) {
  df$`WIND`[i] <- mean(df$`WIND`[(i-1):(i+1)], 
                                na.rm = T)
}


# Removeing everything except for "df" from environment
rm(list=ls()[! ls() %in% c("df")]) 


## Bruno merkliste
# - time zones!
# - maybe use XTS for averaging daily 
# - check if rowsums function works correctly
#     is there an alternative? 
# - alternative for aggregate functions - maybe a bit inefficient?