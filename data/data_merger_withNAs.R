# Importing data and functions
source("raw_data.R")
source("clean_missing_values.R")


## Choose time frame to analyze 
#df.dm       <- time.FRAME(df.dm)
#df.pun      <- time.FRAME(df.pun)
#df.solar    <- time.FRAME(df.solar)
#df.wind     <- time.FRAME(df.wind)
#df.solar.AT <- time.FRAME(df.solar.AT)
#df.wind.AT  <- time.FRAME(df.wind.AT)


# Function for computing MW/h from quater-hourly values
hour.MW.comp <- function(x){
  # Computes MW per hour from 15 minute intervalls
  y <- sum(x)*0.25
  return(y)
}

# Calculate daily averages/sums
df.dm       <- aggregate(list("DM" = df.dm$dm),
                         list("TIME" = cut(df.dm$TIME, "1 day")),
                         FUN = sum)
df.pun      <- aggregate(list("PUN" = df.pun$PUN),
                         list("TIME" = cut(df.pun$TIME, "1 day")),
                         FUN = mean)  # 'FUN = mean' because PUN is prize.
df.solar    <- aggregate(list(df.solar$`50Hertz`, df.solar$`Amprion`, 
                           df.solar$`TenneT.TSO`, df.solar$`Transnet.BW`), 
                         list("TIME" = cut(df.solar$TIME, "1 day")),
                         FUN = hour.MW.comp)  # Note use of hour.MW.comp!
df.wind     <- aggregate(list(df.wind$`50Hertz`, df.wind$`Amprion`,
                              df.wind$`TenneT.TSO`, df.wind$`Transnet.BW`), 
                         list("TIME" = cut(df.wind$TIME, "day")), 
                         FUN = hour.MW.comp)  # Note use of hour.MW.comp!
df.solar.AT <- aggregate(list("SOLAR.MW.AT" = df.solar.AT$`SOLAR.MW.AT`), 
                         list("TIME" = cut(df.solar.AT$TIME, "1 day")),
                         FUN = sum)
df.wind.AT  <- aggregate(list("WIND.MW.AT" = df.wind.AT$`WIND.MW.AT`), 
                         list("TIME" = cut(df.wind.AT$TIME, "1 day")),
                         FUN = sum)


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
#df.solar <- sum.f(df.solar, df.solar.AT)
#df.wind  <- sum.f(df.wind, df.wind.AT)
