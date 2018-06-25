# Importing data and functions
source("MOErawdata/MOErawdata.R")

# Small fix
names(df.dm) <- c("TIME", "DEM")

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

df.dm       <- time.FRAME(df.dm)
df.pun      <- time.FRAME(df.pun)
df.solar    <- time.FRAME(df.solar)
df.wind     <- time.FRAME(df.wind)
df.solar.AT <- time.FRAME(df.solar.AT)
df.wind.AT  <- time.FRAME(df.wind.AT)

# Function for computing MW/h from quater-hourly values
hour.MW.comp <- function(x){
  # Computes MW per hour from 15 minute intervalls
  y <- sum(x)*0.25
  return(y)
}

# Calculate daily averages/sums
df.pun      <- aggregate(list("PUN" = df.pun$PUN),
                         list("TIME" = cut(df.pun$TIME, "1 day")),
                         FUN = mean)  # 'FUN = mean' because PUN is prize.
df.dm       <- aggregate(list("DM" = df.dm$DEM),
                         list("TIME" = cut(df.dm$TIME, "1 day")),
                         FUN = sum)
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

# Fix formating (formating is lost bc use of aggregate())
names(df.pun)     <- c("TIME", "PUN")
names(df.dm)      <- c("TIME", "DEM")
names(df.solar)   <- c("TIME", "50Hertz", "Amprion",
                       "TenneT.TSO", "Transnet.BW")
names(df.wind)    <- c("TIME", "50Hertz", "Amprion",
                       "TenneT.TSO", "Transnet.BW")
names(df.solar.AT)<- c("TIME", "SOLAR")
names(df.wind.AT) <- c("TIME", "WIND")
df.pun$TIME       <- ymd(df.pun$TIME)
df.dm$TIME        <- ymd(df.dm$TIME)
df.solar$TIME     <- ymd(df.solar$TIME)
df.wind$TIME      <- ymd(df.wind$TIME)
df.solar.AT$TIME  <- ymd(df.solar.AT$TIME)
df.wind.AT$TIME   <- ymd(df.wind.AT$TIME)

# Aggregate producers for df.solar and df.wind
df.solar  <- data.frame(df.solar$TIME, rowSums(df.solar[ ,-1]))
df.wind   <- data.frame(df.wind$TIME, rowSums(df.wind[ ,-1]))
names(df.solar)   <- c("TIME", "SOLAR")
names(df.wind)    <- c("TIME", "WIND")

# Merge AT & DE renewable data  
df.solar[,-1]   <- df.solar[,-1] + df.solar.AT[,-1]
df.wind[,-1]    <- df.wind[,-1] + df.wind.AT[,-1]

# Bind final Dataframe
df <- cbind(df.pun, df.dm, df.solar, df.wind)
df <- df[ -c(3,5,7) ]

# Removeing everything except for "df" from environment
rm(list=ls()[! ls() %in% c("df")]) 
