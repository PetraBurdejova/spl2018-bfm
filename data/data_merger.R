# ------------------------------
# -       dataMerger.R         -
# -                            -
# 
# Authors:  Bruno Puri, 
#           Felix Germaine,
#           Manuel Pfeuffer
# 
# Data from multiple .csv files is formated coherently and merged in a single
# dataframe.
#
# Input:  .csv files in the data/sources directory
#
# Ouput:  A dataframe containing the variables
#             TIME
#             PUN
#             SOLAR
#             WIND
#             DEM
#             (GAS)
#

# 0. PACKAGES AND ENVIRONMENT
### 0. PACKAGES      ----

rm(list = ls())
library(lubridate)
library(tidyr)
library(stringr)


# 1. READ DATA FROM DATA/SOURCE SUBDIRECTORY
### 1. LOAD DATA     ----

# 
df.pun.0   <-  read.csv("source/Elspot_Prices_Data-5375228caa4c48ad9b969f250d70fe2e.csv")

df.solar.D <-  read.csv2("source/Solarenergie_DE.csv")
df.wind.D  <-  read.csv2("source/Windenergie_DE.csv")

df.ren.AT1  <-  read.csv2("source/AT - renewables/export_daftg_2015-01-01T00_00_00Z_2015-06-30T23_45_00Z_60M_de.csv", header = F)
df.ren.AT2  <-  read.csv2("source/AT - renewables/export_daftg_2015-07-01T00_00_00Z_2015-12-31T23_45_00Z_60M_de.csv", header = F)
df.ren.AT3  <-  read.csv2("source/AT - renewables/export_daftg_2016-01-01T00_00_00Z_2016-06-30T23_45_00Z_60M_de.csv", header = F)
df.ren.AT4  <-  read.csv2("source/AT - renewables/export_daftg_2016-07-01T00_00_00Z_2016-12-31T23_45_00Z_60M_de.csv", header = F)
df.ren.AT5  <-  read.csv2("source/AT - renewables/export_daftg_2017-01-01T00_00_00Z_2017-06-30T23_45_00Z_60M_de.csv", header = F)
df.ren.AT6  <-  read.csv2("source/AT - renewables/export_daftg_2017-07-01T00_00_00Z_2017-12-31T23_45_00Z_60M_de.csv", header = F)
df.ren.AT7  <-  read.csv2("source/AT - renewables/export_daftg_2018-01-01T00_00_00Z_2018-06-04T23_45_00Z_60M_de.csv", header = F)


df.dem.2015.0 <- read.csv("source/Total Load - Day Ahead _ Actual_201501010000-201601010000.csv")
df.dem.2016.0 <- read.csv("source/Total Load - Day Ahead _ Actual_201601010000-201701010000.csv")
df.dem.2017.0 <- read.csv("source/Total Load - Day Ahead _ Actual_201701010000-201801010000.csv")
df.dem.2018.0 <- read.csv("source/Total Load - Day Ahead _ Actual_201801010000-201901010000.csv")

 
# 2. PREPARE DATA FOR MERGE
# 2. CLEAN ALL VARIABLES
### 2a. PUN       ("fertig")   ----

# Select important data/ variables 
df.pun <- subset( df.pun.0, select = c(HourUTC, SpotPriceEUR ) )


# Adding names and POSIXct Time 
colnames(df.pun) = c("TIME", "PUN")
df.pun$TIME <- ymd_hm(df.pun$TIME)

###-CUT--###

# Choose time-frame to analyze 
start.d <- ymd_hm("2015-01-01 00:00")
stop.d <- ymd_hm("2017-12-31 23:00")
ind.start <- which(df.pun$TIME == start.d)
ind.stop <- which(df.pun$TIME == stop.d)
ind <- (ind.start: ind.stop)
df.pun <- df.pun[ind, ]
## Comment: This is a stupid way to do it. do you know a better way? ## 
## Funktion draus schreiben?

### 2b.1  SOLAR DE  ("fertig") ----

# Select important data/ variables 
df.solar <- subset(df.solar.D, select = c("Datum","von","X50Hertz..MW.", 
                        "Amprion..MW.", "TenneT.TSO..MW.", "Transnet.BW..MW.") )
df.solar <- unite(df.solar, TIME, c("Datum", "von"), sep = " ")

# Adding names and POSIXct Time 
names(df.solar) <- c("TIME", "50Hertz (MW)", 
                     "Amprion (MW)", "TenneT TSO (MW)", "Transnet BW (MW)")
df.solar$TIME <- dmy_hm(df.solar$TIME)

###-CUT--###

# Choose time-frame to analyze 
start.d <- ymd_hm("2015-01-01 00:00")
stop.d <- ymd_hm("2017-12-31 23:00")
ind.start <- which(df.solar$TIME == start.d)
ind.stop <- which(df.solar$TIME == stop.d)
ind <- (ind.start: ind.stop)
df.solar <- df.solar[ind, ]
## Comment: This is a stupid way to do it. do you know a better way? ## 


##### 
# hier mit XTS eher? also nochmal gucken, wie du die averages bildest
# Hourly average of MW produced per Firm 
#    df.solar <- aggregate(list(df.solar$`50Hertz (MW)`, 
#                              df.solar$`Amprion (MW)`, df.solar$`TenneT TSO (MW)`, 
#                               df.solar$`Transnet BW (MW)`), 
#                          list("TIME" = cut(df.solar$TIME, "hour")), FUN = mean)
#    
#    # Adding names and POSIXct Time 
#    names(df.solar) <- c("TIME", "50Hertz (MW)", "Amprion (MW)", "TenneT TSO (MW)", 
#                        "Transnet BW (MW)")
#    df.solar$TIME <- ymd_h(df.solar$TIME)

# erst mal durch 4 teilen --> megawatt stunde und dann summe des tages berechnen
######

hour.MW.comp <- function(x){
  # Computes MW per hour from 15 minute intervalls
  # 
  # Args:
  #   x =...
  y <- sum(x)*0.25
  return(y)
}

# Daily average of MW produced per Firm
df.solar <- aggregate(list(df.solar$`50Hertz (MW)`, 
                           df.solar$`Amprion (MW)`, df.solar$`TenneT TSO (MW)`, 
                           df.solar$`Transnet BW (MW)`), 
                      list("TIME" = cut(df.solar$TIME, "day")), FUN = hour.MW.comp)


# Adding names and POSIXct Time 
names(df.solar) <- c("TIME", "50Hertz (MW)", "Amprion (MW)", "TenneT TSO (MW)", 
                     "Transnet BW (MW)")
df.solar$TIME <- ymd(df.solar$TIME)

# evtl. geht hier auch einfach mal 24 nehmen, um aus den MW Tagen MW Stunden zu machen. ?


# Sum of the MW per Day produced by the different Firms
MW.per.Day <- rowSums(df.solar[ ,-1])
df.solar <- data.frame(df.solar$TIME, MW.per.Day)


str(df.solar)
plot(df.solar)
summary(df.solar)
tail(df.solar, n = 50)


### 2b.2 SOLAR AT  ("fertig") ----

# Write a function to select the important data/ variables
select.ATSOLAR <- function(x){
    # Selects the important variables for the ren.AT data
    #
    # Args:
    #   x: Imported raw dataframe
    #
    # Returns:
    #   y: Corrected solar.AT dataframe
    y <- subset(x, select = c("V1", "V7"))
    names(y) <- c("TIME", "SOLAR MW AT")
    y$`SOLAR MW AT` <- as.numeric(y$`SOLAR MW AT`)
    y$TIME <- dmy_hms(y$TIME)
    return(y)
}


# Select important data/ variables 
df.solar.AT1 <- select.ATSOLAR(df.ren.AT1)
df.solar.AT2 <- select.ATSOLAR(df.ren.AT2)
df.solar.AT3 <- select.ATSOLAR(df.ren.AT3)
df.solar.AT4 <- select.ATSOLAR(df.ren.AT4)
df.solar.AT5 <- select.ATSOLAR(df.ren.AT5)
df.solar.AT6 <- select.ATSOLAR(df.ren.AT6)
df.solar.AT7 <- select.ATSOLAR(df.ren.AT7)

# Bind the dataframes together
df.solar.AT <- rbind(df.solar.AT1, df.solar.AT2,df.solar.AT3,df.solar.AT4,
                      df.solar.AT5,df.solar.AT6,df.solar.AT7)

###-CUT--###

# Choose time-frame to analyze 
start.d <- ymd_hm("2015-01-01 00:00")
stop.d <- ymd_hm("2017-12-31 23:00")
ind.start <- which(df.solar.AT$TIME == start.d)
ind.stop <- which(df.solar.AT$TIME == stop.d)
ind <- (ind.start: ind.stop)
df.solar.AT <- df.solar.AT[ind, ]
## Comment: This is a stupid way to do it. do you know a better way? ## 


# Calculate the sum MW per hour/ day
df.solar.AT <- aggregate(list("SOLAR MW AT" = df.solar.AT$`SOLAR MW AT`), 
                        list("TIME" = cut(df.solar.AT$TIME, "1 day")), FUN = sum)


# Adding names and POSIXct Time 
names(df.solar.AT) <- c("TIME", "SOLAR MW AT")
df.solar.AT$TIME <- ymd(df.solar.AT$TIME)


summary(df.solar.AT)
str(df.solar.AT)
head(df.solar.AT)
tail(df.solar.AT)
plot(df.solar.AT)
### 2c. WIND DE     ----

df.wind <- subset(df.wind.D, select = c("Datum","von","X50Hertz..MW.", 
                                        "Amprion..MW.", "TenneT.TSO..MW.", "TransnetBW") )
df.wind <- unite(df.wind, TIME, c("Datum", "von"), sep = " ")
names(df.wind) <- c("TIME", "50Hertz (MW)", 
                     "Amprion (MW)", "TenneT TSO (MW)", "Transnet BW (MW)")
df.wind$TIME <- dmy_hm(df.wind$TIME)

###-CUT--###

start.d <- ymd_hm("2015-01-01 00:00")
stop.d <- ymd_hm("2017-12-31 23:00")
ind.start <- which(df.wind$TIME == start.d)
ind.stop <- which(df.wind$TIME == stop.d)
df.wind <- df.wind[ind.start: ind.stop, ]


### 2c. WIND AT  ("fertig")   -----

# Write a function to select the important data/ variables
bruno.atwind <- function(x){
  # Selects the important variables for the ren.AT data
  #
  # Args:
  #   x: Imported raw dataframe
  #
  # Returns:
  #   y: Corrected wind.AT dataframe
  y <- subset(x, select = c("V1", "V5"))
  names(y) <- c("TIME", "WIND MW AT")
  y$`WIND MW AT` <- as.numeric(y$`WIND MW AT`)
  y$TIME <- dmy_hms(y$TIME)
  return(y)
}


# Select important data/ variables 
df.wind.AT1 <- bruno.atwind(df.ren.AT1)
df.wind.AT2 <- bruno.atwind(df.ren.AT2)
df.wind.AT3 <- bruno.atwind(df.ren.AT3)
df.wind.AT4 <- bruno.atwind(df.ren.AT4)
df.wind.AT5 <- bruno.atwind(df.ren.AT5)
df.wind.AT6 <- bruno.atwind(df.ren.AT6)
df.wind.AT7 <- bruno.atwind(df.ren.AT7)


# Bind the dataframes together
df.wind.AT <- rbind(df.wind.AT1,df.wind.AT2,df.wind.AT3,df.wind.AT4,
                    df.wind.AT5,df.wind.AT6,df.wind.AT7)

###-CUT--###

# Choose time-frame to analyze 
start.d <- ymd_hm("2015-01-01 00:00")
stop.d <- ymd_hm("2017-12-31 23:00")
ind.start <- which(df.wind.AT$TIME == start.d)
ind.stop <- which(df.wind.AT$TIME == stop.d)
ind <- (ind.start: ind.stop)
df.wind.AT <- df.wind.AT[ind, ]
## Comment: This is a stupid way to do it. do you know a better way? ## 


# Calculate the mean MW per hour/ day
df.wind.AT <- aggregate(list("WIND MW AT" = df.wind.AT$`WIND MW AT`), 
                   list("TIME" = cut(df.wind.AT$TIME, "1 day")), FUN = mean)


# Adding names and POSIXct Time 
names(df.wind.AT) <- c("TIME", "WIND MW AT")
df.wind.AT$TIME <- ymd(df.wind.AT$TIME)


summary(df.wind.AT)
str(df.wind.AT)
head(df.wind.AT)
tail(df.wind.AT)
plot(df.wind.AT)
### 2d. DEMAND    ("fertig")  ----

# Write a function to select the important data/ variables
select.DEM = function(x) {
  # Selects the important variables for the demand data
  # Also checks if variable is factor or not
  # Args:
  #   x: Imported raw dataframe
  #
  # Returns:
  #   y: Corrected demand dataframe
  y <- subset(x, select = c("Time..CET.", 
                       "Day.ahead.Total.Load.Forecast..MW....BZN.DE.AT.LU"))
  
  names(y) <- c("TIME", "DAY-AHEAD MW")
  y <- separate(y, col = TIME, into = c("TIME","bis"), sep =  " - ")
  y <- subset(y, select = c("TIME","DAY-AHEAD MW"))
  y$TIME <- dmy_hm(y$TIME)
  
  if (class(y$`DAY-AHEAD MW` ) == "factor") {
    y$`DAY-AHEAD MW` <- as.numeric(levels(y$`DAY-AHEAD MW`))[y$`DAY-AHEAD MW`]
    return(y)
    
  } else {
    y$`DAY-AHEAD MW` <- as.numeric(y$`DAY-AHEAD MW`)
    return(y)
  }
  
}


# Select important data/ variables 
df.dem.2015 <- select.DEM(df.dem.2015.0)
df.dem.2016 <- select.DEM(df.dem.2016.0)
df.dem.2017 <- select.DEM(df.dem.2017.0)
df.dem.2018 <- select.DEM(df.dem.2018.0)


# Bind the dataframes together
df.dm <- rbind(df.dem.2015,df.dem.2016,df.dem.2017,df.dem.2018)


# Choose time frame to analyze 
start.d <- ymd_hm("2015-01-01 00:00")
stop.d <- ymd_hm("2017-12-31 23:00")
ind.start <- which(df.dm$TIME == start.d)
ind.stop <- which(df.dm$TIME == stop.d)
ind <- (ind.start: ind.stop)
df.dm <- df.dm[ind, ]
## Comment: This is a stupid way to do it. do you know a better way? ## 

# Checking for NAs
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
ind <- FindMissingValues(df.dm$`DAY-AHEAD MW`, verbose = F, days = F)

# Dirty removing said NAs
for (i in ind) {
df.dm$`DAY-AHEAD MW`[i] <- mean(df.dm$`DAY-AHEAD MW`[(i-5):(i+5)], 
                                  na.rm = T)
}
## Comment: Statistisch gesehen böse, aber klappt. was meint ihr? ## 


# Calculate the sum MW per hour/ day
df.dm <- aggregate(list("DAY-AHEAD-MW" = df.dm$`DAY-AHEAD MW`), 
                  list("TIME" = cut(df.dm$TIME, "1 day")), FUN = sum)
## Comment: 
# besser wäre hier evtl. : 
# https://stackoverflow.com/questions/13915549/
# average-in-time-series-based-on-time-and-date-in-r
# muss ich nochmal checken, ob das nicht besser mit einem time series package ist..


# Adding names and POSIXct Time 
names(df.dm) <- c("TIME", "DAY-AHEAD MW")
df.dm$TIME <- ymd(df.dm$TIME)

summary(df.dm)
str(df.dm)
head(df.dm)
tail(df.dm)
plot(df.dm)



### 3.  FINAL DATAFRAME        ----

df <- cbind()




##### KRAM DEN ICH NOCH NICHT WEGWERFEN WOLLTE; DER ABER SONST NERVT.. -----

# Solar DE - KRAMS





## TEST KRAMS 
a <- bruno.verarbeitung(df.dem.2015.0)
dem.vec <- c(df.dem.2015.0,df.dem.2016.0,df.dem.2017.0,df.dem.2018.0)
test <- sapply(dem.vec, bruno.verarbeitung)
identical(a, df.dem.2015)
## ALT KRAMS
df.dem.2015 <- subset(df.dem.2015.0, select = c("Time..CET.", 
                                                "Day.ahead.Total.Load.Forecast..MW....BZN.DE.AT.LU"))
names(df.dem.2015) <- c("TIME", "DAY-AHEAD MW")
df.dem.2015 <- separate(df.dem.2015, col = TIME, into = c("TIME","bis"), sep =  " - ")
df.dem.2015 <- subset(df.dem.2015, select = c("TIME","DAY-AHEAD MW"))
df.dem.2015$TIME <- dmy_hm(df.dem.2015$TIME)
df.dem.2015$`DAY-AHEAD MW` <- as.numeric(df.dem.2015$`DAY-AHEAD MW`)
## 



head(df.dem.2017.0)
str(df.dem.2017)

identical(as.numeric(df.dem.2017.0$Day.ahead.Total.Load.Forecast..MW....BZN.DE.AT.LU), 
          df.dem.2017$`DAY-AHEAD MW`)








# Bruno merkliste
# - alle werte pro tag ausrechnen
# - sonne und wind de noch die einzelnen betreiber zusammenfassen
# - zusammenführen von allen daten
# - time zones!
