# ------------------------------
# ------- dataMerger.R ---------
# ------------------------------
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
# ------------------------------

rm(list = ls())
library(lubridate)
library(tidyr)

# 1. READ DATA FROM DATA/SOURCE SUBDIRECTORY

### 1.a LOAD DATA  ----
df.pun.0   = read.csv("source/Elspot_Prices_Data-5375228caa4c48ad9b969f250d70fe2e.csv")
df.solar.D  = read.csv2("source/Solarenergie_DE.csv")
df.wind.D   = read.csv2("source/Windenergie_DE.csv")

neuerv  = read.csv2("source/Solarenergie_DE.csv")
head(neuerv)
ind <- which(is.na(neuerv))
neuerv[ind, ]
na.omit(neuerv)


df.dem.2015.0 = read.csv("source/Total Load - Day Ahead _ Actual_201501010000-201601010000.csv")
df.dem.2016.0 = read.csv("source/Total Load - Day Ahead _ Actual_201601010000-201701010000.csv")
df.dem.2017.0 = read.csv("source/Total Load - Day Ahead _ Actual_201701010000-201801010000.csv")
df.dem.2018.0 = read.csv("source/Total Load - Day Ahead _ Actual_201801010000-201901010000.csv")

# 1.b LOOK FOR NA'S



# 2. PREPARE DATA FOR MERGE

### 2a. PUN ----
df.pun <- subset( df.pun.0, select = c(HourUTC, SpotPriceEUR ) )
colnames(df.pun) = c("TIME", "PUN")
df.pun$TIME <- ymd_hm(df.pun$TIME)




### 2b. SOLAR ----
# subset

df.solar <- subset(df.solar.D, select = c("Datum","von","X50Hertz..MW.", 
                        "Amprion..MW.", "TenneT.TSO..MW.", "Transnet.BW..MW.") )
df.solar <- unite(df.solar, TIME, c("Datum", "von"), sep = " ")
names(df.solar) <- c("TIME", "50Hertz (MW)", 
                     "Amprion (MW)", "TenneT TSO (MW)", "Transnet BW (MW)")

#time
df.solar$TIME <- dmy_hm(df.solar$TIME)


## Damit nochmal nach NA etc. checken ## 
names(df.solar)
head(df.solar)
summary(df.solar)
ind <- which(is.na(df.solar))
df.solar[ind, ]
class(df.solar$TIME)

ind <- which(is.na(df.solar[df.solar$TIME > ymd("2015-01-01"), ]))
df.solar[ind, ]
summary(df.wind.D)

# same for wind
## --------------------------- ## 

### 2c. WIND ----
#df.wind <- subset(df.wind0, select = c("Datum","von","50Hertz (MW)", 
#                                         "Amprion (MW)", "TenneT TSO (MW)", "Transnet BW (MW)") )
#df.solar <- unite(df.solar, TIME, c("Datum", "von"), sep = " ")
#df.solar$TIME <- dmy_hms(df.solar$TIME)


### 2d. DEMAND ----

### 2be. (GAS) ----

