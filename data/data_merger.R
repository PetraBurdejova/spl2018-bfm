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

# 0. PACKAGES AND ENVIRONMENT

rm(list = ls())
library(lubridate)
library(tidyr)


# 1. READ DATA FROM DATA/SOURCE SUBDIRECTORY
### 1.a LOAD DATA  ----
df.pun.0   <-  read.csv("source/Elspot_Prices_Data-5375228caa4c48ad9b969f250d70fe2e.csv")

df.solar.D <-  read.csv2("source/Solarenergie_DE.csv")
df.wind.D  <-  read.csv2("source/Windenergie_DE.csv")

df.ren.AT1  <-  read.csv2("source/AT - renewables/export_daftg_2015-01-01T00_00_00Z_2015-07-01T23_45_00Z_60M_de.csv")
df.ren.AT2  <-  read.csv2("source/AT - renewables/export_daftg_2015-07-01T00_00_00Z_2016-01-01T23_45_00Z_60M_de.csv")
df.ren.AT3  <-  read.csv2("source/AT - renewables/export_daftg_2016-01-01T00_00_00Z_2016-07-01T23_45_00Z_60M_de.csv")
df.ren.AT4  <-  read.csv2("source/AT - renewables/export_daftg_2016-07-01T00_00_00Z_2017-01-01T23_45_00Z_60M_de.csv")
df.ren.AT5  <-  read.csv2("source/AT - renewables/export_daftg_2017-01-01T00_00_00Z_2017-07-01T23_45_00Z_60M_de.csv")
df.ren.AT6  <-  read.csv2("source/AT - renewables/export_daftg_2017-07-01T00_00_00Z_2018-01-01T23_45_00Z_60M_de.csv")
df.ren.AT7  <-  read.csv2("source/AT - renewables/export_daftg_2018-01-01T00_00_00Z_2018-05-25T23_45_00Z_60M_de.csv")

df.dem.2015.0 <- read.csv("source/Total Load - Day Ahead _ Actual_201501010000-201601010000.csv")
df.dem.2016.0 <- read.csv("source/Total Load - Day Ahead _ Actual_201601010000-201701010000.csv")
df.dem.2017.0 <- read.csv("source/Total Load - Day Ahead _ Actual_201701010000-201801010000.csv")
df.dem.2018.0 <- read.csv("source/Total Load - Day Ahead _ Actual_201801010000-201901010000.csv")

# 1.b LOOK FOR NA'S

# 2. PREPARE DATA FOR MERGE


# 2. CLEAN ALL VARIABLES
### 2a. PUN ----
df.pun <- subset( df.pun.0, select = c(HourUTC, SpotPriceEUR ) )
colnames(df.pun) = c("TIME", "PUN")
df.pun$TIME <- ymd_hm(df.pun$TIME)


### 2b. SOLAR ----
## SOLAR DE --
df.solar <- subset(df.solar.D, select = c("Datum","von","X50Hertz..MW.", 
                        "Amprion..MW.", "TenneT.TSO..MW.", "Transnet.BW..MW.") )
df.solar <- unite(df.solar, TIME, c("Datum", "von"), sep = " ")
names(df.solar) <- c("TIME", "50Hertz (MW)", 
                     "Amprion (MW)", "TenneT TSO (MW)", "Transnet BW (MW)")
df.solar$TIME <- dmy_hm(df.solar$TIME)


## Damit nochmal nach NA etc. checken ## 
names(df.solar)
head(df.solar)
summary(df.solar)
ind <- which(is.na(df.solar))
df.solar[ind, ]
class(df.solar$TIME)
head(ind, n=100)

ind <- which(is.na(df.solar[df.solar$TIME > ymd("2015-01-01"), ]))
df.solar[ind, ]
summary(df.wind.D)




neuerv  = read.csv2("source/Solarenergie_DE.csv")
head(neuerv)
ind <- which(is.na(neuerv))
neuerv[ind, ]
na.omit(neuerv)

## SOLAR AT --
df.solar.AT <- subset(df.ren.AT, select = (""))
names(df.ren.AT)

## UNITE --

# same for wind
## --------------------------- ## 

### 2c. WIND ----
# + add AT
#df.wind <- subset(df.wind0, select = c("Datum","von","50Hertz (MW)", 
#                                         "Amprion (MW)", "TenneT TSO (MW)", "Transnet BW (MW)") )
#df.solar <- unite(df.solar, TIME, c("Datum", "von"), sep = " ")
#df.solar$TIME <- dmy_hms(df.solar$TIME)


### 2d. DEMAND ----
head(df.dem.2015.0)
names(df.dem.2015.0)

df.dem.2015 <- subset(df.dem.2015.0, select = c("Time..CET.", 
                      "Day.ahead.Total.Load.Forecast..MW....BZN.DE.AT.LU"))

names(df.dem.2015) <- c("TIME", "DAY-AHEAD MW")
df.dem.2015$TIME <- separate(df.dem.2015, )
df.dem.2015$TIME <- dmy_hm(df.dem.2015$TIME)

### 2e. (GAS) ----

