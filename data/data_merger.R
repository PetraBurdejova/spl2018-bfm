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
library(readr)
library(lubridate)
library(tidyr)

# 1. READ DATA FROM DATA/SOURCE SUBDIRECTORY

df.pun0    = read_csv("source/Elspot_Prices_Data-5375228caa4c48ad9b969f250d70fe2e.csv")
df.solar0  = read_csv2("source/Solarenergie_DE.csv")
df.wind0   = read_csv2("source/Windenergie_DE.csv")

df.dem.20150 = read_csv("source/Total Load - Day Ahead _ Actual_201501010000-201601010000.csv")
df.dem.20160 = read_csv("source/Total Load - Day Ahead _ Actual_201601010000-201701010000.csv")
df.dem.20170 = read_csv("source/Total Load - Day Ahead _ Actual_201701010000-201801010000.csv")
df.dem.20180 = read_csv("source/Total Load - Day Ahead _ Actual_201801010000-201901010000.csv")


# 2. PREPARE DATA FOR MERGE


# 2a. PUN
df.pun <- subset( df.pun0, select = c(HourUTC, SpotPriceEUR ) )
colnames(df.pun) = c("TIME", "PUN")
df.pun$TIME <- ymd_hms(df.pun$TIME)

# 2b. SOLAR
df.solar <- subset(df.solar0, select = c("Datum","von","50Hertz (MW)", 
                        "Amprion (MW)", "TenneT TSO (MW)", "Transnet BW (MW)") )
df.solar <- unite(df.solar, TIME, c("Datum", "von"), sep = " ")
df.solar$TIME <- dmy_hms(df.solar$TIME)


## Damit nochmal nach NA etc. checken ## 
names(df.solar)
head(df.solar)
summary(df.solar)
ind <- which(is.na(df.solar))
df.solar[ind, ]
# same for wind
## --------------------------- ## 

# 2c. WIND
#df.wind <- subset(df.wind0, select = c("Datum","von","50Hertz (MW)", 
#                                         "Amprion (MW)", "TenneT TSO (MW)", "Transnet BW (MW)") )
#df.solar <- unite(df.solar, TIME, c("Datum", "von"), sep = " ")
#df.solar$TIME <- dmy_hms(df.solar$TIME)


# 2d. DEM

# (2e. GAS)

