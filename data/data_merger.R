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

# 1. READ DATA FROM DATA/SOURCE SUBDIRECTORY

df.pun    = read_csv("source/Elspot_Prices_Data-5375228caa4c48ad9b969f250d70fe2e.csv")
df.solar  = read_csv2("source/Solarenergie_DE.csv")
df.wind   = read_csv2("source/Windenergie_DE.csv")

df.dem.2015 = read_csv("source/Total Load - Day Ahead _ Actual_201501010000-201601010000.csv")
df.dem.2016 = read_csv("source/Total Load - Day Ahead _ Actual_201601010000-201701010000.csv")
df.dem.2017 = read_csv("source/Total Load - Day Ahead _ Actual_201701010000-201801010000.csv")
df.dem.2018 = read_csv("source/Total Load - Day Ahead _ Actual_201801010000-201901010000.csv")


# 2. PREPARE DATA FOR MERGE


# 2a. PUN
df.pun <- subset( df.pun, select = c(HourUTC, SpotPriceEUR ) )
colnames(df.pun) = c("TIME", "PUN")

# 2b. SOLAR
#df.solar <- subset( df.solar, select = c(Datum, von, bis, SpotPriceEUR ) )
#colnames(df.pun) = c("TIME", "PUN")

# 2c. WIND

# 2d. DEM

# (2e. GAS)

