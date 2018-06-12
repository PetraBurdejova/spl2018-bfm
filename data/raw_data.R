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

