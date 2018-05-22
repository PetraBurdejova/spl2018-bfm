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
# Input: .csv files in the data/sources directory

# Ouput: energy.csv
#
# ------------------------------


# Problem with working directory: Where?
# setwd(??)
# getwd()


# read in our data
df.pun = read.csv("Elspot_Prices_Data-5375228caa4c48ad9b969f250d70fe2e.csv")
df.solar = read.csv("Solarenergie_DE.csv")
df.wind = read.csv("Windenergie_DE.csv")

df.dem.2015 = read.csv("Total Load - Day Ahead _ Actual_201501010000-201601010000.csv")
df.dem.2016 = read.csv("Total Load - Day Ahead _ Actual_201601010000-201701010000.csv")
df.dem.2017 = read.csv("Total Load - Day Ahead _ Actual_201701010000-201801010000.csv")
df.dem.2018 = read.csv("Total Load - Day Ahead _ Actual_201801010000-201901010000.csv")


# now cleaning of unnecessary variables


