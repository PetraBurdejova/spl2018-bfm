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

getwd()

df.pun = read.csv("Elspot_Prices_Data.5375228caa4c48ad9b969f250d70fe2e", 
               header = TRUE)


