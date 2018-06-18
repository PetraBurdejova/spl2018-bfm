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


### 1. LOAD DATA     ----

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


### 2a. PUN          ----

# Select important data/ variables 
df.pun <- subset( df.pun.0, select = c(HourUTC, SpotPriceEUR ) )


# Adding names and POSIXct Time 
colnames(df.pun) = c("TIME", "PUN")
df.pun$TIME <- ymd_hm(df.pun$TIME)


### 2b.1  SOLAR DE   ----

# Select important data/ variables 
df.solar <- subset(df.solar.D, select = c("Datum","von","X50Hertz..MW.", 
                                          "Amprion..MW.", "TenneT.TSO..MW.", "Transnet.BW..MW.") )
df.solar <- unite(df.solar, TIME, c("Datum", "von"), sep = " ")

# Adding names and POSIXct Time 
names(df.solar) <- c("TIME", "50Hertz", 
                     "Amprion", "TenneT.TSO", "Transnet.BW")
df.solar$TIME <- dmy_hm(df.solar$TIME)


### 2b.2 SOLAR AT    ----

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
  names(y) <- c("TIME", "SOLAR.MW.AT")
  y$`SOLAR.MW.AT` <- as.numeric(y$`SOLAR.MW.AT`)
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


### 2c. WIND DE      ----

# Select important data/ variables 
df.wind <- subset(df.wind.D, select = c("Datum","von","X50Hertz..MW.", 
                                        "Amprion..MW.", "TenneT.TSO..MW.", "TransnetBW") )
df.wind <- unite(df.wind, TIME, c("Datum", "von"), sep = " ")


# Adding names and POSIXct Time 
names(df.wind) <- c("TIME", "50Hertz", 
                    "Amprion", "TenneT.TSO", "Transnet.BW")
df.wind$TIME <- dmy_hm(df.wind$TIME)


### 2c. WIND AT      -----

# Write a function to select the important data/ variables
select.ATWIND <- function(x){
  # Selects the important variables for the ren.AT data
  #
  # Args:
  #   x: Imported raw dataframe
  #
  # Returns:
  #   y: Corrected wind.AT dataframe
  y <- subset(x, select = c("V1", "V5"))
  names(y) <- c("TIME", "WIND.MW.AT")
  y$`WIND.MW.AT` <- as.numeric(y$`WIND.MW.AT`)
  y$TIME <- dmy_hms(y$TIME)
  return(y)
}


# Select important data/ variables 
df.wind.AT1 <- select.ATWIND(df.ren.AT1)
df.wind.AT2 <- select.ATWIND(df.ren.AT2)
df.wind.AT3 <- select.ATWIND(df.ren.AT3)
df.wind.AT4 <- select.ATWIND(df.ren.AT4)
df.wind.AT5 <- select.ATWIND(df.ren.AT5)
df.wind.AT6 <- select.ATWIND(df.ren.AT6)
df.wind.AT7 <- select.ATWIND(df.ren.AT7)


# Bind the dataframes together
df.wind.AT <- rbind(df.wind.AT1,df.wind.AT2,df.wind.AT3,df.wind.AT4,
                    df.wind.AT5,df.wind.AT6,df.wind.AT7)


### 2d. DEMAND       ----

# Write a function to select the important data/ variables
# IN ARBEIT!! MÜSSEN ERST NA-PROBLEM LÖSEN!!!
select.DEM = function(x) {
  # Selects the important variables for the demand data
  # Also checks if variable is factor or not
  # Args:
  #   x: Imported raw dataframe
  #
  # Returns:
  #   y: Selection of demand dataframes
  y <- subset(x, select = c("Time..CET.", 
                            "Day.ahead.Total.Load.Forecast..MW....BZN.DE.AT.LU"))
  
  names(y) <- c("TIME", "DAY-AHEAD.MW")
  y <- separate(y, col = TIME, into = c("TIME","bis"), sep =  " - ")
  y <- subset(y, select = c("TIME","DAY-AHEAD.MW"))
  y$TIME <- dmy_hm(y$TIME)
  
  if (class(y$`DAY-AHEAD.MW` ) == "factor") {
    # zwischenzeitig ausgeschaltet, um an NAs zu arbeiten...
    # y$`DAY-AHEAD.MW` <- as.numeric(levels(y$`DAY-AHEAD.MW`))[y$`DAY-AHEAD.MW`]
    return(y)
    
  } else {
    # hier jetzt statt as.numeric zu Faktor gemacht. Wird dann umgestellt! 
    y$`DAY-AHEAD.MW` <- as.factor(y$`DAY-AHEAD.MW`)
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


### 3. DELETING UNNECESSARY DATA ----

rm(list=ls()[! ls() %in% c("df.pun", "df.solar", "df.solar.AT", "df.wind", 
                        "df.wind.AT", "df.dm")]) 
