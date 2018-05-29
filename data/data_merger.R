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
### 0.a PACKAGES ----
rm(list = ls())
library(lubridate)
library(tidyr)
library(stringr)


# 1. READ DATA FROM DATA/SOURCE SUBDIRECTORY
### 1.a *LOAD DATA  ----
df.pun.0   <-  read.csv("source/Elspot_Prices_Data-5375228caa4c48ad9b969f250d70fe2e.csv")

df.solar.D <-  read.csv2("source/Solarenergie_DE.csv")
df.wind.D  <-  read.csv2("source/Windenergie_DE.csv")

df.ren.AT1  <-  read.csv2("source/AT - renewables/export_daftg_2015-01-01T00_00_00Z_2015-07-01T23_45_00Z_60M_de.csv", header = F)
df.ren.AT2  <-  read.csv2("source/AT - renewables/export_daftg_2015-07-01T00_00_00Z_2016-01-01T23_45_00Z_60M_de.csv", header = F)
df.ren.AT3  <-  read.csv2("source/AT - renewables/export_daftg_2016-01-01T00_00_00Z_2016-07-01T23_45_00Z_60M_de.csv", header = F)
df.ren.AT4  <-  read.csv2("source/AT - renewables/export_daftg_2016-07-01T00_00_00Z_2017-01-01T23_45_00Z_60M_de.csv", header = F)
df.ren.AT5  <-  read.csv2("source/AT - renewables/export_daftg_2017-01-01T00_00_00Z_2017-07-01T23_45_00Z_60M_de.csv", header = F)
df.ren.AT6  <-  read.csv2("source/AT - renewables/export_daftg_2017-07-01T00_00_00Z_2018-01-01T23_45_00Z_60M_de.csv", header = F)
df.ren.AT7  <-  read.csv2("source/AT - renewables/export_daftg_2018-01-01T00_00_00Z_2018-05-25T23_45_00Z_60M_de.csv", header = F)


df.dem.2015.0 <- read.csv("source/Total Load - Day Ahead _ Actual_201501010000-201601010000.csv")
df.dem.2016.0 <- read.csv("source/Total Load - Day Ahead _ Actual_201601010000-201701010000.csv")
df.dem.2017.0 <- read.csv("source/Total Load - Day Ahead _ Actual_201701010000-201801010000.csv")
df.dem.2018.0 <- read.csv("source/Total Load - Day Ahead _ Actual_201801010000-201901010000.csv")

# 1.b LOOK FOR NA'S

# 2. PREPARE DATA FOR MERGE
# 2. CLEAN ALL VARIABLES
### 2a. *PUN ----
df.pun <- subset( df.pun.0, select = c(HourUTC, SpotPriceEUR ) )
colnames(df.pun) = c("TIME", "PUN")
df.pun$TIME <- ymd_hm(df.pun$TIME)

str(df.pun)
head(df.pun)
### 2b. *SOLAR DE ----
df.solar <- subset(df.solar.D, select = c("Datum","von","X50Hertz..MW.", 
                        "Amprion..MW.", "TenneT.TSO..MW.", "Transnet.BW..MW.") )
df.solar <- unite(df.solar, TIME, c("Datum", "von"), sep = " ")
names(df.solar) <- c("TIME", "50Hertz (MW)", 
                     "Amprion (MW)", "TenneT TSO (MW)", "Transnet BW (MW)")
df.solar$TIME <- dmy_hm(df.solar$TIME)

# ich muss noch lernen eleganter mit lubridate umzugehen
# aber trotzdem hier von 2015-2018 normiert
start.d <- ymd_hm("2015-01-01 00:00")
stop.d <- ymd_hm("2017-12-31 23:00")
ind.start <- which(df.solar$TIME == start.d)
ind.stop <- which(df.solar$TIME == stop.d)
df.solar <- df.solar[ind.start: ind.stop, ]


### 2b. *SOLAR AT ----

bruno.atsolar <- function(x){
    y <- subset(x, select = c("V1", "V7"))
    names(y) <- c("TIME", "SOLAR MW AT")
    y$`SOLAR MW AT` <- as.numeric(y$`SOLAR MW AT`)
    y$TIME <- dmy_hms(y$TIME)
    return(y)
}

df.solar.AT1 <- bruno.atsolar(df.ren.AT1)
df.solar.AT2 <- bruno.atsolar(df.ren.AT2)
df.solar.AT3 <- bruno.atsolar(df.ren.AT3)
df.solar.AT4 <- bruno.atsolar(df.ren.AT4)
df.solar.AT5 <- bruno.atsolar(df.ren.AT5)
df.solar.AT6 <- bruno.atsolar(df.ren.AT6)
df.solar.AT7 <- bruno.atsolar(df.ren.AT7)





## UNITE --

# same for wind
## --------------------------- ## 

### 2c. *WIND DE----

df.wind <- subset(df.wind.D, select = c("Datum","von","X50Hertz..MW.", 
                                        "Amprion..MW.", "TenneT.TSO..MW.", "TransnetBW") )
df.wind <- unite(df.wind, TIME, c("Datum", "von"), sep = " ")
names(df.wind) <- c("TIME", "50Hertz (MW)", 
                     "Amprion (MW)", "TenneT TSO (MW)", "Transnet BW (MW)")
df.wind$TIME <- dmy_hm(df.wind$TIME)


start.d <- ymd_hm("2015-01-01 00:00")
stop.d <- ymd_hm("2017-12-31 23:00")
ind.start <- which(df.wind$TIME == start.d)
ind.stop <- which(df.wind$TIME == stop.d)
df.wind <- df.wind[ind.start: ind.stop, ]


### 2c. *WIND AT -----

head(df.ren.AT1)

bruno.atwind <- function(x){
  y <- subset(x, select = c("V1", "V5"))
  names(y) <- c("TIME", "WIND MW AT")
  y$`WIND MW AT` <- as.numeric(y$`WIND MW AT`)
  y$TIME <- dmy_hms(y$TIME)
  return(y)
}

df.wind.AT1 <- bruno.atwind(df.ren.AT1)
df.wind.AT2 <- bruno.atwind(df.ren.AT2)
df.wind.AT3 <- bruno.atwind(df.ren.AT3)
df.wind.AT4 <- bruno.atwind(df.ren.AT4)
df.wind.AT5 <- bruno.atwind(df.ren.AT5)
df.wind.AT6 <- bruno.atwind(df.ren.AT6)
df.wind.AT7 <- bruno.atwind(df.ren.AT7)




### 2d. *DEMAND ----

bruno.DEM = function(x) {
  y <- subset(x, select = c("Time..CET.", 
                       "Day.ahead.Total.Load.Forecast..MW....BZN.DE.AT.LU"))
  names(y) <- c("TIME", "DAY-AHEAD MW")
  y <- separate(y, col = TIME, into = c("TIME","bis"), sep =  " - ")
  y <- subset(y, select = c("TIME","DAY-AHEAD MW"))
  y$TIME <- dmy_hm(y$TIME)
  y$`DAY-AHEAD MW` <- as.numeric(y$`DAY-AHEAD MW`)
  return(y)
}

df.dem.2015 <- bruno.DEM(df.dem.2015.0)
df.dem.2016 <- bruno.DEM(df.dem.2016.0)
df.dem.2017 <- bruno.DEM(df.dem.2017.0)
df.dem.2018 <- bruno.DEM(df.dem.2018.0)

df.dem.2017 <- na.omit(df.dem.2017)

str(df.dem.2015)
head(df.dem.2018)

### 2e. (GAS) ----












##### KRAM DEN ICH NOCH NICHT WEGWERFEN WOLLTE; DER ABER SONST NERVT.. -----

# Solar DE - KRAMS


## Damit nochmal nach NA etc. checken ## 
str(df.solar)
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



#Solar AT - alt krams# 
df.solar.AT1 <- subset(df.ren.AT1, select = c("V1", "V7"))
names(df.solar.AT1) <- c("TIME", "SOLAR MW AT")
df.solar.AT1$`SOLAR MW AT` <- as.numeric(df.solar.AT1$`SOLAR MW AT`)
df.solar.AT1$TIME <- dmy_hms(df.solar.AT1$TIME)



# DEM - Alt krams 



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

