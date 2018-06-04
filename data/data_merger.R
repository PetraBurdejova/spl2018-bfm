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

# don't forget to include timezones!

# 1. READ DATA FROM DATA/SOURCE SUBDIRECTORY
### 1. *LOAD DATA    ----
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


# 1.b LOOK FOR NA'S

# 2. PREPARE DATA FOR MERGE
# 2. CLEAN ALL VARIABLES
### 2a. *PUN         ----
df.pun <- subset( df.pun.0, select = c(HourUTC, SpotPriceEUR ) )
colnames(df.pun) = c("TIME", "PUN")
df.pun$TIME <- ymd_hm(df.pun$TIME)

str(df.pun)
head(df.pun)

### 2b.1 *SOLAR DE   ----

#subsetting and first cleaning
df.solar <- subset(df.solar.D, select = c("Datum","von","X50Hertz..MW.", 
                        "Amprion..MW.", "TenneT.TSO..MW.", "Transnet.BW..MW.") )
df.solar <- unite(df.solar, TIME, c("Datum", "von"), sep = " ")
names(df.solar) <- c("TIME", "50Hertz (MW)", 
                     "Amprion (MW)", "TenneT TSO (MW)", "Transnet BW (MW)")
df.solar$TIME <- dmy_hm(df.solar$TIME)

#abgrenzen der Zeiträume
# ich muss noch lernen eleganter mit lubridate umzugehen
# aber trotzdem hier von 2015-2018 normiert
start.d <- ymd_hm("2015-01-01 00:00")
stop.d <- ymd_hm("2017-12-31 23:00")
ind.start <- which(df.solar$TIME == start.d)
ind.stop <- which(df.solar$TIME == stop.d)
df.solar <- df.solar[ind.start: ind.stop, ]

#durchschnitt pro stunde
df.solar <- aggregate(list(df.solar$`50Hertz (MW)`, 
                           df.solar$`Amprion (MW)`, df.solar$`TenneT TSO (MW)`, 
                           df.solar$`Transnet BW (MW)`), 
                      list("TIME" = cut(df.solar$TIME, "1 hour")), FUN = mean)

names(df.solar) <- c("TIME", "50Hertz (MW)", "Amprion (MW)", "TenneT TSO (MW)", 
                  "Transnet BW (MW)")

df.solar$TIME <- ymd_hms(df.solar$TIME)

names(df.solar)
#durchschnitt pro stunde und durchschnitt der firmen


# funktioniert nicht... :/ gucke ich später an
# problem ist, dass er nicht über die verschiedenen rows laufen will mit sapply oder so..

mean.bruno <- function(x) {
  x1 <- x$`50Hertz (MW)`
  x2 <- x$`Amprion (MW)`
  x3 <- x$`TenneT TSO (MW)`
  x4 <- x$`Transnet BW (MW)`
   y <- (x1 + x2 + x3 + x4)
  return(y)
}


# alternativer versuch - geht bestimmt auch einfacher oder??

sum.bruno <- sapply(df.solar[, -1], sum, FUN.VALUE = T)





-
mean.bruno(df.solar[41, 2:5])
df.solar[36:45, 2:5]

test <- sapply(df.solar, mean.bruno)

str(df.solar)

plot(df.solar)
summary(df.solar)

tail(df.solar, n = 50)
### 2b.2 *SOLAR AT   ----

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
# habe jetzt hier das problem, dass der letzte eintrag
# vom ersten df == dem ersten des neuen df ist.
# ich glaube ich hole die daten nochmal neu,
#d dann spar ich mir unnötiges gecode, das albern aussieht.

solar.at <- rbind(df.solar.AT1,df.solar.AT2,df.solar.AT3)
solar.at[4365:4400, ]  # <- hier liegt das problem
tail(df.solar.AT1)
head(df.solar.AT2)

# geschafft.. jetzt rbind und dann läuft

df.solar.AT <- rbind(df.solar.AT1, df.solar.AT2,df.solar.AT3,df.solar.AT4,
                      df.solar.AT5,df.solar.AT6,df.solar.AT7)

# finished
str(df.solar.AT)
plot(df.solar.AT)


### 2c. *WIND DE     ----

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


### 2c. *WIND AT     -----

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


df.wind.AT <- rbind(df.wind.AT1,df.wind.AT2,df.wind.AT3,df.wind.AT4,
                    df.wind.AT5,df.wind.AT6,df.wind.AT7)



### 2d. *DEMAND      ----

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

# bind 
df.dm <- rbind(df.dem.2015,df.dem.2016,df.dem.2017,df.dem.2018)

#mean per hour
df.dm <- aggregate(list("DAY-AHEAD-MW" = df.dm$`DAY-AHEAD MW`), 
                  list("TIME" = cut(df.dm$TIME, "1 hour")), FUN = mean)
names(df.dm) <- c("TIME", "DAY-AHEAD MW")
df.dm$TIME <- ymd_hms(df.dm$TIME)
# besser wäre hier evtl. : 
# https://stackoverflow.com/questions/13915549/
# average-in-time-series-based-on-time-and-date-in-r
# muss ich nochmal checken, ob das nicht besser mit einem time series package ist..

# Chek for NA
summary(df.dm)
ind <- which(is.na(df.dm$`DAY-AHEAD MW`))
df.dm[(ind-5):(ind+5), ]
# Darf man nicht machen, aber: Fülle den Value mit mean aus den umgebungen
df.dm$`DAY-AHEAD MW`[ind] <- mean(df.dm$`DAY-AHEAD MW`[(ind-5):(ind+5)], 
                                  na.rm = T)





test <- aggregate(list("DAY-AHEAD-MW" = df.dm$`DAY-AHEAD MW`), 
                   list("TIME" = cut(df.dm$TIME, "1 day")), FUN = mean)
names(test) <- c("TIME", "DAY-AHEAD MW")
test$TIME <- ymd(test$TIME)

plot(df.dm)

tail(df.dm)
summary(df.dm)
str(df.dm)
### 2e.  GAS         ----










### 3.  FINAL DATAFRAME        ----

df <- rbind()




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



head(df.dem.2017.0)
str(df.dem.2017)

identical(as.numeric(df.dem.2017.0$Day.ahead.Total.Load.Forecast..MW....BZN.DE.AT.LU), 
          df.dem.2017$`DAY-AHEAD MW`)




## HUCH!!! Was ist mit Demand los? ------


#test: habe ich die daten beim verarbeiten verändert?
test1 <- sapply(as.numeric(df.dem.2017.0$Day.ahead.Total.Load.Forecast..MW....BZN.DE.AT.LU), mean, na.rm = T)
test2 <- sapply(as.numeric(df.dem.2016.0$Day.ahead.Total.Load.Forecast..MW....BZN.DE.AT.LU), mean, na.rm = T)

mean(test1,na.rm =T)
mean(test2,na.rm =T)

# >> ne liegt wohl an den anfangs Daten

# neu daten runtergeladen um das zu prüfen
test3 <- read.csv("source/test - Total Load - Day Ahead _ Actual_201701010000-201801010000.csv")


identical(as.numeric(test3$Day.ahead.Total.Load.Forecast..MW....BZN.DE.AT.LU), as.numeric(df.dem.2017.0$Day.ahead.Total.Load.Forecast..MW....BZN.DE.AT.LU))

# >> ne werte sind gleich.. Großes Problem.
# Aber time series wird ja eh in Jahre gesplittet oder??
# dann könnte das ja funktionieren..




test <- aggregate(list("DAY-AHEAD-MW" = df.dm$`DAY-AHEAD MW`), 
                  list("TIME" = cut(df.dm$TIME, "1 day")), FUN = mean)
names(test) <- c("TIME", "DAY-AHEAD MW")
test$TIME <- ymd(test$TIME)


plot(test)






# Bruno merkliste
# - alle werte pro tag ausrechnen
# - sonne und wind de noch die einzelnen betreiber zusammenfassen
# - zusammenführen von allen daten
# - 
