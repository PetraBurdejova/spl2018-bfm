###############################################################################     
####   Solar_Interpolation.R    ###############################################   
###############################################################################     
# 
# This code deals with NA's for Solar electricity production, after the NA's 
# that happen at night have been replaced by the code of "Solar_Night_Replace.R
# 
#
# Input: "df.solar_step1" with Na's
#
# Ouput: "df.solar_witout_NA" without NA's
#
###############################################################################

#setwd("C:/Users/PC-FELIX/Documents/GitHub/spl2018-bfm/quantlets")

library("xts")

Sys.setenv(TZ = "UTC") #Sets system time to "UTC"




###############################################################################
#### Part 0. : Load data
###############################################################################

load("MOEinterpolation/MOEsolar_xts.Rdata")
load("MOErawdata/MOEdata_clean.Rdata")


df.dm.xts      = xts(df.dm[,-1], order.by = df.dm$TIME)
xts.dm         = subset(df.dm.xts, index(df.dm.xts)< "2018-06-29 00:00:00" )




###############################################################################
#### Part 1. : Interpolating values for NA's using an average of previous
####           and next value. Restricting on a certain amount of maximal 
####           consecutive NA's
###############################################################################


###############################################################################
####  1.1 Interpolate data of df.solar  #######################################
###############################################################################

xts.solar = na.approx(df.solar.xts,na.rm=TRUE, maxgap=4)


###############################################################################
#### 1.2 Interpolate data for df.dm  ##########################################
###############################################################################

xts.dm = na.approx(xts.dm ,na.rm=TRUE, maxgap=4)

  
###############################################################################
#### Part 2. : For values with strong seasonal variations within the day,
####           the week and the year (solar production and demand) n average 
####           of values at the same hour  will be used
###############################################################################


###############################################################################
####  2.1 Interpolate data of df.solar  #######################################
###############################################################################

HM = function(Date){
  format(Date, "%H:%M")
}


Begin    = as.POSIXct("2011-03-31 00:00:00")
End      = as.POSIXct("2011-03-31 23:45:00")
quarters = seq(Begin, End, length.out=96)


quarters.day = HM(quarters)

solar.quarters.list = list()

# Find solution for leading and trailing NA's --> Delete at the end????


for (quarter in quarters.day) {
# This loop splits up the orginal xts into xts files for every quarter hour. 
# (One xts for all data for time "00:00", one for "00:15" etc...)
# This is done so that missing values can be interpolated on data for the
# same quarter hour.
  
  
  solar.quarters.list[[quarter]] = xts.solar[HM(index(xts.solar)) == quarter]
  #splits the solar data up into data by quarter-hours ( 1 xts per quarter)
  
  interpolated.values = na.approx(solar.quarters.list[[quarter]],
                                  na.rm = FALSE,
                                  maxgap = 2)
  # Interpolates NA's by Calculating the mean of the previous and the next
  # hour-quarterly solar generation value
  # ie: if there is the solar generation missing for "2011-03-31 08:00:00"
  # it averages the values for "2011-03-30 08:00:00" 
  # and for "2011-04-01 08:00:00"
  
  
  xts.solar[index(solar.quarters.list[[quarter]]),] = interpolated.values
  #replaces the original data by the interpolated one.
  
  # print(summary(solar.quarters.list[[quarter]]))
  # print(summary(na.approx(solar.quarters.list[[quarter]],
  # na.rm=FALSE,
  # maxgap=4)))
  
}

###############################################################################
####  2.2 Interpolate data for demand  ########################################
###############################################################################

plot(xts.dm [
  index(xts.dm) > "2018-03-01 00:00:00" & 
  index(xts.dm) < "2018-04-01 00:00:00"])
# Plot to illustrate why we average  according to the quarters and the days of 
# the week

dm.day.list = list()
day.week = c(1,2,3,4,5,6,7)

WeekDay = function(Date){
  as.POSIXlt(Date)$wday
}


for (Day in day.week){
# This double loop splits up the orginal xts into xts files for every quarter 
# hour and day of the week.(One xts for all data for time "00:00" on Monday, 
# one for "00:00" on tuesday etc...)This is done so that missing values can be
# interpolated on data for the same time and the same day of the week.
  
  dm.day.list[[Day]]=list()
  #defining the nested list
  
  for (quarter in quarters.day ) {
  
    dm.day.list[[Day]][[quarter]]= xts.dm[HM(index(xts.dm)) == quarter 
                                          &
                                          WeekDay(index(xts.dm)) == Day-1]
    
     # creates a new xts for each quarter hour for each day of the week
    
    
    interpolated.values = na.approx(dm.day.list[[Day]][[quarter]], 
                                    na.rm=FALSE,
                                    maxgap=2)
    # for every such xts the NA's are interpolated. 
    # ie: a NA on Monday 25th of March at 12:00 is interpolated by the demand 
    # values on Monday the 18th of march at 12:00
    # and Monday the 1st of April at 12:00

  
    xts.dm[index(dm.day.list[[Day]][[quarter]])] = interpolated.values
    #all values of the original xts file get replaced by the interpolated ones 
  }
  
}