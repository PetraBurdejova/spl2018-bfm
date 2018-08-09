
###############################################################################     
####    MOEinterpolation.R    ###ä#############################################   
###############################################################################     
# 
# This code deals with NA's for Solar electricity production 
# and electrical demand. 
# The missing values in the wind generation data are not handled here, as they 
# they will be handled on a daily basis in a later step.
# 
#
# Input: 'MOEdata_clean.Rdata' from the 'MOErawdata' Quantlet.
#
# Ouput:  MOEdata_interp_csv/<variable>.csv     - data in table form
#         MOEdata_interp.Rdata                  - data in Rdata form
#
###############################################################################


# Clear all variables.
rm(list = ls(all = TRUE))
graphics.off()

# Install and load libraries.
libraries = c("xts", "StreamMetabolism", "lubridate")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Set default time to "UTC"
Sys.setenv(TZ = "UTC") 



###############################################################################
####    0.  SET WORKING DIRECTORY    ##########################################
###############################################################################

####    ATTENTION: Working directory is assumed to be the root of the MOE 
####    repository, not the MOEmergedata Quantlet subdirectory!!!


# If needed, set working directory accordingly:
#setwd("path/to/MOE_repository")



###############################################################################
####    1.  LOAD CLEAN DATA    ################################################
###############################################################################

load("MOErawdata/MOEdata_clean.Rdata")



###############################################################################
####    2.  MATCH TIMEFRAMES    ###############################################
###############################################################################


###############################################################################
####    DEFINE SUBROUTINES    #################################################

time.FRAME = function(x) {
    # Chooses Time frame for all variables
    #
    # Args:
    #   x: Imported dataframe
    #
    # Returns:
    #   y: Dataframe with right time frame
    #
    # TODO: Adjust timeframe dynamically.
    #
    start.d     = ymd_hm("2015-01-01 00:00")
    stop.d      = ymd_hm("2017-12-31 23:00")
    ind.start   = which(x$TIME == start.d)
    ind.stop    = which(x$TIME == stop.d)
    ind         = (ind.start: ind.stop)
    y           = x[ind, ]
    return(y)
}


###############################################################################
####    APPLY SUBROUTINES    ##################################################

# Match timeframes.
df.dm       = time.FRAME(df.dm)
df.pun      = time.FRAME(df.pun)
df.solar    = time.FRAME(df.solar)
df.wind     = time.FRAME(df.wind)
df.solar.AT = time.FRAME(df.solar.AT)
df.wind.AT  = time.FRAME(df.wind.AT)




###############################################################################
####    3.  CONVERT DATAFRAMES TO XTS FORMAT    ###############################
###############################################################################


xts.dm = xts(df.dm[,-1], order.by = df.dm$TIME)
xts.solar = xts(df.solar[,-1], order.by= df.solar$TIME)



###############################################################################
####    4.  INTERPOLATE MISSING VALUES    #####################################
###############################################################################


###############################################################################
####    4a. ONLY SOLAR: REPLACE NIGHTTIME NA's WITH 0
###############################################################################


###############################################################################
####    DEFINE SUBROUTINES    #################################################

Sunrise.DE = function(Date){
  # Gives Time of Sunrise for "Date" in a matrix of size 1 * 2

  Location = c(13.413215, 52.521918)  ##Coordinates of Berlin (Length, Width)
  Sunrise  = sunriset(matrix(Location, nrow=1),
                     Date , 
                     direction="sunrise", 
                     POSIXct.out = TRUE)
}

Sunset.DE = function(Date){
  # Gives Time of Sunset for for "Date" in a matrix of size 1 * 2
  
  Location = c( 13.413215, 52.521918) ##Coordinates of Berlin (Length, Width)
  Sunset = sunriset( matrix(Location, nrow=1),
                     Date ,
                     direction="sunset",
                     POSIXct.out = TRUE)
}


###############################################################################
####    APPLY SUBROUTINES    ##################################################

for (TSO in names(df.solar[-1])){ 
  # Repeats the following procedure over the 4 colums 
  # FZHertz", "Amprion", "TenneT.TSO", "Transnet.BW" 
 
  if (length(subset(df.solar, is.na(df.solar[,TSO]) == TRUE)[,TSO])>0) { 
    # Tests whether the column has no NA's
    # as this would lead to an error in the loop
    
    Missings = subset(df.solar, is.na(df.solar[,TSO]))[,c("TIME", TSO)]
    # Selects only the rows of df.solar that contain NA's in the column of 
    # the index "TSO"
    # Was implemented for efficiency, as it would take a significant amount 
    # of time to repeat this procedure over all rows
     
    sunrise.DE.List = lapply(Missings$TIME, Sunrise.DE) 
    # Applies the above defined function (Sunrise.DE) on the dates of the 
    # missing values for the column "TSO"
    # This will give a list containing two elements per calculated function
    # ( 1 per row of x.missing)
    
    sunrise.DE.df = do.call(rbind,sunrise.DE.List) 
    # Transforms the List into a data.frame
    
    sunset.DE.List = lapply(Missings$TIME, Sunset.DE)
    # Applies the above defined function (Sunset.DE)on the dates of the 
    # missing values for the column "TSO"
    # This will give a list containing two elements per calculated 
    # function ( 1 per row of x.missing)
    
    sunset.DE.df = do.call(rbind,sunset.DE.List) 
    # Transforms the List into a data.frame (easier to handle)
    
    Missings[Missings$TIME < sunrise.DE.df$time | Missings$TIME > sunset.DE.df$time, TSO] = as.numeric(0.0)
    # For all values of the df "Nissings", replace NA's by the value 0.0 whenever 
    # it is before sunrise or after sunset
     
    xts.solar[Missings$TIME, TSO]=Missings[,TSO]
    # replaces missing solar values at night by 0 in the xts file containing 
    # all solar data
     
  }
}


###############################################################################
####    4b. NEAREST NEIGHBOUR INTERPOLATION
###############################################################################


# Only applies for up to 4 consecutive NA's.
xts.solar = na.approx(xts.solar,na.rm=TRUE, maxgap=4)
xts.dm = na.approx(xts.dm ,na.rm=TRUE, maxgap=4)

  
###############################################################################
####    4c. TREND INTERPOLATION    ############################################
###############################################################################

####    For values with strong seasonal variations within the day,
####    the week and the year (solar production and demand) we
####    interpolate by averaging between neighbouring days.


###############################################################################
####    INTERPOLATE SOLAR BY DAILY TREND    ###################################

# Build vector of quater-hours.
HM = function(Date){
  format(Date, "%H:%M")
}

Begin    = as.POSIXct("2011-03-31 00:00:00")
End      = as.POSIXct("2011-03-31 23:45:00")
quarters = seq(Begin, End, length.out=96)

quarters.day = HM(quarters)

# Interpolate SOLAR NA's by same hour values of neighbouring days.
solar.quarters.list = list()

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
####    INTERPOLATE DEMAND BY DAILY AND WEEKLY TREND    #######################

# Build vector of days.
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
  
    dm.day.list[[Day]][[quarter]]= xts.dm[HM(index(xts.dm)) == quarter &
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



###############################################################################
####    5. CONVERT BACK TO DATAFRAMES AND SAVE    #############################
###############################################################################


dm.temp            = as.data.frame(xts.dm)
colnames(dm.temp)  = "DEM"
df.dm$DEM          = dm.temp[ , 1]

solar.temp         = as.data.frame(xts.solar) 
ColumnNamesSolar   = colnames(df.solar)
df.solar           = cbind(df.solar[, 1], solar.temp)
rownames(df.solar) = c()
colnames(df.solar) = ColumnNamesSolar

# Save dataframes as '.Rdata' file for easy read-in in R.
save(df.pun, df.solar, df.solar.AT, df.wind, df.wind.AT, df.dm,
     file="MOEinterpolation/MOEdata_interp.Rdata"
)

# Save dataframes as '.csv' files for use with other software.
df.list     = list(df.pun, df.solar, df.solar.AT, 
               df.wind, df.wind.AT, df.dm)

df.names   = c("df_pun.csv", "df_solar.csv", "df_solar_AT.csv",
                 "df_wind.csv", "df_wind_AT.csv", "df_dm.csv")

lapply(1:length(df.list), 
       function(i) write.csv((df.list[i]), 
                             file = paste0("MOEinterpolation/MOEdata_interp_csv/", 
                                           df.names[i])))

###############################################################################
####    6. CLEAN UP ENVIRONMENT    ############################################
###############################################################################


rm(list=ls()[! ls() %in% c("df.pun",
                           "df.solar",
                           "df.solar.AT",
                           "df.wind", 
                           "df.wind.AT",
                           "df.dm"
)]) 



