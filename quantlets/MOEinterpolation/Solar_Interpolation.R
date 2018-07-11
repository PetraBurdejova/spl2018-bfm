###############################################################################     
####   Solar_Interpolation.R    #######################################################     
###############################################################################     
#
#This code deals with NA's for Solar electricity production, after the NA's that happen 
#at night have been replaced by the code of "Solar_Night_Replace.R
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


load("MOErawdata/MOEdata_clean.Rdata")

##################################################################################
#### Part 1. : Interpolating values for NA's using an average of previous and next
####           value. Restricting on a certain amount of maximal consecutively NA's
###################################################################################


#################################################################################
####  1.1 Interpolate data of df.solar  ########################################
################################################################################

df.solar.xts_temp=df.solar.xts ##df.solar.xts_temp --> to be changed by df.solar.xts


for (TSO in names(df.solar.xts)){
df.solar.xts_temp[,TSO] = na.approx(df.solar.xts[,TSO],na.rm=TRUE, maxgap=4)

}


#summary(df.solar.xts_temp)

#summary(df.solar.xts)


###################################################################################
#### 1.2 Interpolate data for df.dm  ###############################################
###################################################################################


df.dm.xts= xts(df.dm[,-1], order.by=df.dm$TIME )
df.dm.xts_temp = df.dm.xts




df.dm.xts_temp = na.approx(df.dm.xts,na.rm=TRUE, maxgap=4)
  

#summary(df.dm.xts_temp)

#summary(df.dm.xts)


####################################################################################???
####1.3 Interpolate data for df.dm  ################################################"
#####################################################################################


df.wind.xts= xts(df.wind[,-1], order.by=df.wind$TIME )
df.wind.xts_temp = df.wind.xts

for (TSO in names(df.wind.xts)){
  df.wind.xts_temp[,TSO] = na.approx(df.wind.xts[,TSO],na.rm=TRUE, maxgap=200)
  
}



summary(df.wind.xts_temp)

summary(df.wind.xts)


#######################################################################################################


#######################################################################################################
#### Part 2. : For values with strong seasonal variations within the day, the week and the year
####          (solar production and demand) n average of values at the same hour one week before and week after will be used
###################################################################################


#################################################################################
####  2.1 Interpolate data of df.solar  ########################################
################################################################################

df.solar.xts_temp= df.solar.xts

quarters = seq(as.POSIXct("2011-03-31 00:00:00"), as.POSIXct("2011-03-31 23:45:00"), length.out=96)
quarters.of.the.day = format(quarters, "%H:%M")

df.solar.by.quartes.list = list()


##Test1


for (quarter in quarters.of.the.day ) {
  df.solar.by.quartes.list[[quarter]]= df.solar.xts_temp[format(index(df.solar.xts_temp), "%H:%M")==quarter]
  
  #Length=length(df.solar.by.quartes.list[[quarter]][,1])
  
  
  
  df.solar.xts_temp[index(df.solar.by.quartes.list[[quarter]])]= na.approx(df.solar.by.quartes.list[[quarter]], na.rm=FALSE, maxgap=1000)
 
}

##Test2

df.solar.xts.temp= df.solar.xts

for (quarter in quarters.of.the.day ) {
  df.solar.by.quartes.list[[quarter]]= df.solar.xts_temp[format(index(df.solar.xts_temp), "%H:%M")==quarter]
  #Have a look at this xts method: https://campus.datacamp.com/courses/manipulating-time-series-data-in-r-with-xts-zoo/apply-and-aggregate-by-time?ex=3
  #It might be better
  #Length=length(df.solar.by.quartes.list[[quarter]][,1])
  

    df.solar.xts.temp[index(df.solar.by.quartes.list[[quarter]])]= na.approx(df.solar.by.quartes.list[[quarter]], na.rm=FALSE, maxgap=1000)
}


# df.solar.by.quartes.list[[quarter]][, TSO]
df.solar.xts_temp
df.solar.by.quartes.list



df.solar.by.quartes.list[["07:45"]]= df.solar.xts_temp[format(index(df.solar.xts_temp), "%H:%M")== "07:45"]
df.solar.xts.temp[index(df.solar.by.quartes.list[["07:45"]])] = na.approx(df.solar.by.quartes.list[["07:45"]],na.rm=TRUE, maxgap=2)
summary(na.approx(df.solar.xts_temp,na.rm=TRUE, maxgap=2))


Test =  df.solar.xts.temp[24756:24840,]
na.approx(Test, na.rm=FALSE, maxgap=1000)

na.approx(df.solar.by.quartes.list[["07:45"]][1885:1995,],na.rm=TRUE, maxgap=2)

which(is.na(df.solar.by.quartes.list[["07:45"]][,4]))
which(is.na(df.solar.by.quartes.list[["07:45"]][,2]))

df.solar.xts.temp[index(df.solar.by.quartes.list[["07:45"]])]