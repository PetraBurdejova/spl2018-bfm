###############################################################################     
####   Solar_Night_Replace.R    #######################################################     
###############################################################################     
#
#This code builds up a function that replaces missing values that occur at night by
#the value 0. It is only applicable to solar energy production
# 
#
# Input:  -
#
# Ouput: -
#
###############################################################################

#setwd("C:/Users/PC-FELIX/Documents/GitHub/spl2018-bfm/quantlets")
library("StreamMetabolism")
library("xts")


Sys.setenv(TZ = "UTC") #Sets system time to "UTC"
#index(x[2,1]) == "2011-04-01 00:15:00 UTC" other wise Problem with time zones --> Not matching otherwise

##load df solar first 
load("MOErawdata/MOEdata_clean.Rdata")
x = df.solar  
x1= xts(df.solar[,-1], order.by= df.solar$TIME)


Sunrise.DE= function(Date){
  Location= c(13.413215, 52.521918)  ##Coordinates of Berlin (Length, Width)
  Sunrise = sunriset(matrix(Location, nrow=1), Date , direction="sunrise", POSIXct.out = TRUE)
  ##Gives Time of Sunrise for "Date" in a matrix of size 1 * 2
}

Sunset.DE=function(Date){
  Location= c( 13.413215, 52.521918) ##Coordinates of Berlin (Length, Width)
  Sunset = sunriset( matrix(Location, nrow=1), Date , direction="sunset", POSIXct.out = TRUE)
  ##Gives Time of Sunset for for "Date" in a matrix of size 1 * 2
}



for (TSO in names(x[-1])){ #Repeats the following procedure over the 4 colums 
#"FZHertz", "Amprion", "TenneT.TSO", "Transnet.BW"
print(TSO)
  
if(length(subset(x, is.na(x[,TSO])==TRUE)[,TSO])>0){ 
#Tests whether the column has no NA's
#, as this would lead to an error in the loop
  
x.missing= subset(x, is.na(x[,TSO]))[,c("TIME", TSO)]
#Selects only the rows of x/df.solar that contain NA's in the column of the index "TSO"
#--> Was implemented for efficiency, as it would take a significant amount of time 
#To repeat this procedure over all rows

sunrise.DE.List= lapply(x.missing$TIME, Sunrise.DE) 
##Applies the above defined function (Sunrise.DE) on the dates of the missing values for the column "TSO"
##This will give a list containing two elements per calculated function ( 1 per row of x.missing)

sunrise.DE.df = do.call(rbind,sunrise.DE.List) 
#Transforms the List into a data.frame


sunset.DE.List=  lapply(x.missing$TIME, Sunset.DE)
##Applies the above defined function (Sunset.DE)on the dates of the missing values for the column "TSO"
##This will give a list containing two elements per calculated function ( 1 per row of x.missing)


sunset.DE.df = do.call(rbind,sunset.DE.List) #Transforms the List into a data.frame
#Transforms the List into a data.frame (easier to handle)


x.missing[x.missing$TIME < sunrise.DE.df$time | x.missing$TIME > sunset.DE.df$time, TSO] = as.numeric(0.0)
#For all values of the df "x.missing", replace NA's by the value 0.0 whenever it is before sunrise or after sunset




x1[x.missing$TIME, TSO]=x.missing[,TSO]


}else{
  print("column has no missing values")
}
}
##Convert again into data Frame

df.solar_Test = cbind("TIME"=index(x1), as.data.frame(x1))











  
  
 
  
   
             
  
  
  






