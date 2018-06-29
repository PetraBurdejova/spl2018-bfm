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

library("StreamMetabolism")
library("xts")


Sys.setenv(TZ = "UTC") #Sets system time to "UTC"
#index(x[2,1]) == "2011-04-01 00:15:00 UTC" other wise Problem with time zones --> Not matching

##load df solar first 
 
x = xts(df.solar[,-1], df.solar$TIME)   
#Converts the data frame into an xts file (it has a date index)
  
DataDayOne   = df.solar[1,1] #First day of the data
#DataDayOne   = index(x)[1]
NumberOfDays = length(unique(format(df.solar$TIME, "%D")))  #Length of the data
#NumberOfDays = length(unique(format(index(x), "%D")))

SUN = sunrise.set(52.521918, 13.413215,DataDayOne , timezone = "UTC"
                  , num.days = NumberOfDays )

SUN = matrix(nrow = length(df.solar$TIME), ncol=2)

SUN.xts= xts(SUN, df.solar$TIME)


for (Date in index(SUN.xts)) { SUN.xts[Date,1] = sunrise.set(52.521918, 13.413215, format(Date, "%D") , timezone = "UTC"
                  , num.days = 1 )[1,1]
}
 

#SUN = matrix(nrow = length(df.solar[,1]), ncol=2)

#for (i in c(1:length(df.solar[,"TIME"]))){
 # SUN[i,1]=sunrise.set(52.521918, 13.413215,df.solar[i,"TIME"] , timezone = "UTC", num.days = 1 )[1,1]
  #SUN[i,2]=sunrise.set(52.521918, 13.413215,df.solar[i,"TIME"] , timezone = "UTC", num.days = 1 )[1,2]
#}


#Generates a data.frame giving the sunrise (1.Column) 
# and sunset time (2.Column)
  

df.solar[(is.na(df.solar[,2])==TRUE & ( (df.solar$TIME < SUN[,1]) | (df.solar$TIME > SUN[,2]) ) ), 2] =0
df.solar[(is.na(df.solar[,3])==TRUE & ( (df.solar$TIME < SUN[,1]) | (df.solar$TIME > SUN[,2]) ) ), 3] =0
df.solar[(is.na(df.solar[,4])==TRUE & ( (df.solar$TIME < SUN[,1]) | (df.solar$TIME > SUN[,2]) ) ), 4] =0
df.solar[(is.na(df.solar[,5])==TRUE & ( (df.solar$TIME < SUN[,1]) | (df.solar$TIME > SUN[,2]) ) ), 5] =0
#Replaces NA's for solar production by 0 if before sunrise or after sunset

#x[(is.na(x[,2])==TRUE & ( (index(x) < SUN[,1]) | (index(x) > SUN[,2]) ) ), 1] =0
#x[(is.na(x[,3])==TRUE & ( (index(x) < SUN[,1]) | (index(x) > SUN[,2]) ) ), 1] =0
#x[(is.na(x[,4])==TRUE & ( (index(x) < SUN[,1]) | (index(x) > SUN[,2]) ) ), 1] =0
#If df.solar are converted to xls in x



as.data.frame(x)
  



  
  
 
  
   
             
  
  
  
  
}

sunrise.set(52.521918, 13.413215, index(x[,]), timezone = "UTC", num.days = 1)$sunrise



