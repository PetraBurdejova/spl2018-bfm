###############################################################################     
####   MOEtimedummies.R    ####################################################   
###############################################################################     
# 
# This code defines the function "YMDDummy" that adds a matrix of dummy 
# variables for years, months and days on the right side of an xts object. 
# The parameter "FullDat.xts" specifies the xts object for which to generate 
# time dummies
# For each type of dummy (Years, Months, Days), a dummy to be left out can be 
# specified in the 3 other parameters.
# e.g: the parameter del.Y specifies the number of the year to be left out 
# (1 for the oldest year, 2 for the second oldest year etc...)
# e.g: the parameter del.M specifies the number of the Month to be left out 
# (1 for January, 2 for february...)
# e.g: the parameter del.D specifies the number of the Day to be left out 
# (1 for Sunday, 2 for Monday, 3 for Tuesday etc.. until 7 for Saturday)

###############################################################################


# Install and load libraries.
libraries = c("xts")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)



###############################################################################
####    1. DEFINE YMDdummy FUNCTION    ######################################## 
###############################################################################

YMDDummy = function(FullDat.xts, del.Y = 1, del.M = 1, del.D = 1){
  
  Year.min                    = format(min(index(FullDat.xts)), "%Y")
  Year.max                    = format(max(index(FullDat.xts)), "%Y")
  Year.Vector                 = seq(from=Year.min, to=Year.max, by=1)
  Year.min.number             = as.numeric(Year.min)
  Year.max.number             = as.numeric(Year.max)
  
  #############################################################################
  ####    Step 0: Check parameters    #########################################
  
  if(is(FullDat.xts, "xts")==FALSE){ ###TO BE FIXED
      stop("The called object is not an xts")
  }else if(del.Y<1 | del.Y> Year.max.number-Year.min.number+1 ){
      stop('The specified Year to be left away in the dummy variables does not exist')
  }else if(del.M<1 | del.M>12){
      stop('The specified Month to be left away in the dummy variables does not exist')
  }else if(del.D<1| del.D>7){
      stop('The specified Month to be left away in the dummy variables does not exist')
  }else{
  
    
  #############################################################################
  ####    Step 1: Create the dummy variables for years, months and days    ####
  
  Length = length(FullDat.xts[,1])
  
  Year = function(x){
      #Gives the year for a specific date
      format(index(x), "%Y")
  }
  

  #############################################################################  
  ####        1.1 Dummy variables for Years    ################################
  
  NumYears                      = Year.max.number-Year.min.number+1
  Year.Dummy.matrix             = matrix(,nrow = Length , ncol=NumYears)
  colnames(Year.Dummy.matrix)   = Year.Vector

  for (i in 1:length(Year.Vector)) {
      Year.Dummy.matrix[,i] = Year(FullDat.xts) == Year.Vector[i]
  }


  #############################################################################  
  ####        1.2 Dummy variables for Months    ###############################

  Month = c("01", "02", "03", "04", "05", "06",
            "07", "08", "09", "10", "11", "12")
  Month.Dummy.matrix =  matrix(,nrow = Length, ncol=length(Month))
  colnames(Month.Dummy.matrix) = Month

  for (i in 1:length(Month)) {
    Month.Dummy.matrix[,i] <- format(index(FullDat.xts), "%m")== Month[i]
  }


  #############################################################################  
  ####        1.3 Dummy variables for Days     ################################

  n=6
  days.of.the.week           = c("Sunday",
                                 "Monday",
                                 "Tuesday",
                                 "Wednesday",
                                 "Thursday",
                                 "Friday",
                                 "Saturday")
  
  Day.Dummy.matrix           = matrix(,nrow = Length, ncol=n+1)
  colnames(Day.Dummy.matrix) = days.of.the.week
  Weekdays                   = as.POSIXlt(index(FullDat.xts))$wday  
  # vector of the week day of each day 
  # ( in numbers : 0=Sunday, 1=Monday...6=Saturday)

  for (i in 0:n) {
      Day.Dummy.matrix[,i+1] = Weekdays== i
  }


  ############################################################################ 
  ####    Step 2: Cbind the dummy matrices    ################################

  Time.Dummy.matrix         = cbind(Year.Dummy.matrix[,-del.Y], 
                                    Month.Dummy.matrix[,-del.M], 
                                    Day.Dummy.matrix[,-del.D])
  Time.Dummy.matrix.xts     = xts(x=Time.Dummy.matrix, order.by=index(FullDat.xts))
  Time.Dummy.matrix.xts.num = Time.Dummy.matrix.xts+0  # Converts the data to nummeric

  ############################################################################ 
  ####    Step 3 Cbind the dummy matrices    #################################

  FullDat.xts = cbind(FullDat.xts, Time.Dummy.matrix.xts.num)

  }
}






