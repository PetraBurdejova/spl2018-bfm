
#This is fictionnal data. Real data will be integrated later.

library("xts")

dateVec <- seq(from=as.Date("2015-01-01"), to=as.Date("2018-01-01"), by="days")
Preis <-c(1:1097)
Demand <- c(10001:11097)
Solar <- c(20001:21097)
Wind <- c(30001:31097)

FullDat <- cbind( Preis,Demand, Solar, Wind)

FullDat.xts <-xts(x=FullDat,order.by=dateVec)

FullDat.msts <-msts(FullDat, seasonal.periods=c(7,365.25))

as.ts(FullDat.xts)


#### Generating Time Dummies:

###IMPORTANT: One column needs to be deleted for Year, one for Month and one for days ( Dummy trap!!!)

#Step1: Create the dummy variables for years, months and days 

## 1.a Dummy variables for Years

Year.min  <- format(min(index(FullDat.xts)), "%Y")
Year.max  <- format(max(index(FullDat.xts)), "%Y")

Year.Vector <- seq(from=Year.min, to=Year.max, by=1)


Year.min.number <- as.numeric(Year.min)
Year.max.number <- as.numeric(Year.max)

Year.Dummy.matrix <- matrix(,nrow = length(FullDat.xts[,1]), ncol=Year.max.number-Year.min.number+1)

colnames(Year.Dummy.matrix)=Year.Vector



for (i in 1:length(Year.Vector)) {
  Year.Dummy.matrix[,i] <- format(index(FullDat.xts), "%Y")== Year.Vector[i]
}

#summary(Year.Dummy.matrix)

## 1.b Dummy variables for Months

Month <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Month.Dummy.matrix <- matrix(,nrow = length(FullDat.xts[,1]), ncol=length(Month))
colnames(Month.Dummy.matrix) <- Month

for (i in 1:length(Month)) {
  Month.Dummy.matrix[,i] <- format(index(FullDat.xts), "%m")== Month[i]
}

Month.Dummy.matrix 

## 1.c Dummy variables for DAYS

n=7
days.of.the.week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

Day.Dummy.matrix <- matrix(,nrow = length(FullDat.xts[,1]), ncol=n)
colnames(Day.Dummy.matrix) <-days.of.the.week

Weekdays <- as.POSIXlt(index(FullDat.xts))$wday  ##vector of the week day of each day ( in numbers : 1=Monday..., 7=Sunday)


for (i in 1:n) {
  Day.Dummy.matrix[,i] <- Weekdays== i
}
Day.Dummy.matrix

##2 Cbind the matrices

Time.Dummy.matrix <- cbind(Year.Dummy.matrix, Month.Dummy.matrix, Day.Dummy.matrix)

Time.Dummy.matrix.xts <- xts(x=Time.Dummy.matrix, order.by=dateVec)


Time.Dummy.matrix.xts+0 #Converts the data to nummeric

