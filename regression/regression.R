library("astsa")
library("xts")
library("dyn") #this package includes extensions to time series applications for basic commands
library("tseries")
library("forecast")

###This is fictionnal data 
### It will be replaced by the residuals of the first regression

x=c(1:100)

errorterms <- arima.sim(model = list(order = c(0, 0, 1), ma = .9 ), n = 100)

y=c(1:100)+errorterms

dateVec <- seq(from=as.Date("2015-01-01"), by="days", length.out=100)

mydata <- cbind(y,x)

mydata.xts <- xts(mydata, order.by=dateVec)

mydata.ts <- ts(mydata.xts)

reg <- dyn$lm(y~x)  ###performed regression


#################################################################################


##Tests for Stationarity: Augmented Dickey FUller test:
adf.test(mydata.ts[,"x"], alternative = "stationary", k = trunc((length(x)-1)^(1/3)))
adf.test(mydata.ts[,"x"], alternative = "explosive", k = trunc((length(x)-1)^(1/3)))

adf.test(mydata.ts[,"y"], alternative = "stationary", k = trunc((length(x)-1)^(1/3)))
adf.test(mydata.ts[,"y"], alternative = "explosive", k = trunc((length(x)-1)^(1/3)))


## Detrending

detr.x <- tslm(mydata.xts[,"x"]~ trend)
detr.y <- tslm(mydata.xts[,"y"]~ trend)

res.detr.x <- residuals(detr.x )
res.detr.y <- residuals(detr.y )

mydata.ts.detr <- ts(cbind(res.detr.x,res.detr.y) )

###Again, test for stationarity

adf.test(mydata.ts.detr[,"res.detr.x"], alternative = "stationary", k = trunc((length(x)-1)^(1/3)))
adf.test(mydata.ts.detr[,"res.detr.x"], alternative = "explosive", k = trunc((length(x)-1)^(1/3)))

adf.test(mydata.ts.detr[,"res.detr.y"], alternative = "stationary", k = trunc((length(x)-1)^(1/3)))
adf.test(mydata.ts.detr[,"res.detr.y"], alternative = "explosive", k = trunc((length(x)-1)^(1/3)))

##IF Okay, take the detrended variables for the regression

### Add the dummy variables  here
###Use the function that regresses the variables with dummies







#### Test for autocorrelation of disturbances:

dwtest( reg , order.by = NULL, alternative = c("greater", "two.sided", "less"),
       iterations = 15, exact = NULL, tol = 1e-10, data = list())



###ACF and PACF functions

acf2(residuals(reg))  ### Check Auto correlation function and Partial auto-correlation function







