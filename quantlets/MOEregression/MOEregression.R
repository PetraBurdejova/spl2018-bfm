
###############################################################################
####  ####################################
###############################################################################


library("astsa") # acf2
library("xts") # xts

library("prais") # prais winsten regression
library("lmtest") #dwtest and #
library("forecast") #tslm

library("aTSA") # for pptest
library("tseries") #adf.test
library("xtable")
# library("dyn") ##?
# library("nlme") #??


###############################################################################
####   Loading and preparing the data #########################################
###############################################################################
#setwd("C:/Users/PC-FELIX/Documents/GitHub/spl2018-bfm/quantlets")

source("MOEmergedata/MOEmergedata.R")
# Loads the final df with price, demand, solar generation and wind generation

xts.mydata = xts(df[, -1], order.by=df$TIME)
# create an xts file with df data

xts.mydata[,"WIND"] = na.approx(xts.mydata[,"WIND"], na.rm=TRUE, maxgap=3)
# Interpolate missing values for Wind, on a daily basis

source("MOEtimedummies/MOEtimedummies.R")
# Loads the function YMDDummy which adds dummy Variables for the year,
# the month and the day

xts.mydata = YMDDummy(xts.mydata)
# adds the dummy matrix for Year, Month and day of the week to the data
ts.mydata = as.ts(xts.mydata)
ts.mydata[,2:4] = as.ts(xts.mydata)[,2:4]/24000
## changes the scale of the data to facilitate the interpretation
## generation data is now expressed in mean hourly generation in GWh(thus GW)
## Price is still in EUR/MWh 

# convert to ts format for regression


###############################################################################
####   Part 1. Tests for Stationarity: Augmented Dickey FUller test   #########
###    and Philipps-perron test
###############################################################################

Results.ADF = rep(NA, 4)

for (Column in 1:4) {
  Results.ADF[Column]=tseries::adf.test(ts.mydata[, Column], alternative = "stationary")$p.value
}

Results.PP = rep(NA, 4)

for (Column in 1:4) {
  Results.PP[Column]= tseries::pp.test(ts.mydata[, Column])$p.value
}

Table1 = data.frame(Results.ADF, Results.PP)
colnames(Table1) = c("A. Dickey Fuller", "Philipps-perron")
rownames(Table1) = c("El. Price","El. Demand", "Solar Gen.", "Wind Gen.")

xtable(Table1)



## Trending 
# detr.PUN <- tslm(PUN ~ trend, ts.mydata)
# detr.DEM <- tslm(DEM ~ trend, ts.mydata)
# detr.SOLAR <- tslm(SOLAR ~ trend, ts.mydata)
# detr.WIND <- tslm(WIND ~ trend, ts.mydata)




###############################################################################
####   Part 2. Perform a basic OLS on the data, in order to look at ###########
####           the autocorrelation structure                        ###########
###############################################################################


OLS = tslm(PUN ~ trend + ts.mydata[,-1], ts.mydata)
##summary(OLS)$r.squared 
##summary(OLS)$adj.r.squared 

DWTEST.OLS = dwtest(OLS)
# perform the durbin watson test in order to check for autocorrelation of 
# of the disturbances.

BPTEST.OLS = bptest(OLS)
# Performs the breusch pagan test for heteroscedasticity
# Under H0 the test statistic of the Breusch-Pagan test follows a chi-squared
# distribution with parameter (the number of regressors without the constant in 
# the model) degrees of freedom

Table2 = data.frame(DWTEST.OLS$p.value,BPTEST.OLS$p.value)
colnames(Table2) = c("Durbin-Watson", "Breusch-Pagan")
row.names(Table2) = "p-value"
xtable(Table2)


jpeg('MOEregression/PACF_OLS.jpg')
acf2(OLS$residuals)
dev.off()
# generates a jpeg file of a plot of the PACF and the ACF

##AR and MA signatures: If the PACF displays a sharp cutoff while the ACF decays more slowly 
##(i.e., has significant spikes at higher lags), we say that the stationarized series displays an "AR signature," 
##meaning that the autocorrelation pattern can be explained more easily by adding AR terms than by adding MA terms






###############################################################################
####   Part 3. Perform the prais winsten regression for the modelling #########
####           of an AR(1) process                                    #########
###############################################################################

PWReg = prais.winsten(PUN ~ .,ts.mydata,iter = 50,rho = 0, tol = 1e-08)
# performs the Prais winsten generalised least squares regression 
# ( modelling the distrubances with an AR(1) process )

DWTEST.PW = dwtest(PWReg[[1]])
# Durbin Watson Test for autocorrelation after correcting for serial correlation

jpeg('MOEregression/PACF_PraisWinsten.jpg')
acf2(PWReg[[1]]$residuals)
dev.off()
# generates a jpeg file of the plots of the ACF and the PACF



coef = PWReg[[1]][, drop=F]$coefficients
coef = rbind(coef[-1,], coef[1,])

rho = PWReg[[2]]
rho = as.numeric(c(PWReg[[2]][1,1], NA ,PWReg[[2]][1,2], NA))
# calculate p-statistic?

Table3 = rbind(coef, rho)
Table3 = as.data.frame(Table3)
rownames(Table3) = c("Demand", "Solar Gen.", "Wind Gen.",
                     "Year 2016", "Year 2017", "February", "March", "April", "May", "June", "July", 
                     "August", "September", "October", "November", "December", "Monday", "Tuesday",
                     "Wednesday", "Thursday","Friday", "Saturday", "Intercept",
                     "\rho  (AR1)")

xtable(Table3, caption = "Prais-Winsten regression results", align =c("l", "r","r", "r","r"), digits=2)




Table4 = data.frame(PWReg[[1]]$r.squared, 
           PWReg[[1]]$adj.r.squared, 
           DWTEST.PW$p.value)
colnames(Table4) = c("R^2","Adj. R^2","Durbin-Watson(p-val)")
rownames(Table4) = ""
test = xtable(Table4, caption = "Prais-Winsten regression results", align=c("l","c", "c", "c"), digits=2)
print.xtable(test, type="latex", file="test2")






