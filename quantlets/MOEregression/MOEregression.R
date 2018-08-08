# The clean daily energy data ( without NA's) is loaded in order to perform
# an OLS regression and the prais winsten regression.
# The following tests are performedalong the way: 
# -Augmented Dickey fuller test and philipps perron test for stationarity of 
# the energy data ( with trend and constant)
# -Durbin watson test for autocorrelation in the OLS
# -Breusch-Pagan for heteroscedastiocity in the OLS
# -Durbin watson test for autocorrelation in the Prais winsten regression
#  -The ACF and PACF are also plotted for both OLS and Prais winsten
#
# Input:  MOEmergedata.R 
#
# Ouput: - ACF and PACF plot of the OLS and Prais Winsten (2pdf files)
#        - A latex table for the ADF and PP tests for stationnarity
#        - A latex table for the Durbin Watson and 
#          Breusch Pagan tests on the OLS
#        - A latex table for the coefficients of the Prais Winsten regression
#        - A latex table for R^2, Adj. R^2 and Durbin Watson test results of 
#          the prais winsten regression
###############################################################################


# Clear all variables.
rm(list = ls(all = TRUE))
graphics.off()

# Install and load libraries.
libraries = c("xts", "prais","lmtest", "forecast", "astsa", "tseries", "xtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#library("xts") # xts
#library("prais") # prais winsten regression
#library("lmtest") #dwtest and #
#library("forecast") #tslm
#library("astsa") # acf2
#library("tseries") #adf.test
#library("xtable")



###############################################################################
####   0. Loading and preparing the data ######################################
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
# transforms the data into the ts format in order to perform the regression


ts.mydata[,2:4] = as.ts(xts.mydata)[,2:4]/24000
# changes the scale of the data to facilitate the interpretation
# generation data is now expressed in mean hourly generation in GWh(thus GW)
# Price is still in EUR/MWh 
# convert to ts format for regression


###############################################################################
####   Part 1. Tests for Stationarity: Augmented Dickey FUller test   #########
###    and Philipps-perron test
###############################################################################

Results.ADF = rep(NA, 4)

for (Column in 1:4) {
  Results.ADF[Column]=tseries::adf.test(ts.mydata[, Column],
                                        alternative = "stationary")$p.value
}
# Performs the ADF test for the variables Price, Demand, Solar and Wind
# the p-values are stored in Results.ADF
# H0: non-stationary ( with time trend and constant)

Results.PP = rep(NA, 4)

for (Column in 1:4) {
  Results.PP[Column]= tseries::pp.test(ts.mydata[, Column])$p.value
}
# Performs the PP test for the variables Price, Demand, Solar and Wind
# the p-values are stored in Results.PP
# H0: non-stationary ( with time trend and constant)


Table1 = data.frame(Results.ADF, Results.PP)
colnames(Table1) = c("A. Dickey Fuller", "Philipps-perron")
rownames(Table1) = c("El. Price","El. Demand", "Solar Gen.", "Wind Gen.")
TABLE1 = xtable(Table1)
print.xtable(TABLE1, type="latex", file="MOEregression/ADFandPPTEST")
# Arranges the p-values of the ADF test and PP Test into a Latex table


###############################################################################
####   Part 2. Perform a basic OLS on the data, in order to look at ###########
####           the autocorrelation structure                        ###########
###############################################################################


OLS = tslm(PUN ~  ts.mydata[,-1], ts.mydata)
# Performs the basic OLS 

DWTEST.OLS = dwtest(OLS)
# perform the durbin watson test in order to check for autocorrelation of 
# of the disturbances.

BPTEST.OLS = bptest(OLS)
# Performs the breusch pagan test for heteroscedasticity
# Under H0 the test statistic of the Breusch-Pagan test follows a chi-squared
# distribution degrees of freedom

Table2 = data.frame(DWTEST.OLS$p.value,BPTEST.OLS$p.value)
colnames(Table2) = c("Durbin-Watson", "Breusch-Pagan")
row.names(Table2) = "p-value"
TABLE2 = xtable(Table2)
print.xtable(TABLE2, type="latex", file="MOEregression/OLS_DWandBPTEST")
# Stores the p values of the DWtest and the BP test into a latex table


jpeg('MOEregression/PACF_OLS.jpg')
acf2(OLS$residuals, main="OLS residuals")
dev.off()
# generates a jpeg file of a plot of the PACF and the ACF of the OLS


###############################################################################
####   Part 3. Perform the prais winsten regression for the modelling #########
####           of an AR(1) process                                    #########
###############################################################################

PWReg = prais.winsten(PUN ~ .,ts.mydata,iter = 50,rho = 0, tol = 1e-08)
# performs the Prais winsten generalised least squares regression 
# ( modelling the distrubances with an AR(1) process )

DWTEST.PW = dwtest(PWReg[[1]])
# Durbin Watson Test for autocorrelation after correcting for serial correlation


###############################################################################
# 3.2 Generate latex and jpeg output
###############################################################################

# 3.2.1 ACF and PACF plot

jpeg('MOEregression/PACF_PraisWinsten.jpg')
acf2(PWReg[[1]]$residuals, main="Prais-Winsten residuals")
dev.off()
# generates a jpeg file of the plots of the ACF and the PACF

#3.2.2 Latex for Prais winsten regression

coef = PWReg[[1]][, drop=F]$coefficients
coef = rbind(coef[-1,], coef[1,])

rho = PWReg[[2]]
rho = as.numeric(c(PWReg[[2]][1,1], NA ,PWReg[[2]][1,2], NA))

Table3 = rbind(coef, rho)
Table3 = as.data.frame(Table3)
rownames(Table3) = c("Demand", "Solar Gen.", "Wind Gen.",
                     "Year 2016", "Year 2017", "February",
                     "March", "April", "May", "June", "July", 
                     "August", "September", "October", "November",
                     "December", "Monday", "Tuesday", "Wednesday",
                     "Thursday","Friday", "Saturday", "Intercept",
                     "\rho  (AR1)")

TABLE3 = xtable(Table3,
                caption = "Prais-Winsten regression results",
                align =c("l", "r","r", "r","r"), 
                digits=2)

print.xtable(TABLE3, type ="latex",
             file = "MOEregression/PraisWinstenRegCoefficients")
#generates a Latex table yith the coefficients of the Prais-Winsten regression


Table4 = data.frame(PWReg[[1]]$r.squared, 
           PWReg[[1]]$adj.r.squared, 
           DWTEST.PW$p.value)
colnames(Table4) = c("$R^2$","$Adj. R^2$","Durbin-Watson (p value )")
rownames(Table4) = ""


TABLE4 = xtable(Table4, 
                caption = "Prais-Winsten regression results",
                align=c("l","c", "c", "c"),
                digits=2)

print.xtable(TABLE4, 
             type="latex",
             file="MOEregression/PraisWinstenGoFDW", 
             sanitize.text.function=function(x){x})
# generates a Latex table with the R^2, the Adj R^2 and the Result of the durbin
# watson test fopr the prais winsten regression


rm(list = ls(all = TRUE))
graphics.off()






