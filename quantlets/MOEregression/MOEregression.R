
###############################################################################     
####    MOEregression.R    ####################################################   
###############################################################################     
# 
# Performs an OLS and the Prais-Winsten regression as well as the following
# tests:
#
#   - Augmented Dickey-Fuller test and Philipps-Perron test for stationarity of 
#     the data (with trend and constant)
#   - Durbin-Watson test for autocorrelation in the OLS
#   - Breusch-Pagan for heteroscedasticity in the OLS
#   - Durbin-Watson test for autocorrelation in the Prais-Winsten regression
# 
# The ACF and PACF are plotted for OLS and Prais-Winsten as well.
#
# Input: 'MOEdata_interp.Rdata' from the 'MOEinterpolation' Quantlet.
#
# Ouput: 'PACF_OLS.jpg'          - ACF and PACF plot for OLS
#        'PACF_PraisWinsten.jpg' - ACF and PACF plot for Prais-Winsten
#        'ADFandPPTEST'          - LaTeX table for ADF and PP tests
#        'OLS_DWandBPTEST'       - LaTeX table for Durbini-Watson
#                                  and Breusch-Pagan tests on the OLS
#        'PraisWinstenRegCoefficients'  - LaTeX table of PW reg. coefficents
#        'PraisWinstenGoFDW'     - LaTeX table of R^2, Adj. R^2 and DW test 
#                                  results of the Prais-Winsten regression
#
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

load("MOEmergedata/MOEdata_merge.Rdata")



###############################################################################
####    2.  PREPARE DATA    ###################################################
###############################################################################


###############################################################################
####    INTERPOLATE WIND DATA ON DAILY BASIS    ##############################

xts.mydata          = xts(df[, -1], order.by=df$TIME)
xts.mydata[,"WIND"] = na.approx(xts.mydata[,"WIND"], na.rm=TRUE, maxgap=3)


###############################################################################
####    ADD TIME DUMMIES TO XTS DATAFRAME    ##################################

source("MOEtimedummies/MOEtimedummies.R")

xts.mydata = YMDDummy(xts.mydata)


###############################################################################
####    TRANSFORM TO TS DATAFRAME AND ADJUST UNITS    #########################

ts.mydata = as.ts(xts.mydata)

# changes the scale of the data to facilitate the interpretation
# generation data is now expressed in mean hourly generation in GWh(thus GW)
# Price is still in EUR/MWh 
ts.mydata[,2:4] = as.ts(xts.mydata)[,2:4]/24000



###############################################################################
####    3.  PERFORM TESTS AND REGRESSIONS    ##################################
###############################################################################


###############################################################################
####   Part 1. Tests for Stationarity    ######################################


###############################################################################
####        Augmented Dickey Fuller Test  

Results.ADF = rep(NA, 4)

for (Column in 1:4) {
    # Performs the ADF test for the variables Price, Demand, Solar and Wind
    # the p-values are stored in Results.ADF
    # H0: non-stationary ( with time trend and constant)
    Results.ADF[Column]=tseries::adf.test(ts.mydata[, Column],
                                          alternative = "stationary")$p.value
}


###############################################################################
####    Philipps-Perron Test

Results.PP = rep(NA, 4)

for (Column in 1:4) {
    # Performs the PP test for the variables Price, Demand, Solar and Wind
    # the p-values are stored in Results.PP
    # H0: non-stationary ( with time trend and constant)
    Results.PP[Column]= tseries::pp.test(ts.mydata[, Column])$p.value
}


# Arrange results into LaTeX table
Table1 = data.frame(Results.ADF, Results.PP)
colnames(Table1) = c("A. Dickey Fuller", "Philipps-perron")
rownames(Table1) = c("El. Price","El. Demand", "Solar Gen.", "Wind Gen.")
TABLE1 = xtable(Table1)
print.xtable(TABLE1, type="latex", file="MOEregression/ADFandPPTEST")


###############################################################################
####    Part 2. Basic OLS, Autocorrelation Structure    #######################


###############################################################################
####        Basic OLS

OLS = tslm(PUN ~  ts.mydata[,-1], ts.mydata)


###############################################################################
####        Durbin-Watson Test
####        (check for autocorrelation of the disturbances)

DWTEST.OLS = dwtest(OLS)


###############################################################################
####        Breusch-Pagan Ttest for Heteroscedasticity
####        (Under H0 the test statistic of the Breusch-Pagan test follows a 
####        chi-squared distribution degrees of freedom)

BPTEST.OLS = bptest(OLS)


# Arrange results into LaTeX table
Table2 = data.frame(DWTEST.OLS$p.value,BPTEST.OLS$p.value)
colnames(Table2) = c("Durbin-Watson", "Breusch-Pagan")
row.names(Table2) = "p-value"
TABLE2 = xtable(Table2)
print.xtable(TABLE2, type="latex", file="MOEregression/OLS_DWandBPTEST")


# Generate jpeg file with PACF and ACF of OLS
jpeg('MOEregression/PACF_OLS.jpg')
acf2(OLS$residuals, main="OLS residuals")
dev.off()


###############################################################################
####    Part 3. Prais-Winsten Regression for modelling of AR(1) process    ####


###############################################################################
####        Prais -Winsten Generalised Least Squares Regression 
####        (modelling the distrubances with an AR(1) process)

PWReg = prais.winsten(PUN ~ .,ts.mydata,iter = 50,rho = 0, tol = 1e-08)

# Generate a jpeg for ACF and PACF of PW regression
jpeg('MOEregression/PACF_PraisWinsten.jpg')
acf2(PWReg[[1]]$residuals, main="Prais-Winsten residuals")
dev.off()

# Arrange results into LaTeX table
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


###############################################################################
####      Durbin Watson Test 
####      (for autocorrelation after correcting for serial correlation)

DWTEST.PW = dwtest(PWReg[[1]])

# generates a Latex table with the R^2, the Adj R^2 and the Result of the durbin
# watson test fopr the prais winsten regression
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



###############################################################################
####    5. CLEAN UP ENVIRONMENT    ############################################
###############################################################################


rm(list = ls(all = TRUE))
graphics.off()






