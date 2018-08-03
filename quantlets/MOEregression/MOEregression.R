
###############################################################################
####  ####################################
###############################################################################


library("astsa") # acf2
library("xts") # xts
library("tseries") #adf.test
library("prais") # prais winsten regression
library("lmtest") #dwtest


# library("dyn") ##?
# library("nlme") #??
# library("forecast") ##?

###############################################################################
####   Loading and preparing the data #########################################
###############################################################################


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

ts.mydata <- as.ts(xts.mydata)
# convert to ts format for regression


###############################################################################
####   Part 1. Tests for Stationarity: Augmented Dickey FUller test:  #########
###############################################################################

for (Column in 1:4) {
 print( adf.test(ts.mydata[, Column], alternative = "stationary"))
}


## Trending 
# detr.PUN <- tslm(PUN ~ trend, ts.mydata)
# detr.DEM <- tslm(DEM ~ trend, ts.mydata)
# detr.SOLAR <- tslm(SOLAR ~ trend, ts.mydata)
# detr.WIND <- tslm(WIND ~ trend, ts.mydata)


###############################################################################
####   Part 2. Perform a basic OLS on the data, in order to look at ###########
####           the autocorrelation structure                        ###########
###############################################################################


OLS <- tslm(PUN ~ trend + ts.mydata[,-1], ts.mydata)
##summary(OLS)$r.squared 
##summary(OLS)$adj.r.squared 

dwtest(OLS)
# perform the durbin watson test in order to check for autocorrelation of 
# of the disturbances.

jpeg('MOEregression/PACF_OLS.jpg')
acf2(OLS$residuals)
dev.off()
# generates a jpeg file of a plot of the PACF and the ACF

##AR and MA signatures: If the PACF displays a sharp cutoff while the ACF decays more slowly 
##(i.e., has significant spikes at higher lags), we say that the stationarized series displays an "AR signature," 
##meaning that the autocorrelation pattern can be explained more easily by adding AR terms than by adding MA terms



###############################################################################
####   Part 2. Perform a basic OLS on the data, in order to look at ###########
####           the autocorrelation structure                        ###########
###############################################################################

Prais.winsten.regression <- prais.winsten(PUN ~ .,ts.mydata,iter = 50,rho = 0, tol = 1e-08)
# performs the Prais winsten generalised least squares regression 
# ( modelling the distrubances with an AR(1) process )


Prais.winsten.regression2 =function (formula, data, iter = 50, rho = 0, tol = 1e-08) 
{
  mod <- model.frame(formula, data = data)
  lm <- lm(mod)
  n <- length(mod[, 1])
  list.rho <- c(0)
  imax <- ncol(mod) - 1
  fo <- as.formula(paste("y ~ -1 + x0 +", paste(paste0("x", 
                                                       1:imax), collapse = "+")))
  if (rho != 0) {
    y <- c((1 - rho^2)^(1/2) * mod[1, 1], mod[2:n, 1] - rho * 
             mod[1:(n - 1), 1])
    x0 <- c((1 - rho^2)^(1/2), rep(1 - rho, n - 1))
    for (i in 1:imax) {
      x <- c((1 - rho^2)^(1/2) * mod[1, (i + 1)], mod[2:n, 
                                                      (i + 1)] - rho * mod[1:(n - 1), (i + 1)])
      assign(paste("x", i, sep = ""), x)
    }
    lm <- lm(fo)
    j <- 1
    rho.tstat <- "none"
  }
  else {
    res <- lm$res
    res_1 <- c(NA, res[-n])
    for (i in 1:iter) {
      rho.lm <- lm(res ~ res_1 - 1)
      rho <- rho.lm$coeff[1]
      if (abs(rho - tail(list.rho, n = 1)) < tol) {
        j <- i
        break
      }
      else {
        list.rho <- append(list.rho, rho)
        y <- c((1 - rho^2)^(1/2) * mod[1, 1], mod[2:n, 
                                                  1] - rho * mod[1:(n - 1), 1])
        x0 <- c((1 - rho^2)^(1/2), rep(1 - rho, n - 1))
        for (k in 1:imax) {
          x <- c((1 - rho^2)^(1/2) * mod[1, (k + 1)], 
                 mod[2:n, (k + 1)] - rho * mod[1:(n - 1), 
                                               (k + 1)])
          assign(paste("x", k, sep = ""), x)
        }
        lm <- lm(fo)
        fit <- as.vector(rep(lm$coef[1], n)) + as.vector(as.matrix(mod[, 
                                                                       2:(imax + 1)]) %*% lm$coef[2:(imax + 1)])
        res <- mod[, 1] - fit
        res_1 <- c(NA, res[-n])
        j <- i
        rho.tstat <- summary(rho.lm)$coef[1, 3]
      }
    }
  }
  if (iter == 50) 
    i <- j - 1
  else i <- j
  attr(lm$coefficients, "names") <- c("Intercept", names(mod)[2:ncol(mod)])
  s <- summary(lm)
  r <- data.frame(Rho = rho, `Rho.t-statistic` = rho.tstat, 
                  Iterations = i, row.names = c(""))
  results <- list(s, r)
  return(lm)
}
# Prais winsten function with modified output (copy paste of the original
# function with "return" changed)



PWReg2 = Prais.winsten.regression2(PUN ~ ., ts.mydata, rho=0)

dwtest(PWReg2)

# Durbin Watson Test for autocorrelation

jpeg('MOEregression/PACF_PraisWinsten.jpg')
acf2(PWReg2$residuals)
dev.off()
# generates a jpeg file of the plots of the ACF and the PACF



  









