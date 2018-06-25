library("astsa")
library("xts")
library("dyn") #this package includes extensions to time series applications for basic commands
library("tseries")
library("forecast")
library("prais")
library("lmtest")

library("nlme")



#################################################################################


###Temporary solution: Loading the saved DF

source("MOEmergedata/MOEmergedata.R")

mydata <- df

mydata <- xts(mydata[, -1], order.by=df$TIME)

###Add the dummy variable matrix:

source("regression/generate_dummies.R")
mydata.xts<- YMDDummy(mydata)

##convert to ts format for regression

mydata.ts <- as.ts(mydata.xts)


###


##Tests for Stationarity: Augmented Dickey FUller test:
adf.test(mydata.ts[, 1], alternative = "stationary", k = trunc((length(mydata.ts[, 1])-1)^(1/3)))

adf.test(mydata.ts[, 2], alternative = "stationary", k = trunc((length(mydata.ts[, 2])-1)^(1/3)))
adf.test(mydata.ts[,3], alternative = "stationary", k = trunc((length(mydata.ts[, 3])-1)^(1/3)))
adf.test(mydata.ts[,4], alternative = "stationary", k = trunc((length(mydata.ts[, 4])-1)^(1/3)))


## Trending 


detr.PUN <- tslm(PUN ~ trend, mydata.ts)
detr.DEM <- tslm(mydata.ts[,2] ~ trend, mydata.ts)
detr.SOLAR <- tslm(mydata.ts[,3]~ trend, mydata.ts)
detr.WIND <- tslm(mydata.ts[,4]~ trend, mydata.ts)


###OLS 1 


OLS1 <- tslm(PUN ~ mydata.ts[,-1], mydata.ts)

summary(OLS1)$r.squared 
summary(OLS1)$adj.r.squared 

#### Test for autocorrelation of disturbances:

dwtest(OLS1)


##ACF and PACF

 

jpeg('regression/PACF_OLS.jpg')
acf2(OLS1$residuals)
dev.off()

##AR and MA signatures: If the PACF displays a sharp cutoff while the ACF decays more slowly 
##(i.e., has significant spikes at higher lags), we say that the stationarized series displays an "AR signature," 
##meaning that the autocorrelation pattern can be explained more easily by adding AR terms than by adding MA terms



##Prais winsten generalised least squares regression

Prais.winsten.regression <- prais.winsten(PUN ~ ., mydata.ts, iter = 50, rho = 0, tol = 1e-08)

###Prais winsten function with modified output (copy paste of the original function with "return" changed)

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

###Tests for autocorrelation again

test <- Prais.winsten.regression2(PUN ~ ., mydata.ts, iter = 50, rho = 0, tol = 1e-08)

dwtest(test)
dwtest(Prais.winsten.regression)



jpeg('regression/PACF_PraisWinsten.jpg')
acf2(test$residuals)
dev.off()


####TEST WITH  gls
 


##mod.gls2 <- gls(PUN ~ mydata.ts[,-1] , data=mydata.ts, correlation=corARMA(p=2), method="ML")
##summary(mod.gls2)

##acf2(residuals(mod.gls2))
  



  









