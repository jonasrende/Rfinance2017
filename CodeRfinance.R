
#### Packages ####

library(partialCI)
library(egcm)
library(Quandl)
library(mFilter)



#### Examples ####

### A. Finance ###


# Introduction: Creating a residual plot of an OLS regression
# of Coca-Cola and PepsiCo (1.01.2006 - 1.12.2016, daily)
# Data are downloaded from Yahoo Finance.
 


iStockA<-c("KO")
iStockB<-c("PEP")
iStartDate<-20060101
iEndDate<-20161201

StockA<-getYahooData(iStockA[1], iStartDate, iEndDate)$Close
StockB<-getYahooData(iStockB[1], iStartDate, iEndDate)$Close

#### Classical CI  KO/PEP ####
egcm_default_model<-egcm(StockB,StockA,include.const = FALSE)

# Plot OLS - residuals
EGCM_residuals<-matrix(NA,ncol=1,nrow=length(egcm_default_model$residuals))
EGCM_residuals[,1]<-egcm_default_model$residuals
EGCM_residuals_zoo<-as.zoo(as.matrix(EGCM_residuals[,1]), index(StockA))
plot(EGCM_residuals_zoo,type = "l",ylab = "", xlab = "")
abline(0,0, col="red")


# 1.0) Loading RDS-A and RDS-B data from Yahoo Data covering the time period from 01.01.2006 to 01.12.2016

iStockA<-c("RDS-A") # RDS-A 
iStockB<-c("RDS-B") # RDS-B

iStartDate<-20060101 # Set start date to 01.01.2006
iEndDate<-20161201   # Set end date to 01.12.2016

# Fedge closing price time series ($Close) for RDS-A and RDS-B from Yahoo data

StockA<-getYahooData(iStockA[1], iStartDate, iEndDate)$Close
StockB<-getYahooData(iStockB[1], iStartDate, iEndDate)$Close

# 2.) Perform a classic cointegration analysis, using the default specification (no time trend // with constant) implemented in the R
# package egcm


# 2.1) Using egcm in default specification to investigate if the RDS-A and RDS-B are cointegrated in a classic sense

egcm_default_model<-egcm(StockB,StockA,include.const = FALSE)

# 2.2) Converting the residual series into an zoo object as.zoo(coredata(), index())

# Initializing a matrix of the same length as the residual series with NAs

EGCM_residuals<-matrix(NA,ncol=1,nrow=length(egcm_default_model$residuals)) 


EGCM_residuals[,1]<-egcm_default_model$residuals 

# Generate a zoo object using the residual series as coredata with a daily time index

EGCM_residuals_zoo<-as.zoo(as.matrix(EGCM_residuals[,1]), index(StockA))  

# 2.3) Ploting the residual series from the first stage of the Engle-Granger twostep procedure

plot(EGCM_residuals_zoo,type = "l",ylab = "", xlab = "")

# Adding a horizontal zero line in red

abline(0,0, col="red") 

# 3.1) Partial cointegration (PCI) analysis

# Fitting a PCI model to RDS-A and RDS-B
# The residuals are assumed to follow a partially autoregressive model (PAR) (par_model =c("par"))
# Parameters are estimated using the joint-penalty procedure (pci_opt_method = c("jp"))

RDS_A_B_fit<-fit.pci(StockA,StockB,pci_opt_method = c("jp"),par_model =c("par"),lambda = 0,robust = FALSE,nu = 5,include_alpha=FALSE)

# 3.2) Test for PCI
#The null hypothesis is a union of both, the random walk and the AR(1)-hypothesis (null_hyp =c("rw","ar1"))

RDS_A_B_test<-test.pci(StockA,StockB, alpha = 0.05,null_hyp =c("rw","ar1") ,robust = FALSE,pci_opt_method =c("jp"))

# 3.3) Estimating the hidden states

RDS_A_B_statehist<-statehistory.pci(RDS_A_B_fit)

# 3.4) Extracting the mean-reverting component which is located in the fourth column ([,4])

RDS_A_B_MC<-statehistory.pci(RDS_A_B_fit)[,4]

# Generate a zoo object using the mean-reverting component as coredata with a daily time index

RDS_A_B_MC_zoo<-as.zoo(as.matrix(RDS_A_B_MC), index(StockA)) 

# 3.5) Plot the mean-revering component and add horizontal blue lines which are
# equal to two times the historical in-sample standard deviation

# Calculating the historical standard deviation sd()

sdRDSMC<-sd(RDS_A_B_MC_zoo)

# Plot the mean-reverting component

plot(RDS_A_B_MC_zoo,type = "l",ylab = "", xlab = "")

# Adding the standard deviations bands

abline(2*sdRDSMC,0, col="blue")

abline(-2*sdRDSMC,0, col="blue")

abline(0,0, col="red")


# 3.6) Find the set of sector ETFs forming the best hedging portfolio for the SPY index

sectorETFS <- c("XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY")

# Fedge prices for SPY and the ETFs from Yahoo data

prices <- multigetYahooPrices(c("SPY", sectorETFS), start=20060101)

# Calculating the best hedging portfolio for the target time series SPY (prices[,"SPY"]) 

hedge.pci(prices[,"SPY"], prices)

