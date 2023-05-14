#  Regression Analysis:  Case Study 2}
#   Linear Regression Models for Exchange Rate Regimes}


# 1.0 Exchange Rate Data ----
#The Federal Reserve Economic Database (FRED) provides historical daily exchange rates of all major currencies in the world.

#An R script (``fm\_casestudy\_fx\_1.r") collects these data and stores them in the R workspace ``fm\_casestudy\_fx\_1.RData''.

source("fm_casestudy_fx_1.r")
# This command need be run only once and can be commented out afterward.
# If running
#The following commands re-load the data and provide details explaining the data.


# 0. Install/load libraries ----
source(file="fm_casestudy_0_InstallOrLoadLibraries.r")

# 0.2 Load R workspace created by script fm_casestudy_fx_1.r
load(file="fm_casestudy_fx_1.Rdata")

# 1.0 Extract time series matrix of exchange rates for symbols given by list.symbol0 ----

list.symbol0<-c("DEXCHUS", "DEXJPUS", "DEXKOUS", "DEXMAUS", 
                "DEXUSEU", "DEXUSUK", "DEXTHUS", "DEXSZUS")

fxrates000<-fred.fxrates.00[,list.symbol0]
dim(fxrates000)
head(fxrates000)
tail(fxrates000)

# Print symbol/description/units of these rates from data frame  fred.fxrates.doc 

options(width=120)
print(fred.fxrates.doc[match(list.symbol0, fred.fxrates.doc$symbol),
                       c("symbol0", "fx.desc", "fx.units")])

# Plot exchange rate time series in 2x2 panels
par(mfcol=c(2,2))
for (j0 in c(1:ncol(fxrates000))){
  plot(fxrates000[,j0], 
       main=dimnames(fxrates000)[[2]][j0])
}

# The time series matrix $fxrates000$ has data directly from the FRED website.

#Exchange Rate Regimes for the Chinese Yuan #
#The Chinese Yuan was pegged to the US Dollar prior to July 2005.
#Then, China announced that the exchange rate would be set with 
#reference to a basket of other currencies, allowing for a movement of up to 0.3\% movement within any given day.
#The actual currencies and their basket weights are unannounced by China.


#Converting from USD Base to Swiss Franc Base

#The following R commands convert the dollar-based rates in $fxrates000$ to Swiss-Franc-based rates in $fxrates000.0$

# 2.0 Convert currencies to base rate of DEXSZUS, Swiss Franc
fxrates000.0<-fxrates000

# For exchange rates with 1 U.S. $ in base, divide by DEXSZUS
for (jcol0 in c(1,2,3,4,7)){
  coredata(fxrates000.0)[,jcol0]<- coredata(fxrates000.0[,jcol0])/coredata(fxrates000[,8])
  
}
# For exchange rates with 1 U.S. $ in numerator, divide inverse by DEXSZUS
for (jcol0 in c(5,6)){
  coredata(fxrates000.0)[,jcol0]<- coredata(1./fxrates000.0[,jcol0])/coredata(fxrates000.0[,8])
}
# For USD, divide $1 by the DEXSZUS rate
dimnames(fxrates000.0)[[2]]
coredata(fxrates000.0)[,8]<- 1/coredata(fxrates000)[,8]

# Rename series in terms of the SWIFT currency codes
#     as determined by the International Organization for Standardization. 

list.symbol0.swiftcode<-c("CNY","YEN","WON","MYR","EUR","GBP","THB","USD")
dimnames(fxrates000.0)[[2]]<-paste(list.symbol0.swiftcode,"_SFR",sep="")

head(fxrates000.0)

#  Plot exchange rate time series in 2xs panels
par(mfcol=c(2,2))
for (j0 in c(1:ncol(fxrates000.0))){
  plot(fxrates000.0[,j0], 
       main=dimnames(fxrates000.0)[[2]][j0])
}

# Linear Regression Models of Currency Returns 

# 3.0 Compute daily price changes on the log scale
#     Due to missing data, fill in missing values with previous non-NA
#     To check for presence of missing values, execute
#         apply(is.na(fxrates000.0),2,sum)
#     If necessary apply 
#   fxrates000.0<-na.locf(fxrates000.0)
fxrates000.0.logret<-diff(log(fxrates000.0))
dimnames(fxrates000.0.logret)[[2]]

par(mfcol=c(2,2))
for (j0 in c(1:ncol(fxrates000.0.logret))){
  plot(fxrates000.0.logret[,j0], 
       main=dimnames(fxrates000.0.logret)[[2]][j0])
}

#  First, we fit  the regression model for the period prior to July 2005 when the Chinese currency was pegged to the US dollar.

options(show.signif.stars=FALSE)

lmfit.period1<-lm( CNY_SFR ~ USD_SFR + YEN_SFR + EUR_SFR + GBP_SFR, 
         data=window(fxrates000.0.logret, 
                     start=as.Date("2001-01-01"), end=as.Date("2005-06-30")) )
summary.lm(lmfit.period1)


# The regression fit identifies the pegging of the Yuan (CNR\_SFR) to the US Dollar (USD\_SFR).  The $R-Squared$ is nearly $1.0$


# Second, we fit the regression model for the first six months following the announcement of the change in currency policy.


lmfit.period2<-lm( CNY_SFR ~ USD_SFR + YEN_SFR + EUR_SFR + GBP_SFR + 
                     WON_SFR + MYR_SFR + THB_SFR, 
                   data=window(fxrates000.0.logret, 
                        start=as.Date("2005-07-01"), end=as.Date("2005-12-31")) )
summary.lm(lmfit.period2)

# During this six-month period, there is evidence of the Yuan departing from a US Dollar peg.  The exchange rates with the statsitically significant regression parameters are for the Korean Won (WON\_SFR) and the Malaysian Ringgit (MYR\_SFR). 



# To examine for futher changes in the implicit reference basket, we fit the same model for the annual periods from 2006 through 2012 and for the first 6 months of 2013.



for (year0 in as.character(c(2006:2013))){
  # year0<-"2012"
  lmfit.year0<-lm( CNY_SFR ~ USD_SFR + YEN_SFR + EUR_SFR + GBP_SFR +
                     WON_SFR + MYR_SFR + THB_SFR, 
                   data=fxrates000.0.logret[year0])  
  
  cat("\n\n--------------------------------\n");cat(year0);cat(":\n")
  print(summary.lm(lmfit.year0))
  rate.appreciation.usd<-round( exp(252*log(1+ lmfit.year0$coefficients[1])) -1,digits=3)
  cat("\n"); cat(year0); cat("\t Annualized appreciation rate to implied reference basket: "); cat(rate.appreciation.usd); cat("\n")
}

#From these annual results we note:
#   These fitted regression models demonstrate that the statistical evidence for the underlying reference basket of currencies changes from year to year. 
#   Note how the different exhange rates are significant predictors of the daily change in the Yuan exchange rate for different years.
#   The computations include a measure of the annualized trend in the Yuan exchange rate relative to the other currencies.  Notice that this rate is negative, to varying degrees over the seven-plus years.


#  We illustrate some additional features of exchange rate regime modelling using the reference basket implied by the data for 2012.
 


#First, we plot the currency returns for the Yuan and all currencies included in the analysis.


year0<-"2012"
par(mfcol=c(1,1))
  ts.plot(cumsum(fxrates000.0.logret["2012"]), col=rainbow(NCOL(fxrates000.0.logret)),
          main="2012 Currency Returns")

  legend(x=150,y=.15, legend=dimnames(fxrates000.0.logret)[[2]], lty=rep(1,times=ncol(fxrates000.0.logret)),
       col=rainbow(NCOL(fxrates000.0.logret)), cex=0.70)

#Then, we plot the currency return of the Yuan and that of the implied reference basket specified by the regression:


lmfit.year0<-lm( CNY_SFR ~ USD_SFR + YEN_SFR + EUR_SFR + GBP_SFR + 
                   WON_SFR + MYR_SFR + THB_SFR, 
                 data=fxrates000.0.logret[year0])


y0.actual<-fxrates000.0.logret["2012"][,"CNY_SFR"]

y0.fit<-y0.actual - lmfit.year0$residuals
ts.plot(cumsum(cbind(y0.actual, y0.fit)),
        col=rainbow(NCOL(fxrates000.0.logret))[c(1,5)],
        main="2012 Currency Returns \nCNY_SFR and Implied Basket")

#Note how closely the reference basket tracks the Yuan.  This is to be expected given the high $R-$squared of the regression.


#Finally, we apply the R function $influence.measures()$

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(lmfit.year0)

#These diagnostics indicate:
#   The residuals appear well-behaved as they relate to the size of the fitted values.  The residual variance does not increase with the magnitude of the fitted values.
#   The residuals exhibit heavier tails than those of a normal distribution.  However for those residuals within two standard deviations of their mean, their distribution is close to that of a normal distribution.

