##################################################
## Goodness of fit for exceedance probabilities ##
##################################################
# Note: see ROC in GamPrExc-ROC.R
library(mgcv)
library(tidymv)
library("viridis")
library(schoenberg)
load("german-precipitation.RData")
load("sites_DE_sel_elev.Rdata")
load('GamPrExc/GamData_ber.Rdata')

dat = data.frame("obs"=as.vector(data_ber),
                 "site"=rep(1:ncol(data_ber), each= nrow(data_ber)),
                 "lat"=rep(sites_DE_sel$lat, each= nrow(data_ber)),
                 "lon"=rep(sites_DE_sel$lon, each= nrow(data_ber)),
                 "elev"=rep(sites_DE_sel$elevation, each= nrow(data_ber)),
                 "month"=rep(dates_sel$month, ncol(data_ber)),
                 "year"=rep(dates_sel$year, ncol(data_ber)))

name = 'GamPrExc/GamPrExcSpatioTemporal112stations_elev9_te24_year9.Rdata'
load(name) 
out = b
fit = out$fitted.values

## Chi-square goodness of fit test
r = (out$y - fit)/(sqrt(out$fitted.values*(1-out$fitted.values)))
n = length(out$y); p = 4
1- pchisq(sum(r^2), df = (n-p+1)) # we cannot reject the null hypothesis that this model is correct

# Strategy to calculate the Hosmer-Lemeshow groupings:
# Form a matrix with the outcome and fitted values,
# and re-order according to fitted value probabilities.
index = sort.list(fit)
hosmer = matrix(c(out$y[index], fit[index]), byrow=F, ncol=2)

# Now to group into 10 groups each with m=20 observations, say, and graph:
observed = predicted = rep(NA, 10)
m = floor(1551309/10)
for (i in 1:10) {
  observed[i] <- sum(hosmer[(m*(i-1)+1):(m*i),1])/m
  predicted[i] <- sum(hosmer[(m*(i-1)+1):(m*i),2])/m
}
# the first group consists of the observations with the lowest 10% predicted probabilities. 
# The second group consists of the 10% of the sample whose predicted probabilities are next smallest, etc etc.

plot(predicted, observed, type="b")
abline(0, 1, col = 2)

## Hosmer-Lemeshow Test
# H0: the model fits the data
library(ResourceSelection)
hoslem.test(out$y, fitted(out), g = 10) # reject H0. Might be because of sample size

