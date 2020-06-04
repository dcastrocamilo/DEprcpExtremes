##################################################
## Goodness of fit for exceedance probabilities ##
##################################################
library(mgcv)
library(tidymv)
library("viridis")
library(schoenberg)
load("german-precipitation.RData")
load("sites_DE_sel_elev.Rdata")
load('GamPrExc/GamData_isExcQ75.Rdata')
st = 22


name = 'GamPrExc/GamPrExcSpatioTemporal112stations_elev9_te24_year9'
load(paste0(name,'.Rdata') )
out = b
fit = out$fitted.values

## Chi-square goodness of fit test
# H0: the model is correct
r = (out$y - fit)/(sqrt(out$fitted.values*(1-out$fitted.values)))
n = length(out$y); p = 4
chi2 = 1- pchisq(sum(r^2), df = (n-p+1)); chi2 # > 0.05 we cannot reject the null hypothesis that this model is correct

## Hosmer-Lemeshow Test
# H0: the model fits the data
library(ResourceSelection)
hl = hoslem.test(out$y, fitted(out), g = 10); hl 


# Strategy to calculate the Hosmer-Lemeshow groupings:
# Form a matrix with the outcome and fitted values,
# and re-order according to fitted value probabilities.
index = sort.list(fit)
hosmer = matrix(c(out$y[index], fit[index]), byrow=F, ncol=2)

# Now to group into 10 groups each with m=20 observations, say, and graph:
observed = predicted = rep(NA, 10)
m = floor(nrow(hosmer)/10)
for (i in 1:10) {
  observed[i] <- sum(hosmer[(m*(i-1)+1):(m*i),1])/m
  predicted[i] <- sum(hosmer[(m*(i-1)+1):(m*i),2])/m
}
# the first group consists of the observations with the lowest 10% predicted probabilities. 
# The second group consists of the 10% of the sample whose predicted probabilities are next smallest, etc etc.

pdf(paste0(name, '-GoF.pdf'))
plot(predicted, observed, type="b")
abline(0, 1, col = 2)
mtext(paste('Chi-squared:', round(chi2,4)), side = 1, line = -6)
mtext(paste('HL:', round(hl$p.value,6)), side = 1, line = -5)
dev.off()

