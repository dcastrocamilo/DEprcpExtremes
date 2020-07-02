######################################
## Simulation: temporal Gamma model ##
######################################
nyear = 10 # number of years
ntime = 365 * nyear
samples = NULL

dates = seq.Date(from = as.Date("1990-01-01"), len = ntime, by = 1)
timestamps = as.POSIXlt(dates)

sh.gamma = 0.2
sc.gamma = 1
a = 0
b = 1
set.seed(100)

## Add a monthly and an annual cycle
monthly.cycle = runif(12, a, b)
month = timestamps$mon + 1
annual.cycle = runif(nyear, a, b)
year = timestamps$year -90 + 1

## Time-varying mean
mu = exp(monthly.cycle[month] + annual.cycle[year])
range(mu)
hist(mu)

## Data
y = rgamma(ntime, shape = 1/sh.gamma, scale = mu*sh.gamma)
range(y)

dat = data.frame(year = timestamps$year+ 1900, month = month, obs = y)

#############
## GAM fit ##
#############
library(mgcv)
fit <- gam(list(obs ~ s(year) + s(month, bs = "cc", k = 12), ~1), 
           data = dat,
           family = gammals, 
           select = TRUE)
# save(fit, file="Simulation/gamma_temporal.Rdata")

###############
## Check fit ##
###############
library(schoenberg)
draw(fit)

fitted.mean = fitted(fit)[,1]
fitted.shape = exp(fitted(fit)[,2])
plot(mu, fitted.mean)
abline(0,1,col=2)
sh.gamma
unique(fitted.shape)


################################
## Transform to uniform scale ##
################################
par(mfrow = c(1,2), mar=c(3,3.2,1.5,0.5),mgp=c(1.6,0.5,0),font.main=1.3,cex=1.3,cex.main=1)
u = pgamma(y, shape = 1/fitted.shape, scale = fitted.mean*fitted.shape)
hist(u, freq = F)
abline(h = 1, lty = 2, col = 2)

u.true = pgamma(y, shape = 1/sh.gamma, scale = mu*sh.gamma)
hist(u.true, freq = F)
abline(h = 1, col = 2, lty = 2)


########################
## Plot annual effect ##
########################
library(viridis)
year2pred = seq(min(dat$year),max(dat$year),length.out = 100)
yeffect = yeffect.sd = NULL
for(mm in 1:12){
  tmp = predict(fit, 
                type="response", 
                newdata=data.frame("month"=mm,"year"=year2pred))
  pred.shape = tmp[,2]
  pred.mean  = tmp[,1]
  
  yeffect = cbind(yeffect, qgamma(0.5, shape = 1/exp(pred.shape), scale = pred.mean*exp(pred.shape)))
}

par(mar=c(3,3.2,1.5,0.5),mgp=c(1.6,0.5,0),font.main=1.3,cex=1.3,cex.main=1)
col2use = plasma(12)
plot(year2pred, yeffect[,1], type="l", ylab="Estimated median", xlab="Time", col=col2use[1], 
     ylim=c(range(yeffect)[1],range(yeffect)[2]))
text(x=year2pred[2],y=yeffect[2,1], labels = as.character(1), cex=0.6)
for(mm in 2:12){
  lines(year2pred, yeffect[,mm], col=col2use[mm])
  text(x=year2pred[2],y=yeffect[2,mm], labels = as.character(mm), cex=0.6)
}