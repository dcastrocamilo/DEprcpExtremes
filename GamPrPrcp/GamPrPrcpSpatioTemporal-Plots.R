#############################
## Code to plot GAM models ##
#############################
rm(list = ls())
library(mgcv)
library(tidymv)
library("viridis")
library(schoenberg)
load("german-precipitation.RData")
load("sites_DE_sel_elev.Rdata")
load("GamPrPrcp/GamData_isPrcp.Rdata")
st = 22
name.generic = 'GamPrPrcp/GamPrPrcpSpatioTemporal112stations_'
# -------
elev = 5
te = 24
year = 5
# -------
elev = 10
te = 24
year = 10
# -------
elev = 20
te = 24
year = 20
# -------
elev = 40
te = 24
year = 20
# -------

name = paste0(name.generic, 'elev', elev, '_te', te, '_year', year)
load(paste0(name, '.Rdata'))
out = b
name.generic = 'GamPrPrcp/Plots/GamPrPrcpSpatioTemporal112stations_'
name = paste0(name.generic, 'elev', elev, '_te', te, '_year', year)

## Plot of Spatio-temporal effects
file = paste0(name, '.pdf')
pdf(file = file, width = 12, height = 12)
draw(out)
dev.off()

## Annual effect over the fitted pr
year2pred <- seq(min(dat$year),max(dat$year),length.out = 100)
# elev2pred <- seq(min(dat$elev),max(dat$elev),length.out = 100)
# lon2pred <- seq(min(dat$lon),max(dat$lon),length.out = 100)
# lat2pred <- seq(min(dat$lat),max(dat$lat),length.out = 100)
yeffect <- NULL
for(mm in 1:12){
  newdata = data.frame("month"=mm, "year"=year2pred, "elev"=sites_DE_sel$elevation[st], "lon"=sites_DE_sel$lon[st], "lat"=sites_DE_sel$lat[st])
  yeffect <- cbind(yeffect, as.numeric(predict(out, type = "response", newdata = newdata)))
}

pdf(paste0(name, '-YearEffect.pdf'), width = 7, height = 5)
par(mar=c(3,3.2,1.5,0.5),mgp=c(1.6,0.5,0),font.main=1.3,cex=1.3,cex.main=1)
col2use <- plasma(12)
plot(year2pred, yeffect[,1], type="l", ylab="Fitted probability", xlab="Time", col=col2use[1], ylim=c(.450,0.7))
text(x=year2pred[1],y=yeffect[1,1], labels = month.abb[1], cex=0.6, col = col2use[1])
for(mm in 2:12){
  lines(year2pred, yeffect[,mm], col=col2use[mm])
  text(x=year2pred[6*mm],y=yeffect[(6*mm),mm], labels = month.abb[mm], cex=0.6, col = col2use[mm])
}
dev.off()
  
## Annual effect over fitted pr - Summer/Winter
pdf(paste0(name, '-YearEffectSeassons.pdf'), width = 7, height = 5)
par(mar=c(3,3.2,1.5,0.5),mgp=c(1.6,0.5,0),font.main=1.3,cex=1.3,cex.main=1)
col2use <- plasma(12)
plot(year2pred, yeffect[,1], type="l", ylab="Fitted probability", xlab="Time", col=col2use[1], ylim=c(0,1), lwd = 1.2)
for(mm in 2:12){
  col = col2use[9] # summer
  if(mm<5 || mm>9) col = col2use[1]
  lines(year2pred, yeffect[,mm], col=col, lwd = 1.2)
}
legend("topright", legend=c('Summer', 'Winter'), col=c(col2use[9], col2use[1]), lty=1, lwd = 2)
dev.off()

## Monthly effect over pr at a single location
month2pred = 1:12
elev2pred = rep(dat$elev[st],12)
lon2pred = rep(dat$lon[st],12)
lat2pred = rep(dat$lat[st],12)
yeffect <- NULL
for(mm in unique(dat$year)){
  newdata = data.frame("month"=month2pred, "year"=mm, "elev"=elev2pred, "lon"=lon2pred, "lat"=lat2pred)
  yeffect <- cbind(yeffect, as.numeric(predict(out, type = "response", newdata = newdata)))
}

pdf(paste0(name, '-MonthEffect.pdf'), width = 7, height = 5)
col2use <- plasma(39)
plot(month2pred, yeffect[,1], type="l", ylab="Fitted probability", xlab='', col=col2use[1], ylim=c(0,1), axes = F)
axis(1, at = 1:12, labels = month.abb); axis(2); box()
text(x=month2pred[1],y=yeffect[1,1], labels = as.character(1981), cex=0.6, col = col2use[1], lwd = 2)
sel.year1 = c(1990,2000,2010,2019)
sel.year2 = which(unique(dat$year)%in%sel.year1)
for(i in 1:length(sel.year1)){
  mm = sel.year2[-1][i]
  lines(month2pred, yeffect[,mm], col=col2use[mm])
  text(x=month2pred[3*i],y=yeffect[3*i,mm], labels = sel.year1[i], cex=0.6, col = col2use[mm])
}
dev.off()

