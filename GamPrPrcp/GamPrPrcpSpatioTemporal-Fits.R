##########################################################
## Code to fit GAM fits to probability of precipitation ##
##########################################################
# (i) fit your model and extract the deviance residuals
# (ii) for each smooth term in your model, fit an equivalent, single, smooth to the residuals, 
# using a substantially increased k to see if there is pattern in the residuals that could potentially be explained by increasing k.

library(mgcv)
load('german-precipitation.rdata')
load('sites_DE_sel_elev.Rdata') # 4527 sites (columns), 14030 time points (rows)
load('SpatialSample.Rdata')
load('GamData_isPrcp.Rdata')

##############
## GAM data ##
##############
# data_ber = 1*(data_sel > 0)
# dat = data.frame("obs"=as.vector(data_ber),
#                  "site"=rep(1:ncol(data_ber), each= nrow(data_ber)),
#                  "lat"=rep(sites_DE_sel$lat, each= nrow(data_ber)),
#                  "lon"=rep(sites_DE_sel$lon, each= nrow(data_ber)),
#                  "elev"=rep(sites_DE_sel$elevation, each= nrow(data_ber)),
#                  "month"=rep(dates_sel$month, ncol(data_ber)),
#                  "year"=rep(dates_sel$year, ncol(data_ber)))
# 
# site2keep = spatial.sample$id
# # dat = dat[which(dat$site %in% site2keep & !is.na(dat$obs)), ]
# dat = dat[which(dat$site %in% site2keep), ]
# save(dat, file = 'GamData_isPrcp.Rdata')
# ---------------------------------------------------------------------------------------
t0 = Sys.time()
b = gam(obs ~ s(elev, k=5) + te(lon,lat, k=5) + s(month, bs="cc", k=12) + s(year, k=5), 
        family = binomial, 
        data = dat, 
        select = TRUE)
Sys.time() - t0 # 7 min
gam.check(b) # problems with all variables
rsd = residuals(b)
rsd.dat = dat
rsd.dat$rsd = rsd
tmp1 = gam(rsd ~ s(elev, k=40, bs="cs"), gamma = 1.4, data = rsd.dat)
tmp2 = gam(rsd ~ te(lon, lat, k=10, bs="cs"), gamma = 1.4, data = rsd.dat)
tmp3 = gam(rsd ~ s(year, k=24, bs="cs"), gamma = 1.4, data = rsd.dat)
# Approximate significance of smooth terms
summary(tmp1) # significant
summary(tmp2) # significant
summary(tmp3) # significant
rm(b, tmp1, tmp2, tmp3); gc()
# ---------------------------------------------------------------------------------------
t0 = Sys.time()
b = gam(obs ~ s(elev, k=5) + te(lon,lat) + s(month, bs="cc", k=12) + s(year, k=5), 
        family = binomial, 
        data = dat, 
        select = TRUE)
Sys.time() - t0 # 6.2 min
# gam.check(b) # problems with elev and year
rsd = residuals(b)
rsd.dat = dat[!is.na(dat$obs), ]
rsd.dat$rsd = rsd
tmp1 = gam(rsd ~ s(elev, k=40, bs="cs"), gamma = 1.4, data = rsd.dat)
# tmp2 = gam(rsd ~ te(lon, lat, k=30, bs="cs"), gamma = 1.4, data = rsd.dat)
tmp3 = gam(rsd ~ s(year, k=24, bs="cs"), gamma = 1.4, data = rsd.dat)
summary(tmp1) # significant
summary(tmp3) # significant
save(b, file = 'GamPrPrcpSpatioTemporal112stations_elev5_te24_year5.Rdata')
rm(b, rsd, rsd.dat, tmp1, tmp3); gc()
# ---------------------------------------------------------------------------------------
t0 = Sys.time()
b = gam(obs ~ s(elev, k=10) + te(lon,lat) + s(month, bs="cc", k=12) + s(year, k=10), 
        family = binomial, 
        data = dat, 
        select = TRUE) # te(lon,lat,k=24)
Sys.time() - t0 # 19.6 min
gam.check(b) # it doesn't seem to be a problem
rsd = residuals(b)
rsd.dat = dat[!is.na(dat$obs), ]
rsd.dat$rsd = rsd
tmp1 = gam(rsd ~ s(elev, k=40, bs="cs"), gamma = 1.4, data = rsd.dat)
# tmp2 = gam(rsd ~ te(lon, lat, k=30, bs="cs"), gamma = 1.4, data = rsd.dat)
tmp3 = gam(rsd ~ s(year, k=24, bs="cs"), gamma = 1.4, data = rsd.dat)
summary(tmp1) # significant
summary(tmp3) # significant
save(b, file = 'GamPrPrcpSpatioTemporal112stations_elev10_te24_year10.Rdata')
rm(b, rsd, rsd.dat, tmp1, tmp3); gc()
# ---------------------------------------------------------------------------------------
t0 = Sys.time()
b = gam(obs ~ s(elev, k=20) + te(lon,lat) + s(month, bs="cc", k=12) + s(year, k=20), 
        family = binomial, 
        data = dat, 
        select = TRUE)
Sys.time() - t0 # 47 min
gam.check(b) # problem with year
rsd = residuals(b)
rsd.dat = dat[!is.na(dat$obs), ]
rsd.dat$rsd = rsd
tmp1 = gam(rsd ~ s(elev, k=40, bs="cs"), gamma = 1.4, data = rsd.dat)
# tmp2 = gam(rsd ~ te(lon, lat, k=30, bs="cs"), gamma = 1.4, data = rsd.dat)
tmp3 = gam(rsd ~ s(year, k=30, bs="cs"), gamma = 1.4, data = rsd.dat)
summary(tmp1) # significant
summary(tmp3) # significant
save(b, file = 'GamPrPrcpSpatioTemporal112stations_elev20_te24_year20.Rdata')
rm(b, rsd, rsd.dat, tmp1, tmp3); gc()
# El modelo se ve bien con gam.check() excepto por year. 
# Sin embargo, el analysis de residuos arrojo falta de ajuste de elev y year.
# ---------------------------------------------------------------------------------------
t0 = Sys.time()
b = gam(obs ~ s(elev, k=40) + te(lon,lat) + s(month, bs="cc", k=12) + s(year, k=20), 
        family = binomial, 
        data = dat, 
        select = TRUE)
Sys.time() - t0 # 1.22 hours
gam.check(b) #  problem te(lon,lat)
rsd = residuals(b)
rsd.dat = dat[!is.na(dat$obs), ]
rsd.dat$rsd = rsd
tmp1 = gam(rsd ~ s(elev, k=60, bs="cs"), gamma = 1.4, data = rsd.dat)
tmp2 = gam(rsd ~ te(lon, lat, k=35, bs="cs"), gamma = 1.4, data = rsd.dat)
tmp3 = gam(rsd ~ s(year, k=30, bs="cs"), gamma = 1.4, data = rsd.dat)
summary(tmp1) # significant
summary(tmp3) # significant
save(b, file = 'GamPrPrcpSpatioTemporal112stations_elev40_te24_year20.Rdata')
rm(b, rsd, rsd.dat, tmp1, tmp3); gc()
# No mejora


## Observed vs fitted 







