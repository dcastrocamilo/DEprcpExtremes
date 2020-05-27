#############################
## Code to fit exceedances ##
#############################
library(mgcv)
library(evgam)
load('german-precipitation.rdata')
load('sites_DE_sel_elev.Rdata') # 4527 sites (columns), 14030 time points (rows)
load('SpatialSample.Rdata')
load('GamData_gp.Rdata')

dat = data.frame("obs"=as.vector(data_gp),
                 "site"=rep(1:ncol(data_gp), each= nrow(data_gp)),
                 "lat"=rep(sites_DE_sel$lat, each= nrow(data_gp)),
                 "lon"=rep(sites_DE_sel$lon, each= nrow(data_gp)),
                 "elev"=rep(sites_DE_sel$elevation, each= nrow(data_gp)),
                 "month"=rep(dates_sel$month, ncol(data_gp)),
                 "year"=rep(dates_sel$year, ncol(data_gp)))

site2keep = spatial.sample$id
dat = dat[which(dat$site %in% site2keep), ]

# ---------------------------------------------------------------------------------------
t0 = Sys.time()
fmla_gpd = list(obs ~ s(elev) + te(lon,lat) + s(month, bs="cc", k=12) + s(year), ~1)
b = evgam(fmla_gpd, 
          data = dat, 
          family = "gpd")
Sys.time() - t0 # 5.6mins
# gam.check(b) 
save(b, file = 'GamExcScaleSpatioTemporal112stations_elev9_te24_year9.Rdata')
# ---------------------------------------------------------------------------------------
t0 = Sys.time()
fmla_gpd = list(obs ~ s(elev) + te(lon,lat) + s(month, bs="cc", k=12) + s(year), ~s(month, bs="cc", k=12) + s(year))
b = evgam(fmla_gpd, 
          data = dat, 
          family = "gpd")
Sys.time() - t0 # 5.6mins
# gam.check(b) 
save(b, file = 'GamExcScaleSpatioTemporalShapeTemporal112stations_elev9_te24_year9.Rdata')
