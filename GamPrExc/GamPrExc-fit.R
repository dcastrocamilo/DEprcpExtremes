##################################################
## Code to fit GAMs to exceedance probabilities ##
##################################################
library(mgcv)
load('german-precipitation.rdata')
load('sites_DE_sel_elev.Rdata') # 4527 sites (columns), 14030 time points (rows)
load('SpatialSample.Rdata')
load('GamPrExc/fitted_quant_75.rdata')

dat = data.frame("obs"=as.vector(fitted.quant.mat),
                 "site"=rep(1:ncol(fitted.quant.mat), each= nrow(fitted.quant.mat)),
                 "lat"=rep(sites_DE_sel$lat, each= nrow(fitted.quant.mat)),
                 "lon"=rep(sites_DE_sel$lon, each= nrow(fitted.quant.mat)),
                 "elev"=rep(sites_DE_sel$elevation, each= nrow(fitted.quant.mat)),
                 "month"=rep(dates_sel$month, ncol(fitted.quant.mat)),
                 "year"=rep(dates_sel$year, ncol(fitted.quant.mat)))

site2keep = spatial.sample$id
dat = dat[which(dat$site %in% site2keep), ]
save(dat, file = 'GamPrExc/GamData_isExcQ75.Rdata')

t0 = Sys.time()
b = gam(obs ~ s(elev) + te(lon,lat) + s(month, bs="cc", k=12) + s(year), 
        family = binomial, 
        data = dat, 
        select = TRUE)
Sys.time() - t0 # 18 mins
gam.check(b) 
save(b, file = 'GamPrExcQ75SpatioTemporal112stations_elev9_te24_year9.Rdata')

