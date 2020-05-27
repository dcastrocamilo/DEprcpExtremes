##################################################
## Code to fit GAMs to exceedance probabilities ##
##################################################
library(mgcv)
load('german-precipitation.rdata')
load('sites_DE_sel_elev.Rdata') # 4527 sites (columns), 14030 time points (rows)
load('SpatialSample.Rdata')
load('GamData_ber.Rdata')

dat = data.frame("obs"=as.vector(data_ber),
                 "site"=rep(1:ncol(data_ber), each= nrow(data_ber)),
                 "lat"=rep(sites_DE_sel$lat, each= nrow(data_ber)),
                 "lon"=rep(sites_DE_sel$lon, each= nrow(data_ber)),
                 "elev"=rep(sites_DE_sel$elevation, each= nrow(data_ber)),
                 "month"=rep(dates_sel$month, ncol(data_ber)),
                 "year"=rep(dates_sel$year, ncol(data_ber)))

site2keep = spatial.sample$id
dat = dat[which(dat$site %in% site2keep), ]

t0 = Sys.time()
b = gam(obs ~ s(elev) + te(lon,lat) + s(month, bs="cc", k=12) + s(year), 
        family = binomial, 
        data = dat, 
        select = TRUE)
Sys.time() - t0 # 
gam.check(b) 
save(b, file = 'GamPrExcSpatioTemporal112stations_elev9_te24_year9.Rdata')
