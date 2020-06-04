##################################################
## Code to fit GAMs to exceedance probabilities ##
##################################################
library(mgcv)
load('german-precipitation.rdata')
load('sites_DE_sel_elev.Rdata') # 4527 sites (columns), 14030 time points (rows)
load('SpatialSample.Rdata')
# load('GamPrExc/GamData_isExcQ75.Rdata')
load('GamData_isExcQ75.Rdata')

# load('GamPrExc/GamData_isExcQ75_14030x4527.Rdata')
# 
# dat = data.frame("obs"=as.vector(data_ber),
#                  "site"=rep(1:ncol(data_ber), each= nrow(data_ber)),
#                  "lat"=rep(sites_DE_sel$lat, each= nrow(data_ber)),
#                  "lon"=rep(sites_DE_sel$lon, each= nrow(data_ber)),
#                  "elev"=rep(sites_DE_sel$elevation, each= nrow(data_ber)),
#                  "month"=rep(dates_sel$month, ncol(data_ber)),
#                  "year"=rep(dates_sel$year, ncol(data_ber)))
# 
# site2keep = spatial.sample$id
# dat = dat[which(dat$site %in% site2keep), ]
# save(dat, file = 'GamPrExc/GamData_isExcQ75.Rdata')

t0 = Sys.time()
b = gam(obs ~ s(elev) + te(lon,lat) + s(month, bs="cc", k=12) + s(year), 
        family = binomial, 
        data = dat, 
        select = TRUE)
Sys.time() - t0 # 18 mins
gam.check(b) 
save(b, file = 'GamPrExcQ75SpatioTemporal112stations_elev9_te24_year9.Rdata')

# Method: UBRE   Optimizer: outer newton
# full convergence after 10 iterations.
# Gradient range [-1.419232e-08,5.560796e-08]
# (score -0.2802886 & scale 1).
# eigenvalue range [-1.376961e-08,2.556729e-07].
# Model rank =  53 / 53
# ss
# Basis dimension (k) checking results. Low p-value (k-index<1) may
# indicate that k is too low, especially if edf is close to k'.
# 
#                k'   edf k-index p-value
# s(elev)      9.00  8.93    0.96    0.85
# te(lon,lat) 24.00 23.64    0.94    0.21
# s(month)    10.00  9.98    0.96    0.90
# s(year)      9.00  7.93    0.97    0.94