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

# Gam check:
# Method: UBRE   Optimizer: outer newton
# full convergence after 7 iterations.
# Gradient range [-1.757037e-10,1.469602e-07]
# (score 0.2230398 & scale 1).
# Hessian positive definite, eigenvalue range [4.728343e-10,4.171086e-07].
# Model rank =  53 / 53
# 
# Basis dimension (k) checking results. Low p-value (k-index<1) may
# indicate that k is too low, especially if edf is close to k'.
# 
#                k'   edf k-index p-value
# s(elev)      9.00  8.46    1.00    0.93
# te(lon,lat) 24.00 23.55    0.99    0.53
# s(month)    10.00  9.93    0.98    0.20
# s(year)      9.00  8.97    0.97    0.14

  