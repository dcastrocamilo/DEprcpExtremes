################################################################################################################################################
## Data simulation                                                                                                                            ##
## For each site, fake daily data are constructed using a mixture of Gamma, GPD and point mass at 0. The data have monthly and annual cycles. ##
## We add an upward trend in elevation and random walks for lon and lat                                                                       ##
################################################################################################################################################

RW = function(N, x0, mu, variance) { # simulates a random walk
  # N: length, x0: initial value, mu: drift
  z = cumsum(rnorm(n=N, mean=0, sd=sqrt(variance)))
  t = 1:N
  x = x0+t*mu+z
  return(x)
}

nsites = 100 # number of sites
nyear = 5 # number of years
samples = NULL

pu = 0.1
p0 = 0.95
probs = c(pu, p0-pu, 1-p0)

sh.gamma = 0.2
sc.gamma = 1.2
sh.gpd = 0.1
sc.gpd = 0.5
th = 1

for(i in 1:nsites){
  set.seed(100*i)
  ## Random generation
  # residuals = rnorm(365 * nyear)
  residuals = rgamma(365 * nyear, shape = sh.gamma, scale = sc.gamma)
  ntime = length(residuals)
  
  components = sample(1:3, prob = probs, size = ntime, replace = TRUE)
  residuals[components == 1] = evd::rgpd(sum(components == 1), loc = th, scale = sc.gpd, shape = sh.gpd)
  residuals[components == 3] = rep(0, sum(components == 3))
  r.residuals = range(residuals)
  # plot(residuals)
  # hist(residuals, freq = F)
  # plot(density(residuals))
  
  ## Add a monthly cycle
  dates = seq.Date(from = as.Date("1990-01-01"), len = ntime, by = 1)
  timestamps = as.POSIXlt(dates)
  
  # monthly.cycle = rnorm(12)
  monthly.cycle = runif(12, r.residuals[1], r.residuals[2])
  month = timestamps$mon + 1
  response = monthly.cycle[month] + residuals
  # plot(dates, response)
  
  ## Add annual cycle
  # annual.cycle = rnorm(nyear)
  annual.cycle = runif(nyear, r.residuals[1], r.residuals[2])
  year = timestamps$year -90 + 1
  response = annual.cycle[year] + response
  # plot(dates, response)
  
  response[components == 3] = rep(0, sum(components == 3))
  
  samples = cbind(samples, response)
}

range(samples)

## Add an upward trend effect for elevation
set.seed(2072020)
elev = seq(100,500,length.out = nsites)
elev.trend = 0.5*elev + rnorm(nsites,0,15)
elev.trend = (max(samples)/2)*(elev.trend - min(elev.trend))/(max(elev.trend) - min(elev.trend))
plot(elev, elev.trend)

for(i in 1:nsites)
  samples[, i] = samples[, i] + elev.trend[i]

plot(samples[,1], pch = 16, ylim = range(samples))
points(samples[,50], pch = 16, col = 2)
points(samples[,100], pch = 16, col = 3)

## Add a smooth effect for longitude and latitude
set.seed(2072020)
lon = RW(nsites, 10, 0, 0.0004)
lat = RW(nsites, 10, 0, 0.0008)

plot(lon, pch = 16, ylim = c(min(lon,lat), max(lon,lat)))
points(lat, pch = 16, col = 2)

for(i in 1:nsites)
  samples[, i] = samples[, i] + lon[i] + lat[i]

plot(samples[,1], pch = 16, ylim = range(samples))
points(samples[,50], pch = 16, col = 2)
points(samples[,100], pch = 16, col = 3)

## Data for mgcv
dat = data.frame("samples"=as.vector(samples),
                 "site"=rep(1:ncol(samples), each= nrow(samples)),
                 "lat"=rep(lat, each= nrow(samples)),
                 "lon"=rep(lon, each= nrow(samples)),
                 "elev"=rep(elev, each= nrow(samples)),
                 "month"=rep(timestamps$mon+1, ncol(samples)),
                 "year"=rep(timestamps$year+1900, ncol(samples)))

save(dat, file = 'Simulation/datasim.Rdata')
