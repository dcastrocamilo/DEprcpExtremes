n = 200000

#############################################
# Normal mixture: p1*N(0,1) + (1-p1)*N(5,1) #
#############################################

## Random generation
p1 = 0.3
samples = rep(NA, n)
components = sample(1:2, prob = c(p1, 1-p1), size = n, replace = TRUE)
samples[components == 1] = rnorm(sum(components == 1))
samples[components == 2] = rnorm(sum(components == 2), mean = 5)

plot(density(samples))

## Transformation to uniform scale
u = rep(NA, n)
u = pnorm(samples)
u[components == 2] = pnorm(samples[components == 2], mean = 5)
hist(u, freq = F)

#############################################
# Almost our mixture: p1*GPD + (1-p1)*Gamma #
#############################################
library(evd)

## Random generation
samples = rep(NA, n)

p1 = 0.1
probs = c(p1, 1-p1)

sh.gamma = 0.2
sc.gamma = 1.2
sh.gpd = 0.1
sc.gpd = 0.5
th = 1

components = sample(1:2, prob = probs, size = n, replace = TRUE)
samples[components == 1] = rgpd(sum(components == 1), loc = th, scale = sc.gpd, shape = sh.gpd)
samples[components == 2] = rgamma(sum(components == 2), shape = sh.gamma, scale = sc.gamma)

plot(density(samples))

## Transformation to uniform scale
u = rep(NA, n)
u = pgamma(samples, shape = sh.gamma, scale = sc.gamma)
u[components == 1] = pgpd(samples[components == 1], loc = th, scale = sc.gpd, shape = sh.gpd)
hist(u, freq = F)

#############################################
# Our mixture: p1*GPD + p2*Gamma + p3*delta #
#############################################
library(evd)

## Random generation
samples = rep(NA, n)

pu = 0.1
p0 = 0.95
probs = c(pu, p0-pu, 1-p0)

sh.gamma = 0.2
sc.gamma = 1.2
sh.gpd = 0.1
sc.gpd = 0.5
th = 1

components = sample(1:3, prob = probs, size = n, replace = TRUE)
samples[components == 1] = rgpd(sum(components == 1), loc = th, scale = sc.gpd, shape = sh.gpd)
samples[components == 2] = rgamma(sum(components == 2), shape = sh.gamma, scale = sc.gamma)
samples[components == 3] = rep(0, sum(components == 3))

plot(density(samples))

## Transformation to uniform scale
u = rep(NA, n)
u = pgamma(samples, shape = sh.gamma, scale = sc.gamma)
u[components == 1] = pgpd(samples[components == 1], loc = th, scale = sc.gpd, shape = sh.gpd)
u[components == 3] = (1-p0) #rep(0, sum(components == 3))
hist(u, freq = F)



fct2unif <- function(y,u,sh.gamma,sc.gamma,sh.gpd,sc.gpd,pu,p0){
  if(is.na(y)) ret <- NA
  else{
    if(y <= u) 
      ret <- ((y>0)*pgamma(y, shape=sh.gamma, scale=sc.gamma)) + ((1-p0)*(y==0))
    if(y>u) 
      ret <- 1-pu*(1+sh.gpd*(y-u)/sc.gpd)^(-1/sh.gpd)
  }
  return(ret)
}

sapply(sample, fct2unif, )
