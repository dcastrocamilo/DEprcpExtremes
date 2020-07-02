library(mgcv)
library(evgam)

#####################
## Functional form ##
#####################
f0 = function(x) 2 * sin(pi * x)
f1 = function(x) exp(2 * x)
f2 = function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10 * x)^3 * (1 - x)^10
f3 = function(x) 0 * x

n = 400
set.seed(2047)
x0 = runif(n)
x1 = runif(n)
x2 = runif(n)
x3 = runif(n)

########################
## Purely Gamma model ##
########################
mu.ga = exp((f0(x0)+f2(x2))/5)
sh.ga = exp(f1(x1)/2-2)
y = rgamma(n, shape = 1/sh.ga, scale = mu.ga*sh.ga)

b1 = gam(list(y ~ s(x0) + s(x2), ~s(x1) + s(x3)),
         data = data.frame(y = y, x0 = x0, x1 = x1, x2 = x2, x3 = x3),
         family = gammals)
plot(b1, pages = 1)
# summary(b1); gam.check(b1)
fitted.mu.ga = fitted(b1)[ ,1]
fitted.sh.ga = exp(fitted(b1)[ ,2])

par(mfrow = c(1,2))
plot(mu.ga, fitted.mu.ga)
abline(0, 1, col = 2)
plot(sh.ga, fitted.sh.ga)
abline(0, 1, col = 2)

u = pgamma(y, shape = 1/fitted.sh.ga, scale = fitted.mu.ga*fitted.sh.ga)
hist(u, freq = F, main = 'Unif using fitted values')
abline(h = 1, col = 2, lty = 2)

u.true = pgamma(y, shape = 1/sh.ga, scale = mu.ga*sh.ga)
hist(u.true, freq = F, main = 'Unif using true values')
abline(h = 1, col = 2, lty = 2)

# H0: x ~ y
ks.test('punif', u)$p.value <= 0.05 # rejects
ks.test('punif', u.true)$p.value <= 0.05 # rejects


######################
## Purely GPD model ##
######################
set.seed(10)
sc.gpd = exp((f0(x0)+f2(x2))/5)
sh.gpd = (f1(x1)/2-2)
range(sc.gpd)
range(sh.gpd)

y = rep(0, length(sc.gpd))
for(i in 1:length(sc.gpd))
  y[i] <- evd::rgpd(1, shape = sh.gpd[i], scale = sc.gpd[i])

fmla_gpd = list(y~s(x0) + s(x2), ~s(x1) + s(x3))
b2 <- evgam(fmla_gpd,
            data = data.frame(y = y, x0 = x0, x1 = x1, x2 = x2, x3 = x3), 
            family = "gpd")

plot(b2, pages = 1)

fitted.sc.gpd = exp(fitted(b2)[,1])
fitted.sh.gpd = fitted(b2)[,2]

par(mfrow = c(1,2))
plot(sc.gpd, fitted.sc.gpd)
abline(0, 1, col = 2)
plot(sh.gpd, fitted.sh.gpd)
abline(0, 1, col = 2)

u = rep(0, length(y))
for(i in 1:length(y))
  u[i] = evd::pgpd(y[i], shape = fitted.sh.gpd[i], scale = fitted.sc.gpd[i])

hist(u, freq = F, main = 'Unif using fitted values')
abline(h = 1, col = 2, lty = 2)

u.true = rep(0, length(y))
for(i in 1:length(y))
  u.true[i] = evd::pgpd(y[i], shape = sh.gpd[i], scale = sc.gpd[i])

hist(u.true, freq = F, main = 'Unif using true values')
abline(h = 1, col = 2, lty = 2)

# H0: x ~ y
ks.test('punif', u)$p.value <= 0.05 # rejects
ks.test('punif', u.true)$p.value <= 0.05 # rejects


#########################
## Gamma - GPD mixture ##
#########################
# install.packages("TruncatedDistributions", repos="http://R-Forge.R-project.org")
library(TruncatedDistributions)
pr.exc = 0.75

## Sim exceedances locations
set.seed(2122)
pr = 1 - exp(f0(x0)+f2(x2) + f1(x1)/2-2)/(1+exp(f0(x0)+f2(x2) + f1(x1)/2-2))
components = rbinom(n, 1, prob = pr)

## Sim Gamma
set.seed(2125)
mu.ga = exp((f0(x0)+f2(x2))/5)
sh.ga = exp(f1(x1)/2-2)
y.ga = rgamma(n, shape = 1/sh.ga, scale = mu.ga*sh.ga)

## Threshold
th.ga = qgamma(pr.exc, shape = 1/sh.ga, scale = mu.ga*sh.ga)

## Sim truncated Gamma
set.seed(2147)
y.ga = rtgamma(n, shape = 1/sh.ga, scale = mu.ga*sh.ga, a = 0, b = th.ga)

## Sim GPD
set.seed(2132)
sc.gpd = exp((f0(x0)+f2(x2))/5)
sh.gpd = (f1(x1)/2-2)
y.gpd = rep(0, length(sc.gpd))
for(i in 1:length(sc.gpd))
  y.gpd[i] = evd::rgpd(1, loc = th.ga[i], shape = sh.gpd[i], scale = sc.gpd[i])

## Mixture sample
samples = y.ga
samples[components == 1] = y.gpd[components == 1]
  
## Plot data
plot(samples[components == 0], ylim = range(samples), pch = 16)
points((1:n)[components == 1], samples[components == 1], pch = 16, col = 2)
lines(th.ga, col = 3, lwd = 2)
any(which(components == 1) != which(samples >th.ga)) # dummy check: exceedances are located according to 'components'. THIS MIGHT NOT BE THE CASE IF WE DONT REPLACE GAMMA SAMPLES WITH TRUNCATED GAMMA

par(mfrow = c(1,1))
plot(density(samples))


## Gamma fit
b3 = gam(list(y ~ s(x0) + s(x2), ~s(x1) + s(x3)),
         data = data.frame(y = samples, x0 = x0, x1 = x1, x2 = x2, x3 = x3),
         family = gammals)

fitted.mu.ga = fitted(b3)[ ,1]
fitted.sh.ga = exp(fitted(b3)[ ,2])
# Note that Gamma estimates are influenced by GPD values. We should re-estimate using truncated Gamma
fitted.q75.ga = qgamma(pr.exc, shape = 1/fitted.sh.ga, scale = fitted.mu.ga*fitted.sh.ga)

## GPD fit
is.exc = samples > fitted.q75.ga; mean(is.exc)
samples.exc = samples[is.exc]
fmla_gpd = list(y~s(x0) + s(x2), ~s(x1) + s(x3))
b4 <- evgam(fmla_gpd,
            data = data.frame(y = samples.exc, x0 = x0[is.exc], x1 = x1[is.exc], x2 = x2[is.exc], x3 = x3[is.exc]), 
            family = "gpd")

fitted.sc.gpd = exp(fitted(b4)[,1])
fitted.sh.gpd = fitted(b4)[,2]

## Exceedance prob
exc = 1*is.exc
b5 = gam(y ~ s(x0) + s(x1) + s(x2),
         data = data.frame(y = exc, x0 = x0, x1 = x1, x2 = x2, x3 = x3),
         family = 'binomial')

fitted.pr.exc = fitted(b5); range(fitted.pr.exc)

## Plots for the fits
par(mfrow = c(3,2))
plot(mu.ga, fitted.mu.ga)
abline(0, 1, col = 2)
plot(sh.ga, fitted.sh.ga)
abline(0, 1, col = 2)

plot(sc.gpd[is.exc], fitted.sc.gpd) # only the values that were identified by the models as "exceedances"
abline(0, 1, col = 2)
plot(sh.gpd[is.exc], fitted.sh.gpd)
abline(0, 1, col = 2)

plot(pr, fitted.pr.exc)
abline(0, 1, col = 2)
mean(pr)
mean(fitted.pr.exc)


## Transform to uniform margins
u = pgamma(samples, shape = 1/fitted.sh.ga, scale = fitted.mu.ga*fitted.sh.ga)
u.exc = rep(NA, sum(is.exc))
for(i in 1:sum(is.exc))
  u.exc[i] = evd::pgpd(samples[is.exc][i], loc = fitted.q75.ga[is.exc][i], scale = fitted.sc.gpd[i], shape = fitted.sh.gpd[i])
u[is.exc] = u.exc

par(mfrow = c(2,1))
hist(u, freq = F, main = 'Unif using fitted values')
abline(h = 1, col = 2, lty = 2)

u.true = ptgamma(samples, shape = 1/sh.ga, scale = mu.ga*sh.ga, a = 0, b = th.ga)
u.exc = rep(NA, sum(components))
for(i in 1:sum(components))
  u.exc[i] = evd::pgpd(samples[components][i], loc = th.ga[components][i], scale = sc.gpd[components][i], shape = sh.gpd[components][i])
u.true[components] = u.exc

hist(u.true, freq = F, main = 'Unif using true values')
abline(h = 1, col = 2, lty = 2)


