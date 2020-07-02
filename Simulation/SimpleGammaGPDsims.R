library(mgcv)

###########
## Gamma ##
###########
f0 <- function(x) 2 * sin(pi * x)
f1 <- function(x) exp(2 * x)
f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 * 
  (10 * x)^3 * (1 - x)^10
f3 <- function(x) 0 * x
n <- 400;set.seed(9)
x0 <- runif(n);x1 <- runif(n);
x2 <- runif(n);x3 <- runif(n);

mu <- exp((f0(x0)+f2(x2))/5)
th <- exp(f1(x1)/2-2)
y <- rgamma(n,shape=1/th,scale=mu*th)

b1 <- gam(list(y~s(x0)+s(x2),~s(x1)+s(x3)),family=gammals)
plot(b1,pages=1)
summary(b1)
# gam.check(b1)
fitted.mean = fitted(b1)[,1]
fitted.shape = exp(fitted(b1)[,2])

plot(mu, fitted.mean);abline(0,1,col=2)
plot(th, fitted.shape);abline(0,1,col=2)

u = pgamma(y, shape = 1/fitted.shape, scale = fitted.mean*fitted.shape)
hist(u, freq = F)
abline(h = 1, col = 2, lty = 2)

u.true = pgamma(y, shape = 1/th, scale = mu*th)
hist(u.true, freq = F)
abline(h = 1, col = 2, lty = 2)

#########
## GPD ##
#########
set.seed(10)
scale <- exp((f0(x0)+f2(x2))/5)
sh <- (f1(x1)/2-2)
# sh = 0.1
range(scale)
range(sh)

y = rep(0, length(scale))
for(i in 1:length(scale))
  y[i] <- evd::rgpd(1, shape = sh[i], scale = scale[i])

fmla_gpd = list(y~s(x0)+s(x2),~s(x1)+s(x3))
# fmla_gpd = list(y~s(x0)+s(x2),~1)
b2 <- evgam(fmla_gpd,
            data = data.frame(y = y, x0 = x0, x1 = x1, x2 = x2, x3 = x3), 
            family = "gpd")

plot(b2, pages = 1)

fitted.scale = exp(fitted(b2)[,1])
fitted.shape = fitted(b2)[,2]
plot(scale, fitted.scale);abline(0,1,col=2)
plot(sh, fitted.shape);abline(0,1,col=2)

par(mfrow = c(1,2), mar=c(3,3.2,1.5,0.5),mgp=c(1.6,0.5,0),font.main=1.3,cex=1.3,cex.main=1)
u = rep(0, length(y))
for(i in 1:length(y))
  u[i] = evd::pgpd(y[i], shape = fitted.shape[i], scale = fitted.scale[i])

hist(u, freq = F)
abline(h = 1, col = 2, lty = 2)

u.true = rep(0, length(y))
for(i in 1:length(y))
  u.true[i] = evd::pgpd(y[i], shape = sh[i], scale = scale[i])

hist(u.true, freq = F)
abline(h = 1, col = 2, lty = 2)
