########################################################
## Code to sample locations regularly (112 locations) ##
########################################################
library(sp)
library(rje)
library(fields)
loc = cbind(sites_DE_sel[,4], sites_DE_sel[,5])
# loc = cbind(loc[,1]*67.7,loc[,2]*111.2) #transform app to Euclidean coordinates
loc.sr = SpatialPolygons(list(Polygons(list(Polygon(loc)), "x")))
# plot(loc.sr)
plot(loc, col = 'grey80')
set.seed(20200518)
sub.loc.sr1 = spsample(loc.sr, n = 1013, "regular") # n = 5000
points(sub.loc.sr1, pch = 16, col = 3)
subloc.sp = data.frame(sub.loc.sr1); dim(subloc.sp)

subloc = matrix(NA, nrow(subloc.sp), 3)
sub.id = rep(NA, nrow(subloc.sp))
for(i in 1:nrow(subloc.sp)){
  printPercentage(i, nrow(subloc.sp))
  tmp = rdist.earth(subloc.sp[i, ], loc, miles = F)
  id = which.min(tmp)
  sub.id[i] = id
  subloc[i, ] = c(loc[id, ], sites_DE_sel[id,6]) 
}
points(subloc[,1:2], pch = 16, col = 2, cex = .6)
dim(subloc)
head(subloc)
spatial.sample = data.frame(id = sub.id, lon = subloc[,1], lat = subloc[,2], elev = subloc[,3])
save(spatial.sample, file = 'SpatialSample.Rdata')


