#############################################################################################################################
## Code to construct 0-1 matrix for threshold exceedances and matrix of threshold exceedances (same dimension as data_sel) ##
#############################################################################################################################
load('german-precipitation.rdata')
load('sites_DE_sel_elev.Rdata') # 4527 sites (columns), 14030 time points (rows)
load('SpatialSample.Rdata')
load('quantile.mat.Rdata')
dates_quant = data.frame(quantile.mat$dates); names(dates_quant) = c('year', 'month')
quant = data.frame(quantile.mat$quant)

t0 = Sys.time()
dummy1 = function(x,y) x>y
dummy2 = function(x,y) ifelse(x>y, x-y, NA)
data_ber = data_gp = matrix(NA, nrow(data_sel), ncol(data_sel))
for(i in 1:nrow(dates_quant)){
  dates_tmp = dates_quant[i,]
  id = dates_sel$year == dates_tmp$year & dates_sel$month == dates_tmp$month
  data_tmp = data_sel[id, ] # they all have the same threshold vector, quant[i, ]
  data_ber[id, ] = t(apply(data_tmp, 1, dummy1, y = quant[i, ]))
  data_gp[id, ] = t(apply(data_tmp, 1, dummy2, y = as.numeric(quant[i, ])))
}
Sys.time() - t0 # 14.7 mins
data_ber = 1*data_ber
save(data_ber, file = 'GamData_ber.Rdata')
save(data_gp, file = 'GamData_gp.Rdata')

# Some testing
mean(data_ber, na.rm = T) # 0.3
tmp1 = which(!data_ber)
tmp2 = which(is.na(data_gp))
sum(tmp1 != tmp2) # should be 0

st.sample = sample(1:ncol(data_sel), 10, replace = F)
time.sample = sample(1:nrow(data_sel), 10, replace = F)
out = NULL
for(st in st.sample){
  for(j in time.sample){
    tmp = dates_sel[j,]
    a = data_sel[j,st]
    b = quant[dates_quant$year == tmp$year & dates_quant$month == tmp$month,st]
    out = c(out, (a > b)*1 != data_ber[j,st])
  }
}
sum(out, na.rm = T) # should be 0


# # Matrix of thresholds, same dimension as data_sel
# t0 = Sys.time()
# visited = NULL
# th = matrix(NA, nrow(data_sel), ncol(data_sel))
# prop.exc = list(); cont = 0
# for(y in unique(dates_sel$year)){
#   for(m in unique(dates_sel$month)){
#     cont = cont + 1
#     # row IDs in data_sel/th where year = y and month = m
#     id_sel = which(outer(dates_sel$year, y, "==") & outer(dates_sel$month, m, "=="), arr.ind=TRUE)[,1] 
#     if(length(id_sel) > 0){
#       # row IDs in quant where year = y and month = m
#       id_quant = which(outer(dates_quant$year, y, "==") & outer(dates_quant$month, m, "=="), arr.ind=TRUE)[,1] 
#       th[id_sel, ] = as.numeric(quant[id_quant, ])
#       visited = c(visited, id_sel)
#       prop.exc[[cont]] = colMeans(data_sel[id_sel, ] > th[id_sel, ], na.rm = T)
#     }
#     
#   }
# }
# Sys.time() - t0 # 22 sec
# 
# sum(sort(visited) != 1:nrow(data_sel)) # should be 0
# sum(duplicated(visited)) # should be 0
# save(th, file = 'threshold075.Rdata')
# save(prop.exc, file = 'propexc075.Rdata')
# 
# length(unique(dates_sel$year))*length(unique(dates_sel$month))
# length(prop.exc[[1]])
# 
# # Check proportion of threshold is close to theoretical one
# load('GamPrExc/propexc075.Rdata')


# # Matrix of thresholds, same dimension as data_sel (slow version)
# th = matrix(NA, nrow(data_sel), ncol(data_sel))
# for(i in 1:nrow(data_sel)){
#   date.y = dates_sel[i, ]
#   id = which(outer(dates_quant$year, date.y$year, "==") & outer(dates_quant$month, date.y$month, "=="), arr.ind=TRUE) # should have id[1] == id[2]
#   id = unique(as.numeric(id)) # this is the id in the "quant" matrix
#   print(id)
#   if(length(id) == 1)
#     th[i,] = as.numeric(quant[id, ])
#   else
#     stop('Incorrect ID')
#   # Sys.sleep(2)
# }
