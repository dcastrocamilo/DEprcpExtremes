#############################################################################################################################
## Code to construct 0-1 matrix for threshold exceedances and matrix of threshold exceedances (same dimension as data_sel) ##
#############################################################################################################################
load('german-precipitation.rdata')
load('sites_DE_sel_elev.Rdata') # 4527 sites (columns), 14030 time points (rows)
load('SpatialSample.Rdata')
load('GamPrExc/fitted_quant_75.rdata')

tmp = (data_sel > fitted.quant.mat)
data_ber = 1*tmp
save(data_ber, file = 'GamPrExc/GamData_isExcQ75_14030x4527.Rdata')

# dates_quant = data.frame(quantile.mat$dates); names(dates_quant) = c('year', 'month')
# quant = data.frame(quantile.mat$quant)
# 
# t0 = Sys.time()
# dummy1 = function(x,y) x>y
# dummy2 = function(x,y) ifelse(x>y, x-y, NA)
# data_ber = data_gp = matrix(NA, nrow(data_sel), ncol(data_sel))
# for(i in 1:nrow(dates_quant)){
#   dates_tmp = dates_quant[i,]
#   id = dates_sel$year == dates_tmp$year & dates_sel$month == dates_tmp$month
#   data_tmp = data_sel[id, ] # they all have the same threshold vector, quant[i, ]
#   data_ber[id, ] = t(apply(data_tmp, 1, dummy1, y = quant[i, ]))
#   data_gp[id, ] = t(apply(data_tmp, 1, dummy2, y = as.numeric(quant[i, ])))
# }
# Sys.time() - t0 # 14.7 mins
# data_ber = 1*data_ber
# save(data_ber, file = 'GamData_ber.Rdata')
# save(data_gp, file = 'GamData_gp.Rdata')
# 
# # Some testing
# mean(data_ber, na.rm = T) # 0.3
# tmp1 = which(!data_ber)
# tmp2 = which(is.na(data_gp))
# sum(tmp1 != tmp2) # should be 0
# 
# st.sample = sample(1:ncol(data_sel), 10, replace = F)
# time.sample = sample(1:nrow(data_sel), 10, replace = F)
# out = NULL
# for(st in st.sample){
#   for(j in time.sample){
#     tmp = dates_sel[j,]
#     a = data_sel[j,st]
#     b = quant[dates_quant$year == tmp$year & dates_quant$month == tmp$month,st]
#     out = c(out, (a > b)*1 != data_ber[j,st])
#   }
# }
# sum(out, na.rm = T) # should be 0


