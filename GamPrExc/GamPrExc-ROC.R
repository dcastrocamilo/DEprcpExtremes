##############################################################################################################################
## ROC for exceedance probabilities: random sample doesn't really make sense since we have a cyclic smooth effect on months ##
##############################################################################################################################
# Reference: https://daviddalpiaz.github.io/r4sl/logistic-regression.html

library(mgcv)
load('german-precipitation.rdata')
load('sites_DE_sel_elev.Rdata') # 4527 sites (columns), 14030 time points (rows)
load('SpatialSample.Rdata')
load('GamPrExc/GamData_ber.Rdata')

dat = data.frame("obs"=as.vector(data_ber),
                 "site"=rep(1:ncol(data_ber), each= nrow(data_ber)),
                 "lat"=rep(sites_DE_sel$lat, each= nrow(data_ber)),
                 "lon"=rep(sites_DE_sel$lon, each= nrow(data_ber)),
                 "elev"=rep(sites_DE_sel$elevation, each= nrow(data_ber)),
                 "month"=rep(dates_sel$month, ncol(data_ber)),
                 "year"=rep(dates_sel$year, ncol(data_ber)))

site2keep = spatial.sample$id
dat = dat[which(dat$site %in% site2keep), ]

# Divide data, 50% training, 50% testing
set.seed(42)
dat_idx = sample(1:nrow(dat), nrow(dat)*0.5)
dat_trn = dat[dat_idx, ]
dat_tst = dat[-dat_idx, ]

# Fit the model using the training data
t0 = Sys.time()
b = gam(obs ~ s(elev) + te(lon,lat) + s(month, bs="cc", k=12) + s(year), 
        family = binomial, 
        data = dat_trn, 
        select = TRUE)
Sys.time() - t0 # 36 mins
save(b, file = 'GamPrExcSpatioTemporal112stations_elev9_te24_year9_TrainingData50ROC.Rdata')

load('GamPrExc/GamPrExcSpatioTemporal112stations_elev9_te24_year9_TrainingData90ROC.Rdata')
load('GamPrExc/GamPrExcSpatioTemporal112stations_elev9_te24_year9_TrainingData50ROC.Rdata')

# We write a function which allows use to make predictions based on different probability cutoffs.
get_logistic_pred = function(mod, data, res = "y", pos = 1, neg = 0, cut = 0.5) {
  probs = predict(mod, newdata = data, type = "response")
  ifelse(probs > cut, pos, neg)
}

# Let’s use this to obtain predictions using a low, medium, and high cutoff. (0.1, 0.5, and 0.9)
test_pred_10 = get_logistic_pred(b, data = dat_tst, res = "obs", 
                                 pos = "Yes", neg = "No", cut = 0.1)
test_pred_50 = get_logistic_pred(b, data = dat_tst, res = "obs", 
                                 pos = "Yes", neg = "No", cut = 0.5)
test_pred_90 = get_logistic_pred(b, data = dat_tst, res = "obs", 
                                 pos = "Yes", neg = "No", cut = 0.9)

# We evaluate accuracy, sensitivity, and specificity for these classifiers.
library(caret)

test_tab_10 = table(predicted = test_pred_10, actual = dat_tst$obs)
test_tab_50 = table(predicted = test_pred_50, actual = dat_tst$obs)
test_tab_90 = table(predicted = test_pred_90, actual = dat_tst$obs)

test_con_mat_10 = confusionMatrix(test_tab_10, positive = "Yes")
test_con_mat_50 = confusionMatrix(test_tab_50, positive = "Yes")
test_con_mat_90 = confusionMatrix(test_tab_90, positive = "Yes")

metrics = rbind(
  
  c(test_con_mat_10$overall["Accuracy"], 
    test_con_mat_10$byClass["Sensitivity"], 
    test_con_mat_10$byClass["Specificity"]),
  
  c(test_con_mat_50$overall["Accuracy"], 
    test_con_mat_50$byClass["Sensitivity"], 
    test_con_mat_50$byClass["Specificity"]),
  
  c(test_con_mat_90$overall["Accuracy"], 
    test_con_mat_90$byClass["Sensitivity"], 
    test_con_mat_90$byClass["Specificity"])
  
)

rownames(metrics) = c("c = 0.10", "c = 0.50", "c = 0.90")
metrics

# ROC curve which will sweep through all possible cutoffs, and plot the sensitivity and specificity
library(pROC)
test_prob = predict(b, newdata = dat_tst, type = "response")
test_roc = roc(dat_tst$obs ~ test_prob, plot = TRUE, print.auc = TRUE)


