
ntrees <- 1000

watchlist <- list(val = dval, train = dtrain)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eta                 = 6 / ntrees,
                gamma               = 0.3,
                max_depth           = 7,
                subsample           = 0.7,
                colsample_bytree    = 0.7
)

cv.res <- xgb.cv(   params              = param, 
                    data                = dtrain, 
                    nrounds             = ntrees,
                    verbose             = 1, # Whether to print output or not
                    early.stop.round    = 75,
                    watchlist           = watchlist,
                    maximize            = TRUE,
                    eval.metric         = "auc",
                    nfold               = 4
)

mod <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = ntrees,
                    verbose             = 1, # Whether to print output or not
                    early.stop.round    = 75,
                    watchlist           = watchlist,
                    maximize            = TRUE,
                    eval.metric         = "auc"        
)



















