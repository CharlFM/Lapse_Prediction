

test$pred <- predict(mod, data.matrix(test[, feature.names]))

auc(roc(test$pred, as.factor(test$STAT))) # 0.7635025

importance_matrix <- xgb.importance(feature.names, model = mod) 
xgb.plot.importance(importance_matrix)


























