####################################################################################################################
# Preparing the model All_lap_Data #
####################################

feature.names <- colnames(All_lap_Data)

# Leave out features that are proxies for lapse status or just useless info
feature.names <- feature.names[!(feature.names %in% c("STAT", "STATUS", "COMMENCEMENTDATEOFPOLICY", "STATUSEFFECTIVEENDDATE",
                                                      "IDNUMBEROFLIFEINSURED", "VOICELOGGED",
                                                      "POLICYHOLDERSURNAME", "LAP", "DURATION"))]


train <- subset(All_lap_Data, VOICELOGGED <  seq(as.Date(fileXLSDate), length = 2, by = "-8 months")[2]) 
test  <- subset(All_lap_Data, VOICELOGGED >= seq(as.Date(fileXLSDate), length = 2, by = "-8 months")[2])

# put IDs for text variables - replace missings by 0 - boosting runs faster
for (f in feature.names) {
  if (class(train[[f]])=="Date") {
    train[[f]] <- as.character(train[[f]])
    test[[f]] <- as.character(test[[f]])
    }
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]])) # Combines all possible factors (both from train and test)
    train[[f]] <- ifelse(is.na(train[[f]]), 0, as.integer(factor(train[[f]], levels=levels))) # Create a numeric ID
    test[[f]]  <- ifelse(is.na(test[[f]]), 0, as.integer(factor(test[[f]],  levels=levels)))  # instead of a character ID (If NA then 0)
  }
  else if (class(train[[f]]) == "integer" | class(train[[f]]) == "numeric"){
    train[[f]] <- ifelse(is.na(train[[f]]), 0, train[[f]]) # If NA then
    test[[f]] <- ifelse(is.na(test[[f]]), 0, test[[f]])    # 0
  }
}

tra <- train[,feature.names]

h <- base::sample(nrow(train), 0.2 * nrow(train)) # Takes a sample of 20% of the training All_lap_Dataa

# Split training All_lap_Dataa into 2 All_lap_Dataasets
dval <- xgb.DMatrix(data = data.matrix(tra[h,]), 
                    label = train$LAP[h])
dtrain <- xgb.DMatrix(data = data.matrix(tra[-h,]), 
                      label = train$LAP[-h])





