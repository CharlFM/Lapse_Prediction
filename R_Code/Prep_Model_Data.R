####################################################################################################################
# Preparing the model All_lap_Data #
####################################

# Set Lapse Status to binary
All_lap_Data$STAT <- 0
# Status For Pahse 1
# All_lap_Data$STAT[All_lap_Data$STATUS == "LAP" & (All_lap_Data$DURATION >= 3 & All_lap_Data$DURATION <= 5)] <- 1
All_lap_Data$STAT[All_lap_Data$STATUS == "LAP"] <- 1

All_lap_Data$rand <- runif(nrow(All_lap_Data))

feature.names <- colnames(All_lap_Data)
# Features for Predicting Phase 1
# feature.names <- feature.names[!(feature.names %in% c("STAT", "STATUS", "DURATION","COMMENCEMENTDATEOFPOLICY", "STATUSEFFECTIVEENDDATE",
#                                                       "rand", "INCREASE", "TOTALAMOUNTPAIDSINCEINCEPTIONTOCURRENTMONTH",
#                                                       "VOICELOGGEDENDORSEMENT", "IDNUMBEROFLIFEINSURED", "VOICELOGGED",
#                                                       "POLICYHOLDERSURNAME", "LASTPREMIUM"))]

feature.names <- feature.names[!(feature.names %in% c("STAT", "STATUS", "COMMENCEMENTDATEOFPOLICY", "STATUSEFFECTIVEENDDATE",
                                                      "rand", "IDNUMBEROFLIFEINSURED", "VOICELOGGED",
                                                      "POLICYHOLDERSURNAME"))]


train <- subset(All_lap_Data, rand < 0.8) 
test  <- subset(All_lap_Data, rand >= 0.8)

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

train <- lapply(train, as.numeric)
test  <- lapply(test, as.numeric)

tra <- train[,feature.names]

h <- base::sample(nrow(train), 0.2 * nrow(train)) # Takes a sample of 20% of the training All_lap_Dataa

# Split training All_lap_Dataa into 2 All_lap_Dataasets
dval <- xgb.DMatrix(data = data.matrix(tra[h,]), 
                    label = train$STAT[h])
dtrain <- xgb.DMatrix(data = data.matrix(tra[-h,]), 
                      label = train$STAT[-h])





