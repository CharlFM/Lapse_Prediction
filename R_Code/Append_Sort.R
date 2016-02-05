#####################################################################################################
# Cleans Province info #
########################

# Append City with Accent City info
City_Data$ACCENTCITY   <-  gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(City_Data$ACCENTCITY)))
City_Data$CITY         <-  gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(City_Data$CITY)))
City_Data$PROVINCENAME <-  gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(City_Data$PROVINCENAME)))

ACC_temp <- City_Data[City_Data$CITY != City_Data$ACCENTCITY,]
ACC_temp <- subset(ACC_temp, select = - CITY)

colnames(ACC_temp)[colnames(ACC_temp) == "ACCENTCITY"] <- "CITY"
  
City_Data <- subset(City_Data, select = -ACCENTCITY)

City_Data <- rbind(City_Data, ACC_temp)

City_Data <- subset(City_Data, select = -c(LATITUDE, LONGITUDE, PROVINCEID))

rm(ACC_temp)

# Clean adress names
All_lap_Data$POSTALADDRESS1 <- gsub(" ","",gsub("[^[:alnum:] ]", "", toupper(All_lap_Data$POSTALADDRESS1)))
All_lap_Data$POSTALADDRESS2 <- gsub(" ","",gsub("[^[:alnum:] ]", "", toupper(All_lap_Data$POSTALADDRESS2)))
All_lap_Data$POSTALADDRESS3 <- gsub(" ","",gsub("[^[:alnum:] ]", "", toupper(All_lap_Data$POSTALADDRESS3)))

# Clean province info
Provinces <- c("WESTERNCAPE", "LIMPOPO", "MPUMALANGA", "GAUTENG", "EASTERNCAPE", "KWAZULUNATAL", "NORTHERNCAPE", "FREESTATE", "NORTHWEST")

# Select given province info
All_lap_Data$PROVINCE <- ""
All_lap_Data$PROVINCE[All_lap_Data$POSTALADDRESS3 %in% Provinces] <- All_lap_Data$POSTALADDRESS3[All_lap_Data$POSTALADDRESS3 %in% Provinces]
All_lap_Data$PROVINCE[All_lap_Data$POSTALADDRESS2 %in% Provinces] <- All_lap_Data$POSTALADDRESS2[All_lap_Data$POSTALADDRESS2 %in% Provinces]
All_lap_Data$PROVINCE[All_lap_Data$POSTALADDRESS1 %in% Provinces] <- All_lap_Data$POSTALADDRESS1[All_lap_Data$POSTALADDRESS1 %in% Provinces]

# Merge City info with each of the postal adressess and complete where missing
All_lap_Data <- merge(All_lap_Data, City_Data, by.x = "POSTALADDRESS1", by.y = "CITY", all.x = TRUE)
All_lap_Data$PROVINCENAME[is.na(All_lap_Data$PROVINCENAME)] <- ""
All_lap_Data$PROVINCE[All_lap_Data$PROVINCE == ""] <- All_lap_Data$PROVINCENAME[All_lap_Data$PROVINCE == ""]
All_lap_Data <- subset(All_lap_Data, select = -PROVINCENAME)

All_lap_Data <- merge(All_lap_Data, City_Data, by.x = "POSTALADDRESS2", by.y = "CITY", all.x = TRUE)
All_lap_Data$PROVINCENAME[is.na(All_lap_Data$PROVINCENAME)] <- ""
All_lap_Data$PROVINCE[All_lap_Data$PROVINCE == ""] <- All_lap_Data$PROVINCENAME[All_lap_Data$PROVINCE == ""]
All_lap_Data <- subset(All_lap_Data, select = -PROVINCENAME)

All_lap_Data <- merge(All_lap_Data, City_Data, by.x = "POSTALADDRESS3", by.y = "CITY", all.x = TRUE)
All_lap_Data$PROVINCENAME[is.na(All_lap_Data$PROVINCENAME)] <- ""
All_lap_Data$PROVINCE[All_lap_Data$PROVINCE == ""] <- All_lap_Data$PROVINCENAME[All_lap_Data$PROVINCE == ""]
All_lap_Data <- subset(All_lap_Data, select = -PROVINCENAME)

rm(Provinces, City_Data)

#####################################################################################################
# Determine Duration #
######################

fileXLSDate <- file.mtime(paste(Path, "/Data/AssetLife Data/", lap_File_List[lapfile], sep = ""))

All_lap_Data$COMMENCEMENTDATEOFPOLICY <- DateConv(All_lap_Data$COMMENCEMENTDATEOFPOLICY)
All_lap_Data$COMMENCEMENTDATEOFPOLICY <- firstDayMonth(All_lap_Data$COMMENCEMENTDATEOFPOLICY)
All_lap_Data$STATUSEFFECTIVEENDDATE   <- DateConv(All_lap_Data$STATUSEFFECTIVEENDDATE)

All_lap_Data$STATUSEFFECTIVEENDDATE <- as.Date(ifelse(All_lap_Data$STATUSEFFECTIVEENDDATE < All_lap_Data$COMMENCEMENTDATEOFPOLICY, 
                                               All_lap_Data$COMMENCEMENTDATEOFPOLICY, 
                                               All_lap_Data$STATUSEFFECTIVEENDDATE))

# If there is an end date and the number of collected premiums equals 0, then force the end date to equal start date
All_lap_Data$STATUSEFFECTIVEENDDATE[!is.na(All_lap_Data$STATUSEFFECTIVEENDDATE) & 
                                      All_lap_Data$TOTALAMOUNTPAIDSINCEINCEPTIONTOCURRENTMONTH == 0] <- 
  All_lap_Data$COMMENCEMENTDATEOFPOLICY[!is.na(All_lap_Data$STATUSEFFECTIVEENDDATE) & 
                                        All_lap_Data$TOTALAMOUNTPAIDSINCEINCEPTIONTOCURRENTMONTH == 0]
  

All_lap_Data$DURATION                               <-  as.numeric(((All_lap_Data$STATUSEFFECTIVEENDDATE - 
                                                                       All_lap_Data$COMMENCEMENTDATEOFPOLICY) / 365.25) * 12)
All_lap_Data$DURATION[is.na(All_lap_Data$DURATION)] <-  as.numeric(((as.Date(substr(fileXLSDate, 1, 10)) - 
                                                                       All_lap_Data$COMMENCEMENTDATEOFPOLICY[is.na(All_lap_Data$DURATION)]) / 365.25) * 12)

rm(lap_File_List, lapfile)

#####################################################################################################
# Determine Status #
####################

# If there is end date = Lapse, else Active ( or 1, 0)
All_lap_Data$STATUS[!is.na(All_lap_Data$STATUSEFFECTIVEENDDATE)] <- "LAP"    
All_lap_Data$STATUS[is.na(All_lap_Data$STATUSEFFECTIVEENDDATE)]  <- "ACT"

#####################################################################################################
# Weight Cleaning #
###################

# Rename
colnames(All_lap_Data)[which(names(All_lap_Data) == "WEIGHT130KGSORWEIGHTOLDPOLICIES")] <- "WEIGHT"

# If weight is outside the interquartile range, bring it to IQR boundry.
# first find the interquartile range
weight <- gsub(" ", "", toupper(All_lap_Data$WEIGHT))
weight[weight == "NOTANSWERED" | weight == ""] <- NA

temp_weight  <-  as.numeric(weight)
AboveMean    <-  mean(temp_weight[temp_weight > 130], na.rm = TRUE)
BelowMean    <-  mean(temp_weight[temp_weight < 130], na.rm = TRUE)

weight[weight == "YES"]  <-  as.character(AboveMean)
weight[weight == "NO"]   <-  as.character(BelowMean)
weight                   <-  as.numeric(weight)

mean  <-  mean(weight, na.rm = TRUE)
IQR   <-  IQR(weight,  na.rm = TRUE)

weight[weight > mean + 2*IQR] <- mean + IQR
weight[weight < mean - 2*IQR] <- mean - IQR

All_lap_Data$WEIGHT <-  weight

rm(mean, IQR, weight, temp_weight, BelowMean, AboveMean)

#####################################################################################################
# Minor Cleaning #
##################

### Voice Log day + Month ###
All_lap_Data$VOICELOGGED       <-  DateConv(All_lap_Data$VOICELOGGED)
All_lap_Data$VOICELOGGEDDAY    <-  format(All_lap_Data$VOICELOGGED, format = "%d")
All_lap_Data$VOICELOGGEDMONTH  <-  format(All_lap_Data$VOICELOGGED, format = "%m")

### Clean smoking columns ###
All_lap_Data$SMOKERNONSMOKER <- gsub(" ","",gsub("[^[:alnum:] ]", "", toupper(All_lap_Data$SMOKERNONSMOKER)))
All_lap_Data$SMOKERNONSMOKER[All_lap_Data$SMOKERNONSMOKER == "YES"]   <- "SMOKER"
All_lap_Data$SMOKERNONSMOKER[All_lap_Data$SMOKERNONSMOKER == "NO"]    <- "NON-SMOKER"

### Clean More than 30 cigs column. anything that is a "no" becomes a "no" ###
colnames(All_lap_Data)[which(names(All_lap_Data) == "DOYOUSMOKEMORETHAN30CIGARETTESPERDAY")] <- "MORETHAN30"
All_lap_Data$MORETHAN30 <- gsub(" ","",gsub("[^[:alnum:] ]", "", toupper(All_lap_Data$MORETHAN30)))

All_lap_Data$MORETHAN30[All_lap_Data$MORETHAN30 %in% c("LESSTHAN30ADAY", "LESSTHAN30PERDAY", "NONSMOKER", "NOTAPPLICABLE")] <-  "NO"
All_lap_Data$MORETHAN30[All_lap_Data$MORETHAN30 %in% c("MORETHAN30ADAY", "MORETHAN30PERDAY")]                               <-  "YES"

### Checking if height is non-numeric or if it is less than 100cm. if the height is less than 100 then we add a 100 ###
All_lap_Data$HEIGHTINCM <- as.numeric(All_lap_Data$HEIGHTINCM)
All_lap_Data$HEIGHTINCM[All_lap_Data$HEIGHTINCM < 100 & !is.na(All_lap_Data$HEIGHTINCM)] <- All_lap_Data$HEIGHTINCM[All_lap_Data$HEIGHTINCM < 100 & !is.na(All_lap_Data$HEIGHTINCM)] + 100 
All_lap_Data$HEIGHTINCM[is.na(All_lap_Data$HEIGHTINCM)] <- median(All_lap_Data$HEIGHTINCM, na.rm = TRUE)

### PPB indicator (1 if PPB, 0 if not) ###
All_lap_Data$PPB <- gsub(" ", "", All_lap_Data$PPBTOCELLCAPTIVE)
All_lap_Data$PPB[All_lap_Data$PPB != ""]  <- 1
All_lap_Data$PPB[All_lap_Data$PPB == ""]  <- 0

# Find Payment Day
All_lap_Data$PREMIUMPAYERDEBITORDERDAY <- as.numeric(gsub("([0-9]+).*$", "\\1", All_lap_Data$PREMIUMPAYERDEBITORDERDAY))

# We need the number of beneficiaries and the relationship to the policyholder.
# Cleaning all relationship columns
temp     <-  select(All_lap_Data, contains("RELATIONSHIPOFBENEFICIARY"))
temp     <-  toupper(gsub(" ", "", as.matrix(temp)))
All_lap_Data[colnames(All_lap_Data) %in% colnames(temp)] <- temp 

# finding the names of all the columns with "relationshipofbeneficiary" to count the number of beneficiaries
All_lap_Data$NUMBEROFBENEFICIARIES <- 0
temp[temp != ""] <- 1
temp[temp == ""] <- 0
temp <- apply(temp, 2, as.numeric)
All_lap_Data$NUMBEROFBENEFICIARIES <- rowSums(temp)

# Number of credit providers and cleaning provider names.
temp     <-  select(All_lap_Data, contains("CREDITPROVIDER"))
temp     <-  toupper(gsub(" ", "", as.matrix(temp)))
All_lap_Data[colnames(All_lap_Data) %in% colnames(temp)] <- temp

# count the number of credit providers
All_lap_Data$NOCREDITPROVIDERS          <-  0
temp[temp != ""]                        <-  1
temp[temp == ""]                        <-  0
temp                                    <-  apply(temp, 2, as.numeric)
All_lap_Data$NOCREDITPROVIDERS          <-  rowSums(temp)

rm(temp)

# Clean email info
All_lap_Data$EMAILADDRESS <- sub(".*\\@", "", All_lap_Data$EMAILADDRESS)
All_lap_Data$DOMAIN       <- sub("\\..*$", "", All_lap_Data$EMAILADDRESS)
All_lap_Data$EXTENTION    <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", All_lap_Data$EMAILADDRESS)))
All_lap_Data$EMAILDOTS    <- as.numeric(countLetter(All_lap_Data$EMAILADDRESS, "."))
All_lap_Data              <- subset(All_lap_Data, select = -EMAILADDRESS)

# Clean phone number info
All_lap_Data$PHONEW                             <-  gsub(" ", "", All_lap_Data$PHONEW)
All_lap_Data$PHONEW[All_lap_Data$PHONEW == ""]  <-  "0000000000"
All_lap_Data$PHONEW                             <-  ifelse(as.character(substr(All_lap_Data$PHONEW, 1, 1)) == "0", 
                                                           All_lap_Data$PHONEW, 
                                                           paste("0", All_lap_Data$PHONEW, sep = ""))

All_lap_Data$PHONEAREA     <-  substr(All_lap_Data$PHONEW, 1, 3)
All_lap_Data$PHONESUBAREA  <-  substr(All_lap_Data$PHONEW, 4, 6)

All_lap_Data              <- subset(All_lap_Data, select = -PHONEW)

# Get Race Info
Race_Data$RACE     <-  gsub(" ","", gsub("[^[:alpha:] ]", "", toupper(Race_Data$RACE)))
Race_Data$SURNAME  <-  gsub(" ","", gsub("[^[:alpha:] ]", "", toupper(Race_Data$SURNAME)))
Race_Data$CULTURE  <-  gsub(" ","", gsub("[^[:alpha:] ]", "", toupper(Race_Data$CULTURE)))

All_lap_Data$POLICYHOLDERSURNAME <- gsub(" ","", gsub("[^[:alpha:] ]", "", toupper(All_lap_Data$POLICYHOLDERSURNAME)))

All_lap_Data <- merge(All_lap_Data, Race_Data, by.x = "POLICYHOLDERSURNAME", by.y = "SURNAME", all.x = TRUE)





























