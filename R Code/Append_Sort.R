#####################################################################################################
# Cleans Data #
###############

fileXLSDate <- file.mtime(paste(Path, "/Data/", lap_File_List[lapfile], sep = ""))

All_lap_Data$COMMENCEMENTDATEOFPOLICY <- DateConv(All_lap_Data$COMMENCEMENTDATEOFPOLICY)

All_lap_Data$STATUSEFFECTIVEENDDATE <- DateConv(All_lap_Data$STATUSEFFECTIVEENDDATE)

All_lap_Data$POSTALADDRESS1 <- gsub(" ","",gsub("[^[:alnum:] ]", "", toupper(All_lap_Data$POSTALADDRESS1)))
All_lap_Data$POSTALADDRESS2 <- gsub(" ","",gsub("[^[:alnum:] ]", "", toupper(All_lap_Data$POSTALADDRESS2)))
All_lap_Data$POSTALADDRESS3 <- gsub(" ","",gsub("[^[:alnum:] ]", "", toupper(All_lap_Data$POSTALADDRESS3)))

All_lap_Data$DURATION                                               <-  as.numeric(((as.Date(substr(fileXLSDate,1,10)) - 
                                                                                       All_lap_Data$COMMENCEMENTDATEOFPOLICY) / 365.25) * 12)
All_lap_Data$DURATION[!is.na(All_lap_Data$STATUSEFFECTIVEENDDATE)]  <-  as.numeric(((All_lap_Data$STATUSEFFECTIVEENDDATE[!is.na(All_lap_Data$STATUSEFFECTIVEENDDATE)] 
                                                                                     - All_lap_Data$COMMENCEMENTDATEOFPOLICY[!is.na(All_lap_Data$STATUSEFFECTIVEENDDATE)]) / 365.25) * 12)


# Remove data from workspace (to save memory and time)
rm(lap_Data, lapfile, num_lap_file, lap_File_List, common_cols, file_name, CSV_file_name) 

###################### Calculate the year-to-year increase in QUOTED premiums

All_lap_Data$Increase        <-  1
All_lap_Data$CURRENTPREMIUM  <-  All_lap_Data$YEAR1QUOTEDTOTALPREMIUM
All_lap_Data$LASTREMIUM      <-  All_lap_Data$YEAR1QUOTEDTOTALPREMIUM
year_len                     <-  ceiling(max(All_lap_Data$DURATION) / 12)

for (i in 2:year_len){
  # if duration is less than or equal to i but greater than i - 1
  # TOOK data$year i premium as currentpremium
  dur1 <- All_lap_Data[All_lap_Data$DURATION <= 12*i & All_lap_Data$DURATION >= 12*(i - 1), paste0("YEAR", i, "QUOTEDTOTALPREMIUM")]
  # TOOK data$year i - 1 premium as lastpremium
  dur2 <- All_lap_Data[All_lap_Data$DURATION <= 12*i & All_lap_Data$DURATION >= 12*(i - 1), paste0("YEAR", i - 1, "QUOTEDTOTALPREMIUM")]
  
  All_lap_Data$CURRENTPREMIUM[All_lap_Data$DURATION <= 12*i & All_lap_Data$DURATION >= 12*(i - 1)]  <- dur1
  All_lap_Data$LASTREMIUM[    All_lap_Data$DURATION <= 12*i & All_lap_Data$DURATION >= 12*(i - 1)]  <- dur2
            
}

All_lap_Data$INCREASE                               <-  as.numeric(All_lap_Data$CURRENTPREMIUM)/as.numeric(All_lap_Data$LASTREMIUM)
All_lap_Data$INCREASE[All_lap_Data$INCREASE == 1]   <-  NA
All_lap_Data$INCREASE[All_lap_Data$STATUS == "NTU"] <-  NA

########### Clean postal codes
# We want province and postal codes
Provinces <- c("WESTERNCAPE", "LIMPOPO", "MPUMALANGA", "GAUTENG", "EASTERNCAPE", "KWAZULUNATAL", "NORTHERNCAPE", "FREESTATE", "NORTHWEST",
               "WESTERNPROVINCE", 
               "CAPETOWN", "PRETORIA", "EASTLONDON", "SOWETO", "DURBAN", "JOHANNESBURG", "PORTELIZABETH", "PIETERMARITZBURG", 
               "RONDEBOSCH", "BENONI")

ProperProv <- c("WESTERNCAPE", "LIMPOPO", "MPUMALANGA", "GAUTENG", "EASTERNCAPE", "KWAZULUNATAL", "NORTHERNCAPE", "FREESTATE", "NORTHWEST")

###########################################################################################
###################################### SOME WORK TO BE DONE HERE ########################## 123
###########################################################################################

All_lap_Data2<-All_lap_Data
All_lap_Data2$PROVINCE <- ""
All_lap_Data2 <- All_lap_Data2[!(All_lap_Data$POSTALADDRESS3 %in% ProperProv) & !(All_lap_Data$POSTALADDRESS2 %in% ProperProv) & !(All_lap_Data$POSTALADDRESS1 %in% ProperProv),]
t1 <- as.data.frame(table(All_lap_Data2$POSTALADDRESS3))
t2 <- as.data.frame(table(All_lap_Data2$POSTALADDRESS2))
t3 <- as.data.frame(table(All_lap_Data2$POSTALADDRESS1))

# Look at all of the three postal columns and see if you can find one of the 9 provinces
All_lap_Data$PROVINCE <- ""
All_lap_Data$PROVINCE[All_lap_Data$POSTALADDRESS3 %in% Provinces] <- All_lap_Data$POSTALADDRESS3[All_lap_Data$POSTALADDRESS3 %in% Provinces]
All_lap_Data$PROVINCE[All_lap_Data$POSTALADDRESS2 %in% Provinces] <- All_lap_Data$POSTALADDRESS2[All_lap_Data$POSTALADDRESS2 %in% Provinces]
All_lap_Data$PROVINCE[All_lap_Data$POSTALADDRESS1 %in% Provinces] <- All_lap_Data$POSTALADDRESS1[All_lap_Data$POSTALADDRESS1 %in% Provinces]

# Replace cities with provinces:
All_lap_Data$PROVINCE[All_lap_Data$PROVINCE %in% c("CAPETOWN", "RONDEBOSCH", "WESTERNPROVINCE")]     <- "WESTERNCAPE"
All_lap_Data$PROVINCE[All_lap_Data$PROVINCE %in% c("PRETORIA", "SOWETO", "JOHANNESBURG", "BENONI")]  <- "GAUTENG"
All_lap_Data$PROVINCE[All_lap_Data$PROVINCE %in% c("PIETERMARITZBURG", "DURBAN")]                    <- "KWAZULUNATAL"
All_lap_Data$PROVINCE[All_lap_Data$PROVINCE %in% c("PORTELIZABETH", "EASTLONDON")]                   <- "EASTERNCAPE"

# strip payment day into just numbers
######################### All_lap_Data$ ############## which payment date????????// lkjlk;j ########J
  
All_lap_Data$VOICELOGGED <- DateConv(All_lap_Data$VOICELOGGED)

# from voice log extract day and month.
All_lap_Data$VOICELOGGEDDAY <- format(All_lap_Data$VOICELOGGED, format = "%d")
All_lap_Data$VOICELOGGEDMONTH <- format(All_lap_Data$VOICELOGGED, format = "%m")

# Clean smoking columns
All_lap_Data$SMOKERNONSMOKER <- gsub(" ","",gsub("[^[:alnum:] ]", "", toupper(All_lap_Data$SMOKERNONSMOKER)))
All_lap_Data$SMOKERNONSMOKER[All_lap_Data$SMOKERNONSMOKER == "YES"]   <- "SMOKER"
All_lap_Data$SMOKERNONSMOKER[All_lap_Data$SMOKERNONSMOKER == "NO"]    <- "NON-SMOKER"

# Clean More than 30 cigs column. anything that is a "no" becomes a "no".
colnames(All_lap_Data)[which(names(All_lap_Data) == "DOYOUSMOKEMORETHAN30CIGARETTESPERDAY")] <- "MORETHAN30"
All_lap_Data$MORETHAN30 <- gsub(" ","",gsub("[^[:alnum:] ]", "", toupper(All_lap_Data$MORETHAN30)))
All_lap_Data$MORETHAN30[All_lap_Data$MORETHAN30 %in% c("LESSTHAN30ADAY",
                                                       "LESSTHAN30PERDAY",
                                                       "NON-SMOKER",
                                                       "NOTAPPLICABLE")]       <-  "NO"

All_lap_Data$MORETHAN30[All_lap_Data$MORETHAN30 %in% c("MORETHAN30ADAY",
                                                       "YES",
                                                       "MORETHAN30PERDAY")]    <-  "YES"

# checking if height is non-numeric or if it is less than 100cm. if the height is less than 100 then we add a 100.
All_lap_Data$HEIGHTINCM <- as.numeric(All_lap_Data$HEIGHTINCM)
All_lap_Data$HEIGHTINCM[!as.numeric(All_lap_Data$HEIGHTINCM)] <- mean(All_lap_Data$HEIGHTINCM[!is.na(All_lap_Data$HEIGHTINCM)])
All_lap_Data$HEIGHTINCM[All_lap_Data$HEIGHTINCM < 100 & !is.na(All_lap_Data$HEIGHTINCM)] <- All_lap_Data$HEIGHTINCM[All_lap_Data$HEIGHTINCM < 100 & !is.na(All_lap_Data$HEIGHTINCM)] + 100 



# choose the columns you want to keep

Names <- c("AFFINITYGROUP",
           "CURRENTPREMIUM",
           "LASTPREMIUM",
           "INCREASE",
           "EMPLOYEEBATCHNUMBER",
           "TITLEOFPOLICYHOLDER",
           "POLICYHOLDERSURNAME",
           "PROVINCE",
           "POSTALCODE",
           "EMAILADDRESS",
           "PHONEW",
           "PREMIUMPAYERBANK",
           "PREMIUMPAYERBRANCHCODE",
           "ACCOUNTTYPECODE",
           "PREMIUMPAYERDEBITORDERDAY",
           "COMMENCEMENTDATEOFPOLICY",
           "STATUS",
           "STATUSEFFECTIVEENDDATE",
           "AGENTNAME",
           "VOICELOGGEDDAY",
           "VOICELOGGEDMONTH",
           "IDNUMBEROFLIFEINSURED",
           "DOBOFLIFEINSURED",
           "GENDER", 
           "LEVELOFINCOME",
           "EDUCATION",
           "SMOKERNONSMOKER",
           "MORETHAN30",
           "HEIGHTINCM",
           "WEIGHT130KGSORWEIGHTOLDPOLICIES",
           "CREDITPROTECTIONDEATHSUMASSURED",
           "CREDITPROTECTIONDISABILITYSUMASSURED",
           "CREDITPROTECTIONTEMPORARYDISABILITYMONTHLYBENEFIT",
           "TEMPORARYDISABILITYPERIOD",
           "RETRENCHMENTBENEFIT",
           "RETRENCHMENTPERIOD",
           "RAFBENEFIT",
           "RAFNIGHTSINHOSPITAL",
           "RAFTYPEOFCOVER",
           "CRITICALILLNESSCOVER",
           "ROADCOVERINCLUDED",
           "CREDITPROVIDER1",
           "ACCOUNTTYPEWITHCREDITPROVIDER1",
           "ACCOUNTNUMBERWITHCREDITPROVIDER1",
           "DEATHBENEFITTOCEDETOCREDITPROVIDER1",
           "PTDBENEFITTOCEDETOCREDITPROVIDER1",
           "CRITICALILLNESSBENEFITTOCEDETOCREDITPROVIDER1",
           "TTDBENEFITTOCEDETOCREDITPROVIDER1",
           "RETRENCHMENTBENEFITTOCEDETOCREDITPROVIDER1",
           "RETRENCHMENTBENEFITPERIODFORCREDITPROVIDER1",
           "CREDITPROVIDER2", 
           "ACCOUNTTYPEWITHCREDITPROVIDER2",
           "ACCOUNTNUMBERWITHCREDITPROVIDER2",
           "DEATHBENEFITTOCEDETOCREDITPROVIDER2",
           "PTDBENEFITTOCEDETOCREDITPROVIDER2",
           "CRITICALILLNESSBENEFITTOCEDETOCREDITPROVIDER2",
           "TTDBENEFITTOCEDETOCREDITPROVIDER2",
           "RETRENCHMENTBENEFITTOCEDETOCREDITPROVIDER2",
           "RETRENCHMENTBENEFITPERIODFORCREDITPROVIDER2",
           "CREDITPROVIDER3",
           "ACCOUNTTYPEWITHCREDITPROVIDER3",
           "ACCOUNTNUMBERWITHCREDITPROVIDER3",
           "DEATHBENEFITTOCEDETOCREDITPROVIDER3",
           "PTDBENEFITTOCEDETOCREDITPROVIDER3",
           "CRITICALILLNESSBENEFITTOCEDETOCREDITPROVIDER3",
           "TTDBENEFITTOCEDETOCREDITPROVIDER3",
           "RETRENCHMENTBENEFITTOCEDETOCREDITPROVIDER3",
           "RETRENCHMENTBENEFITPERIODFORCREDITPROVIDER3",
           "CREDITPROVIDER4",
           "ACCOUNTTYPEWITHCREDITPROVIDER4",
           "ACCOUNTNUMBERWITHCREDITPROVIDER4",
           "DEATHBENEFITTOCEDETOCREDITPROVIDER4",
           "PTDBENEFITTOCEDETOCREDITPROVIDER4",
           "CRITICALILLNESSBENEFITTOCEDETOCREDITPROVIDER4",
           "TTDBENEFITTOCEDETOCREDITPROVIDER4",
           "RETRENCHMENTBENEFITTOCEDETOCREDITPROVIDER4",
           "RETRENCHMENTBENEFITPERIODFORCREDITPROVIDER4",
           "CREDITPROVIDER5",
           "ACCOUNTTYPEWITHCREDITPROVIDER5",
           "ACCOUNTNUMBERWITHCREDITPROVIDER5",
           "DEATHBENEFITTOCEDETOCREDITPROVIDER5",
           "PTDBENEFITTOCEDETOCREDITPROVIDER5",
           "CRITICALILLNESSBENEFITTOCEDETOCREDITPROVIDER5",
           "TTDBENEFITTOCEDETOCREDITPROVIDER5",
           "RETRENCHMENTBENEFITTOCEDETOCREDITPROVIDER5",
           "RETRENCHMENTBENEFITPERIODFORCREDITPROVIDER5",
           "CREDITPROVIDER6",
           "ACCOUNTTYPEWITHCREDITPROVIDER6",
           "ACCOUNTNUMBERWITHCREDITPROVIDER6",
           "DEATHBENEFITTOCEDETOCREDITPROVIDER6",
           "PTDBENEFITTOCEDETOCREDITPROVIDER6",
           "CRITICALILLNESSBENEFITTOCEDETOCREDITPROVIDER6",
           "TTDBENEFITTOCEDETOCREDITPROVIDER6",
           "RETRENCHMENTBENEFITTOCEDETOCREDITPROVIDER6",
           "RETRENCHMENTBENEFITPERIODFORCREDITPROVIDER6",
           "CREDITPROVIDER7",
           "ACCOUNTTYPEWITHCREDITPROVIDER7",
           "ACCOUNTNUMBERWITHCREDITPROVIDER7",
           "DEATHBENEFITTOCEDETOCREDITPROVIDER7",
           "PTDBENEFITTOCEDETOCREDITPROVIDER7",
           "CRITICALILLNESSBENEFITTOCEDETOCREDITPROVIDER7",
           "TTDBENEFITTOCEDETOCREDITPROVIDER7",
           "RETRENCHMENTBENEFITTOCEDETOCREDITPROVIDER7",
           "RETRENCHMENTBENEFITPERIODFORCREDITPROVIDER7",
           "SURNAMEOFBENEFICIARY1",
           "RELATIONSHIPOFBENEFICIARY1",
           "PERCENTAGEBENEFITOFBENEFICIARY1",
           "FULLNAMESBENEFICIARY2",
           "SURNAMEOFBENEFICIARY2",
           "RELATIONSHIPOFBENEFICIARY2",
           "IDNUMBEROFBENEFICIARY2",
           "PERCENTAGEBENEFITOFBENEFICIARY2",
           "FULLNAMESBENEFICIARY3",
           "SURNAMEOFBENEFICIARY3",
           "RELATIONSHIPOFBENEFICIARY3",
           "IDNUMBEROFBENEFICIARY3",
           "PERCENTAGEBENEFITOFBENEFICIARY3",
           "FULLNAMESBENEFICIARY4",
           "SURNAMEOFBENEFICIARY4",
           "RELATIONSHIPOFBENEFICIARY4",
           "IDNUMBEROFBENEFICIARY4",
           "PERCENTAGEBENEFITOFBENEFICIARY4",
           "FULLNAMESBENEFICIARY5",
           "SURNAMEOFBENEFICIARY5",
           "RELATIONSHIPOFBENEFICIARY5",
           "IDNUMBEROFBENEFICIARY5",
           "PERCENTAGEBENEFITOFBENEFICIARY5",
           "FULLNAMESBENEFICIARY6",
           "SURNAMEOFBENEFICIARY6",
           "RELATIONSHIPOFBENEFICIARY6",
           "IDNUMBEROFBENEFICIARY6",
           "PERCENTAGEBENEFITOFBENEFICIARY6",
           "FULLNAMESBENEFICIARY7",
           "SURNAMEOFBENEFICIARY7",
           "RELATIONSHIPOFBENEFICIARY7",
           "IDNUMBEROFBENEFICIARY7",
           "PERCENTAGEBENEFITOFBENEFICIARY7",
           "VOICELOGGEDENDORSEMENT",
           "TOTALAMOUNTPAIDSINCEINCEPTIONTOCURRENTMONTH",
           "NUMBEROFBENEFICIARIES")



# If weight is outside the interquartile range, bring it to IQR boundry.

# We need the number of beneficiaries and the relationship to the policyholder.
# Cleaning all relationship columns


# finding the names of all the columns with "relationshipofbeneficiary" to count the number of beneficiaries
temp     <-  select(All_lap_Data, contains("RELATIONSHIPOFBENEFICIARY"))
All_lap_Data$NUMBEROFBENEFICIARIES <- 0
temp[temp != ""] <- 1
temp[temp == ""] <- 0
temp <- apply(temp, 2, as.numeric)
All_lap_Data$NUMBEROFBENEFICIARIES <- rowSums(temp)

# Number or credit providers.

# sort out premium debit day column (take away th and rd....)

# if there is end date = lapse
    # if no end date = active             ########## do these changes at the beginning of the code.

# exclude all data four months back.

# do the PPB indicator (1 if PPB, 0 if not)









############ Remove columns

All_lap_Data <- subset(All_lap_Data, select = Names)

                                    
HEADERS <- c()
HEADERS <- c(HEADERS, setdiff(c(), colnames(All_lap_Data)))                    
