#########################

Month_End <- as.Date(eom(fileXLSDate))
Month_1   <- min(All_lap_Data$COMMENCEMENTDATEOFPOLICY)

Tot_Months <- as.numeric(ceiling((Month_End - Month_1) / 30.4375)) + 1

months <- seq(Month_1, by = "month", length.out = Tot_Months)

for (i in 1:Tot_Months) {
  
  if (i < Tot_Months) {
    
    # Filter the individuals that were active at the start of the particular month in the iteration
    # Minus 1 from month i + 1, because the commencement date could be 2010/01/26, so filter everyone that starts in 01 Feb 2010 - 1 day 
    # (thus end of Jan) 
    TempDF <- All_lap_Data %>% 
      filter(COMMENCEMENTDATEOFPOLICY <= months[i + 1] - 1, is.na(STATUSEFFECTIVEENDDATE) | STATUSEFFECTIVEENDDATE >= months[i])
    
    # Calculate the duration for all ACTIVE, which was filtered as active above, regardless whether they lapsed in that month or not
    # this will be corrected below (if the policyholder lapsed)
    TempDF$DUR <- round(as.numeric(months[1 + i] - TempDF$COMMENCEMENTDATEOFPOLICY) / 30.4375)
    
    # Initialise the lapse indicator to 0
    TempDF$LAP <- 0
    
    # Set the lapse indicator to 1 if they lapsed in the next month
    # Greater than or equal to month[i] -> because they can lapse during current month  e.g. 2015/01/01
    #                 Equal - because it can be an NTU (policy not taken up - said yes, but lapsed in month of purchase)
    # Less than month[i + 1] - 1 -> because they can lapse at the end of the month      e.g. 2015/01/31 
    # These are the same thing in.
    TempDF$LAP[TempDF$STATUSEFFECTIVEENDDATE >= months[i] & TempDF$STATUSEFFECTIVEENDDATE < months[i + 1] - 1] <- 1
    
    # Remove 1 from duration if the person lapsed in a particular month
    TempDF$DUR[TempDF$LAP == 1] <- TempDF$DUR[TempDF$LAP == 1] - 1
    
    # Get Current and last premium
    maxDur <- max(ceiling(TempDF$DUR / 12))
    
    # Initialize
    TempDF$TOTPAID <- 0
    
    for (j in 1:maxDur) {
      
      TempDF$TOTPAID <- TempDF$TOTPAID + 
        pmax(pmin(TempDF$DUR - 12 * (j - 1), 12), 0) * as.numeric(TempDF[, paste0("YEAR", j, "QUOTEDTOTALPREMIUM")])
      
      TempDF2 <- TempDF %>% filter(ceiling(DUR / 12) == j) 
      
      TempDF2$CURRENTPREMIUM <- TempDF2[, paste0("YEAR", j, "QUOTEDTOTALPREMIUM")]
      TempDF2$LASTPREMIUM    <- TempDF2[, paste0("YEAR", max(j - 1, 1), "QUOTEDTOTALPREMIUM")]
      
      TempDF2$TOTPAID <- TempDF2$TOTPAID + TempDF2$DUR * as.numeric(TempDF2$CURRENTPREMIUM)
      
      TempDF2$LASTPREMIUM <- ifelse(j == 1, NA, TempDF2$LASTPREMIUM)
      
      if (j > 1) {
        FinDF2 <- rbind(FinDF2, TempDF2)
      } else {
        FinDF2 <- TempDF2
      }
      
    } 
    
    TempDF <- FinDF2
    
    # Bind Data
    if (i > 1) {
      FinDF <- rbind(FinDF, TempDF)
    } else {
      FinDF <- TempDF
    }
    
    print(paste(i, "of", Tot_Months - 1))
    
  }
  
}

All_lap_Data <- FinDF

rm(Month_End, Month_1, Tot_Months, months, i, j, FinDF, TempDF, FinDF2, TempDF2, maxDur)

# choose the columns you want to keep
source(paste(Path, "/R_Code/Colnames.R", sep = ""))

# Remove the unwanted data
All_lap_Data <- subset(All_lap_Data, select = Names)

rm(Names)











