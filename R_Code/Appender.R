#########################

Month_End <- as.Date(eom(fileXLSDate))
Month_1   <- as.Date(firstDayMonth(min(All_lap_Data$COMMENCEMENTDATEOFPOLICY)))

Tot_Months <- as.numeric(ceiling((Month_End - Month_1)/30.4375)) + 1

months <- seq(Month_1, by = "month", length.out = Tot_Months)

for (i in 1:Tot_Months) {
  
  TempDF <- All_lap_Data %>% 
              filter(COMMENCEMENTDATEOFPOLICY >= months[i], STATUSEFFECTIVEENDDATE <= months[1 + i])
  
  TempDF$DUR <- 1
  
  TempDF$LAP <- 1
  
  if (i > 1) {
    
  } else {
    FinDF <- TempDF
  }
  
}

All_lap_Data <- FinDF

rm(fileXLSDate, Month_End, Month_1, Tot_Months, months, i, FinDF, TempDF)








