####################### Convert excel to CSV if they have been updated ############################

# Start
lap_File_List      <-  list.files(paste(Path, "/Data", sep = ""))
lap_File_List      <-  lap_File_List[lap_File_List != "CSV"]
num_lap_file       <-  length(lap_File_List)     #  Number of files in folder - 2 due to folders

for(lapfile in 1:num_lap_file){
  
  # Check to see if CSV should be created
  file_name <- lap_File_List[lapfile]
  CSV_file_name <- paste0(substr(file_name, 1, nchar(file_name) - 5), ".csv") # remove everything after the . rather than just the last 5 characters
  
  if (file.exists(paste(Path, "/Data/CSV/", CSV_file_name, sep=""))) {
    
    fileCSVDate <- file.mtime(paste(Path, "/Data/CSV/", CSV_file_name, sep = ""))
    fileXLSDate <- file.mtime(paste(Path, "/Data/", file_name, sep = ""))
    
    if (fileXLSDate > fileCSVDate){
      excelToCsv(paste(Path, "/Data/CSV/", file_name, sep = ""))
    }
  } else {
    excelToCsv(paste(Path, "/Data/",   file_name, sep = ""))
  }
  
  CSV_lap_File_List  <-  list.files(paste(Path, "/Data/CSV/", sep = ""))
  
  lap_Data <- fread(paste(Path, "/Data/CSV/", CSV_file_name, sep = ""),
                    colClasses  =  "character",
                    header      =  TRUE,
                    skip        = 0)
  lap_Data <- as.data.frame(lap_Data)
  
  colnames(lap_Data)  <-  gsub(" ","", gsub("[^[:alnum:] ]", "", toupper(colnames(lap_Data))))
  
  lap_Data <- lap_Data[lap_Data$POLICYNUMBER != "", ]
  
  if(lapfile == 1) {
    All_lap_Data <- lap_Data
    
  } else{
    
    common_cols <- intersect(colnames(All_lap_Data), colnames(lap_Data)) # Combine only the common columns (in case of missmatches)
    
    All_lap_Data <- rbind(
      subset(All_lap_Data,  select = common_cols), 
      subset(lap_Data,      select = common_cols)
    )
    
  }
  
  print(lap_File_List[lapfile])
  
} 