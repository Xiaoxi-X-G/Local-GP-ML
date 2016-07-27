RegularCloseDayofWeek <-function(LocationID, RScriptPath){
  ###1. Pull data from DataBase with the Right LocationID
  ###2. Find Regular Closing date and exclude from input
  ### Save as CloseDayofWeek
  
  source(paste(RScriptPath, "/TranslateDayofWeek.R", sep=""))
  library(RODBC)


  
  ###I.1. Pull data from DataBase with the Right LocationID
  odbcDataSources()
  conn<-odbcConnect(dsn="localdb") 
  if (DatabaseName == "RetailDemo2"){
    SQL.OpeningHours <- "SELECT [RetailDemo2].[dbo].[OpeningHours].[OpenFrom]
    ,[RetailDemo2].[dbo].[OpeningHours].[OpenTo]
    ,[RetailDemo2].[dbo].[OpeningHours].[EffectiveFrom]
    ,[RetailDemo2].[dbo].[OpeningHours].[EffectiveTo]
    ,[RetailDemo2].[dbo].[OpeningHours].[DayOfWeek]
    FROM [RetailDemo2].[dbo].[OpeningHours]
    WHERE  [RetailDemo2].[dbo].[OpeningHours].[HoursType] =2
    AND [RetailDemo2].[dbo].[OpeningHours].[LocationID] = "
    OpenDayResults<-sqlQuery(conn, paste(SQL.OpeningHours, LocationID, ";", sep=""))
    odbcClose(conn)
  }else if (DatabaseName == "Time2Work_MultiRetailAU"){
    SQL.OpeningHours <- "SELECT [Time2Work_MultiRetailAU].[dbo].[OpeningHours].[OpenFrom]
    ,[Time2Work_MultiRetailAU].[dbo].[OpeningHours].[OpenTo]
    ,[Time2Work_MultiRetailAU].[dbo].[OpeningHours].[EffectiveFrom]
    ,[Time2Work_MultiRetailAU].[dbo].[OpeningHours].[EffectiveTo]
    ,[Time2Work_MultiRetailAU].[dbo].[OpeningHours].[DayOfWeek]
    FROM [Time2Work_MultiRetailAU].[dbo].[OpeningHours]
    WHERE  [Time2Work_MultiRetailAU].[dbo].[OpeningHours].[HoursType] =1
    AND [Time2Work_MultiRetailAU].[dbo].[OpeningHours].[LocationID] = "
    OpenDayResults<-sqlQuery(conn, paste(SQL.OpeningHours, LocationID, ";", sep=""))
    odbcClose(conn)
  }else{
    stop("No Such DataBase")
  }
  
  
  ###I.2. Find Regular Closing date and exclude from input
  if (nrow(OpenDayResults)==0){
    Closedayofweek <-c()
  }else{
    
    RegularDates <- c()
    RegularHourInd <-which(is.na(OpenDayResults$EffectiveTo))
    if (length(RegularHourInd)>0){
      for (i in 1:length(RegularHourInd)){
        RegularDates<-c(RegularDates, TranslateDayofWeek(OpenDayResults$DayOfWeek[RegularHourInd[i]]))
      }
    }
    Closedayofweek <- setdiff(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), unique(RegularDates))
  }
   
  
  write.csv(Closedayofweek, file = paste(RScriptPath, "/RegularCloseDayofWeek.csv", sep=""), row.names = F)
  
  return ()
}