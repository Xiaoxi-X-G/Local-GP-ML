OpenCloseDayTime<-function(FirstDate, FinishDate.T, LocationID, RScriptPath, DatabaseName){
  # Pull data from DataBase with the Right LocationID
  # CloseDatesRight.csv is save in RScriptPath
  # Output = OpenDayTime, dataframe(Dates, OpenFrom, OpenTo)
  
  
  #########################################################################################
  ###1. Pull data from DataBase with the Right LocationID
  ###2. Find regular working hours
  ###3. Find irregular working hours
  ###4. Find Closing Days and save as CloseDatesRight.csv
  #########################################################################################
  
  # source(paste(RScriptPath, "/TranslateDayofWeek.R", sep=""))
  # library(RODBC)
  
  ###1. Pull data from DataBase with the Right LocationID
  odbcDataSources()
  conn<-odbcConnect(dsn="localdb") 
  if (DatabaseName == "RetailDemo2"){
    SQL.OpeningHours <- "SELECT [RetailDemo2].[dbo].[OpeningHours].[OpenFrom]
    ,[RetailDemo2].[dbo].[OpeningHours].[OpenTo]
    ,[RetailDemo2].[dbo].[OpeningHours].[EffectiveFrom]
    ,[RetailDemo2].[dbo].[OpeningHours].[EffectiveTo]
    ,[RetailDemo2].[dbo].[OpeningHours].[DayOfWeek]
    FROM [RetailDemo2].[dbo].[OpeningHours]
    WHERE  [RetailDemo2].[dbo].[OpeningHours].[HoursType] =1
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
  
  if (nrow(OpenDayResults)==0){
    CloseDays <- NA
    AllDates <- seq(as.Date(FirstDate), as.Date(FinishDate.T), by ="1 day")
    OpenDayTime <-data.frame(Dates = AllDates, 
                             OpenFrom = rep(NA, length=length(AllDates)),
                             OpenTo = rep(NA, length=length(AllDates)), stringsAsFactors=FALSE)
  }else{
    
    OpenDayResults$OpenFrom <- format(OpenDayResults$OpenFrom, "%H:%M:%S")
    OpenDayResults$OpenTo <- format(OpenDayResults$OpenTo, "%H:%M:%S")
    OpenDayResults$EffectiveFrom <- as.Date(format(OpenDayResults$EffectiveFrom, "%Y-%m-%d"))
    OpenDayResults$EffectiveTo <- as.Date(format(OpenDayResults$EffectiveTo, "%Y-%m-%d"))
    
    
    
    
    
    
    
    ###2. Find regular working hours
    AllDates <- seq(as.Date(FirstDate), as.Date(FinishDate.T), by ="1 day")
    OpenDayTime <-data.frame(Dates = AllDates, 
                             OpenFrom = rep("00:00:00", length=length(AllDates)),
                             OpenTo = rep("00:00:00", length=length(AllDates)), stringsAsFactors=FALSE)
    
    
    RegularHourInd <-which(is.na(OpenDayResults$EffectiveTo))
    for (i in 1:length(RegularHourInd)){
      RegularDates<-TranslateDayofWeek(OpenDayResults$DayOfWeek[RegularHourInd[i]])
      for (j in 1:length(RegularDates)){
        OpenDayTime$OpenFrom[which(weekdays(OpenDayTime$Dates)==RegularDates[j])] <-OpenDayResults$OpenFrom[RegularHourInd[i]]
        OpenDayTime$OpenTo[which(weekdays(OpenDayTime$Dates)==RegularDates[j])] <-OpenDayResults$OpenTo[RegularHourInd[i]]
      }
    }
    
    
    ###3. Find irregular working hours
    IregularHourInd <-which(! is.na(OpenDayResults$EffectiveTo))
    for (i in 1:length(IregularHourInd)){
      IregularDatesInd<-TranslateDayofWeek(OpenDayResults$DayOfWeek[IregularHourInd[i]])
      IregularDates <- seq(OpenDayResults$EffectiveFrom[IregularHourInd[i]], OpenDayResults$EffectiveTo[IregularHourInd[i]], by="1 day")
      for (j in 1:length(IregularDates)){
        if(weekdays(IregularDates[j]) %in% IregularDatesInd){
          OpenDayTime$OpenFrom[which(OpenDayTime$Dates==IregularDates[j])] <- OpenDayResults$OpenFrom[IregularHourInd[i]]
          OpenDayTime$OpenTo[which(OpenDayTime$Dates==IregularDates[j])] <- OpenDayResults$OpenTo[IregularHourInd[i]]
        }
      }
    }
    
    
    ###4. Find Closing Days and save as CloseDatesRight.csv
    CloseDays <- OpenDayTime$Dates[which((OpenDayTime$OpenFrom=="00:00:00") &(OpenDayTime$OpenTo=="00:00:00") )]
    
  }
  
  write.csv(CloseDays, file = paste(RScriptPath, "/CloseDatesRight.csv", sep=""), row.names = F)
  return(OpenDayTime)
}