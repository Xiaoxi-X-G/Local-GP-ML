Holidays2LocationID <-function(LocationID, RScriptPath, DatabaseName){
  # Pull data from DataBase with the Right LocationID
  # ExceptionalDatesRight.csv is save in RScriptPath
  
  
  
  library(RODBC)
  odbcDataSources()
  conn<-odbcConnect(dsn="localdb") 
  
  
  if (DatabaseName == "RetailDemo2"){
    SQL.TableJoin <- "SELECT [RetailDemo2].[dbo].[PublicHoliday].[HolidayDate] AS ExceptionalDate
    ,[RetailDemo2].[dbo].[PublicHoliday].[Annual]
    ,[RetailDemo2].[dbo].[PublicHoliday].[ForecastIgnoreHistory]
    ,[RetailDemo2].[dbo].[PublicHoliday].[ForecastDateSpecific]
    ,[RetailDemo2].[dbo].[PublicHoliday].[ExceptionalDayTypeID]
    FROM [RetailDemo2].[dbo].[PublicHoliday]
    LEFT OUTER JOIN [RetailDemo2].[dbo].[PublicHoliday2PublicHolidayGroup]
    ON [RetailDemo2].[dbo].[PublicHoliday].[PublicHolidayID] = [RetailDemo2].[dbo].[PublicHoliday2PublicHolidayGroup].[PublicHolidayID]
    LEFT OUTER JOIN [RetailDemo2].[dbo].[Location]
    ON [RetailDemo2].[dbo].[Location].[PublicHolidayGroupID] = [RetailDemo2].[dbo].[PublicHoliday2PublicHolidayGroup].[PublicHolidayGroupID]
    WHERE [RetailDemo2].[dbo].[Location].[LocationID] = "
    ExceptionalDates.DB <- sqlQuery(conn,  paste(SQL.TableJoin, LocationID, ";", sep=""))
    odbcClose(conn)
  }else if (DatabaseName == "Time2Work_MultiRetailAU"){
    SQL.TableJoin <- "SELECT [Time2Work_MultiRetailAU].[dbo].[PublicHoliday].[HolidayDate] AS ExceptionalDate
    ,[Time2Work_MultiRetailAU].[dbo].[PublicHoliday].[Annual]
    ,[Time2Work_MultiRetailAU].[dbo].[PublicHoliday].[ForecastIgnoreHistory]
    ,[Time2Work_MultiRetailAU].[dbo].[PublicHoliday].[ForecastDateSpecific]
    ,[Time2Work_MultiRetailAU].[dbo].[PublicHoliday].[ExceptionalDayTypeID]
    FROM [Time2Work_MultiRetailAU].[dbo].[PublicHoliday]
    LEFT OUTER JOIN [Time2Work_MultiRetailAU].[dbo].[PublicHoliday2PublicHolidayGroup]
    ON [Time2Work_MultiRetailAU].[dbo].[PublicHoliday].[PublicHolidayID] = [Time2Work_MultiRetailAU].[dbo].[PublicHoliday2PublicHolidayGroup].[PublicHolidayID]
    LEFT OUTER JOIN [Time2Work_MultiRetailAU].[dbo].[Location]
    ON [Time2Work_MultiRetailAU].[dbo].[Location].[PublicHolidayGroupID] = [Time2Work_MultiRetailAU].[dbo].[PublicHoliday2PublicHolidayGroup].[PublicHolidayGroupID]
    WHERE [Time2Work_MultiRetailAU].[dbo].[Location].[LocationID] = "
    ExceptionalDates.DB <- sqlQuery(conn,  paste(SQL.TableJoin, LocationID, ";", sep=""))
    odbcClose(conn)
  }else{
    stop("No Such DataBase")
  }
  
  ExceptionalDates.DB$ExceptionalDayTypeID<-as.character(ExceptionalDates.DB$ExceptionalDayTypeID)
  
  write.csv(ExceptionalDates.DB, file = paste(RScriptPath, "/ExceptionalDatesRight.csv", sep=""), row.names = F)
  
  
  return()
}