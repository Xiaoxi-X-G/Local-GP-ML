DataHoursV2_ML <-function(AllData, col, FirstDate, FinishDateT){
  ## AllData is a dataframe =(FinishTime=Epoch, Items = items/Tran, )
  ## But at least AllData needs to include 2 terms, with first being FinishTime (Finish time of each transaction)
  ## col =  indicates what term to analyse
  ## FirstDate = character
  
  
  ## Change the Azure ML epoch format to DayTime
  HourlySale <- aggregate(VendData.stor.temp0$Transactions,
                          list(hour=cut(as.POSIXct(VendData.stor.temp0$FinishTime, origin = "1970-01-01", tz="GMT"),"hour")),sum)
  HourlySale.arry <- array(HourlySale$x, dim = nrow(HourlySale), dimnames = list(as.character(HourlySale$hour)))  
  
  hrs <- c("00:00:00", "01:00:00", "02:00:00", "03:00:00", "04:00:00", "05:00:00", "06:00:00", 
           "07:00:00", "08:00:00", "09:00:00", "10:00:00", "11:00:00", "12:00:00", "13:00:00", 
           "14:00:00", "15:00:00", "16:00:00", "17:00:00", "18:00:00", "19:00:00", "20:00:00", 
           "21:00:00", "22:00:00", "23:00:00")
  
  
  A1 <- seq(as.Date(FirstDate), as.Date(FinishDateT), by="1 day") # all days
  
  allhoursName <- paste(rep(A1, each=24), rep(hrs, length(A1)))
  allhours <- array(data=rep(0, length(allhoursName)), dim=length(allhoursName), dimnames=list(allhoursName)) 
  
  for (i in 1:length(allhours)){
    if (names(allhours[i]) %in% names(HourlySale.arry)){
      allhours[i] <- HourlySale.arry[which(names(HourlySale.arry)==names(allhours[i]))]
    }
  }
  
  AllData.hours <- data.frame(Time = as.POSIXlt(names(allhours), "%Y-%m-%d %H:%M:%S", tz="GMT"), Items = unname(allhours))
  
  return(AllData.hours)
  
}