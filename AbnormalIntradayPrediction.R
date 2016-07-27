AbnormalIntradayPrediction <-function(HistoryAndPredictHourlyInfo_updated, HistoryAndPredictInfo, PredictInfor){
  # HistoryAndPredictHourlyInfo_updated = data.frame(Time, Items)
  # HistoryAndPredictInfo = data.frame(Dates, Items, DayofWeek, OpenFrom, OpenTo, SD.Type, PD.Type, Outlier)
  # PredictInfor = data.frame(Dates, Items, DayofWeek, OpenFrom, OpenTo, SD.Type, PD.Type, Outlier)
  # Output = updated HistoryAndPredictHourlyInfo
  
  
  ###############################################################################################################
  ###  I: One-step-ahead Proximity day prediction 
  ###     I.1: Find history proximity data, if no record, use paste two weeks information
  ###     I.2: Hour-to-day ratio 
  ###     I.3: Exponianl smoothing is used to predict
  ###     I.4: Moving to next prediction index until finishes
  ### II: One-step-ahead Special day prediction 
  ###############################################################################################################
  
  source(paste(RScriptPath, "/ExponentialCoeff.R", sep=""))
  
  
  
  ###  I: One-step-ahead Proximity day prediction 
  PDInd <-c()
  PDInd <- which(!is.na(PredictInfor$PD.Type))
  
  if (length(PDInd) > 0){
    for (i in 1:length(PDInd)){
      ###     I.1: Find history proximity data, if no record, use paste two weeks information
      HistoryPD <- tryCatch(
        {temp<-HistoryAndPredictInfo$Dates[ which((HistoryAndPredictInfo$PD.Type==PredictInfor$PD.Type[PDInd[i]]) & (HistoryAndPredictInfo$Dates <PredictInfor$Dates[PDInd[i]]) )]
        if (length(temp)==0){stop("No history data")}
        HistoryPD<-temp 
        },
        error = function(err){
          HistoryPD <- HistoryAndPredictInfo$Dates[ tail(which((HistoryAndPredictInfo$DayofWeek==PredictInfor$DayofWeek[PDInd[i]])& 
                                                                 (HistoryAndPredictInfo$Dates <PredictInfor$Dates[PDInd[i]])),n=2)]
          
          return(HistoryPD)
        }
      )
      ###     I.2: Hour-to-day ratio 
      HistoryPD.HourData <- matrix(rep(0, 24*length(HistoryPD)), nrow=length(HistoryPD), ncol = 24)
      for (j in 1:length(HistoryPD)){
        HourlyData.temp <-HistoryAndPredictHourlyInfo_updated$Items[sort(which(format(HistoryAndPredictHourlyInfo_updated$Time, "%Y-%m-%d") ==as.character(HistoryPD[j])))]
        if (sum(HourlyData.temp)==0){next} 
        HistoryPD.HourData[j,] <- HourlyData.temp/sum(HourlyData.temp)
      }
      
      ###     I.3: Exponianl smoothing is used to predict
      Coeff <- (ExponentialCoeff(length(HistoryPD), 0.40) %*% HistoryPD.HourData)/sum(ExponentialCoeff(length(HistoryPD), 0.40) %*% HistoryPD.HourData)
      Coeff[! is.finite(Coeff)] <-0
      HistoryAndPredictHourlyInfo_updated$Items[sort(which(format(HistoryAndPredictHourlyInfo_updated$Time, "%Y-%m-%d")==as.character(PredictInfor$Dates[PDInd[i]])))]<-
        PredictInfor$Items[PDInd[i]]*Coeff
    }
  }  
    
    
    
    ### II: One-step-ahead Special day prediction 
    SDInd <-c()
    SDInd <- which(!is.na(PredictInfor$SD.Type))
    
  if (length(SDInd)>0){
    for (i in 1:length(SDInd)){
      ###    II.1: Find history proximity data, if no record, use paste two weeks information
      HistorySD <- tryCatch(
        {temp<-HistoryAndPredictInfo$Dates[ which((HistoryAndPredictInfo$SD.Type==PredictInfor$SD.Type[SDInd[i]]) & (HistoryAndPredictInfo$Dates <PredictInfor$Dates[SDInd[i]]) )]
        if (length(temp)==0){stop("No history data")}
        HistorySD<-temp 
        },
        error = function(err){
          HistorySD <- HistoryAndPredictInfo$Dates[ tail(which((HistoryAndPredictInfo$DayofWeek==PredictInfor$DayofWeek[SDInd[i]])& 
                                                                 (HistoryAndPredictInfo$Dates <PredictInfor$Dates[SDInd[i]])),n=2)]
          
          return(HistorySD)
        }
      )
      ###     II.2: Hour-to-day ratio 
      HistorySD.HourData <- matrix(rep(0, 24*length(HistorySD)), nrow=length(HistorySD), ncol = 24)
      for (j in 1:length(HistorySD)){
        HourlyData.temp <-HistoryAndPredictHourlyInfo_updated$Items[sort(which(format(HistoryAndPredictHourlyInfo_updated$Time, "%Y-%m-%d") ==as.character(HistorySD[j])))]
        if (sum(HourlyData.temp)==0){next} 
        HistorySD.HourData[j,] <- HourlyData.temp/sum(HourlyData.temp)
      }
      ###     II.3: Exponianl smoothing is used to predict
      Coeff <- (ExponentialCoeff(length(HistorySD), 0.40) %*% HistorySD.HourData)/sum(ExponentialCoeff(length(HistorySD), 0.40) %*% HistorySD.HourData)
      Coeff[! is.finite(Coeff)] <-0
      HistoryAndPredictHourlyInfo_updated$Items[sort(which(format(HistoryAndPredictHourlyInfo_updated$Time, "%Y-%m-%d")==as.character(PredictInfor$Dates[SDInd[i]])))]<-
        PredictInfor$Items[SDInd[i]]* Coeff
    }
  }
  
  return(HistoryAndPredictHourlyInfo_updated)
}