NormalIntradayPrediction_ML <- function(HistoryAndPredictHourlyInfo, HistoryAndPredictInfo, PredictInfor){
  # HistoryAndPredictHourlyInfo = data.frame(Time, Items)
  # HistoryAndPredictInfo = data.frame(Dates, Items, DayofWeek, OpenFrom, OpenTo, SD.Type, PD.Type, Outlier)
  # PredictInfor = data.frame(Dates, Items, DayofWeek, OpenFrom, OpenTo, SD.Type, PD.Type, Outlier)
  # Output = updated HistoryAndPredictHourlyInfo
  
  
  ###############################################################################################################
  ###  I: Find one-step-ahead prediction index, mapped to prediction date
  ### II: Find last 4 weeks data, which matches date, opening and closing hours (release constraint if no record)
  ###III: Hour-to-day ratio 
  ### IV: Exponianl smoothing is used to predict
  ###  V: Moving to next prediction index until finishes
  ###############################################################################################################
  
  #source(paste(RScriptPath, "/ExponentialCoeff.R", sep=""))

  
  
  ### I: Find one-step-ahead prediction index, which always use the updated last 4 weeks data from the last prediction
  NormalPredictionDayInd <- which(is.na(PredictInfor$SD.Type) & is.na(PredictInfor$PD.Type) & (!PredictInfor$Outlier))
  NormalHistoryAndPredictInfo <-
    HistoryAndPredictInfo[which(is.na(HistoryAndPredictInfo$SD.Type) & is.na(HistoryAndPredictInfo$PD.Type) & (!HistoryAndPredictInfo$Outlier)), ]
  
  if (length(NormalPredictionDayInd) > 0){
    for (i in 1:length(NormalPredictionDayInd)){
      ### II: Find last 4 weeks data, which matches date, opening and closing hours (release constraint if no record)
      UpdatedSameNormalDayInd<-tryCatch(
        {temp<-tail(which((NormalHistoryAndPredictInfo$DayofWeek[1: (which(NormalHistoryAndPredictInfo$Dates== PredictInfor$Dates[NormalPredictionDayInd[i]])-1)] == PredictInfor$DayofWeek[NormalPredictionDayInd[i]]) 
                          & (NormalHistoryAndPredictInfo$OpenFrom[1: (which(NormalHistoryAndPredictInfo$Dates== PredictInfor$Dates[NormalPredictionDayInd[i]])-1)] == PredictInfor$OpenFrom[NormalPredictionDayInd[i]])
                          & (NormalHistoryAndPredictInfo$OpenTo[1: (which(NormalHistoryAndPredictInfo$Dates== PredictInfor$Dates[NormalPredictionDayInd[i]])-1)] == PredictInfor$OpenTo[NormalPredictionDayInd[i]])),
                    n = 4)
        if (length(temp)==0){stop("No 100% matched history")}
        UpdatedSameNormalDayInd <- temp
        },
        warning = function(w){
          UpdatedSameNormalDayInd<-tail(which((NormalHistoryAndPredictInfo$DayofWeek[1: (which(NormalHistoryAndPredictInfo$Dates== PredictInfor$Dates[NormalPredictionDayInd[i]])-1)] == PredictInfor$DayofWeek[NormalPredictionDayInd[i]])),
                                        n = 4)
          return(UpdatedSameNormalDayInd)
        },
        error = function(err){ # release constraint if no history
          UpdatedSameNormalDayInd<-tail(which((NormalHistoryAndPredictInfo$DayofWeek[1: (which(NormalHistoryAndPredictInfo$Dates== PredictInfor$Dates[NormalPredictionDayInd[i]])-1)] == PredictInfor$DayofWeek[NormalPredictionDayInd[i]])),
                                        n = 4)
          return(UpdatedSameNormalDayInd)
        }
      )
      
      ### III: Hour-to-day ratio of last 4 weeks
      Hour2DayRatioHistory <-matrix(rep(0, 24*length(UpdatedSameNormalDayInd)), nrow=length(UpdatedSameNormalDayInd), ncol=24)
      #print(i)
      for (j in 1:length(UpdatedSameNormalDayInd)){
        UpdatedSameNormalHourInd.temp <- HistoryAndPredictHourlyInfo$Items[sort(which(format(HistoryAndPredictHourlyInfo$Time, "%Y-%m-%d") == as.character(NormalHistoryAndPredictInfo$Dates[UpdatedSameNormalDayInd[j]])))]
        if (sum(UpdatedSameNormalHourInd.temp)==0){next}
        Hour2DayRatioHistory[j,] <- UpdatedSameNormalHourInd.temp/ sum(UpdatedSameNormalHourInd.temp)
      }
      
      ### IV: Exponianl smoothing is used to predict + update 
      Coeff <- (ExponentialCoeff(length(UpdatedSameNormalDayInd), 0.40)%*%Hour2DayRatioHistory)/sum(ExponentialCoeff(length(UpdatedSameNormalDayInd), 0.40)%*%Hour2DayRatioHistory)
      Coeff[!is.finite(Coeff)] <-0
      HistoryAndPredictHourlyInfo$Items[sort(which(format(HistoryAndPredictHourlyInfo$Time, "%Y-%m-%d") == as.character(PredictInfor$Dates[NormalPredictionDayInd[i]])))]<- 
        PredictInfor$Items[PredictInfor$Dates==PredictInfor$Dates[NormalPredictionDayInd[i]]]*Coeff
    }
  }
    
    
  return(HistoryAndPredictHourlyInfo)
  
}