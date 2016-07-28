ExceptionalDayandEffectFormatV2_ML<-function(ExceptionalDatesCSV, FirstDate, FinishDateT){
  ## ExceptionalDatesCSV = ["ExceptionalDate","Annual","ForecastIgnoreHistory","ForecastDateSpecific","ExceptionalDayTypeID"]
  ## FirstDate, FinishDate.T = Character
  ## Output = list(ExceptionalDays, ProximityDays)
  
  
  #########################################################################
  #####  I: Complete Exceptional Days, from FirstDate to FinishDate.T
  ##### II: Find proximity days
  #########################################################################

  
  #####  I: Complete Exceptional Days, from FirstDate to FinishDate.T
  
  #ExceptionalDayandEffects<-ExceptionalDayandEffectFormatV2(ExceptionalDates, FirstDate, FinishDateT)
  #################
  FirstYear.temp<- strsplit(FirstDate, split = "-")
  FirstYear <- FirstYear.temp[[1]]
  LastYear.temp<- strsplit(FinishDateT, split = "-")
  LastYear <- LastYear.temp[[1]]
  
  AllYears<-seq(from = as.integer(FirstYear[1]), to = as.integer(LastYear[1]), by = 1)
  
  ExceptionalDays$ExceptionalDate<- as.Date(ExceptionalDays$ExceptionalDate)
  ExceptionalDays$Annual <- as.logical((ExceptionalDays$Annual))
  ExceptionalDays$Annual[is.na(ExceptionalDays$Annual)]<-FALSE
  print(ExceptionalDays)  
  
  
  ###### Deal with duplicated FALSE and TRUE at Annual: delete FALSE row if same day Annual is TRUE
  FalseAnnulInd <- which(! ExceptionalDays$Annual)
  TrueAnnulInd <- which(ExceptionalDays$Annual)
  print(FalseAnnulInd)
  print(TrueAnnulInd)
  
  if (length(FalseAnnulInd)>0){
    DeleteInd <-c()
    for (i in 1 : length(FalseAnnulInd)){
      print(format(ExceptionalDays$ExceptionalDate[FalseAnnulInd[i]], "%m-%d"))
      if(format(ExceptionalDays$ExceptionalDate[FalseAnnulInd[i]], "%m-%d") %in% format(ExceptionalDays$ExceptionalDate[TrueAnnulInd], "%m-%d")){
        DeleteInd<-c(DeleteInd, FalseAnnulInd[i])
      }
    }
  
    #print(DeleteInd)
    if (length(DeleteInd)>0){
      ExceptionalDays <- ExceptionalDays[0-DeleteInd,]
    }
  }
  
  ## Define Unique Index for ExceptionalDayTypeID
  Annual.Dates<-unique(format(ExceptionalDays$ExceptionalDate[which(ExceptionalDays$Annual)], "%m-%d"))
  UniqueInd<-setdiff(sample(1:(nrow(ExceptionalDays)+1), nrow(ExceptionalDays), replace=F), 
                     unique(ExceptionalDays$ExceptionalDayTypeID[!is.na(ExceptionalDays$ExceptionalDayTypeID)]))
  
  if (length(Annual.Dates) > 0){
    for (i in 1:length(Annual.Dates)){
      ExceptionalDays$ExceptionalDayTypeID[which(format(ExceptionalDays$ExceptionalDate, "%m-%d") 
                                                 == Annual.Dates[i])] <- UniqueInd[i]
    }
    
    ExceptionalDays$ExceptionalDayTypeID[is.na(ExceptionalDays$ExceptionalDayTypeID)]<-
      format(round(runif(length(which(is.na(ExceptionalDays$ExceptionalDayTypeID))), min=0, max=9), 3),nsmall = 4) #fill-in a random number if NA
  }
  ExceptionalDays2 <- ExceptionalDays[,c(1,2,5)]
  ExceptionalDays2$ExceptionalDate<- as.Date(ExceptionalDays2$ExceptionalDate)
  
  
  
  ## Add missing information
  TrueAnnulInd <- c()
  TrueAnnulInd <- which(ExceptionalDays$Annual)
  if (length(TrueAnnulInd)>0){
    for (i in 1:length(TrueAnnulInd)){
      ExceptionalDate <-as.Date(paste(as.character(AllYears), "-", as.character(format(ExceptionalDays$ExceptionalDate[TrueAnnulInd[i]], "%m-%d")), sep=""))
      ExceptionalDayTypeID <- rep(ExceptionalDays$ExceptionalDayTypeID[TrueAnnulInd[i]], length=length(ExceptionalDate))
      Annual <- rep("TRUE", length(ExceptionalDate))
      
      ExceptionalDays2 <- rbind(ExceptionalDays2, data.frame(ExceptionalDate, Annual, ExceptionalDayTypeID))
      # }
    }
    ExceptionalDays2 <- ExceptionalDays2[(! duplicated(ExceptionalDays2$ExceptionalDate)) ,]
    ExceptionalDays2 <- ExceptionalDays2[order(ExceptionalDays2$ExceptionalDate), ]
    ExceptionalDays2$Annual <- as.logical(ExceptionalDays2$Annual)
  }
  
  
  
  ##### II: Find proximity days
  ProximityDays <- data.frame(Dates = rep(as.Date("2000-01-01"),length= 2*nrow(ExceptionalDays2)),
                              Annual = rep(FALSE, length = 2*nrow(ExceptionalDays2)),
                              ProximityDaysTypeID = rep("???", length = 2*nrow(ExceptionalDays2)))
  ProximityDays$ProximityDaysTypeID <- as.character(ProximityDays$ProximityDaysTypeID)
  
  if (nrow(ExceptionalDays) > 0){
    for (i in 1:nrow(ExceptionalDays2)){
      if (!(as.character(ExceptionalDays2$ExceptionalDate[i]-1) %in% as.character(ExceptionalDays2$ExceptionalDate))){
        ProximityDays$Dates[i] <- ExceptionalDays2$ExceptionalDate[i]-1
        ProximityDays$Annual[i] <- ExceptionalDays2$Annual[i]
        ProximityDays$ProximityDaysTypeID[i] <- paste(as.character(ExceptionalDays2$ExceptionalDayTypeID[i]), "-", sep="")
      }
    }
    
    for (i in 1:nrow(ExceptionalDays2)){
      if ((!(as.character(ExceptionalDays2$ExceptionalDate[i]+1) %in% as.character(ExceptionalDays2$ExceptionalDate)))
          &&(!(as.character(ExceptionalDays2$ExceptionalDate[i]+1) %in% as.character(ProximityDays$Dates) )) ){
        ProximityDays$Dates[nrow(ProximityDays)-i+1] <- ExceptionalDays2$ExceptionalDate[i]+1
        ProximityDays$Annual[nrow(ProximityDays)-i+1] <- ExceptionalDays2$Annual[i]
        ProximityDays$ProximityDaysTypeID[nrow(ProximityDays)-i+1] <- paste(as.character(ExceptionalDays2$ExceptionalDayTypeID[i]), "+", sep="")
      }
    }
    
    
    if (length(which(ProximityDays$ProximityDaysTypeID == "???")) != 0){
      ProximityDays<-ProximityDays[0-which(ProximityDays$ProximityDaysTypeID == "???"), ]
    }
    ProximityDays<- ProximityDays[order(ProximityDays$Dates), ]
}
  
  return(list(ExceptionalDays2, ProximityDays))
}