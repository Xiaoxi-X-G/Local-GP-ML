ExceptionalDayandEffectFormatV2<-function(ExceptionalDatesCSV, FirstDate, FinishDate.T){
  ## ExceptionalDatesCSV = ["ExceptionalDate","Annual","ForecastIgnoreHistory","ForecastDateSpecific","ExceptionalDayTypeID"]
  ## FirstDate, FinishDate.T = Character
  ## Output = list(ExceptionalDays, ProximityDays)
  
  
  #########################################################################
  #####  I: Complete Exceptional Days, from FirstDate to FinishDate.T
  ##### II: Find proximity days
  #########################################################################
  library(lubridate)
  # con <- textConnection("ExceptionalDatesCSV.csv")
  # ExceptionalDays.temp <-read.csv(con)
  # close(con)
  
  
  #####  I: Complete Exceptional Days, from FirstDate to FinishDate.T
  ExceptionalDays.temp<-read.csv(paste(RScriptPath, ExceptionalDatesCSV, sep=""), header = TRUE)
  
  FirstYear<-year(as.Date(FirstDate))
  LastYear <-year(as.Date(FinishDate.T))
  AllYears<-year(seq(as.Date(FirstDate), length=abs(LastYear - FirstYear)+1, by="years"))
  
  ExceptionalDays.temp$ExceptionalDate<- as.Date(ExceptionalDays.temp$ExceptionalDate)
  ExceptionalDays.temp$Annual <- as.logical((ExceptionalDays.temp$Annual))
  #ExceptionalDays.temp$Annual <- as.logical((as.character(ExceptionalDays.temp$Annual)))
  ExceptionalDays.temp$Annual[is.na(ExceptionalDays.temp$Annual)]<-FALSE
  
  
  ###### Deal with duplicated FALSE and TRUE at Annual: delete FALSE row if same day Annual is TRUE
  FalseAnnulInd <- which(! ExceptionalDays.temp$Annual)
  TrueAnnulInd <- which(ExceptionalDays.temp$Annual)
  if (length(FalseAnnulInd)>0){
    DeleteInd <-c()
    for (i in 1 : length(FalseAnnulInd)){
      if(format(ExceptionalDays.temp$ExceptionalDate[FalseAnnulInd[i]], "%m-%d")
            %in% format(ExceptionalDays.temp$ExceptionalDate[TrueAnnulInd], "%m-%d")){
        DeleteInd<-c(DeleteInd, FalseAnnulInd[i])
      }
    }
  
  
    if (length(DeleteInd)>0){
      ExceptionalDays.temp <- ExceptionalDays.temp[0-DeleteInd,]
    }
    
  }
  
  ## Define Unique Index for ExceptionalDayTypeID
  Annual.Dates<-unique(format(ExceptionalDays.temp$ExceptionalDate[which(ExceptionalDays.temp$Annual)], "%m-%d"))
  UniqueInd<-setdiff(sample(1:(nrow(ExceptionalDays.temp)+1), nrow(ExceptionalDays.temp), replace=F), 
                     unique(ExceptionalDays.temp$ExceptionalDayTypeID[!is.na(ExceptionalDays.temp$ExceptionalDayTypeID)]))
  if (length(Annual.Dates) > 0 ){
    for (i in 1:length(Annual.Dates)){
      ExceptionalDays.temp$ExceptionalDayTypeID[which(format(ExceptionalDays.temp$ExceptionalDate, "%m-%d") 
                                                      == Annual.Dates[i])] <- UniqueInd[i]
    }
    
    ExceptionalDays.temp$ExceptionalDayTypeID[is.na(ExceptionalDays.temp$ExceptionalDayTypeID)]<-
      format(round(runif(length(which(is.na(ExceptionalDays.temp$ExceptionalDayTypeID))), min=0, max=9), 3),nsmall = 4) #fill-in a random number if NA
    
  }
  ExceptionalDays <- ExceptionalDays.temp[,c(1,2,5)]
  ExceptionalDays$ExceptionalDate<- as.Date(ExceptionalDays$ExceptionalDate)
  
  
  
  
  ## Add missing information
  TrueAnnulInd <- c()
  TrueAnnulInd <- which(ExceptionalDays.temp$Annual)
  if (length(TrueAnnulInd)>0){
    for (i in 1:length(TrueAnnulInd)){
     # if (ExceptionalDays$Annual[i]){
      ExceptionalDate <-as.Date(paste(as.character(AllYears), "-", as.character(format(ExceptionalDays.temp$ExceptionalDate[TrueAnnulInd[i]], "%m-%d")), sep=""))
      ExceptionalDayTypeID <- rep(ExceptionalDays.temp$ExceptionalDayTypeID[TrueAnnulInd[i]], length=length(ExceptionalDate))
      Annual <- rep("TRUE", length(ExceptionalDate))
      
      ExceptionalDays <- rbind(ExceptionalDays, data.frame(ExceptionalDate, Annual, ExceptionalDayTypeID))
      # }
    }
    ExceptionalDays <- ExceptionalDays[(! duplicated(ExceptionalDays$ExceptionalDate)) ,]
    ExceptionalDays <- ExceptionalDays[order(ExceptionalDays$ExceptionalDate), ]
    ExceptionalDays$Annual <- as.logical(ExceptionalDays$Annual)
  }
  
  
  ##### II: Find proximity days
  ProximityDays <- data.frame(Dates = rep(as.Date("2000-01-01"),length= 2*nrow(ExceptionalDays)),
                              Annual = rep(FALSE, length = 2*nrow(ExceptionalDays)),
                              ProximityDaysTypeID = rep("???", length = 2*nrow(ExceptionalDays)))
  ProximityDays$ProximityDaysTypeID <- as.character(ProximityDays$ProximityDaysTypeID)
  
  if (nrow(ExceptionalDays) > 0){
    for (i in 1:nrow(ExceptionalDays)){
      if (!(as.character(ExceptionalDays$ExceptionalDate[i]-1) %in% as.character(ExceptionalDays$ExceptionalDate))){
        ProximityDays$Dates[i] <- ExceptionalDays$ExceptionalDate[i]-1
        ProximityDays$Annual[i] <- ExceptionalDays$Annual[i]
        ProximityDays$ProximityDaysTypeID[i] <- paste(as.character(ExceptionalDays$ExceptionalDayTypeID[i]), "-", sep="")
      }
    }
    
    for (i in 1:nrow(ExceptionalDays)){
      if ((!(as.character(ExceptionalDays$ExceptionalDate[i]+1) %in% as.character(ExceptionalDays$ExceptionalDate)))
          &&(!(as.character(ExceptionalDays$ExceptionalDate[i]+1) %in% as.character(ProximityDays$Dates) )) ){
        ProximityDays$Dates[nrow(ProximityDays)-i+1] <- ExceptionalDays$ExceptionalDate[i]+1
        ProximityDays$Annual[nrow(ProximityDays)-i+1] <- ExceptionalDays$Annual[i]
        ProximityDays$ProximityDaysTypeID[nrow(ProximityDays)-i+1] <- paste(as.character(ExceptionalDays$ExceptionalDayTypeID[i]), "+", sep="")
      }
    }
    
    if (length(which(ProximityDays$ProximityDaysTypeID == "???")) != 0){
      ProximityDays<-ProximityDays[0-which(ProximityDays$ProximityDaysTypeID == "???"), ]
    }
    ProximityDays<- ProximityDays[order(ProximityDays$Dates), ]
  }
  
  ###################### Output
  return(list(ExceptionalDays, ProximityDays))
}