rm(list = ls())
ptm <- proc.time()

##############################################################################################
### Add Library, functions, and set scriptpath.
library(forecast)
library(nnet)
library(caret)
library(MASS)
library(plyr)
RScriptPath<-"C://Users/ptech3/Dropbox/Ploytech/Regression/machineLearning/gp/GaussianProcess_AzureML/GP_ML"
DataPath <- "C://Users/ptech3/Dropbox/Ploytech/Regression/AzureML/AZureML/AllData"



source(paste(RScriptPath,"/DataHoursV2_ML.R", sep=""))
source(paste(RScriptPath,"/ExceptionalDayandEffectFormatV2_ML.R", sep=""))
source(paste(RScriptPath,"/FindQtrOutliers.R", sep=""))
source(paste(RScriptPath,"/ExponentialCoeff.R", sep=""))
source(paste(RScriptPath,"/NormalIntradayPrediction_ML.R", sep=""))
source(paste(RScriptPath,"/AbnormalIntradayPrediction_ML.R", sep=""))
source(paste(RScriptPath,"/RegularCloseDayofWeek_MLV2.R", sep=""))

source(paste(RScriptPath, "/BestArimaParam.R", sep=""))
source(paste(RScriptPath,"/PreProcessing_GP_ML.R", sep=""))
source(paste(RScriptPath, "/DailyPred_GP_ML.R", sep=""))




Options <- read.csv(paste(DataPath, "/OptionsTest.csv", sep=""), header = T)


############################## Load ExceptionalDatesOpeningHours Data in the same format as AZure ML

ExceptionalDatesOpeningHours <- tryCatch( # catach errors at the inputs: ExceptionalDates and Openinghours
  {
    ExceptionalDates <- read.csv(paste(DataPath, "/ExceptionalDatesTest.csv", sep="") ,header = T) # class: data.frame
    ExceptionalDates$ExceptionalDayTypeID <- as.integer(ExceptionalDates$ExceptionalDayTypeID) # Fix data type
    openingHours2 <- read.csv(paste(DataPath, "/openingHoursTest.csv", sep=""),header = T)
    
    openingHours222 <- data.frame(LocationID.H = openingHours2$LocationID,
                                  Dates = as.Date(as.POSIXct(openingHours2$OpenFrom, origin = "1970-01-01", tz="GMT")),
                                  OpenFrom =  format(as.POSIXct(openingHours2$OpenFrom, origin = "1970-01-01", tz="GMT"), "%H:%M:%S"),
                                  OpenTo =  format(as.POSIXct(openingHours2$OpenTo, origin = "1970-01-01", tz="GMT"), "%H:%M:%S"))
    
     ExceptionalDatesOpeningHours <-   rbind.fill(ExceptionalDates, openingHours222)
    
  },
  
  error = function(cond){ # all other errors would be caught and gives no output
    print("Errors  occur at ExceptionalDates or Opening hours")
    ExceptionalDatesOpeningHours <- data.frame(LocationID = NA, ExceptionalDate = NA, Annual = NA,
                                               ForecastIgnoreHistory = NA, ForecastDateSpecific = NA,
                                               ExceptionalDayTypeID = NA, LocationID.H = NA, 
                                               Dates = NA, OpenFrom = NA, OpenTo = NA)
    return(ExceptionalDatesOpeningHours)
  }
)

print("######## check ########")
print(ExceptionalDatesOpeningHours)


############################## Load Startand Finish Data in the same format as AZure ML
OtherInfor <- tryCatch(
  {
    OtherInfor <-  rbind.fill(Options, ExceptionalDatesOpeningHours)
    
  },
  error = function(cond){
    print("If ExceptionalDatesOpening Hours is correct, error is at Start and Finish Date")
    OtherInfor <- data.frame(StartDate = NA, FinishDate = NA, Breakdown = NA, Interval = NA,
                             LocationID = NA, ExceptionalDate = NA, Annual = NA,
                             ForecastIgnoreHistory = NA, ForecastDateSpecific = NA,
                             ExceptionalDayTypeID = NA, LocationID.H = NA, 
                             Dates = NA, OpenFrom = NA, OpenTo = NA)
    return(OtherInfor)
  }
)




###### Main function
StartDate <- as.character(OtherInfor$StartDate[1])
FinishDate <-as.character(OtherInfor$FinishDate[1])
Breakdown <- as.character(OtherInfor$Breakdown[1])
Interval <- as.character(OtherInfor$Interval[1])


################################# Segmentation
##I: format files
##II: construct unique ID based on SegRequirement
##III: loop through each segments

salesHistories.temp3 <- read.csv(paste(DataPath, "/salesHistoriesTest.csv", sep=""),
                                 header = T)
# covert real NA
salesHistories.temp2 <- cbind(as.data.frame(sapply(salesHistories.temp3[,c(1:4)], as.integer)),
                              salesHistories.temp3[,c(5,6)])
# remove NA rows 
salesHistories.temp <- salesHistories.temp2[which(rowSums(is.na(as.matrix(salesHistories.temp2[, which(as.integer(strsplit(as.character(Breakdown),"")[[1]])==1)])))==0),]


##############################################################################################      
if (nrow(salesHistories.temp) == 0){
  ErrMsg <- "Invalid breakdown requriements cause empty historical data."
  print(ErrMsg)
  PredictionAllResults <- data.frame(LocationID = NA,
                                     DepartmentID = NA,
                                     JobRoleID = NA,
                                     SkillID = NA,
                                     Time = "9999-01-01", Items = ErrMsg, 
                                     stringsAsFactors=FALSE)
}else{
  # findout the max digits
  DigitNo <- max(max(nchar(salesHistories.temp[which(!is.na(salesHistories.temp[,1])),  1])), 
                 max(nchar(salesHistories.temp[which(!is.na(salesHistories.temp[,2])),  2])),
                 max(nchar(salesHistories.temp[which(!is.na(salesHistories.temp[,3])),  3])),
                 max(nchar(salesHistories.temp[which(!is.na(salesHistories.temp[,4])),  4])))
  
  for (i in 1:4){
    salesHistories.temp[,i] <- sprintf(paste("%0",DigitNo,"d",sep=""), salesHistories.temp[,i])
  }
  
  salesHistoriesID <- data.frame(ID = as.character(interaction(salesHistories.temp[,which(as.integer(strsplit(as.character(Breakdown),"")[[1]])==1)], sep=""))
                                 ,FinishTime =salesHistories.temp$FinishTime
                                 ,ValueItem=salesHistories.temp$ValueItem
                                 ,stringsAsFactors=FALSE)
  
  
  ## prepare other information
  ExceptionalDatesCSV.temp <- OtherInfor[c(!is.na(OtherInfor$LocationID)), c(5:10)]
  ExceptionalDatesCSV.temp$ExceptionalDate <- format(as.POSIXct(ExceptionalDatesCSV.temp$ExceptionalDate, origin = "1970-01-01", tz="GMT"), "%Y-%m-%d");
  
  OpenDayResults.temp <- OtherInfor[c(!is.na(OtherInfor$LocationID.H)), c(11:14)]
  OpenDayResults.temp$Dates <- as.Date(OpenDayResults.temp$Dates)
  colnames(OpenDayResults.temp)[1] <- "LocationID"
  
  
  print("####### Check-2 ###########")
  print(head(salesHistoriesID, n =5))
  print(head(ExceptionalDatesCSV.temp, n =5))
  print(head(OpenDayResults.temp, n =5))
  
  ## start segmentation and prediction
  PredictionAllResults <- data.frame(LocationID = character(), DepartmentID = character(),
                                     JobRoleID = character(), SkillID = character(),
                                     Time = character(), Items = character(), 
                                     stringsAsFactors=FALSE)
  UniqueID <- as.character(unique(salesHistoriesID$ID))
  for (m in 1:length(UniqueID)){
    id <- UniqueID[m]
    
    FullIDs <- rep("000",4) # Full ID of [LocationaID, DepartmentID, JobRoleID, SkillID]
    FullIDs[which(as.integer(strsplit(as.character(Breakdown),"")[[1]])==1)]<-
      substring(id, seq(1, nchar(id)-1, DigitNo), seq(DigitNo, nchar(id), DigitNo))
    FullIDs <- as.character(as.numeric(FullIDs))
    
    salesHistories <- salesHistoriesID[which(salesHistoriesID$ID == id), c(2,3)]
    ExceptionalDatesCSV <- ExceptionalDatesCSV.temp[which(ExceptionalDatesCSV.temp$LocationID == as.integer(substr(id, 1, DigitNo))),
                                                    c(-1)]
    OpenDayResults <- OpenDayResults.temp[which(OpenDayResults.temp$LocationID == as.integer(substr(id, 1, DigitNo))),
                                          c(-1)]
    
    
    
    print("####### Check-3 ###########")  
    print(head(salesHistories, n =5))                                 
    print(head(ExceptionalDatesCSV, n =5))
    print(head(OpenDayResults, n =5))


    #############################Prediction############################################      
    PredictionResults <- tryCatch( # catch all other errors that may occur
      {
        
        if ((nrow(salesHistories)==0) | is.na(StartDate) | is.na(FinishDate)
            | (as.integer(as.Date(FinishDate) - as.Date(StartDate))<=0) ){ # no hisotical data or Start Finish Date
          
          ErrMsg <- "No historical data, or no start date or finish date information, or errors at data importing"
          print(ErrMsg)
          PredictionResults <- data.frame(LocationID = FullIDs[1],
                                          DepartmentID = FullIDs[2],
                                          JobRoleID = FullIDs[3],
                                          SkillID = FullIDs[4],
                                          Time = "9999-01-01", Items = ErrMsg, 
                                          stringsAsFactors=FALSE)
        }else{
          
          salesHistories$FinishTime <- as.POSIXct(salesHistories$FinishTime, origin = "1970-01-01", tz="GMT")
          
          VendData.stor.temp0 <- salesHistories[order(salesHistories$FinishTime), ] 
          VendData.stor.temp0$FinishTime <- as.POSIXct(VendData.stor.temp0$FinishTime, origin = "1970-01-01", tz="GMT") # Order changes DayTime format to Epoch
          
          
          VendData.stor <- data.frame(Time = VendData.stor.temp0$FinishTime, Items = VendData.stor.temp0$ValueItem)
          VendData.stor$Time <- as.POSIXct(VendData.stor$Time, origin = "1970-01-01", tz="GMT")
          
          ################## Daily aggregation
          temp3 <- tapply(VendData.stor.temp0$ValueItem, format(VendData.stor.temp0$FinishTime, "%Y-%m-%d"), sum)
          InputData  <- data.frame(Dates = as.Date(names(temp3)), Values = unname(temp3))
          
          ######
          FirstDate <- as.character(format(VendData.stor.temp0$FinishTime[1],"%Y-%m-%d"))
          LastDate <- as.character(format(tail(VendData.stor.temp0$FinishTime, n=1),"%Y-%m-%d"))
          
          
          ### Update First Date, InputData, VendDate.stor based on OpenDay Results,
          ### Otherwise, cannot identify RegularClose Day
          if (OpenDayResults$Dates[1] > as.Date(FirstDate)){
            FirstDate <- as.character(OpenDayResults$Dates[1])
            
            InputData <- InputData[which(InputData$Dates >= as.Date(FirstDate)),]
            
            VendData.stor <- VendData.stor[which(as.Date(format(VendData.stor$Time, "%Y-%m-%d")) 
                                                 >= as.Date(FirstDate)), ]
          }
          
          
          ##############################################################################################################
          ###  I: Reset the StartDate to the first unavailiable day
          FinishDateT <- FinishDate
          if (as.Date(StartDate) > (as.Date(LastDate)+1)){
            StartDateT <-as.character(as.Date(LastDate) +1)
          }else{
            StartDateT <-StartDate
          }
          
          print("####### Check-4 ###########") 
          print(FinishDateT)
          print(StartDateT)
          print(FirstDate) 
          print(LastDate)
          
          if ((as.integer(as.Date(StartDateT) - as.Date(FirstDate)) <= 8*7) | (as.integer(as.Date(FinishDateT) - as.Date(StartDateT)) <= 0)
              | (as.integer(as.Date(LastDate) - as.Date(FirstDate)) <= 8*7)){ # at least 8 weeks data are required 
            
            ErrMsg <- "Start date or finish Dates error; or the data length is less than eight weeks"
            print(ErrMsg)
            PredictionResults <- data.frame(LocationID = FullIDs[1],
                                            DepartmentID = FullIDs[2],
                                            JobRoleID = FullIDs[3],
                                            SkillID = FullIDs[4],
                                            Time = "9999-01-01", Items = ErrMsg,
                                            stringsAsFactors=FALSE)
          }else{
            ##############################################################################################################
            ### II: Calculate Full exceptional days and proximity days
            ExceptionalDays <- ExceptionalDatesCSV
            ExceptionalDayandEffects<-ExceptionalDayandEffectFormatV2_ML(ExceptionalDatesCSV, FirstDate, FinishDateT)
            #print(ExceptionalDayandEffects)
            
            ##############################################################################################################
            ###III: Find opening and closing day-time from first day in history to last day of prediction
            ### Change format to dataframe
            OpenDayTime <- data.frame(Dates = seq(as.Date(FirstDate), as.Date(FinishDateT), by="day"),
                                      OpenFrom= rep("00:00:00", length = (1+as.integer(as.Date(FinishDateT)-as.Date(FirstDate)))),
                                      OpenTo= rep("00:00:00", length = (1+as.integer(as.Date(FinishDateT)-as.Date(FirstDate))))
                                      , stringsAsFactors=FALSE)
            
            for (i in 1:nrow(OpenDayResults)){
              OpenDayTime$OpenFrom[which(OpenDayTime$Dates == OpenDayResults$Dates[i])] <- as.character(OpenDayResults$OpenFrom[i])
              OpenDayTime$OpenTo[which(OpenDayTime$Dates == OpenDayResults$Dates[i])] <- as.character(OpenDayResults$OpenTo[i])
            }
            
            OpenDayTime <- OpenDayTime[which(OpenDayTime$Dates >= as.Date(FirstDate)),]
            CloseDays <- OpenDayTime$Dates[which((OpenDayTime$OpenFrom=="00:00:00") &(OpenDayTime$OpenTo=="00:00:00") )]
            OpenDayTime$Dates <- as.Date(OpenDayTime$Dates)
        
            ### IV: Find weekly closing day 
            NoOfWeek <- (as.integer(as.Date(LastDate) - as.Date(FirstDate)) / 7)
            RegularCloseDayofWeekCSV <- RegularCloseDayofWeek_MLV2(CloseDays, NoOfWeek)
        
        
            print("####### Check-5 ###########") 
            print(head(InputData, n =4))
            print(head(ExceptionalDatesCSV, n =4))
            print(CloseDays) 
            print(RegularCloseDayofWeekCSV)
            
            ### V: Data Preprocessing
            XXX <- PreProcessing_GP_ML(InputData, ExceptionalDayandEffects, CloseDays, RegularCloseDayofWeekCSV)
            print("####### Check-6 ###########")
            

        
            ### VI: Daily prediction
            YYYY <- DailyPred_GP_ML(FinishDateT, StartDateT, XXX, ExceptionalDayandEffects, CloseDays)
            print("######### check-7 ###########")
            head(YYYY, n =5)

            if (as.character(Interval) == "0"){  
              PredictionResults.temp <- data.frame(LocationID = rep(FullIDs[1], length=nrow(YYYY)),
                                                   DepartmentID = rep(FullIDs[2], length=nrow(YYYY)),
                                                   JobRoleID = rep(FullIDs[3], length=nrow(YYYY)),
                                                   SkillID = rep(FullIDs[4], length=nrow(YYYY)),
                                                   Time = as.character(YYYY$Dates), 
                                                   Items= as.character(YYYY$Rev2_Orig), 
                                                   stringsAsFactors=FALSE)
              PredictionResults <- PredictionResults.temp[which(as.Date(PredictionResults.temp$Time) >= as.Date(StartDate)),]
            }else{
              ##############################################################################################################
              ### VII: Initialization intraday prediction
              XXXX<-XXX[[6]]
              
              HistoryInfo <-data.frame(Dates = XXXX$Dates, Items=XXXX$Values,  DayofWeek = weekdays(XXXX$Dates),
                                       OpenFrom = OpenDayTime$OpenFrom[1: which(OpenDayTime$Dates == as.Date(LastDate))], 
                                       OpenTo = OpenDayTime$OpenTo[1: which(OpenDayTime$Dates == as.Date(LastDate))], 
                                       SD.Type = XXXX$SD.Type, PD.Type = XXXX$PD.Type, Outlier = XXXX$Outliers
                                       ,stringsAsFactors=FALSE)
              
              PredictInfor <-data.frame(Dates = YYYY$Dates, Items=YYYY$Rev2_Orig, DayofWeek = weekdays(YYYY$Dates),
                                        OpenFrom = OpenDayTime$OpenFrom[which(OpenDayTime$Dates == as.Date(StartDateT)): nrow(OpenDayTime)], 
                                        OpenTo = OpenDayTime$OpenTo[which(OpenDayTime$Dates == as.Date(StartDateT)): nrow(OpenDayTime)], 
                                        SD.Type = YYYY$SD.Type, PD.Type = YYYY$PD.Type, Outlier = rep(FALSE, length = nrow(YYYY))
                                        ,stringsAsFactors=FALSE)
              
              HistoryAndPredictInfo <- rbind(HistoryInfo, PredictInfor) 


              ### format hourly data
              HistoryAndPredictHourlyInfo<-DataHoursV2_ML(VendData.stor, col=2, FirstDate, FinishDateT)
              
              ##############################################################################################################
              ### VIII: Normal intraday prediction
              HistoryAndPredictHourlyInfo_updated <- NormalIntradayPrediction_ML(HistoryAndPredictHourlyInfo, HistoryAndPredictInfo, PredictInfor)
              
          
          
          
          
              ##############################################################################################################
              ### IX: Abnormal intraday prediction
              HistoryAndPredictHourlyInfo_updated2 <- AbnormalIntradayPrediction_ML(HistoryAndPredictHourlyInfo_updated, HistoryAndPredictInfo, PredictInfor)
              
              PredictionResults.temp <- tail(HistoryAndPredictHourlyInfo_updated2, 
                                             n = (24*as.integer(1+as.Date(FinishDateT)- as.Date(StartDate))))
              
              PredictionResults <- data.frame(LocationID = rep(FullIDs[1], length=nrow(PredictionResults.temp)),
                                              DepartmentID = rep(FullIDs[2], length=nrow(PredictionResults.temp)),
                                              JobRoleID = rep(FullIDs[3], length=nrow(PredictionResults.temp)),
                                              SkillID = rep(FullIDs[4], length=nrow(PredictionResults.temp)),
                                              Time = as.character(PredictionResults.temp$Time), 
                                              Items = as.character(PredictionResults.temp$Items),
                                              stringsAsFactors=FALSE)        
            }
          }
        }
      },
  
      error = function(cond){ # all other errors would be caught and gives no output
        ErrMsg <- "Errors may occur at inputs, libraries or functions, or the data length doesn't match-up; otherwise, a thoroughly code-check is requried"
        print(ErrMsg)
        PredictionResults <- data.frame(LocationID = FullIDs[1],
                                        DepartmentID = FullIDs[2],
                                        JobRoleID = FullIDs[3],
                                        SkillID = FullIDs[4],
                                        Time = "9999-01-01", Items = ErrMsg, stringsAsFactors=FALSE)
        return(PredictionResults)
      }
    )
    PredictionAllResults <- rbind(PredictionAllResults, PredictionResults)
  }
}
CkTime <- proc.time() - ptm 

###
Dailys <- XXX[[6]]
plot(c(Dailys$Values_Scale01, rep(0, length=nrow(YYYY))), type="o", col="blue")
lines(c(rep(0,length=nrow(Dailys)), YYYY$Preds), type="o", col="red")

###
plot(c(InputData$Values, rep(0, length=nrow(Dailys))), type="o", col="blue")
lines(c(rep(0,length=nrow(InputData)), YYYY$Rev2_Orig), type="o", col="red")
