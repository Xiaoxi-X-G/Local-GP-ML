rm(list = ls())
ptm <- proc.time()

##############################################################################################
### Add Library, functions, and set scriptpath.
library(RODBC)
library(GPfit)
library(forecast)
library(nnet)
library(caret)
library(MASS)
RScriptPath<-"C://Users/ptech3/Dropbox/Ploytech/Regression/machineLearning/gp/GaussianProcess_AzureML/GP_ML"


source(paste(RScriptPath, "/DataHoursV2.R", sep=""))
source(paste(RScriptPath, "/OpenCloseDayTime.R", sep=""))
source(paste(RScriptPath, "/Holidays2LocationID.R", sep=""))
source(paste(RScriptPath, "/ExceptionalDayandEffectFormatV2.R", sep=""))
source(paste(RScriptPath, "/TranslateDayofWeek.R", sep=""))
source(paste(RScriptPath, "/RegularCloseDayofWeek.R", sep=""))
source(paste(RScriptPath, "/PreProcessing_GP.R", sep=""))
source(paste(RScriptPath, "/BestArimaParam.R", sep=""))
source(paste(RScriptPath, "/ExponentialCoeff.R", sep=""))
source(paste(RScriptPath, "/DailyPred_GP.R", sep=""))
source(paste(RScriptPath, "/NormalIntradayPrediction.R", sep=""))
source(paste(RScriptPath, "/AbnormalIntradayPrediction.R", sep=""))


##############################################################################################
################# Input data
NoDays <-14
Wks <- 12 # no of weeks data to use
StartDate<- "2015-08-11"
FinishDate<- as.Date(StartDate) + NoDays - 1
# DatabaseName<-"Time2Work_MultiRetailAU" ### Ezcorp data
# LocationID <- 28                        

DatabaseName<-"RetailDemo2"  ### Matchbox


RMSE.Day <-c()
Rsqr.Day <- c()


######### link to drag all LocationID and corresponding SalesHistory
odbcDataSources()
conn<-odbcConnect(dsn="localdb") #
LocationAndJobRoleIDResult <- sqlQuery(conn, "SELECT DISTINCT [JobRoleID], [LocationID] FROM [RetailDemo2].[dbo].[SalesHistory] Order by LocationID;;")
odbcClose(conn)


RMS.Day <-c();
Rsqr.Day <- c()

for (i in 1:nrow(LocationAndJobRoleIDResult)){
  if (is.na(LocationAndJobRoleIDResult$LocationID[i]) || 
      is.na(LocationAndJobRoleIDResult$JobRoleID[i])){next}
  
  JobRoleIDs <- LocationAndJobRoleIDResult$JobRoleID[i]
  
  DB_Path <- paste(LocationAndJobRoleIDResult$LocationID[i], ";", sep="")
  #print(DB_Path)
  
  conn<-odbcConnect(dsn="localdb") 
  DataResult <- sqlQuery(conn, paste("SELECT [FinishTime], [Sales] FROM [RetailDemo2].[dbo].[SalesHistory] WHERE JobRoleID = ", JobRoleIDs, " And LocationID = ", DB_Path, sep="" ))
  #DataResult <- sqlQuery(conn, paste("SELECT [FinishTime], [Sales] FROM [Time2Work_MultiRetailAU].[dbo].[SalesHistory] WHERE LocationID = ", DB_Path, sep="" ))
  odbcClose(conn)
  
  if (nrow(DataResult)< 300 || is.na(sum(DataResult[, 2]))){
    conn<-odbcConnect(dsn="localdb") #
    DataResult <- sqlQuery(conn, paste("SELECT [FinishTime], [Transactions] FROM [RetailDemo2].[dbo].[SalesHistory] WHERE JobRoleID = ", JobRoleIDs, " And LocationID = ", DB_Path, sep="" ))
    #DataResult <- sqlQuery(conn, paste("SELECT [FinishTime], [Transactions] FROM [Time2Work_MultiRetailAU].[dbo].[SalesHistory] WHERE LocationID = ", DB_Path, sep="" ))
    odbcClose(conn)
    if(nrow(DataResult)<300 || is.na(sum(DataResult[, 2]))){
      print("Not Data OR SMALL DATA")
      next
    }
  }



############### Holiday data
Holidays2LocationID(as.character(LocationAndJobRoleIDResult$LocationID[i]), RScriptPath, DatabaseName)
ExceptionalDatesCSV <- "/ExceptionalDatesRight.csv"



#############################################################################################
##########################Segment to InputData and InputData.testing for training and testing

### InputData
VendData.stor.temp0<-DataResult[order(DataResult$FinishTime), ]
VendData.stor.temp <- data.frame(Time = VendData.stor.temp0$FinishTime, 
                                 Items  = VendData.stor.temp0$Transactions)
VendData.stor <- VendData.stor.temp[which((as.Date(format(VendData.stor.temp$Time, "%Y-%m-%d")) < as.Date(StartDate) )
                                          &(as.Date(format(VendData.stor.temp$Time, "%Y-%m-%d")) > (as.Date(StartDate)-Wks*7+1)) ), ]
temp3<-tapply(VendData.stor[,2], format(VendData.stor$Time, "%Y-%m-%d"),sum)
InputData <- data.frame(Dates = as.Date(names(temp3)), Values = unname(temp3))


### Figure out the true start date, StartDate.T, which is the first day after LastDate
FirstDate <- as.character(format(VendData.stor$Time[1],"%Y-%m-%d"))
LastDate <- as.character(format(tail(VendData.stor$Time, n=1),"%Y-%m-%d"))
FinishDate.T <- FinishDate
if (as.Date(StartDate) > (as.Date(LastDate)+1)){
  StartDate.T <-as.character(as.Date(LastDate) +1)
}else{
  StartDate.T <-StartDate
}
NoDays <- as.integer(as.Date(FinishDate.T) - as.Date(StartDate.T) + 1)


#### testing data
VendData.stor.testing0 <- VendData.stor.temp[which( (as.Date(format(VendData.stor.temp$Time, "%Y-%m-%d")) >= as.Date(StartDate.T))
                                                    & (as.Date(format(VendData.stor.temp$Time, "%Y-%m-%d")) <= as.Date(FinishDate.T))), ]
VendData.stor.testing <- DataHoursV2(VendData.stor.testing0, col=2, StartDate.T, FinishDate.T)
temp4 <-tapply(VendData.stor.testing[,2], format(VendData.stor.testing$Time, "%Y-%m-%d"),sum)
InputData.testing <- data.frame(Dates = as.Date(names(temp4)), Values = unname(temp4))


### Calculate Full exceptional days and proximity days
source(paste(RScriptPath, "/ExceptionalDayandEffectFormatV2.R", sep=""))
ExceptionalDayandEffects<-ExceptionalDayandEffectFormatV2(ExceptionalDatesCSV, FirstDate, FinishDate.T)


### Opening hour data
OpenDayTime <- OpenCloseDayTime(FirstDate, FinishDate.T, as.character(LocationAndJobRoleIDResult$LocationID[i]), RScriptPath, DatabaseName)
CloseDatesCSV<-"/CloseDatesRight.csv"


####  the Closing dates are saved in CloseDatesRight.csv
RegularCloseDayofWeek(as.character(LocationAndJobRoleIDResult$LocationID[i]), RScriptPath)
RegularCloseDayofWeekCSV<-"/RegularCloseDayofWeek.csv"


#### Data Preprocessing
XXX<-PreProcessing_GP(FinishDate.T, InputData, RScriptPath, ExceptionalDayandEffects, CloseDatesCSV, RegularCloseDayofWeekCSV)
#print(XXX)







YYYY <- DailyPred_GP(FinishDate.T, XXX, ExceptionalDayandEffects, RScriptPath, CloseDatesCSV)
#requires 8 weeks data at least



#### plots
historys <- XXX[["OutputData"]]
N=length(historys$Values)
PastData <- ts(c(historys$Values, InputData.testing$Values))
PredictData <- c(rep(0, length = length(historys$Values)), YYYY$Rev2_Orig)

plot(PastData, type="o" ,col = "red", main=paste(DB_Path, as.character(JobRoleIDs), sep="-"), xaxt = "n")
lines(PredictData, type="o", pch = 22,  col = "blue" )
axis(1, at=1:length(PredictData), labels=as.character(seq(as.Date(FirstDate), as.Date(FinishDate.T), by = "1 day")))
#Sys.sleep(2)


plot(InputData.testing$Values, type="o" ,col = "red", main=paste(DB_Path, as.character(JobRoleIDs), sep="-"), xaxt = "n")
lines(as.vector(YYYY$Rev2_Orig), type="o", pch = 22,  col = "blue" )
axis(1, at=1:length(InputData.testing$Values), labels=as.character(seq(as.Date(StartDate.T), as.Date(FinishDate.T), by = "1 day")))
## accuracy measurement
Rsqr.Day[i] <- R2(YYYY$Rev2_Orig, InputData.testing$Values)
RMSE.Day[i] <- sqrt(mean((InputData.testing$Values-as.vector(YYYY$Rev2_Orig))^2))
acf((YYYY$Rev2_Orig - InputData.testing$Values))
#Sys.sleep(2)

plot(YYYY$Rev2_Orig, InputData.testing$Values, 
     ylim=c(0, max(YYYY$Rev2_Orig, InputData.testing$Values)), 
     xlim=c(0, max(YYYY$Rev2_Orig, InputData.testing$Values)),
     main = paste(DB_Path, as.character(JobRoleIDs), ";", 
                  "Rsqr=", round(Rsqr.Day[i], digits = 3), "; 
                  RMSE=", round(RMSE.Day[i], digits = 3)))

### Intraday
########### 
XXXX<-XXX[[6]]

HistoryInfo <-data.frame(Dates = XXXX$Dates, Items=XXXX$Values,  DayofWeek = weekdays(XXXX$Dates),
                         OpenFrom = OpenDayTime$OpenFrom[1: which(OpenDayTime$Dates == as.Date(LastDate))], 
                         OpenTo = OpenDayTime$OpenTo[1: which(OpenDayTime$Dates == as.Date(LastDate))], 
                         SD.Type = XXXX$SD.Type, PD.Type = XXXX$PD.Type, Outlier = XXXX$Outliers
                         ,stringsAsFactors=FALSE)

PredictInfor <-data.frame(Dates = YYYY$Dates, Items=YYYY$Rev2_Orig, DayofWeek = weekdays(YYYY$Dates),
                          OpenFrom = OpenDayTime$OpenFrom[which(OpenDayTime$Dates == as.Date(StartDate.T)): nrow(OpenDayTime)], 
                          OpenTo = OpenDayTime$OpenTo[which(OpenDayTime$Dates == as.Date(StartDate.T)): nrow(OpenDayTime)], 
                          SD.Type = YYYY$SD.Type, PD.Type = YYYY$PD.Type, Outlier = rep(FALSE, length = nrow(YYYY))
                          ,stringsAsFactors=FALSE)


HistoryAndPredictInfo <- rbind(HistoryInfo, PredictInfor)


### format hourly data
HistoryAndPredictHourlyInfo<-DataHoursV2(VendData.stor, col=2, FirstDate, FinishDate.T)

### Normal intraday prediction
HistoryAndPredictHourlyInfo_updated <- NormalIntradayPrediction(HistoryAndPredictHourlyInfo, HistoryAndPredictInfo, PredictInfor)

### Abnormal intraday prediction
HistoryAndPredictHourlyInfo_updated2 <- AbnormalIntradayPrediction(HistoryAndPredictHourlyInfo_updated, HistoryAndPredictInfo, PredictInfor)



### check 
# plot(VendData.stor.testing$Items, type="o" ,col = "red", main=paste(DB_Path, as.character(JobRoleIDs), sep="-"), xaxt = "n")
# lines(tail(HistoryAndPredictHourlyInfo_updated2$Items, n = NoDays*24), type="o", pch = 22,  col = "blue" )


# CheckDay <- "Saturday"
# plot(tail(HistoryAndPredictHourlyInfo_updated2$Items[which(weekdays(as.Date(format(HistoryAndPredictHourlyInfo_updated2$Time, "%Y-%m-%d"))) == CheckDay)], n = 400), type="o", col="blue")
}



###### To compared with State-Space model
# Acc_GP <- data.frame(Location = LocationAndJobRoleIDResult$LocationID,
#            JobRole = LocationAndJobRoleIDResult$JobRoleID,
#            Rsqr = Rsqr.Day,
#            RMSE = RMSE.Day)
# 
# 

# write.csv(Acc_GP, file = "Acc_GP.csv")

Acc_GP <- read.csv("Acc_GP.csv")
mean(Acc_GP$RMSE[which(Acc_GP$JobRole == 20)], na.rm = T)
proc.time() - ptm  #user  system elapsed :  319.77    5.80  373.29
                   # no. unfitness 3