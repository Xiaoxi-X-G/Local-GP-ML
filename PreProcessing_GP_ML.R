PreProcessing_GP_ML <- function(InputData, ExceptionalDayandEffects, CloseDays, RegularCloseDayofWeekCSV){
  # FristDate.T, LastDate.T = character 
  # ExceptionalDayandEffects = list(ExceptionalDays, ProximityDays)
  #          where ExceptionalDays = data.frame(ExceptionalDate, Annual, ExceptionalDayTypeID)
  #          where ProximityDays = data.frame(Dates, Annual, ProximityDaysTypeID)
  # InputData = data.frame(Dates(as.Date), Values)
  # Output = list(lambda, NewMax, NewMin, OldMax, OldMin, OutputData=data.frame); 
  #          where Output[[lambda]] is the Box-Cox transformation index
  #          where Output[[NewMax]] is the Max after [0 1] normalization, or 1
  #          where Output[[NewMin]] is the Min after [0 1] normalization, or 0
  #          where Output[[OldMax]] is the Max after Box-Cox transformation
  #          where Output[[OldMin]] is the Min after Box-Cox transformation
  #          where Output[[OutputData]] is the data.frame(Dates, Values(WithoutMissing), SD.Annual, SD.Types, ProximityDays, PD.Type, CloseDays, Values_BoxCox(without Outliers), 
  #                                                       Outliers, X_coeff(OutliterCoeff), Values_Scale01(NormalizedValued))
  
  
  
  #####################################################################################################
  ########## -I: Complement the Exceptional Days to a clean, full(from 1st Day), chronological format
  ##########  0: Incorporate Proximity Day in a chronological format
  ##########  I: Format data started from FirstDate to LastDate
  ########## II: Deal with missing values (i.e. NAs)
  ##########III: Data transformation, box-cox by default
  ########## IV: Identify and replace outliers by prediction. Outliers & ExceptionalDay & ProximityDays X_coeff are stored 
  ##########  V: Scaling: min-max normalization to [0, 1]
  #####################################################################################################  
  
  #Dates<-seq(as.Date(FirstDate), as.Date("2016-01-31"), by ="1 day")
  #CloseDays<-Dates[weekdays(Dates)=="Sunday"]
  #write.csv(CloseDays, paste(RScriptPath,"/CloseDates.csv", sep=""), row.names=FALSE)
  
  
  ########### -I: Complement the Exceptional Days to a clean, full(from 1st Day), chronological format
  ExceptionalDays<-ExceptionalDayandEffects[[1]]
  
  ##########  0: Incorporate Proximity Day in a chronological format
  ProximityDays<- ExceptionalDayandEffects[[2]]
  
  ################### I: Format data started from FirstDate to LastDate
  #CloseDays<-read.csv(paste(RScriptPath, CloseDatesCSV, sep=""), header = TRUE)
  FirstDate <- as.character(InputData$Dates[1])
  LastDate <- as.character(tail(InputData$Dates, n=1))
  
  
  OutputData <- data.frame(Dates=seq(as.Date(FirstDate), as.Date(LastDate), by ="1 day"),
                           Values = rep(NA, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1)), 
                           SpecialDays =rep(FALSE, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1)),
                           SD.Annual = rep(FALSE, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1)),
                           SD.Type = rep(NA, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1)),
                           ProximityDays =rep(FALSE, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1)),
                           PD.Type = rep(NA, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1)),
                           CloseDays =rep(FALSE, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1)))
  
  
  days <- factor(as.factor(weekdays((OutputData$Dates))), levels = c("Monday", "Tuesday", "Wednesday", 
                                                                     "Thursday", "Friday", "Saturday", "Sunday"),
                 ordered = TRUE)
  OutputData$DayofWk <- as.integer(days) # Add Day of Week info for later use
  
  for (i in 1:nrow(OutputData)){
    if (OutputData$Dates[i] %in% InputData$Dates){
      OutputData$Values[i] <- InputData$Values[which(InputData$Dates == OutputData$Dates[i])]
    }
    if (OutputData$Dates[i] %in% ExceptionalDays$ExceptionalDate){
      OutputData$SpecialDays[i] <- TRUE
      IndAnn <- which(ExceptionalDays$ExceptionalDate == OutputData$Dates[i])
      OutputData$SD.Annual[i] <- ExceptionalDays$Annual[IndAnn]
      OutputData$SD.Type[i] <- ExceptionalDays$ExceptionalDayTypeID[IndAnn]
    }
    if (OutputData$Dates[i] %in% ProximityDays$Dates){
      OutputData$ProximityDays[i] <- TRUE
      PDInd <- which(ProximityDays$Dates == OutputData$Dates[i])
      OutputData$PD.Type[i] <- ProximityDays$ProximityDaysTypeID[PDInd]
    }
    if (as.factor(OutputData$Dates[i]) %in% CloseDays){
      OutputData$CloseDays[i] <- TRUE
    }
  }
  
  ################### II: Deal with missing values (or NAs)
  Ind<-which(is.na(OutputData$Values))
  if (length(Ind)>0){
    for (i in 1:length(Ind)){
      if (is.na(OutputData$Values[Ind[i]])&(OutputData$SpecialDays[Ind[i]] | OutputData$CloseDays[Ind[i]]) ){
        OutputData$Values[Ind[i]] <-0 #not data at a special day or close date, indicate 0 
      }else{ # average of same day of week
        WkInd.temp <- OutputData$DayofWk[Ind[i]]
        OutputData$Values[Ind[i]] <- mean(OutputData$Values[which(OutputData$DayofWk==WkInd.temp)], na.rm = T) 
      }
    }  
  }
  
  # need to add trycatch later
  if(length(which(is.nan(OutputData$Values)| is.na(OutputData$Values))) > 0) {stop("need to ck")}
  
  #}
  
  
  
  ################# III: Data transformation, box-cox
  lmd<-BoxCox.lambda(OutputData$Values)
  OutputData$Values_BoxCox<-BoxCox(OutputData$Values, lmd)
  
  
  ################## IV: Identify. Replace outliers and SpecialDay by mean, not Regular closing days
  #RegularCloseDayofWeek<-read.csv(paste(RScriptPath, RegularCloseDayofWeekCSV, sep=""), header = TRUE)
  RegularCloseDayofWeek<-RegularCloseDayofWeekCSV
  
  outliers <- unique(boxplot.stats(OutputData$Values_BoxCox, coef = 1.75)$out)
  OutputData$Outliers <- rep(FALSE, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1))
  OutputData$X_coeff <-  rep(NA, length = as.integer(as.Date(LastDate)-as.Date(FirstDate)+1))
  
  
  if (length(outliers)>0){
    for (i in 1:length(outliers)){
      OutputData$Outliers[which(OutputData$Values_BoxCox==outliers[i])] <- TRUE
    }
  }
  ################### Untick regular closing day
  if (length(RegularCloseDayofWeek)>0){
    for (j in 1:length(RegularCloseDayofWeek)){
      OutputData$Outliers[which(weekdays(OutputData$Dates)==as.character(RegularCloseDayofWeek$x[j]))] <- FALSE
    }
  }
  
  
  ####
  if ((length(outliers)>0) | (length(which(OutputData$SpecialDays)) >0) |(length(which(OutputData$ProximityDays)) >0) ){
    Ind <-c()
    Ind<- sort(unique(c(which(OutputData$SpecialDays), which(OutputData$Outliers), which(OutputData$ProximityDays))))
    for (j in 1:length(Ind)){
      ReplaceValue<-tryCatch(
        {
          WkInd.temp <- OutputData$DayofWk[Ind[j]]
          ReplaceValue <- mean(OutputData$Values_BoxCox[which((!OutputData$Outliers)
                                                              &(!OutputData$ProximityDays)
                                                              &(!OutputData$SpecialDays)
                                                              & (OutputData$DayofWk==WkInd.temp))], na.rm = T)
        },
        warning = function(cond){ # increase feasible region if no solution
          WkInd.temp <- OutputData$DayofWk[Ind[j]]
          ReplaceValue <- mean(OutputData$Values_BoxCox[which((!OutputData$Outliers)
                                                              & (OutputData$DayofWk==WkInd.temp))], na.rm = T)
          return(ReplaceValue)
        },
        error = function(cond){ # increase feasible region if no solution
          ReplaceValue <- mean(OutputData$Values_BoxCox, na.rm = T)
          return(ReplaceValue)
        }
      )
      OutputData$X_coeff[Ind[j]] <- OutputData$Values_BoxCox[Ind[j]]/ReplaceValue
      OutputData$Values_BoxCox[Ind[j]] <- ReplaceValue
    }
    
  }
  
  
  ################## V: Scaling: min-max normalization to [0, 1]
  new_max <- 1
  new_min <- 0
  old_min <-min(OutputData$Values_BoxCox)
  old_max <-max(OutputData$Values_BoxCox)
  OutputData$Values_Scale01 <- new_min + (new_max-new_min)*(OutputData$Values_BoxCox - old_min)/( old_max- old_min)
  
  return(list("lambda" = lmd, "NewMax"=new_max, "NewMin"=new_min,
              "OldMax" = max(OutputData$Values_BoxCox), "OldMin" = min(OutputData$Values_BoxCox),
              "OutputData" = OutputData))
}