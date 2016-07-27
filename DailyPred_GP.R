DailyPred_GP<-function(FinishDate.T, InputData, ExceptionalDayandEffects, RScriptPath, CloseDatesCSV){
  # FristDate.T, LastDate.T = character
  # ExceptionalDayandEffects = list(ExceptionalDays, ProximityDays)
  #          where ExceptionalDays = data.frame(ExceptionalDate, Annual, ExceptionalDayTypeID)
  #          where ProximityDays = data.frame(Dates, Annual, ProximityDaysTypeID)
  # InputData= list(lambda, NewMax, NewMin, OldMax, OldMin, OutputData); 
  #          where InputData[[lambda]] is the Box-Cox transformation index
  #          where InputData[[NewMax]] is the Max after [0 1] normalization, or 1
  #          where InputData[[NewMin]] is the Min after [0 1] normalization, or 0
  #          where InputData[[OldMax]] is the Max after Box-Cox transformation
  #          where InputData[[OldMin]] is the Min after Box-Cox transformation
  #          where InputData[[OutputData]] is the data.frame(Dates, Values(WithoutMissing), SD.Annual, SD.Type, ProximityDays, PD.Type, CloseDays, Values_BoxCox(without Outliers), 
  #                                                       Outliers, X_coeff(OutliterCoeff), Values_Scale01(NormalizedValued))
  
  # Output = data.frame(Dates, Preds(between 0, 1), Rev2_BoxCox, SepcialDays, SD.Annual, 
  #                     SD.Type, CloseDays, Rev2_Orig(convert back to original format))
  
  
  
  
  
  ##########  I: Daily prediction: Predict each day of week individually assuming the input is random and less pattern
  ### I.1: ARIMA for Linear 
  ### I.2: ANN for Nonlinear 
  
  NoDays <- as.integer(as.Date(FinishDate.T)-as.Date(StartDate.T) +1)
  H <- ceiling(NoDays/7)
  CleanedData <- InputData[["OutputData"]]
  nonlinearPred2Use <- data.frame(DayofWk=rep(tail(CleanedData$DayofWk, 7), H), 
                                  Pred01=rep(NA, 7*H), Rev2_orig=rep(NA, 7*H))
  linearPred2Use <- data.frame(DayofWk = rep(tail(CleanedData$DayofWk, 7), H), Pred = rep(NA, 7*H))
  
  
  
  for (n in 1:7){ # for each day of a week
    Data2Use <- CleanedData$Values_Scale01[which(CleanedData$DayofWk == n)]
    
    ### I.1: linear part
    ArimaFit2Use <- tryCatch(
      {
        BestArima<-BestArimaParam(Data2Use, maxord=c(1,1,1,0,0,0)) # to avoid overfitting
        orders <- BestArima[[3]]
        ArimaFit2Use <- arima(Data2Use, order = head(orders, n =3),
                              seasonal = list(order = tail(orders,n=3)))
        
        
      },
      error = function(cond){
        ArimaFit2Use <- auto.arima(Data2Use, max.p=2, max.q=2, max.d = 1, seasonal = FALSE,
                                   lambda=NULL)
      }
    )
    ArimaPred2Use <- forecast(ArimaFit2Use, h=H)
    
    ### I.2: Nonlinear part
    if (sum(as.numeric(ArimaPred2Use$residuals))==0){
      nonlinearPred2Use$Pred01 <- 0
      nonlinearPred2Use$Rev2_orig <- 0
      linearPred2Use$Pred[which(linearPred2Use$DayofWk==n)] <- as.numeric(ArimaPred2Use$mean)
    }else{
      #plot(ArimaPred2Use)
      
      linearPred2Use$Pred[which(linearPred2Use$DayofWk==n)] <- as.numeric(ArimaPred2Use$mean)
      
      # scale Data to [0 1]
      old_min_Resid2Use <-min(ArimaFit2Use$residuals) 
      old_max_Resid2Use <-max(ArimaFit2Use$residuals)
      new_max <- 1
      new_min <- 0
      
      Nonliear_Resid2Use <- data.frame(Resid = as.numeric(ArimaFit2Use$residuals))
      Nonliear_Resid2Use$Resid01 <- new_min + (new_max-new_min)*(Nonliear_Resid2Use$Resid - old_min_Resid2Use)/( old_max_Resid2Use - old_min_Resid2Use)
      
      form.in<-as.formula('Y1~x1+x2+x3+x4+x5+x6+x7') 
      
      
      data.temp <- Nonliear_Resid2Use$Resid01
      predNonlinear2Use <- c()
      for (i in 1:H){ # 1-step ahead prediction
        dataIn2Use <- data.frame(Y1=data.temp, x1=lag(data.temp, k=1), x2 = lag(data.temp, k=2),
                                 x3=lag(data.temp, k=3), x4 = lag(data.temp, k=4),
                                 x5=lag(data.temp, k=5), x6 = lag(data.temp, k=6),
                                 x7=lag(data.temp, k=7))
        nnetModel2Use <- nnet(form.in, dataIn2Use, size = 15, decay= 0.1, maxit = 5000, linout = TRUE)
        
        dat.pred2Use <-as.vector(c(0, tail(data.temp, n=7)))
        names(dat.pred2Use) <-c("Y1", "x1","x2","x3","x4","x5","x6","x7")
        resul <- predict(nnetModel2Use, as.data.frame(as.list(dat.pred2Use)), type="raw")
        
        predNonlinear2Use[i] <-unname(resul[1,1])
        data.temp <- c(data.temp, predNonlinear2Use[i])
      }
      
      # ## plot
      # plot(dataIn2Use$Y1, type="o", col = "blue", main=paste("NNET,", "DayofWk=",n))
      # lines(predict(nnetModel2Use, dataIn2Use),  type="o", col = "red", pch =22)
      
      
      # Scale back
      nonlinearPred2Use$Pred01[which(nonlinearPred2Use$DayofWk == n)] <- predNonlinear2Use
      nonlinearPred2Use$Rev2_orig[which(nonlinearPred2Use$DayofWk == n)] <- 
        (predNonlinear2Use - new_min)/(new_max-new_min)*(old_max_Resid2Use - old_min_Resid2Use) + old_min_Resid2Use
    }
    
  }
  
  
  
  
  ### II: Combine results, and reverse back to BoxCox format
  NewMax<-InputData[["NewMax"]]
  NewMin<-InputData[["NewMin"]]
  OldMax<-InputData[["OldMax"]]
  OldMin<-InputData[["OldMin"]]
  
  Daypred <- data.frame(Dates=seq(as.Date(StartDate.T), as.Date(FinishDate.T), by = "1 day"),
                        DayofWk = head(nonlinearPred2Use$DayofWk, n=NoDays),
                        Pred01 = head( (nonlinearPred2Use$Rev2_orig + linearPred2Use$Pred), n=NoDays))
  Daypred$Rev2_Boxcox <- (Daypred$Pred01 - NewMin)/(NewMax-NewMin)*(OldMax - OldMin) + OldMin
  
  
  ########## III: Add Exceptional Days and Closing day information
  ########## 1)   Complement the Exceptional Days to a clean, full(from 1st Day), chronological format
  ########## 2)   Closing day
  ########## 3)   Proximity day
  ########## 4)   Add information 
  
  
  ########## 1)   Complement the Exceptional Days to a clean, full(from 1st Day), chronological format
  ExceptionalDays <- ExceptionalDayandEffects[[1]]
  
  
  ########## 2)   Closing day
  CloseDays<-read.csv(paste(RScriptPath, CloseDatesCSV, sep=""), header = TRUE)
  
  ########## 3)   Proximity day
  ProximityDays <- ExceptionalDayandEffects[[2]]
  
  
  ########## 4)   Add information 
  Daypred$SpecialDays<-rep(FALSE, length = as.integer(tail(Daypred$Dates, n=1)-Daypred$Dates[1]+1))
  Daypred$SD.Annual <- rep(FALSE, length = as.integer(tail(Daypred$Dates, n =1)-Daypred$Dates[1]+1))
  Daypred$SD.Type <- rep(NA, length = as.integer(tail(Daypred$Dates, n =1)-Daypred$Dates[1]+1))
  Daypred$ProximityDays <- rep(FALSE, length = as.integer(tail(Daypred$Dates, n =1)-Daypred$Dates[1]+1))
  Daypred$PD.Type <- rep(NA, length = as.integer(tail(Daypred$Dates, n =1)-Daypred$Dates[1]+1))
  Daypred$CloseDays<-rep(FALSE, length = as.integer(tail(Daypred$Dates, n =1)-Daypred$Dates[1]+1))
  
  for (i in 1:nrow(Daypred)){
    if (Daypred$Dates[i] %in% ExceptionalDays$ExceptionalDate){
      Daypred$SpecialDays[i] <- TRUE
      IndAnn <- which(ExceptionalDays$ExceptionalDate == Daypred$Dates[i])
      Daypred$SD.Annual[i] <- ExceptionalDays$Annual[IndAnn]
      Daypred$SD.Type[i] <- ExceptionalDays$ExceptionalDayTypeID[IndAnn]
    }
    
    if (Daypred$Dates[i] %in% ProximityDays$Dates){
      Daypred$ProximityDays[i] <- TRUE
      PDInd <- which(ProximityDays$Dates == Daypred$Dates[i])
      Daypred$PD.Type[i] <- ProximityDays$ProximityDaysTypeID[PDInd]
    }
    
    if (as.factor(Daypred$Dates[i]) %in% CloseDays$x){
      Daypred$CloseDays[i] <- TRUE
    }
  }
  
  
  
  
  
  
  ########## IV: Deal with Exceptional&Proximity days: using annual history or DayTypeID to groupe data
  # 0: Deal with Proximity days first, which can be replace by exceptional day replacement
  # 1: Find history Exceptional days
  # 2: Create exponential factors
  # 3: Replace exceptional days with history
  
  
  # 0: Deal with Proximity days first, which can be replace by exceptional day replacement
  Ind <- c()
  Ind <- which(Daypred$ProximityDays)
  if (length(Ind)>0){
    for (j in 1:length(Ind)){
      ProximityDayTypeID <- Daypred$PD.Type[Ind[j]]
      Hist.Dates <- sort(CleanedData$Dates[which(CleanedData$PD.Type == ProximityDayTypeID)])
      
      if (length(Hist.Dates)>0){
        ProximityDayCoeff<-ExponentialCoeff(length(Hist.Dates), 0.65)
        Daypred$Rev2_BoxCox[Ind[j]]<-sum(ProximityDayCoeff*(CleanedData$X_coeff[which(CleanedData$Dates %in% Hist.Dates)]*CleanedData$Values_BoxCox[which(CleanedData$Dates %in% Hist.Dates)]))
      }
    }
  }
  
  
  
  # Special days
  Hist.Dates<-c()
  Ind <- c()
  Ind <- which(Daypred$SpecialDays)
  if (length(Ind)>0){
    for (i in 1:length(Ind)){
      if (Daypred$SD.Annual[Ind[i]]){ 
        #Deal with annually repeated special days
        Dates<-Daypred$Dates[Ind[i]]
        
        # 1: Find history Exceptional days
        Hist.Dates <-c(Dates %m-% years(1) ) 
        while (Hist.Dates[1] > as.Date(FirstDate)){
          Hist.Dates <- c(Hist.Dates[1] %m-% years(1), Hist.Dates) #Old to New, i.e.[2013, 2014, 2015]
        }
        if (Hist.Dates[1] < as.Date(FirstDate)){
          Hist.Dates <-Hist.Dates[2:length(Hist.Dates)]
        }
      }else { 
        #Otherwise use SpecialDayTypeID to group and forecast
        SpecialDayTypeID <- Daypred$SD.Type[Ind[i]]
        Hist.Dates <- sort(CleanedData$Dates[which(CleanedData$SD.Type == SpecialDayTypeID)])
      } 
      
      # 2: Create exponential factors and Update the forecast if there is history
      if (length(Hist.Dates)>0){
        ExceptionalDayCoeff<-ExponentialCoeff(length(Hist.Dates), 0.65)
        
        # 3: Replace exceptinal days with history
        Daypred$Rev2_BoxCox[Ind[i]]<-sum(ExceptionalDayCoeff*(CleanedData$X_coeff[which(CleanedData$Dates %in% Hist.Dates)]*CleanedData$Values_BoxCox[which(CleanedData$Dates %in% Hist.Dates)]))
      }
    }
  }
  
  ##########  V: Convert back from BoxCox transformation to original format
  Lambda <- InputData[["lambda"]]
  Daypred$Rev2_Orig <- InvBoxCox(Daypred$Rev2_Boxcox,Lambda)
  Daypred$Rev2_Orig[which(Daypred$Rev2_Orig<0)]<-0
  
  
  
  
  ########## VI: Deal with CloseDays. Change to 0 if it's a close day; and repalced by mean if it's not but = 0 
  Daypred$Rev2_Orig[which(Daypred$CloseDays)]<-0
  for (i in 1:nrow(Daypred)){
    if ((!Daypred$CloseDays[i]) & (Daypred$Rev2_Orig[i]==0) & (!Daypred$SpecialDays[i]) ){
      Daypred$Rev2_Orig[i]<- mean(Daypred$Rev2_Orig) 
    }
  }
  
  return(Daypred)
}