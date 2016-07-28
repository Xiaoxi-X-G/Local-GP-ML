FindQtrOutliers<-function(Data.temp){
  #### Data.temp = data.frame(Dates, Values)
  #### outliers = numeric arrays
  

  require(zoo)
  
  Data.temp$Qtr <- as.Date(as.yearqtr(Data.temp$Dates))
  AllQtrs <- unique(Data.temp$Qtr)
  outliers <- c()
  for (m in 1:length(AllQtrs)){
    outliers.temp <- boxplot.stats(Data.temp$Values[Data.temp$Qtr==AllQtrs[m]])$out
    outliers <- c(outliers, outliers.temp)
  }
  
  return(outliers)
}