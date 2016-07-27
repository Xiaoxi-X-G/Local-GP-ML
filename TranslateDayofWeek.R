TranslateDayofWeek <-function(DayofWeek){
  # Find day of week index
  # 1. works for all days with upto 2 days in exception
  # 2. Three types = each day, except X-days, except X&Y-days
  require(combinat)
  AllCombs<-list(t(c(1:7)), as.matrix(combn(7,7)), combn(7,6), combn(7,5), combn(7,2), combn(7,4) ,combn(7,3))
  
  flg<-FALSE
  Ind<-c()
  for (j in 1:length(AllCombs)){
    AllCombs.temp <- AllCombs[[j]]
    for (i in 1:ncol(AllCombs.temp)){
      if (DayofWeek == sum(32*2^((AllCombs.temp[, i] %% 7)-1))){
        Ind <- AllCombs.temp[, i]
        flg <- TRUE # break outloop
        break
      }
    }
    if (flg){break}
  }
  days.of.week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  return(days.of.week[Ind])
  #return(Ind)
}