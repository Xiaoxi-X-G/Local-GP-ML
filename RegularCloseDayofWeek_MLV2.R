RegularCloseDayofWeek_MLV2 <- function(CloseDays, NoOfWeek){
  #### Input1: CloseDays = Date
  #### Input2: Total Number of weeks
  #### Output: Name of the days that are regularly closed
  
  CloseDayTime2 <- data.frame(CloseDays, DayOfWeek = weekdays(as.Date(CloseDays)))
  AllNames <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  
  x <- c()
  
  for (i in length(AllNames)){
    if (length(which(CloseDayTime2$DayOfWeek == AllNames[i])) > (2/3 * NoOfWeek))
      x <- c(x, AllNames[i])
  }
  
  CloseDayofWeek <- data.frame(x)
  return (CloseDayofWeek)
}