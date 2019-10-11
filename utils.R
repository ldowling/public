#takes an interval and checks if it's less than a day, hour, minute and converts
#it to those time units, then returns a vector of the converted time and a 
#character string indicating the unit
nice_dur <- function(t){
  u <- "days"
  if (t<1) {
    t <- t*24
    u <- "hours"
    if (t<1) {
      t <- t*60
      u <- "minutes"
      if(t<1){
        t <- t*60
        u <- "seconds"
      }
    }
  }
  return(c(t,u))
}

#given a date returns a season
season <- function(x){
  if (all(class(x)!=c("POSIXct","POSIXt"))) {
    stop("`x` is not a POSIXct object.")
  }else{
    out <- case_when(
                     month(x)%in%c(3:5) ~ "spring",
                     month(x)%in%c(6:8) ~ "summer",
                     month(x)%in%c(9:11) ~ "fall",
                     month(x)%in%c(12,1,2) ~ "winter"
                     )
    return(out)
    }
}

#given a short_code, returns a test plot from ty_count summary DF
test_plot <- function(x){
  n <- which(x==ty_count$sr_short_code)
  plot(as.numeric(ty_count[n,-1]),main=ty_count[n,1], xlab="month", 
       ylab="count", type="b")
}