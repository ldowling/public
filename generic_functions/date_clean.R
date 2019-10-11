date_clean <- function(input, hms=FALSE){
  #takes in a vector of dates and cleans them with lubridate
  datetime_orders <- c("ymd_HMS", "ymd_HM", "ymd_H", "ymd_HMS", "ydm_HMS", 
                       "ydm_HM", "ydm_H", "ydm_HMS", "mdy_HMS", "mdy_HM", 
                       "mdy_H", "mdy_HMS", "dmy_HMS", "dmy_HM", "dmy_H", 
                       "dmy_HMS", "ymd", "ydm", "ymd", "mdy", "myd", "mdy", 
                       "dmy", "dym", "dmy", "yq")
  if (hms) {
    parse_date <- lubridate::parse_date_time(input,datetime_orders)
  }else{
    parse_date <- as.Date(lubridate::parse_date_time(input,datetime_orders) )
  }
  return(parse_date)
  }