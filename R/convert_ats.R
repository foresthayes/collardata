#' Convert ATS collar data into common format
#'
#' @param data data frame containing raw ATS data. See combine function for merging csv files.
#'
#' @return Returns collar data converted into common format
#' @export
#'
#' @examples   ATS_out <- convert_ATS(ATS_combined)

convert_ATS <- function(data = ATS_combined){

  # Generate new dataframe
  names <- c("obs", "ID", "date_time", "UTM_zone", "easting", "northing", "lat", "lon", "elev", "DOP", "numsats", "fixtime", "mort_status", "main_v", "beacon_v", "temperature")
  new <- data.frame(matrix(NA, nrow = nrow(data), ncol = length(names)))
  colnames(new) <- names

  # Transfer to common format
  new$ID <- as.factor(data$CollarSerialNumber)
  new$lat <- data$Latitude
  new$lon <- data$Longitude
  new$DOP <- data$HDOP
  new$numsats <- data$NumSats
  new$fixtime <- data$FixTime

  # Convert date and time
  year <- paste(20, data$Year, sep = "")
  date <- paste(year, 01, 01, sep = "/")
  time <- paste(data$Hour, "00", "00", sep = ":")
  date_time <- as.POSIXct(paste(date, time, sep = " "), tz = "MST")
  new$date_time <- date_time

  return(new)
}
