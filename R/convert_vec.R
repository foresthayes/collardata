#' Convert raw vectronics GPS collar data into common format
#'
#' @param data Dataframe containing raw Vectronics data to be converted
#'
#' @return Return data frame of converted data in common format
#' @export
#'
#' @examples   vec_out <- convert_vec(vec_combined)

convert_vec <- function(data = vec_combined){

  # Generate new dataframe
  names <- c("obs", "ID", "date_time", "UTM_zone", "easting", "northing", "lat", "lon", "elev", "DOP", "numsats", "fixtime", "mort_status", "main_v", "beacon_v", "temperature")
  new <- data.frame(matrix(NA, nrow = nrow(data), ncol = length(names)))
  colnames(new) <- names

  # Transfer values to new format
  new$obs <- data$No
  new$ID <- as.factor(data$CollarID)

  # Convert date and time to MST
  UTCdatetime <- strptime(paste(data$UTC_Date, data$UTC_Time, sep = " "), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
  date_time <- lubridate::with_tz(UTCdatetime, tz = "MST")

  new$date_time <- date_time
  new$lat <- data$Latitude....
  new$lon <- data$Longitude....
  new$elev <- data$Height..m.
  new$DOP <- data$DOP
  new$mort_status <- as.factor(data$Mort..Status)
  new$main_v <- data$Main..V.
  new$beacon_v <- data$Beacon..V.
  new$temperature <- data$Temp...C.
  # new$UTM_zone <- substr(data$Easting, 0, 2)
  # new$easting <- substring(data$Easting, 2)
  # new$northing <- data$Northing

  return(new)
}
