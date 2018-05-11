#' Convert collar data from multiple manufacturers into a common format
#'
#' @param data List of data frames to convert to a common format.  Automatically recognizes ATS and Vectronics based on dataframe names.
#'
#' @return Returns a single dataframe of combined and converted data
#' @export
#'
#' @examples converted_collardata <- convert_format(collardata)

convert_format <- function(data){

  # Create list for converted data
  out <- list()

  # common format names
  names <- c("obs", "ID", "date_time", "UTM_zone", "easting", "northing", "lat", "lon", "elev", "DOP", "numsats", "fixtime", "activity", "mort_status", "main_v", "beacon_v", "temperature")


  # Convert ATS collar data
  for(i in 1:length(data)){

    if(identical(names(data[[i]]), c("CollarSerialNumber","Year","Julianday","Hour","Latitude","Longitude","HDOP","NumSats","FixTime","X2D.3D"))){

      # Generate new dataframe
      new <- data.frame(matrix(NA, nrow = nrow(data[[i]]), ncol = length(names)))
      colnames(new) <- names

      # Transfer to common format
      new$ID <- as.factor(data[[i]]$CollarSerialNumber)
      new$lat <- data[[i]]$Latitude
      new$lon <- data[[i]]$Longitude
      new$DOP <- data[[i]]$HDOP
      new$numsats <- data[[i]]$NumSats
      new$fixtime <- data[[i]]$FixTime

      # Convert date and time
      year <- paste(20, data[[i]]$Year, sep = "")
      date <- paste(year, 01, 01, sep = "/")
      time <- paste(data[[i]]$Hour, "00", "00", sep = ":")
      date_time <- as.POSIXct(paste(date, time, sep = " "), tz = "MST")
      new$date_time <- date_time

      out[[i]] <- new

    } # closing ats

    # Convert ATS iridium collar data
    if(identical(names(data[[i]]), c("CollarSerialNumber","Year","Julianday","Hour", "Minute", "Activity", "Temperature", "Latitude","Longitude","HDOP","NumSats","FixTime","2D/3D"))){

      # Generate new dataframe
      new <- data.frame(matrix(NA, nrow = nrow(data[[i]]), ncol = length(names)))
      colnames(new) <- names

      # Transfer to common format
      new$ID <- as.factor(data[[i]]$CollarSerialNumber)
      new$activity <- data[[i]]$Activity
      new$temperature <- data[[i]]$Temperature
      new$lat <- data[[i]]$Latitude
      new$lon <- data[[i]]$Longitude
      new$DOP <- data[[i]]$HDOP
      new$numsats <- data[[i]]$NumSats
      new$fixtime <- data[[i]]$FixTime

      # Convert date and time
      year <- paste(20, data[[i]]$Year, sep = "")
      date <- paste(year, 01, 01, sep = "/")
      time <- paste(data[[i]]$Hour, data$Minute, "00", sep = ":")
      date_time <- as.POSIXct(paste(date, time, sep = " "), tz = "MST")
      new$date_time <- date_time

      out[[i]] <- new

    } # closing ats iridium

    # Convert Vectronics collar data
    if(identical(names(data[[i]]), c("No","CollarID","UTC_Date","UTC_Time","LMT_Date","LMT_Time","Origin","SCTS_Date","SCTS_Time","ECEF_X..m.","ECEF_Y..m.","ECEF_Z..m.","Latitude....","Longitude....", "Height..m.","DOP","FixType","X3D_Error..m.","Sats","Sat","C.N","Sat.1","C.N.1","Sat.2","C.N.2","Sat.3","C.N.3","Sat.4","C.N.4","Sat.5","C.N.5","Sat.6","C.N.6","Sat.7","C.N.7","Sat.8","C.N.8","Sat.9","C.N.9","Sat.10","C.N.10","Sat.11", "C.N.11","Mort..Status","Activity","Main..V.","Beacon..V.","Temp...C.","No.1","No.2"))){

      # Generate new dataframe
      new <- data.frame(matrix(NA, nrow = nrow(data[[i]]), ncol = length(names)))
      colnames(new) <- names

      # Transfer values to new format
      new$obs <- data[[i]]$No
      new$ID <- as.factor(data[[i]]$CollarID)

      # Convert date and time to MST
      UTCdatetime <- strptime(paste(data[[i]]$UTC_Date, data[[i]]$UTC_Time, sep = " "), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
      date_time <- lubridate::with_tz(UTCdatetime, tz = "MST")

      new$date_time <- date_time
      new$lat <- data[[i]]$Latitude....
      new$lon <- data[[i]]$Longitude....
      new$elev <- data[[i]]$Height..m.
      new$DOP <- data[[i]]$DOP
      new$mort_status <- as.factor(data[[i]]$Mort..Status)
      new$main_v <- data[[i]]$Main..V.
      new$beacon_v <- data[[i]]$Beacon..V.
      new$temperature <- data[[i]]$Temp...C.
      # new$UTM_zone <- substr(data$Easting, 0, 2)
      # new$easting <- substring(data$Easting, 2)
      # new$northing <- data$Northing

      out[[i]] <- new

    } # closing vectronics

  }

  # Combine separate dataframes
  combined <- do.call("rbind", out)

  # Return all data combined into common format
  return(combined)

}
