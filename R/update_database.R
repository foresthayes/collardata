#' Update database file and produce GPX of latest positions
#'
#' @param range Cut off number of days old for GPS positions when creating GPX file
#'
#' @return
#' @export
#'
#' @examples update_database()

update_database <- function(range = 180){

  # Combine CSV files for both ATS and Vectronics folders
  ATS_combined <- combine(data_folder = "data/ATS")
  vec_combined <- combine(data_folder = "data/Vectronics")

  # Convert to common data format
  ATS_out <- convert_ATS(ATS_combined)
  vec_out <- convert_vec(vec_combined)

  # Read in old database
  old_database <- read.csv(here::here("data/CombinedOutput", tail(dir(here::here("data/CombinedOutput")), n = 1)))
  old_database$date_time <- as.POSIXct(old_database$date_time, tz = "MST")

  # Combine new ATS and Vectronics data  with old database
  all_collars <- unique(rbind(old_database, ATS_out, vec_out))

  # Write new database to CSV
  write.csv(all_collars, here::here("data/CombinedOutput", paste("Combined"," ", Sys.Date(), ".csv", sep = "")), row.names = F)

  # GPX file
  # find latest position for each animal
  latest_position <- latest(all_collars, range)

  # Create GPX file from latest data points
  create_gpx(latest_position)

}
