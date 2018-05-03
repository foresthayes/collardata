#' Read multiple collar data files from computer hard drive
#'
#' @param data_folder Folder location(s) of collar data files relative to parent directory
#' @param file_type Default is csv and txt
#' @param sep Delineator of data within files - default is ","
#'
#' @return Returns a list of dataframes, one for each folder
#' @export
#'
#' @examples combined <- read.files(data_folder = c("data/Vectronics", "data/ATS", "data/database"))

read_files <- function( data_folder = "data", file_type = c("csv", "txt"), sep = ","){

  # create empty list for output
  out <- as.list(data_folder)


  # Repeat for each value of data_folder
  for (i in 1:length(data_folder)){

    # Read in old database if folder name "database" is one of the data folders listed
    if(str_sub(data_folder[i], -8L, end = -1L) == "database"){

      # Identify latest file location
      database_file_location <- tail(list.files(here::here(data_folder), pattern = file_type, full.names = T), n = 1)

      # Read lastest file into database
      latest_database <- read.delim(database_file_location, sep = sep)

      # Convert database time to POSIXct
      # latest_database$date_time <- as.POSIXct(latest_database$date_time, tz = "MST")

      out[[i]] <- latest_database

    } else {

      # create empty vector for file_list
      file_list <- as.list(data_folder)

      # Read read path for each data file
      file_list[[i]] <-  list.files(here::here(data_folder[i]), pattern = file_type, full.names = T)

      out[[i]] <- do.call(rbind, lapply(file_list[[i]], read.delim, sep = sep))

      } # end of else statement


  }
  return(out)

}
