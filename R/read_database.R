#' Read latest local database
#'
#' @param data_folder Folder containing database to be read in.
#'
#' @return Copy of target database
#' @export
#'
#' @examples read_database("data/ats")

read_database <- function(data_folder = NA){

  # Identify the most recent database file
  database_file <- tail(list.files(here::here(data_folder), pattern = "csv", full.names = T))

  # Read in the latest database file
  database <- read.delim(database_file, sep = ",")

  # Return the database
  return(database)
}
