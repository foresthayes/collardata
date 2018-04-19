#' Combine CSV files
#'
#' @param data_folder Folder that CSV files are contained within
#'
#' @return Returns a data table of unique rows (removes duplicates)
#' @export
#'
#' @examples ATS_collar_data <- combine("data/ATS")
combine <- function(data_folder = "data"){

  # List files within "data_folder" directory
  file_list <- dir(here::here(data_folder))

  # Set directory to data_folder location
  setwd(here::here(data_folder))

  # Combine csv files into single data frame
  combined_data <- do.call(rbind,lapply(file_list,read.csv)) # Possible to change to read.table to handle .txt or .csv?

  # Return only unique rows
  unique(combined_data[1:length(combined_data[1, ])])
}
