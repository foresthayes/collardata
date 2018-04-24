#' Combine CSV files
#'
#' @param data_folder Folder that CSV files are contained within
#' @param file_type Extensions of files to combine.  Default is .txt and .csv
#' @param sep Delimiter of data.  Defailt is "," i.e. for .csv files
#'
#' @return Returns a data table of unique rows (removes duplicates)
#' @export
#'
#' @examples ATS_collar_data <- combine("data/ATS")
combine <- function(data_folder = "data", file_type = c("csv", "txt"), sep = ","){

  # List files within "data_folder" directory
  file_list <- list.files(
    here::here(data_folder),
    pattern = file_type,
    full.names = T)

  # Combine files into single data frame
  combined_data <- do.call(rbind,lapply(file_list,read.delim, sep = sep)) # Possible to change to read.table to handle .txt or .csv?

  # Return only unique rows
  unique(combined_data[1:length(combined_data[1, ])])
}
