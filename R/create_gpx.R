#' Create GPX file from dataframe of GPS collar data
#'
#' @param data Dataframe containing GPS point data
#' @param savelocation Location to save output GPX file within parent directory
#'
#' @return Create GPX file with latest points
#' @export
#'
#' @examples create_gpx(latest_position)

create_gpx <- function(data = "latest_position", savelocation = "data/GPX"){

  # Set Projection
  latslongs <- SpatialPointsDataFrame(data.frame(data$lon, data$lat), data = data, proj4string = CRS ("+proj=longlat + ellps=WGS84"))

  # Add point names
  latslongs@data$name <- paste(data$ID, format(data$date_time, "%Y-%m-%d"), paste("h",format(data$date_time,"%H"), sep = ""), sep = " ")

  # Write GPX file
  writeOGR(latslongs["name"], dsn=here::here(savelocation, paste("Points"," ", Sys.Date(), " h", format(Sys.time(), "%H"), ".gpx", sep = "")),
           dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)
}
