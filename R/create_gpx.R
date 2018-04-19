#' Create GPX file from dataframe of GPS collar data
#'
#' @param data Dataframe containing GPS collar data
#'
#' @return Create GPX file with latest points
#' @export
#'
#' @examples create_gpx(latest_position)

create_gpx <- function(data = "latest_position"){

  require(rgdal)

  latslongs <- SpatialPointsDataFrame(data.frame(data$lon, data$lat), data = data, proj4string = CRS ("+proj=longlat + ellps=WGS84"))

  # Add point names
  latslongs@data$name <- paste(data$ID, format(data$date_time, "%m-%d"), paste("h",format(data$date_time,"%H"), sep = ""), sep = " ")


  writeOGR(latslongs["name"], dsn=here::here("data/LatestPoints", paste("Points"," ", Sys.Date(), " h", format(Sys.time(), "%H"), ".gpx", sep = "")),
           dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)
}
