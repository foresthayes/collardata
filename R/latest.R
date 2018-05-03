#' Return latest position for each animal ID from matric of GPS collar data
#'
#' @param data Data frame with ID and GPS collar data
#' @param range Restrict data to within x number of days from current system time
#'
#' @return
#' @export
#'
#' @examples latest_position <- latest(all_collars, range = 180)

latest <- function(data = data, range = 180){

  latest <- data %>%
    # filter(date_time > now() - months(monthrange)) %>%
    group_by(ID) %>%
    slice(n())

  after <- filter(latest, date_time > Sys.Date()-range)

  return(after)
}
