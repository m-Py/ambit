
#' Extract GPS data from an openambit log file
#' 
#' @param log_file The path to an openambit log file
#' 
#' @return A data.frame with two columns \code{lon} and
#'    \code{lat} containing the GPS logs for longitude and latitude,
#'    respectively.
#' @export
#' 
#' @importFrom xml2 read_xml xml_text xml_child xml_children xml_find_all
#' 
ambit_get_gps <- function(log_file) {
  xml <- read_xml(log_file)
  gps_data <- type_nodes_from_xml(xml, "gps-small")
  # Select Latitude data
  lat <- xml_find_all(gps_data, "Latitude")
  # Select Longitude data
  lon <- xml_find_all(gps_data, "Longitude")
  data.frame(
    lon = xml_integer(lon) / 10000000,
    lat = xml_integer(lat) / 10000000
  )
}

type_nodes_from_xml <- function(xml, type) {
  log_data <- xml_child(xml, "Log")
  samples <- xml_child(log_data, "Samples")
  samples <- xml_children(samples)
  selector <- grepl(pattern = type, x = xml_text(samples))
  samples[selector]
}
