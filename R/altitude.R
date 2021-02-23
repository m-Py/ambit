#' Extract altitude data from an openambit log file
#' 
#' @param log_file The path to an openambit log file
#' 
#' @return A data.frame with two columns \code{altitude} and
#'    \code{distance} containing the logs for altitude by distance within
#'    the track.
#' @export
#' 
#' @importFrom xml2 xml_integer
#' 

ambit_get_altitude <- function(log_file) {
  xml <- read_xml(log_file)
  periodic <- type_nodes_from_xml(xml, "periodic")
  data.frame(
    distance = xml_integer(xml_find_all(periodic, "Distance")),
    altitude = xml_integer(xml_find_all(periodic, "Altitude"))    
  )
}
