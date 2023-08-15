#' Extract altitude data from an openambit log file
#' 
#' @param log_file The path to an openambit log file
#' 
#' @return A data.frame with two columns \code{altitude} and
#'    \code{distance} containing the logs for altitude by distance within
#'    the track.
#' @export
#' 
#' @importFrom xml2 xml_integer as_list
#' 

ambit_get_altitude <- function(log_file) {
  xml <- read_xml(log_file)
  periodic <- type_nodes_from_xml(xml, "periodic")
  node_list <- as_list(periodic) # guess this makes it slow =(, but I need to check if both attributes are available
  distance_nodes <- sapply(node_list, function(x) "Distance" %in% names(x))
  altitude_nodes <- sapply(node_list, function(x) "Altitude" %in% names(x))
  # only get data where both informations are available
  select_nodes <- distance_nodes & altitude_nodes
  nodes <- periodic[select_nodes]
  data.frame(
    distance = xml_integer(xml_find_all(nodes, "Distance")),
    altitude = xml_integer(xml_find_all(nodes, "Altitude"))
  )
}
