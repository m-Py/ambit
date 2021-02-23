
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
    lon = ambit_to_decimal_gps(lon),
    lat = ambit_to_decimal_gps(lat)
  )
}

# This function pretty much just adds a decimal number at the right point
# to the coordinates because the AMBIT xml file does not have them.
# Ambit GPS data has 7 numbers after the decimal point.
# This function might have problems with "negative" GPS coordinates,
# but unfortunately I cannot test that right now.
ambit_to_decimal_gps <- function(coords) {
  # split all coordinates and extract numbers before and after the decimal point
  splitted <- strsplit(xml_text(coords), "")
  ndecimals <- lapply(splitted, length)
  N <- ndecimals[[1]]
  # Problem if not all coordinates have the same precision:
  stopifnot(all(ndecimals == N))
  # split before and after decimal point
  suffixes <- lapply(splitted, function(x) rev(x[N:(N-6)]))
  prefixes <- lapply(splitted, function(x) x[1:(N-7)])
  suffixes <- lapply(suffixes, paste0, collapse = "")
  prefixes <- lapply(prefixes, paste0, collapse = "")
  as.numeric(paste(prefixes, suffixes, sep = "."))  
}

type_nodes_from_xml <- function(xml, type) {
  log_data <- xml_child(xml, "Log")
  samples <- xml_child(log_data, "Samples")
  samples <- xml_children(samples)
  selector <- grepl(pattern = type, x = xml_text(samples)) # select gps samples
  samples[selector]
}

