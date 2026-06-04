#' DMS to DD
#' 
#' Convert strings in degree-minute-second format to decimal degrees
#'
#' @param dms string in degree-minute-second format
#'
#' @return numeric coordinates in decimal degrees or NA for failed conversions
#' @export
#'
#' @examples
#'
#' # degree symbol causes roxygen issues in examples, but it works the function
#' dms_examples <- c(
#'   "106 deg 51' 57.37\" W",
#'   "-106D 51m 57.37s W",
#'   "106 deg 51' West"
#' )
#' 
#' dms_to_dd(dms_examples)
#' 
dms_to_dd <- function(dms) {

  d_m_s <- stringr::str_extract_all(dms, "([0-9.]+)")
  d_m_s <- suppressWarnings(lapply(d_m_s, as.numeric))

  dd <- sapply(
    d_m_s,
    \(x) ifelse(length(x) > 2, x[1] + x[2] / 60 + x[3] / 3600, NA_real_)
  )

  is_neg <- stringr::str_detect(dms, "^-") |
    stringr::str_detect(dms, "(w|W|west|West|s|S|south|South)$")

  dd[is_neg] <- abs(dd[is_neg]) * -1

  dd

}
