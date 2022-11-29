#' @title downloadNaturalEarth
#' @description download Natural Earth shape files
#'
#' @import madrat
#' @importFrom utils download.file unzip
#'
#' @author  Felicitas Beier
#' @md
#' @seealso [downloadSource()]
#' @examples
#' \dontrun{
#' a <- downloadSource("NaturalEarth")
#' }
#'

downloadNaturalEarth <- function() {

  url <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip" # nolint

  download.file(url, destfile = "data.zip")

  unzip("data.zip")

  return(list(url          = url,
              title        = "Admin 0 Countries",
              revision     = "5.1.1",
              release_date = "2009-2022",
              author       = "Natural Earth. All rights reserved."))
}
