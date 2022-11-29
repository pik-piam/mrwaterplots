#' @title       calcCellAreaShare
#' @description This function transforms a cellular magpie object
#'              with 67420 grid cells to an raster object that can
#'              be plotted in the chosen projection
#'
#' @param x     MAgPIE object in grid-cellular (67420) resolution
#'              containing area in Mha
#'              for which cell area share is to be calculated
#'
#' @importFrom magclass as.magpie getItems
#' @importFrom madrat toolGetMapping
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @export

calcCellAreaShare <- function(x) {

  # Calculate cell area (in Mha)
  mapping            <- madrat::toolGetMapping("LPJ_CellBelongingsToCountries.csv",
                                       type = "cell")
  # Transform: square meter -> Mha (1ha = 10000m^2)
  cellarea           <- (111e3 * 0.5) * (111e3 * 0.5) * cos(mapping$lat / 180 * pi) / 1e+10
  cellarea           <- magclass::as.magpie(cellarea, spatial = 1)
  magclass::getItems(cellarea, dim = 1) <- magclass::getItems(x, dim = 1)

  # Cell area share
  out <- ifelse(cellarea > 0,
                  x / cellarea,
                0)

  return(list(x            = out,
              weight       = NULL,
              unit         = "share",
              description  = "Cell area share",
              isocountries = FALSE))

}
