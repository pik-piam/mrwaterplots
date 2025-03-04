#' @title       toolCellAreaShare
#' @description This function calculates the share of total grid cell area
#'              for a given cellular magpie object with 67420 grid cells
#'
#' @param x     MAgPIE object in grid-cellular (67420) resolution
#'              containing area in Mha
#'              for which cell area share is to be calculated
#'
#' @importFrom magclass as.magpie getItems getSets getCells
#' @importFrom madrat toolGetMapping
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#'
#' @export

toolCellAreaShare <- function(x) {

  # Calculate cell area (in Mha)
  mapping            <- madrat::toolGetMapping("LPJ_CellBelongingsToCountries.csv",
                                               type = "cell", where = "mappingfolder")
  # Transform: square meter -> Mha (1ha = 10000m^2)
  cellarea           <- (111e3 * 0.5) * (111e3 * 0.5) * cos(mapping$lat / 180 * pi) / 1e+10
  cellarea           <- magclass::as.magpie(cellarea, spatial = 1)
  magclass::getCells(cellarea) <- magclass::getCells(x)

  # Cell area share
  out <- ifelse(cellarea > 0,
                x / cellarea,
                0)

  # Dimension names
  getSets(out)["d1.1"] <- "x"
  getSets(out)["d1.2"] <- "y"
  getSets(out)["d1.3"] <- "iso"

  return(out)

}
