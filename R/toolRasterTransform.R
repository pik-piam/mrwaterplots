#' @title       toolRasterTransform
#' @description This function transforms a cellular magpie object
#'              with 67420 grid cells to an raster object that can
#'              be plotted in the chosen projection
#'
#' @param x          MAgPIE object in grid-cellular (67420) resolution
#'                   to be transformed to raster
#' @param projection Choose projection.
#'                   Currently available options:
#'                   "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#'                   for EqualEarth projection
#'
#' @importFrom magclass as.RasterBrick collapseNames
#' @importFrom raster projectRaster
#' @importFrom terra crs
#' @import sp
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @export

toolRasterTransform <- function(x,
                                projection = "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") { # nolint
  # Remove dimension names
  x <- collapseNames(x)

  # Note: terra cannot transform raster object to Equal-Earth projection yet
  x0  <- as.RasterBrick(x)
  out <- raster::projectRaster(x0, crs = terra::crs(projection), over = TRUE)

  return(out)
}
