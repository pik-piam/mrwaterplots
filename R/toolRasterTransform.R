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
#'                   for EqualEarth projection;
#'                   "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#'                   for RobinsonProj; and
#'                   "+proj=longlat +datum=WGS84" for LatLon projection
#'
#' @importFrom magclass as.RasterBrick collapseNames
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

  # Transform to raster
  x0  <- as.RasterBrick(x)
  # Reproject to chosen projection
  # Note: method "near" chosen (good method for discrete values)
  # Consider using other method for continuous values
  out <- terra::project(terra::rast(x0), projection,
                        method = "near", mask = TRUE)

  return(out)
}
