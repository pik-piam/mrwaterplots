#' @title       toolPrepareLandMask
#' @description This function sets up the land mask and country
#'              polygons to plot magpie objects using raster/terra
#'              with an Equal Earth Projection
#'
#' @param projection Choose projection.
#'                   Currently available options:
#'                   "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" for EqualEarth projection
#'
#' @importFrom terra vect project as.polygons ext symdif
#' @importFrom madrat getConfig
#'
#' @return list of raster files
#' @author Felicitas Beier, Jens Heinke, Patrick v. Jeetze, Jan P. Dietrich
#'
#' @export

toolPrepareLandMask <- function(projection =
                                "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") {

  ### Handle vector data (country shape file and land mask) with terra package ###
  # Country borders from Natural Earth Data (http://www.naturalearthdata.com/downloads/10m-cultural-vectors/).
  # last accessed on Dec. 13th 2021
  worldCountries <- terra::vect(paste0(madrat::getConfig()$sourcefolder, "//NaturalEarth",
                                       "//ne_10m_admin_0_countries.shp"))
  worldCountries <- terra::project(worldCountries, projection)

  # Create land mask for pretty continent borders
  landMask       <- terra::as.polygons(ext(worldCountries))
  landMask       <- terra::symdif(landMask, worldCountries)

  return(list(landMask = landMask,
              worldCountries = worldCountries))
}
