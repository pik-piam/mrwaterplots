#' @title       plotMap
#' @description This function plots a raster object of halfdegree
#'              resolution (with 67420 grid cells)
#'              and saves it as PDF
#'
#' @param x            MAgPIE object in grid-cellular (67420) resolution
#'                     to be transformed to raster
#' @param projection   Choose projection.
#'                     Currently available options:
#'                     "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#'                     for EqualEarth projection
#' @param outputfolder Path to which plot should be saved
#' @param name         Title of plot
#'                     (default: "name")
#' @param title        Plot title displayed in the plot
#' @param ylim         y-axis limits of plot
#'                     (default: c(-6500000, 8300000))
#' @param xlim         x-axis limits of plot
#'                     (default: c(-12577316, 15581284))
#' @param legendcolor  vector of colors
#' @param colNA        color for NAs
#'                     (default: "#d9d9d9" (gray))
#' @param legendlimit  vector with min and max of legend
#'                     (default: c(0, 1))
#' @param legendbreaks vector of legend breaks
#'                     (default: seq(0, 1, 0.1))
#' @param legendname   legend name as character
#'                     (default: "legendname")
#'
#' @importFrom magclass as.RasterBrick
#' @importFrom raster projectRaster
#' @importFrom terra crs
#' @importFrom grDevices pdf dev.off
#' @import sp
#' @import sf
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @export

plotMap <- function(x,
                    projection = "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                    outputfolder = ".\\",
                    name = "name",
                    title = "",
                    ylim = c(-6500000, 8300000),
                    xlim = c(-12577316, 15581284),
                    legendcolor = c("#7f0000", "#b30000", "#d7301f", "#ef6548", "#fc8d59",
                                    "#fdbb84", "#fdd49e", "#fee8c8", "#fff7ec", "#045a8d"),
                    colNA = "#d9d9d9",
                    legendlimit = c(0, 1),
                    legendbreaks = seq(0, 1, 0.1),
                    legendname = "legendname") {
  # Get land mask and country borders
  tmp            <- toolPrepareLandMask(projection = projection)
  landMask       <- tmp$landMask
  worldCountries <- tmp$worldCountries

  # Transform magpie object to raster
  x <- toolRasterTransform(x = x,
                           projection = projection)

  # Plot and save
  pdf(paste0(outputfolder, name, ".pdf"),
      width = 26, height = 15)

  # Map plot
  plot(landMask, col = "white", border = "white",
       ylim = ylim, xlim = xlim, asp = NA,
       axes = FALSE)
  plot(x, border = "white",
       ylim = ylim, xlim = xlim,  asp = NA,
       axes = FALSE,
       legend = FALSE,
       col = legendcolor,
       colNA = colNA,
       breaks = legendbreaks,
       add = TRUE)
  plot(landMask, col = "white", border = "white",
       ylim = ylim, xlim = xlim,  asp = NA,
       axes = FALSE, add = TRUE)
  plot(worldCountries,
       main = title,
       ylim = ylim, xlim = xlim,  asp = NA,
       axes = FALSE, add = TRUE)

  # Legend
  plot(x,
       legend.only   = TRUE,
       horizontal    = TRUE,
       col           = legendcolor,
       breaks        = legendbreaks,
       colNA         = colNA,
       legend.args   = list(text = legendname,
                            side = 3, font = 1, line = 2, cex = 4),
       axis.args     = list(cex.axis = 4, at = legendbreaks,
                            line = 0, tick = FALSE, hadj = 0.5, padj = 0.5),
       smallplot     = c(0.4, 0.75, 0.05, 0.1),
       add = TRUE)

  dev.off()
}
