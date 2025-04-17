#' @title       plotMap
#' @description This function plots a raster object of halfdegree
#'              resolution (with 67420 grid cells) as world map
#'              and saves it as PDF
#'
#' @param x            MAgPIE object in grid-cellular (67420) resolution
#'                     to be plotted
#' @param projection   Choose projection.
#'                     Currently available options:
#'                     "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#'                     for EqualEarth projection;
#'                     "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#'                     for RobinsonProj; and
#'                     "+proj=longlat +datum=WGS84" for LatLon projection
#' @param outputtype   Output type: pdf or jpeg
#' @param outputfolder Path to which plot should be saved
#' @param name         Title of plot
#'                     (default: "name")
#' @param title        Plot title displayed in the plot
#' @param titlesize    Text size of plot title
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
#' @param legendtextsize text size of legendtext
#' @param minVal       minimum value at which x should be chopped
#' @param maxVal       maximum value at which x should be chopped
#'
#' @importFrom magclass as.RasterBrick collapseDim
#' @importFrom grDevices jpeg png pdf dev.off
#' @import sp
#' @import sf
#'
#' @return map as pdf, jpg or png
#' @author Felicitas Beier, Jens Heinke
#'
#' @export

plotMap <- function(x,
                    projection = "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                    outputfolder = ".\\",
                    name = "name",
                    title = "",
                    titlesize = 3,
                    ylim = c(-6500000, 8300000),
                    xlim = c(-12577316, 15581284),
                    legendcolor = c("#7f0000", "#b30000", "#d7301f", "#ef6548", "#fc8d59",
                                    "#fdbb84", "#fdd49e", "#fee8c8", "#fff7ec", "#045a8d"),
                    colNA = "#d9d9d9",
                    legendlimit = c(0, 1),
                    legendbreaks = seq(0, 1, 0.1),
                    legendname = "legendname",
                    legendtextsize = 2,
                    outputtype = "png",
                    minVal = NULL, maxVal = NULL) {

  ####################
  ### Prepare data ###
  ####################
  # Get land mask and country borders
  tmp            <- toolPrepareLandMask(projection = projection)
  landMask       <- tmp$landMask
  worldCountries <- tmp$worldCountries

  # Transform magpie object to raster
  x                  <- collapseDim(x)
  getSets(x)["d1.1"] <- "x"
  getSets(x)["d1.2"] <- "y"
  getSets(x)["d1.3"] <- "iso"
  x <- toolRasterTransform(x = x,
                           projection = projection)

  # Chop off min and max values
  if (!is.null(minVal)) {
    x[x < minVal]  <- minVal
  }
  if (!is.null(maxVal)) {
    x[x >= maxVal]  <- maxVal
  }

  #####################
  ### Plot and save ###
  #####################
  # Choose output type
  if (outputtype == "pdf") {

    pdf(paste0(outputfolder, name, ".pdf"), width = 26, height = 15)

  } else if (outputtype == "jpeg") {

    jpeg(paste0(outputfolder, name, ".jpeg"),
         width = 8, height = 4.5, units = "in", res = 400)


  } else if (outputtype == "png") {

    png(paste0(outputfolder, name, ".png"),
        width = 4000, height = 2200, units = "px", res = 200)

  } else {
    stop("Please select output type of graph:
         `pdf`, `png` or `jpeg`")
  }

  # Map plot
  terra::plot(landMask, col = "white", border = "white",
              ylim = ylim, xlim = xlim, asp = NA,
              axes = FALSE)
  terra::plot(x, border = "white",
              ylim = ylim, xlim = xlim,  asp = NA,
              axes = FALSE,
              legend = FALSE,
              col = legendcolor,
              colNA = colNA,
              breaks = legendbreaks,
              add = TRUE)
  terra::plot(landMask, col = "white", border = "white",
              ylim = ylim, xlim = xlim,  asp = NA,
              axes = FALSE, add = TRUE)
  terra::plot(worldCountries,
              ylim = ylim, xlim = xlim,  asp = NA,
              axes = FALSE, add = TRUE)

  # Legend
  terra::plot(x,
              ylim = ylim, xlim = xlim, asp = NA, axes = FALSE, add = TRUE,
              col = legendcolor,
              range = legendlimit,
              legend.only = TRUE,
              plg = list(title = legendname,
                         horiz = FALSE,   # Legend orientation
                         title.cex = legendtextsize, # Legend title size
                         cex = legendtextsize,       # Legend text size
                         ext = c(xlim[[1]], xlim[[1]] + 1000000,
                                 ylim[[1]], 0), # Legend position
                         at = legendbreaks))

  # Title
  title(title,  line = 2, col.main = "black", cex.main = titlesize)

  dev.off()

}
