#' @title       plotMapDiscrete
#' @description This function plots a raster object of halfdegree
#'              resolution (with 67420 grid cells)
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
#' @param ylim         y-axis limits of plot
#'                     (default: c(-6500000, 8300000))
#' @param xlim         x-axis limits of plot
#'                     (default: c(-12577316, 15581284))
#' @param legendcolor  vector of colors
#' @param colNA        color for NAs
#'                     (default: "#d9d9d9" (gray))
#' @param legendbreaks vector of legend breaks
#'                     (default: seq(0, 1, 0.1))
#' @param legend       legend element names (if non-numeric characters shall be returned).
#'                     (default: NULL, then legendbreaks defines displayed legend elements)
#' @param legendname   legend name as character
#'                     (default: "legendname")
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

plotMapDiscrete <- function(x,
                            projection = "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", # nolint: line_length_linter.
                            outputfolder = ".\\",
                            name = "name",
                            title = "",
                            ylim = c(-6500000, 8300000),
                            xlim = c(-12577316, 15581284),
                            legendcolor = c("#7f0000", "#b30000", "#d7301f", "#ef6548", "#fc8d59",
                                            "#fdbb84", "#fdd49e", "#fee8c8", "#fff7ec", "#045a8d"),
                            colNA = "#d9d9d9",
                            legendbreaks = seq(0, 1, 0.1),
                            legend = NULL,
                            legendname = "legendname",
                            outputtype = "png",
                            minVal = NULL, maxVal = NULL) {
  # Legend arguments
  if (is.null(legend)) {
    for (i in seq_along(legendcolor)) {
      tmp <- paste0(legendbreaks[i], " - <", legendbreaks[i + 1])
      legend <- c(legend, tmp)
    }
  }

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

    legendtextsize <- 2

    pdf(paste0(outputfolder, name, ".pdf"), width = 26, height = 15)

  } else if (outputtype == "jpeg") {

    legendtextsize <- 1

    jpeg(paste0(outputfolder, name, ".jpeg"),
         width = 8, height = 4.5, units = "in", res = 400)


  } else if (outputtype == "png") {

    legendtextsize <- 2

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
  legend(x = xlim[[1]], y = 0, # legend position (adjust further by inset?)
         bty = "n",
         pch = 22, col = "black", # shape of legend elements
         legend = legend,       # legendtext
         fill = legendcolor,    # legendcolor
         title = legendname,    # legentitle
         cex = legendtextsize)  # legendsize

  # Title
  title(title,  line = 2, col.main = "black", cex.main = 1.5)

  dev.off()

}
