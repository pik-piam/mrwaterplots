#' @title       plotMapLayered
#' @description This function plots several magpie objects of halfdegree
#'              resolution (with 67420 grid cells) as overlayed map
#'
#' @param x            list of MAgPIE objects in grid-cellular (67420) resolution
#'                     to be plotted
#' @param projection   Choose projection.
#'                     Currently available options:
#'                     "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#'                     for EqualEarth projection;
#'                     "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#'                     for RobinsonProj; and
#'                     "+proj=longlat +datum=WGS84" for LatLon projection
#' @param outputtype   Output type: pdf or jpeg or png
#' @param outputfolder Path to which plot should be saved
#' @param name         Title of plot
#'                     (default: "name")
#' @param title        Plot title displayed in the plot
#' @param ylim         y-axis limits of plot
#'                     (default: c(-6500000, 8300000))
#' @param xlim         x-axis limits of plot
#'                     (default: c(-12577316, 15581284))
#' @param legendcolor  List of vector of colors of the same length as the list of objects
#'                     provided
#' @param legendtitles Vector for title for legendcolors.
#'                     Should have the same length as the list of legendcolor
#' @param naObject     magpie object that defines which areas to mask out as NA
#'                     with 1 being the ones to be masked out and 0 the ones
#'                     that are transparent, i.e. show the previous plots
#' @param legendlimit  vector with min and max of legend
#'                     (default: c(0, 1))
#' @param legendbreaks vector of legend breaks
#'                     (default: seq(0, 1, 0.1))
#' @param legendname   legend name as character
#'                     (default: "legendname")
#' @param minVal       minimum value at which x should be chopped
#' @param maxVal       maximum value at which x should be chopped
#'
#' @importFrom magclass as.RasterBrick collapseDim
#' @importFrom grDevices jpeg png pdf dev.off
#' @importFrom graphics rect text
#' @import sp
#' @import sf
#'
#' @return map as pdf, jpg or png
#' @author Felicitas Beier, Jens Heinke
#'
#' @export

plotMapLayered <- function(x,
                           projection = "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", # nolint: line_length_linter
                           outputfolder = ".\\",
                           name = "name",
                           title = "",
                           ylim = c(-6500000, 8300000),
                           xlim = c(-12577316, 15581284),
                           legendcolor = list(c("#dfc27d", "#bf812d", "#8c510a", "#543005"),
                                              c("#80cdc1", "#35978f", "#01665e", "#003c30")),
                           legendtitles = c("scale1", "scale2"),
                           legendname = "legendtitle (unit)",
                           naObject = NULL,
                           legendlimit = c(0, 1),
                           legendbreaks = seq(0, 1, 0.25),
                           outputtype = "png",
                           minVal = NULL, maxVal = NULL) {
  # Checks
  if (!is.list(x)) {
    stop("Please provide magpie objects as list to plotMapLayered")
  }
  if (!is.list(legendcolor)) {
    stop("Please provide legendcolor as list to plotMapLayered")
  }

  ####################
  ### Prepare data ###
  ####################
  # Get land mask and country borders
  tmp            <- toolPrepareLandMask(projection = projection)
  landMask       <- tmp$landMask
  worldCountries <- tmp$worldCountries

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
  withr::with_par(list(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0)), {

    terra::plot(landMask, col = "white", border = "white",
                ylim = ylim, xlim = xlim, asp = NA,
                axes = FALSE)

    for (i in seq(1, length(x), 1)) {
      # Transform magpie object to raster
      plotObject <- collapseDim(x[[i]])
      getSets(plotObject)["d1.1"] <- "x"
      getSets(plotObject)["d1.2"] <- "y"
      getSets(plotObject)["d1.3"] <- "iso"
      plotObject <- toolRasterTransform(x = plotObject,
                                        projection = projection)
      # Chop off min and max values
      if (!is.null(minVal)) {
        plotObject[plotObject < minVal]  <- minVal
      }
      if (!is.null(maxVal)) {
        plotObject[plotObject >= maxVal]  <- maxVal
      }

      # Map
      terra::plot(plotObject, bg = "transparent", border = "white",
                  ylim = ylim, xlim = xlim, asp = NA,
                  axes = FALSE,
                  legend = FALSE,
                  col = legendcolor[[i]],
                  breaks = legendbreaks,
                  colNA = NA,
                  add = TRUE)
    }
    if (!is.null(naObject)) {
      # Transform magpie object to raster
      naObject                  <- collapseDim(naObject)
      getSets(naObject)["d1.1"] <- "x"
      getSets(naObject)["d1.2"] <- "y"
      getSets(naObject)["d1.3"] <- "iso"
      naObject <- toolRasterTransform(x = naObject,
                                      projection = projection)
      # grey mask for NA's
      terra::plot(naObject, bg = "transparent", border = "white",
                  ylim = ylim, xlim = xlim, asp = NA,
                  axes = FALSE,
                  legend = FALSE,
                  col = c("transparent", "#d9d9d9"),
                  breaks = c(0, 0.5, 1),
                  colNA = NA,
                  add = TRUE)
    }
    terra::plot(landMask, col = "white", border = "white",
                ylim = ylim, xlim = xlim,  asp = NA,
                axes = FALSE, add = TRUE)
    terra::plot(worldCountries,
                ylim = ylim, xlim = xlim,  asp = NA,
                axes = FALSE, add = TRUE)

    # Legend
    plotCustomLegend(ticks = legendbreaks,
                     colorVectors = legendcolor,
                     boxTitles = legendtitles,
                     legendTitle = legendname,
                     legendPosX = mean(xlim),
                     legendPosY = ylim[1] + (ylim[2] - ylim[1]) * 0.05,
                     boxWidth = (xlim[2] - xlim[1]) * 0.3,
                     boxHeight = (ylim[2] - ylim[1]) * 0.025)

    # Title
    title(title, line = 2, col.main = "black", cex.main = 1.5)
  })
  dev.off()
}
