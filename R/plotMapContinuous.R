#' @title       plotMapContinuous
#' @description This function plots a raster object of halfdegree
#'              resolution (with 67420 grid cells) as continuous world map
#'              and saves it as PDF, PNG or JPEG.
#'
#' @param x            MAgPIE object in grid-cellular (67420) resolution
#'                     to be plotted
#' @param projection   Choose projection.
#' @param outputtype   Output type: pdf, png or jpeg
#' @param outputfolder Path to which plot should be saved
#' @param name         Filename of plot
#' @param title        Plot title displayed in the plot
#' @param titlesize    Text size of plot title
#' @param ylim         y-axis limits of plot
#' @param xlim         x-axis limits of plot
#' @param legendcolor  Vector of colors used as anchor points
#'                     for continuous color interpolation
#' @param ncol         Number of interpolated colors used
#'                     in continuous color ramp
#' @param colNA        Color for NAs
#' @param legendlimit  Vector with minimum and maximum values
#'                     of continuous legend
#' @param legendticks  Numeric vector defining tick marks displayed
#'                     on continuous legend
#' @param legendname   Legend title as character
#' @param legendtextsize Text size of legend text
#' @param minVal       Minimum value at which x should be chopped
#' @param maxVal       Maximum value at which x should be chopped
#'
#' @importFrom magclass collapseDim
#' @importFrom grDevices jpeg png pdf dev.off colorRampPalette
#' @importFrom graphics segments
#' @importFrom stats approx
#' @import sp
#' @import sf
#'
#' @return map as pdf, jpg or png
#' @author Felicitas Beier
#'
#' @export
#'
plotMapContinuous <- function(x,
                              projection = "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", # nolint: line_length_linter
                              outputfolder = ".\\",
                              name = "plotname",
                              title = "plot title",
                              titlesize = 3,
                              ylim = c(-6500000, 8300000),
                              xlim = c(-12577316, 15581284),
                              legendcolor = c("#edf8e9", "#00441b"),
                              ncol = 80,
                              colNA = "#d9d9d9",
                              legendlimit = c(0, 1),
                              legendticks = seq(0, 1, 0.2),
                              legendname = "legendname",
                              legendtextsize = 2,
                              outputtype = "png",
                              minVal = NULL,
                              maxVal = NULL) {
  ####################
  ### Prepare data ###
  ####################

  tmp            <- toolPrepareLandMask(projection = projection)
  landMask       <- tmp$landMask
  worldCountries <- tmp$worldCountries

  x                  <- collapseDim(x)
  getSets(x)["d1.1"] <- "x"
  getSets(x)["d1.2"] <- "y"
  getSets(x)["d1.3"] <- "iso"

  x <- toolRasterTransform(x = x,
                           projection = projection)

  if (!is.null(minVal)) {
    x[x < minVal] <- minVal
  }

  if (!is.null(maxVal)) {
    x[x >= maxVal] <- maxVal
  }

  cols <- grDevices::colorRampPalette(legendcolor)(ncol)

  continuousBreaks <- seq(legendlimit[1],
                          legendlimit[2],
                          length.out = ncol + 1)

  #####################
  ### Plot and save ###
  #####################

  if (outputtype == "pdf") {

    pdf(paste0(outputfolder, name, ".pdf"),
        width = 26,
        height = 15)

  } else if (outputtype == "jpeg") {

    jpeg(paste0(outputfolder, name, ".jpeg"),
         width = 8,
         height = 4.5,
         units = "in",
         res = 400)

  } else if (outputtype == "png") {

    png(paste0(outputfolder, name, ".png"),
        width = 4000,
        height = 2200,
        units = "px",
        res = 200)

  } else {

    stop("Please select output type of graph:
         `pdf`, `png` or `jpeg`")
  }

  ################
  ### Map plot ###
  ################

  terra::plot(landMask,
              col = "white",
              border = "white",
              ylim = ylim,
              xlim = xlim,
              asp = NA,
              axes = FALSE)

  terra::plot(x,
              border = "white",
              ylim = ylim,
              xlim = xlim,
              asp = NA,
              axes = FALSE,
              legend = FALSE,
              col = cols,
              colNA = colNA,
              breaks = continuousBreaks,
              smooth = FALSE,
              add = TRUE)

  terra::plot(landMask,
              col = "white",
              border = "white",
              ylim = ylim,
              xlim = xlim,
              asp = NA,
              axes = FALSE,
              add = TRUE)

  terra::plot(worldCountries,
              ylim = ylim,
              xlim = xlim,
              asp = NA,
              axes = FALSE,
              add = TRUE)

  #########################
  ### Continuous legend ###
  #########################

  withr::with_par(xpd = TRUE)

  legendWidth  <- diff(xlim) * 0.28
  legendHeight <- diff(ylim) * 0.025

  legendX0 <- mean(xlim) - legendWidth / 2
  legendX1 <- mean(xlim) + legendWidth / 2

  legendY0 <- ylim[1] + diff(ylim) * 0.03
  legendY1 <- legendY0 + legendHeight

  legendX <- seq(legendX0,
                 legendX1,
                 length.out = 300)

  legendCols <- grDevices::colorRampPalette(legendcolor)(
    length(legendX) - 1
  )

  for (i in seq_len(length(legendX) - 1)) {

    rect(legendX[i],
         legendY0,
         legendX[i + 1],
         legendY1,
         col = legendCols[i],
         border = NA)
  }

  rect(legendX0,
       legendY0,
       legendX1,
       legendY1,
       border = "black")

  tickX <- approx(legendlimit,
                  c(legendX0, legendX1),
                  xout = legendticks)$y

  segments(tickX,
           legendY0,
           tickX,
           legendY0 - diff(ylim) * 0.008)

  text(tickX,
       legendY0 - diff(ylim) * 0.025,
       labels = legendticks,
       cex = legendtextsize)

  text(mean(c(legendX0, legendX1)),
       legendY1 + diff(ylim) * 0.012,
       labels = legendname,
       cex = legendtextsize)

  withr::with_par(xpd = FALSE)

  ################
  ### Plot title #
  ################

  title(title,
        line = 2,
        col.main = "black",
        cex.main = titlesize)

  dev.off()
}
