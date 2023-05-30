#' @title       plotBivariateMap
#' @description This function plots a raster object of halfdegree
#'              resolution (with 67420 grid cells)
#'              and saves it as PDF
#'
#' @param x            First MAgPIE object in grid-cellular (67420) resolution
#'                     to be plotted
#' @param y            Second MAgPIE object in grid-cellular (67420) resolution
#'                     to be plotted in same map
#' @param xlab         name of first object for legend x label
#' @param ylab         name of second object of legend y label
#' @param breaks       Breaks in bivariate legend
#' @param projection   Choose projection.
#'                     Currently available options:
#'                     "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#'                     for EqualEarth projection;
#'                     "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#'                     for RobinsonProj; and
#'                     "+proj=longlat +datum=WGS84" for LatLon projection
#' @param bivariatelegendstyle number between 1-9 to choose pre-defined bivariate legend colors
#' @param colNA        color for NAs
#'                     (default: "#d9d9d9" (gray))
#' @param outputtype   Output type: pdf or jpeg
#' @param outputfolder Path to which plot should be saved
#' @param name         Title of plot
#'                     (default: "name")
#' @param title        Plot title displayed in the plot
#' @param ylim         y-axis limits of plot
#'                     (default: c(-6500000, 8300000))
#' @param xlim         x-axis limits of plot
#'                     (default: c(-12577316, 15581284))
#'
#' @return map as pdf
#' @author Felicitas Beier
#'
#' @export

plotBivariateMap <- function(x, y,
                             xlab = NULL, ylab = NULL,
                             breaks = 4,
                             projection = "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", # nolint
                             bivariatelegendstyle = 1,
                             colNA = "#d9d9d9",
                             outputfolder = ".\\",
                             name = "name",
                             title = "",
                             ylim = c(-6500000, 8300000),
                             xlim = c(-12577316, 15581284),
                             outputtype = "png") {
  # Get land mask and country borders
  tmp            <- toolPrepareLandMask(projection = projection)
  landMask       <- tmp$landMask
  worldCountries <- tmp$worldCountries

  # Define minimum and maximum values
  minX <- min(x)
  minY <- min(y)
  maxX <- max(x)
  maxY <- max(y)

  #########################
  ### Internal functions ##
  .rasterCombine <- function(x, y, breaks) {
    # Reclassify raster values
    r1 <- raster::cut(x, breaks, include.lowest = TRUE)
    r2 <- raster::cut(y, breaks, include.lowest = TRUE)

    # Calculate rasterCM
    rasterCM <- raster::overlay(r1, r2, fun = function(i, j) {
      (j - 1) * breaks + i
    })

    return(rasterCM)
  }

  .createColorMatrix <- function(breaks, bivariatelegendstyle) {
    # Define edge colors
    if (bivariatelegendstyle == 1) {
      upperleft <- "#2166ac"
      upperright <- "#762a83"
      lowerleft <- "white"
      lowerright <- "#1b7837"
    # Style 2-5 from http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
    } else if (bivariatelegendstyle == 2) {
      upperleft <- "#BE64AC"
      upperright <- "#3B4994"
      lowerleft <- "#E8E8E8"
      lowerright <- "#5AC8C8"
    } else if (bivariatelegendstyle == 3) {
      upperleft <- "#73AE80"
      upperright <- "#2A5A5B"
      lowerleft <- "#E8E8E8"
      lowerright <- "#6C83B5"
    } else if (bivariatelegendstyle == 4) {
      upperleft <- "#9972AF"
      upperright <- "#804D36"
      lowerleft <- "#E8E8E8"
      lowerright <- "#C8B35A"
    } else if (bivariatelegendstyle == 5) {
      upperleft <- "#DA8DC8"
      upperright <- "#697AA2"
      lowerleft <- "#E8E8E8"
      lowerright <- "#73BCA0"
    } else if (bivariatelegendstyle == 6) {
      # Similar to Teuling, Stockli, Seneviratnea (2011) [https://doi.org/10.1002/joc.2153]
      upperleft <- "#F7900A"
      upperright <- "#993A65"
      lowerleft <- "#44B360"
      lowerright <- "#3A88B5"
    } else if (bivariatelegendstyle == 7) {
      # Viridis style
      upperleft <- "#FEF287"
      upperright <- "#21908D"
      lowerleft <- "#E8F4F3"
      lowerright <- "#9874A1"
    } else if (bivariatelegendstyle == 8) {
      # Similar to Fjeldsa, Bowie, Rahbek 2012
      upperleft <- "#34C21B"
      upperright <- "#FFFFFF"
      lowerleft <- "#595757"
      lowerright <- "#A874B8"
    } else if (bivariatelegendstyle == 9) {
      # Default from original source
      upperleft <- "#0096EB"
      upperright <- "#820050"
      lowerleft <- "#BEBEBE"
      lowerright <- "#FFE60F"
    }

    # create color scale for given breaks
    b    <- breaks - 1
    b    <- (0:b) / b
    col1 <- grDevices::rgb(grDevices::colorRamp(c(upperleft,
                                                  lowerleft))(b), max = 255)
    col2 <- grDevices::rgb(grDevices::colorRamp(c(upperright,
                                                  lowerright))(b), max = 255)
    cm   <- apply(cbind(col1, col2), 1,
                  function(i) grDevices::rgb(grDevices::colorRamp(i)(b), max = 255))

    # fill matrix with color values
    return(cm[, ncol(cm):1]) # nolint
  }

  .getBivariateLegend <- function(cmat, xlab = "", ylab = "",
                                  labSize = 12,
                                  labCol = "black") {
    stopifnot(nrow(cmat) == ncol(cmat))
    breaks <- nrow(cmat)
    legendDF <- cmat %>%
                   as.data.frame() %>%
                     tidyr::pivot_longer(cols = tidyselect::everything()) %>%
                       dplyr::mutate(y = rep(1:breaks, times = breaks),
                                     x = rep(1:breaks, each = breaks))
    legendPLOT <- legendDF %>%
      ggplot2::ggplot(ggplot2::aes(x, y, fill = legendDF$value)) +
      ggplot2::geom_raster() +
      ggplot2::scale_fill_identity() +
      ggplot2::coord_equal(expand = FALSE) +
      ggplot2::theme_void() +
      ggplot2::theme(aspect.ratio = 1,
                     axis.title = ggplot2::element_text(size = labSize,
                                                        colour = labCol,
                                                        hjust = 0.5,
                                                        vjust = 1),
                     axis.title.y = ggplot2::element_text(angle = 90,
                                                          hjust = 0.5)) +
      ggplot2::xlab(bquote(.(xlab) ~  symbol("\256"))) +
      ggplot2::ylab(bquote(.(ylab) ~  symbol("\256")))
    return(legendPLOT)
  }
  #########################


  ####################
  ### Prepare data ###
  ####################
  # Transform magpie objects to raster
  x <- raster::raster(toolRasterTransform(x, projection = projection))
  y <- raster::raster(toolRasterTransform(y, projection = projection))
  x[is.na(x)] <- 0
  y[is.na(y)] <- 0

  # Chop off min and max values
  x[x < minX]  <- minX
  y[y < minY]  <- minY
  x[x >= maxX] <- maxX
  y[y >= maxY] <- maxY

  xy        <- .rasterCombine(x = x, y = y, breaks = breaks)
  colmatrix <- .createColorMatrix(breaks = breaks, bivariatelegendstyle = bivariatelegendstyle)


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
  terra::plot(xy, col = colmatrix, border = "white",
              ylim = ylim, xlim = xlim, asp = NA,
              axes = FALSE,
              legend = FALSE,
              add = TRUE)
  terra::plot(landMask, col = "white", border = "white",
              ylim = ylim, xlim = xlim,  asp = NA,
              axes = FALSE, add = TRUE)
  terra::plot(worldCountries,
              ylim = ylim, xlim = xlim,  asp = NA,
              axes = FALSE, add = TRUE)
  dev.off()

  ############################
  ### Plot legend and save ###
  ############################
  legendplot <- .getBivariateLegend(cmat = colmatrix, xlab = xlab, ylab = ylab)
  ggplot2::ggsave(paste0(outputfolder, name, "legend.png"), plot = legendplot)
}
