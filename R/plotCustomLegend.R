#' @title       plotCustomLegend
#' @description This function plots a customized legend
#'
#' @param ticks        Ticks of legend (usually legendBreaks of plot)
#' @param colorVectors List of color vectors with length ticks-1.
#'                     Number of vectors within list defines number of box rows in legend
#' @param boxTitles    Title of boxes within legend.
#'                     Should be same as number of vectors
#' @param legendTitle  Title of legend. If no title wanted choose NULL.
#' @param legendPosX   Controls the horizontal position (from left to right),
#'                     i.e., the starting point of the legend box (default: 0.5 (i.e., center))
#'                     Note: should be set relative to xlim and ylim of plot for which legend
#'                     is produced (e.g., mean(xlim))
#' @param legendPosY   Controls the vertical position (from bottom to top),
#'                     i.e., the middle of the whole legend box (default: 0.15 (i.e., close to bottom))
#'                     Note: should be set relative to xlim and ylim of plot for which legend
#'                     is produced (e.g., ylim[1] + (ylim[2] - ylim[1]) * 0.05).
#' @param boxWidth     Controls the total width.
#'                     Note: should be set relative to xlim and ylim of plot for which legend
#'                     is produced (e.g., (xlim[2] - xlim[1]) * 0.3).
#' @param boxHeight    Controls the individual row height
#'                     Note: should be set relative to xlim and ylim of plot for which legend
#'                     is produced (e.g., (ylim[2] - ylim[1]) * 0.025).
#'
#'
#' @return legend to be added on previously plotted graph
#' @author Felicitas Beier
#'
#' @export
#'
plotCustomLegend <- function(ticks, colorVectors, boxTitles,
                             legendTitle = "legend title (unit)",
                             legendPosX = 0.5, legendPosY = 0.15,
                             boxWidth = 0.4, boxHeight = 0.05) {
  # Check that each color scale has exactly `length(ticks) - 1` colors
  for (i in seq_along(colorVectors)) {
    if (length(colorVectors[[i]]) != (length(ticks) - 1)) {
      stop(paste("Error: Color scale", i, "has", length(colorVectors[[i]]),
                 "colors, but it should have", length(ticks) - 1, "colors (ticks - 1)."))
    }
  }

  # Number of color scales (rows)
  noRows <- length(colorVectors)
  noBreaks <- length(ticks) - 1  # Number of breaks (should match ticks length - 1)

  # Define spacing variables for positioning
  rowHeight <- boxHeight  # Height of each color row
  totHeight <- rowHeight * noRows  # Total height of all rows
  startY <- legendPosY + (totHeight / 2)  # Start at the upper row
  yTick  <- legendPosY - rowHeight * 1.5  # Move ticks further below the lowest row

  # ---- Add the overall legend title ----
  if (!is.null(legendTitle)) {
    text(legendPosX + boxWidth / 2, startY + rowHeight * 1.5,
         labels = legendTitle, cex = 1.2, font = 2, adj = c(0.5, 0))
  }

  # Loop through each color scale (row)
  for (row in seq_along(colorVectors)) {
    colors <- colorVectors[[row]]  # Get current color vector
    yPos <- startY - (row - 1) * rowHeight  # Compute y position for this row

    # Draw boxes for each color break (horizontally)
    for (i in seq_len(noBreaks)) {
      rect(legendPosX + (i - 1) * boxWidth / noBreaks, yPos - rowHeight,
           legendPosX + i * boxWidth / noBreaks, yPos,
           col = colors[i], border = "black")
    }

    # Add title to the left of each row
    text(legendPosX - 0.05, yPos - rowHeight / 2, boxTitles[row], cex = 1, font = 1, adj = 1)
  }

  # Add tick labels below all rows
  for (i in seq_along(ticks)) {
    text(legendPosX + (i - 1) * boxWidth / noBreaks, yTick, labels = ticks[i], adj = c(0.5, 1))
  }
}
