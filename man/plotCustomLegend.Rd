% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotCustomLegend.R
\name{plotCustomLegend}
\alias{plotCustomLegend}
\title{plotCustomLegend}
\usage{
plotCustomLegend(
  ticks,
  colorVectors,
  boxTitles,
  legendTitle = "legend title (unit)",
  legendPosX = 0.5,
  legendPosY = 0.15,
  boxWidth = 0.4,
  boxHeight = 0.05,
  textCex = 1.2,
  titleCex = 1.4,
  textOffset = 0.02
)
}
\arguments{
\item{ticks}{Ticks of legend (usually legendBreaks of plot)}

\item{colorVectors}{List of color vectors with length ticks-1.
Number of vectors within list defines number of box rows in legend}

\item{boxTitles}{Title of boxes within legend.
Should be same as number of vectors}

\item{legendTitle}{Title of legend. If no title wanted choose NULL.}

\item{legendPosX}{Controls the horizontal position (from left to right),
i.e., the starting point of the legend box (default: 0.5 (i.e., center))
Note: should be set relative to xlim and ylim of plot for which legend
is produced (e.g., mean(xlim))}

\item{legendPosY}{Controls the vertical position (from bottom to top),
i.e., the middle of the whole legend box (default: 0.15 (i.e., close to bottom))
Note: should be set relative to xlim and ylim of plot for which legend
is produced (e.g., ylim[1] + (ylim[2] - ylim[1]) * 0.05).}

\item{boxWidth}{Controls the total width.
Note: should be set relative to xlim and ylim of plot for which legend
is produced (e.g., (xlim[2] - xlim[1]) * 0.3).}

\item{boxHeight}{Controls the individual row height
Note: should be set relative to xlim and ylim of plot for which legend
is produced (e.g., (ylim[2] - ylim[1]) * 0.025).}

\item{textCex}{Font size}

\item{titleCex}{Legend title size}

\item{textOffset}{Offset between legend text and legend box}
}
\value{
legend to be added on previously plotted graph
}
\description{
This function plots a customized legend
}
\author{
Felicitas Beier
}
