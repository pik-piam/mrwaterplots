% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotMapDiscrete.R
\name{plotMapDiscrete}
\alias{plotMapDiscrete}
\title{plotMapDiscrete}
\usage{
plotMapDiscrete(
  x,
  projection =
    "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
  outputfolder = ".\\\\",
  name = "name",
  title = "",
  titlesize = 3,
  ylim = c(-6500000, 8300000),
  xlim = c(-12577316, 15581284),
  legendcolor = c("#7f0000", "#b30000", "#d7301f", "#ef6548", "#fc8d59", "#fdbb84",
    "#fdd49e", "#fee8c8", "#fff7ec", "#045a8d"),
  colNA = "#d9d9d9",
  legendbreaks = seq(0, 1, 0.1),
  legend = NULL,
  legendname = "legendname",
  legendtextsize = 2,
  outputtype = "png",
  minVal = NULL,
  maxVal = NULL
)
}
\arguments{
\item{x}{MAgPIE object in grid-cellular (67420) resolution
to be plotted}

\item{projection}{Choose projection.
Currently available options:
"+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
for EqualEarth projection;
"+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
for RobinsonProj; and
"+proj=longlat +datum=WGS84" for LatLon projection}

\item{outputfolder}{Path to which plot should be saved}

\item{name}{Title of plot
(default: "name")}

\item{title}{Plot title displayed in the plot}

\item{titlesize}{Textsize of plot title}

\item{ylim}{y-axis limits of plot
(default: c(-6500000, 8300000))}

\item{xlim}{x-axis limits of plot
(default: c(-12577316, 15581284))}

\item{legendcolor}{vector of colors}

\item{colNA}{color for NAs
(default: "#d9d9d9" (gray))}

\item{legendbreaks}{vector of legend breaks
(default: seq(0, 1, 0.1))}

\item{legend}{legend element names (if non-numeric characters shall be returned).
(default: NULL, then legendbreaks defines displayed legend elements)}

\item{legendname}{legend name as character
(default: "legendname")}

\item{legendtextsize}{text size of legend text}

\item{outputtype}{Output type: pdf or jpeg}

\item{minVal}{minimum value at which x should be chopped}

\item{maxVal}{maximum value at which x should be chopped}
}
\value{
map as pdf, jpg or png
}
\description{
This function plots a raster object of halfdegree
             resolution (with 67420 grid cells)
             and saves it as PDF
}
\author{
Felicitas Beier, Jens Heinke
}
