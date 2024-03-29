% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toolPrepareLandMask.R
\name{toolPrepareLandMask}
\alias{toolPrepareLandMask}
\title{toolPrepareLandMask}
\usage{
toolPrepareLandMask(
  projection =
    "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
)
}
\arguments{
\item{projection}{Choose projection.
Currently available options:
"+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
for EqualEarth projection;
"+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
for RobinsonProj; and
"+proj=longlat +datum=WGS84" for LatLon projection}
}
\value{
list of raster files
}
\description{
This function sets up the land mask and country
             polygons to plot magpie objects using raster/terra
             with an Equal Earth Projection
}
\author{
Felicitas Beier, Jens Heinke, Patrick v. Jeetze, Jan P. Dietrich
}
