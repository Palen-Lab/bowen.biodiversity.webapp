% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{bowen_map}
\alias{bowen_map}
\title{Bowen Map Plotting}
\usage{
bowen_map(
  raster_layer,
  title,
  subtitle,
  caption,
  legend_label,
  pal = "ag_GrnYl"
)
}
\arguments{
\item{raster_layer}{SpatRaster, input raster}

\item{title}{Text, title for map}

\item{subtitle}{Text, subtitle for map}

\item{caption}{Text, caption for map}

\item{legend_label}{Text, legend label}

\item{pal}{Palette, from colorspace package}
}
\value{
ggplot
}
\description{
Bowen Map Plotting
}
