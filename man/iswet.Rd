% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iswet.R
\name{iswet}
\alias{iswet}
\title{Creates 'wet' polygons}
\usage{
iswet(grid, g, EEZ, latlong, proj)
}
\arguments{
\item{grid}{grid polygons}

\item{g}{index of the grid polygon}

\item{EEZ}{polygon for EEZ}

\item{latlong}{Latitude Longitude coordinate reference system}

\item{proj}{Projected coordinate reference system}
}
\value{

}
\description{
Creates 'wet' polygons
}
\examples{
wet <- iswet(grid,g,EEZ,latlong,proj)
}
