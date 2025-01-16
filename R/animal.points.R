#' Example animal location data
#'
#' Hypothetical data set with the GPS coordinates for two turtles
#'
#' @format
#' A data frame with 11 rows and 5 columns:
#' \describe{
#'   \item{lon.raw}{Longitude in decimal degrees}
#'   \item{lat.raw}{Latitude in decimal degrees}
#'   \item{lon.shifted}{Longitude shifted to line up with stream points to illustrate how the functions work}
#'   \item{lat.shifted}{Latitude shifted to line up with stream points to illustrate how the functions work}
#'   \item{id}{Turtle IDs}
#'   \item{date.time}{Dates and times of points in POSIX format}
#'   \item{point}{Names of each coordinate (optional)}
#'   }
#' @source Fictional example generated to demonstrate this package
"animal.points"
