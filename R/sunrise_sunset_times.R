# Copyright (c) 2020 University of Adelaide Biometry Hub
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Code author: Russell A. Edson
# Date last modified: 13/10/2020
# Send all bug reports/questions/comments to
#     russell.edson@adelaide.edu.au
#
# Functions to compute sunrise/sunset times given a date and
# latitude/longitude coordinates. Useful for e.g. factoring a
# dataset into Day/Night phases.
#
# TODO: >90% code duplication between these two functions.
#       Maybe worth refactoring into a single function that
#       parameterises on sunrise/sunset, or perhaps use a
#       function-generating macro?

library(sp)
library(maptools)


#' Return the time of sunrise given the date and GPS coordinates.
#'
#' @param datetime A POSIXct object for the local date (and timezone).
#' @param latitude The decimal-degrees for the latitude (Northing).
#' @param longitude The decimal-degrees for the longitude (Easting).
#' @return A POSIXct object for the local time of sunrise.
#'
#' @example
#' date <- lubridate::as_datetime("2020-01-01", tz = "Australia/Adelaide")
#' sunrise_time(date, -35.69167, 136.9650)
sunrise_time <- function(datetime, latitude, longitude) {
  coordinates <- sp::SpatialPoints(
    matrix(c(longitude, latitude), ncol = 2),
    proj4string = sp::CRS("+proj=longlat +datum=WGS84")
  )

  sunrise <- maptools::sunriset(
    coordinates,
    datetime,
    direction = "sunrise",
    POSIXct.out = TRUE
  )
  sunrise[, 2]
}


#' Return the time of sunset given the date and GPS coordinates.
#'
#' @param datetime A POSIXct object for the local date (and timezone).
#' @param latitude The decimal-degrees for the latitude (Northing).
#' @param longitude The decimal-degrees for the longitude (Easting).
#' @return A POSIXct object for the local time of sunset.
#'
#' @example
#' date <- lubridate::as_datetime("2020-01-01", tz = "Australia/Adelaide")
#' sunset_time(date, -35.69167, 136.9650)
sunset_time <- function(datetime, latitude, longitude) {
  coordinates <- sp::SpatialPoints(
    matrix(c(longitude, latitude), ncol = 2),
    proj4string = sp::CRS("+proj=longlat +datum=WGS84")
  )

  sunset <- maptools::sunriset(
    coordinates,
    datetime,
    direction = "sunset",
    POSIXct.out = TRUE
  )
  sunset[, 2]
}
