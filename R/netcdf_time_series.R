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
# Some useful functions for extracting simple point-data time series
# from gridded NetCDF files of temperature data (e.g. from BOM/SILO
# climate variable datasets).

# library(ncdf4)
# library(sp)
# library(raster)


#' Return the distance (in km) between the latitude/longitude pairs
#' using the Haversine formula.
#'
#' @param latA The latitude for the first coordinate pair, in
#'   decimal-degrees Northing (or a vector of latitudes).
#' @param lonA The longitude for the first coordinate pair, in
#'   decimal-degrees Easting (or a vector of longitudes).
#' @param latB The latitude for the second coordinate pair, in
#'   decimal-degrees Northing (or a vector of latitudes).
#' @param lonB The longitude for the second coordinate pair, in
#'   decimal-degrees Easting (or a vector of longitudes).
#' @return The distance (in kilometres) between the coordinate pairs
#'   (or a vector of respective distances).
#'
#' @export
#' @examples
#' coord_distance(-34.9257, 138.5832, -34.9285, 138.6007)
#'
coord_distance <- function(latA, lonA, latB, lonB) {
  # Estimate Earth's radius (in km)
  earth_radius <- (6378.14 + 6356.75) / 2

  # Convert latitudes and longitudes to radians
  latA = latA * pi / 180
  lonA = lonA * pi / 180
  latB = latB * pi / 180
  lonB = lonB * pi / 180

  # Use the Haversine distance conversion
  2 * earth_radius * asin(
    sqrt(
      sin((latA - latB) / 2)**2 +
        cos(latA) * cos(latB) * sin((lonA - lonB) / 2)**2
    )
  )
}


#' Return a data frame comprising the time series data for the
#' given NetCDF climate grid near the specified coordinates.
#'
#' @param nc_file The file path for the NetCDF file.
#' @param latitude The latitude for the desired time series dataset
#'   (in decimal-degrees Northing).
#' @param longitude The longitude for the desired time series dataset
#'   (in decimal-degrees Easting).
#' @param var (Optional) The variable to extract the time series for
#'   (default=NULL, in which case the first variable is extracted).
#' @return A data frame containing the dates, nearest grid latitude and
#'   longitude, and the variable values.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' time_series <- extract_time_series("test.nc", -34.9257, 138.583)
#' }
extract_time_series <- function(nc_file, latitude, longitude, var = NULL) {
  # Grab the variable names from the NetCDF
  netcdf <- ncdf4::nc_open(nc_file)
  var_names <- names(netcdf$var)

  # If var wasn't specified, we take the first listed variable as
  # the dataset of interest. If var was specified but doesn't
  # appear in the list, exit with an error.
  if (is.null(var)) {
    var = var_names[1]
  } else {
    if (!var %in% var_names) {
      message <- paste0(
        "'",
        var,
        "' is not the name of a variable in this NetCDF file\n",
        "(Available variables: ",
        paste(var_names, collapse = " "),
        ")"
      )
      ncdf4::nc_close(netcdf)
      stop(message)
    }
  }

  # Grab the variable name and units from the NetCDF
  var_title <- netcdf$var[[var]]$longname
  var_units <- netcdf$var[[var]]$units
  var_title <- paste0(var_title, " (", var_units, ")")
  ncdf4::nc_close(netcdf)

  # Read in the NetCDF layers as rasters
  rasters <- raster::brick(nc_file, varname = var)
  rasters.df <- raster::as.data.frame(rasters, xy = TRUE)

  # Grab the dates from the raster brick
  dates <- rasters@z

  # Compute distances between each grid point and the given set
  # of coordinates and determine the closest grid point
  dist <- coord_distance(
    rasters.df$y,
    rasters.df$x,
    rep(latitude, nrow(rasters.df)),
    rep(longitude, nrow(rasters.df))
  )
  min_index <- which(dist == min(dist))

  # Build and return the data frame containing the time series
  df <- data.frame(Date = dates)
  df["Latitude"] <- rep(latitude, nrow(df))
  df["Longitude"] <- rep(longitude, nrow(df))
  df[var_title] <- as.numeric(rasters.df[min_index, -(1:2)])
  df
}
