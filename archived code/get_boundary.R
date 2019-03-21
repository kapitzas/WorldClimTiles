#' Load boundary file
#'
#' This function is a wrapper around the raster::getData and allows the user to download a boundary file for a country from GDAM. A second option is loading an existing boundary file from disk.
#' @import rgdal
#' @param fromdisk Default is \code{fromdisk = F}. Files will be downloaded from GADM database. If /code{fromdisk = T}, function will load file from specified folder.
#' @param path If \code{fromdisk = T}, this is the path to the folder containing the file to be loaded. If \code{fromdisk = F}, this is where downloaded GADM boundary file will be stored. If no path provided, files will be stored in current working directory.
#' @param ... Further parameters passed to nested functions. See details for more.
#' @details bla
#' @return Function returns a georeferenced \code{SpatialPolygons} object.
#'
#' @author Simon Kapitza \email{kapitzas@student.unimelb.edu.au}
#' @export

get_boundary <- function(name, path, ...){
  if(missing(path)){
    path <- getwd()
  }

  #GADM
  if (name == "GADM"){
    out <- getData(name = "GADM", path = path, ...)
  }

  if(name == "LOCAL"){
    out <- readOGR(dsn = path, ...)
  }
  out
}
