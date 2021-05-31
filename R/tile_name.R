#' Get WorldClim and SRTM tile names.
#'
#'Provide projected Spatial object (can be features or raster) and determine which WorldClim or SRTM tiles your study area intersects with.

#' @import raster
#' @import sp

#' @param bnd Projected spatial feature or raster object.
#' @param name Valid values are 'worldclim' and 'srtm'. Default is 'worldclim'
#' @return Character vector of tilenames of WorldClim or SRTM tiles.
#' @details Function uses the spatial object input to identify WorldClim or SRTM tiles to be downloaded. The output of this function can be used in \code{tile_get()} to download the according data via \code{raster::getData()}.
#' @author Simon Kapitza \email{simon.statecology.gmail.com}
#' @export
#'
#' @examples
#' boundary <- getData("GADM", country = "FRA", level = 0) #Downloads France boundaries
#'
#' tilenames <- tile_name(boundary, name = 'worldclim') #Determines names of the worldclim tiles covering France
#' srtmtilenames <- tile_name(boundary, name = 'srtm') #Determines names of the srtm tiles covering France

tile_name <- function(bnd, name = 'worldclim'){



  if(class(bnd)%in%c("RasterLayer", "RasterBrick", "RasterStack")){
    crs_arg <- crs(bnd)
    bnd <- as(extent(bnd), 'SpatialPolygons')
    crs(bnd) <- crs_arg
    rm(crs_arg)
  }

  bnd <- spTransform(bnd, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  if(name == "worldclim"){
    rs <- raster(nrows = 5, ncols = 12, xmn = -180, xmx = 180,
                 ymn = -60, ymx = 90)
    tiles_names <- c(paste0(0, 0:11), paste0(1, 0:11), paste0(2, 0:11), paste0(3, 0:11), paste0(4, 0:11))

  }else if(tolower(name) == "srtm"){
    rs <- raster(nrows = 24, ncols = 72, xmn = -180, xmx = 180,
                 ymn = -60, ymx = 60)
    tiles_names <- as.vector(unlist(as.data.frame(outer(paste0(1:72, "_"), 1:24, FUN = "paste0"))))
  }

  rs[] <- 1:length(rs)
  til <- raster::extract(rs, bnd, weights = TRUE)
  if(is.numeric(til)) til <- sort(unique(til))
  if(is.list(til)) til <- sort(til[[1]][,1])
  #points <- xyFromCell(rs, which(rs[]%in%til))
  tiles_names[til]
}
