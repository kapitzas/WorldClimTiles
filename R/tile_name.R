#' Get the names of 0.5 arcmin WorldClim tiles.
#'
#'Provide projected spatial features of your study area. The function determines which WorldClim tiles they intersect with and returns the tile names as character vector.

#' @import raster
#' @import sp

#' @param bnd Spatial object in latlong projection from which extent can be extracted.
#' @return Character vector of tilenames.
#'
#' @details Function uses extent of input Spatial object to identify which WorldClim tiles are necessary to produce layers for study area. The output can be used in \code{tile_get} to download set of worldclim tiles at 0.5 arc sec for merging to study area extent.
#' @author Simon Kapitza \email{simon.statecology.gmail.com}
#' @export
#'
#' @examples
#' boundary <- getData("GADM", country = "FRA", level = 0) #Downloads France boundaries
#' tilenames <- tile_name(boundary) #Determines names of the worldclim tiles covering France

tile_name <- function(bnd){

  rs <- raster(nrows = 5, ncols = 12, xmn = -180, xmx = 180,
               ymn = -60, ymx = 90)

  bnd <- spTransform(bnd, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  rs[] <- 1:length(rs)
  tiles_names <- c(paste0(0, 0:11), paste0(1, 0:11), paste0(2, 0:11), paste0(3, 0:11), paste0(4, 0:11))

  til <- raster::extract(rs, bnd, weights = TRUE)
  if(is.numeric(til)) til <- sort(unique(til))
  if(is.list(til)) til <- sort(til[[1]][,1])
  #points <- xyFromCell(rs, which(rs[]%in%til))
  tiles_names[til]
}
