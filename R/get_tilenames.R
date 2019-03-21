#' Get the names and center coordinates of worldclim tiles at 0.5 arc sec.
#'

#' @import raster
#' @import sp

#' @param bnd Spatial object in latlong projection from which extent can be extracted.
#' @return List with two elements. First element is Character vector of tilenames, second element contains tile centre coordinates.
#'
#' @details Function uses extent of input to identify which worldclim tiles are necessary to produce layers for study area. The output can be used in \code{get_wctiles} to download set of worldclim tiles at 0.5 arc sec for merging to study area extent.
#' @author Simon Kapitza \email{kapitzas@student.unimelb.edu.au}
#' @export

get_tilenames <- function(bnd){

  rs <- raster(nrows = 5, ncols = 12, xmn = -180, xmx = 180,
               ymn = -60, ymx = 90)

  rs[] <- 1:length(rs)

  tiles_names <- c(paste0(0, 0:11), paste0(1, 0:11), paste0(2, 0:11), paste0(3, 0:11), paste0(4, 0:11))

  til <- raster::extract(disaggregate(rs, 20), extent(bnd), fun = unique)
  points <- xyFromCell(rs, which(rs[]%in%til))
  tiles_names[til]
}
