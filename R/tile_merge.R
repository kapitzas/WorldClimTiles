#' Merge worldclim data.
#'
#' This function merges tiles downloaded from WorldClim into a single raster.

#' @import raster
#' @import sp
#' @import stringr

#' @param tiles "List with 0.5 arc min WorldClim tile stacks (or single rasters) (i.e. produced by \code{tile_get}). Both stacks have to have the same variables in them.
#' @return RasterStack or Raster containing merged WorldClim variable(s).
#'
#' @details \code{raster::merge} is used for merging, at default settings. That means that in case of overlaps the cell values of the object that comes first in the sequence are retained. But this shouldn't matter much, because there are no overlaps between WorldClim tiles.
#'
#' @author Simon Kapitza \email{simon.statecology@gmail.com}
#' @export
#'
#' @examples
#' boundary <- getData("GADM", country = "FRA", level = 0) #Downloads France boundaries
#'
#' tilenames <- tile_name(boundary, name = 'worldclim') #Determines names of the worldclim tiles covering France
#' srtmtilenames <- tile_name(boundary, name = 'srtm') #Determines names of the srtm tiles covering France
#'
#' wctiles <- tile_get(tiles = tilenames, name = 'srtm', var = "bio") #downloads WorldClim tiles covering France
#' srtmtiles <- tile_get(tiles = srtmtilenames, name = 'srtm') #downloads SRTM tiles covering France
#'
#' wcmerged <- tile_merge(wctiles)
#' srtmmerged <- tile_merge(srtmtiles)

tile_merge <- function(tiles){

  if(!is.list(tiles)) stop("Please provide list with stacks for each tile")

  if (length(tiles) == 1){
    out <- tiles[[1]]
    message("Only 1 tile detected, merging not necessary")
  }

  if(length(tiles) > 1){
    layernames <- as.character(sapply(names(tiles[[1]]), FUN = function(x) strsplit(x, '_')[[1]][1]))

    if(!all(sapply(tiles, nlayers) == nlayers(tiles[[1]]))) stop("Supplied tiles have different number of variables")
    if(!length(unique(sapply(sapply(tiles, names), FUN = function(x) strsplit(x, '_')[[1]][1])))   == nlayers(tiles[[1]])) stop("Variable names differ between tiles")
    out <- list()
    message("Merging tiles...")
    for (i in 1:nlayers(tiles[[1]])){
      b <- tiles[[1]][[i]]
      message("Layer", i, "/", length(layernames))

      for(j in 2:length(tiles)){
        message("Tile ", j)
        b <- raster::merge(b, tiles[[j]][[i]])
      }

      out[[i]] <- b
    }
    out <- stack(out)
    names(out) <- layernames
  }
  out
}

