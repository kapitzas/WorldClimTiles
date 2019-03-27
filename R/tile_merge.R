#' Merge worldclim data.
#'
#' This function merges tiles downloaded from WorldClim into a single raster.

#' @import raster
#' @import sp
#' @import stringr

#' @param tiles List with 0.5 arc min WorldClim tile stacks (or single rasters) (i.e. produced by \code{tile_get}). Names and number of variables need to match between stacks.
#' @param fac Factor by which resolution is changed, see details.
#' @return RasterStack or Raster containing merged WorldClim variable(s).
#'
#' @details \code{raster::merge} is used for merging, at default settings. That means that in case of overlaps the cell values of the object that comes first in the sequence are retained. Raster resolution can be changed to speed up merging of very high resolution data. For example, when data are SRTM tiles, \code{fac = 10} will resample to a resolution of 0.5 arcmin (bilinear), which corresponds to the resolution of WorldClim data.
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
#' srtmmerged <- tile_merge(srtmtiles, fac = 10)

tile_merge <- function(tiles, fac){

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
    crs <- crs(tiles[[1]])
    res <- res(tiles[[1]])

    if(!missing(fac)){
      res <- res * fac
    }

    for (i in 1:nlayers(tiles[[1]])){

      if(!missing(fac)){
        b <- projectRaster(tiles[[1]][[i]], res = res, crs = crs)
      }else{
        b <- tiles[[1]][[i]]
      }

      message("Layer ", i, "/", length(layernames))

      for(j in 2:length(tiles)){
        message("Adding tile ", j)
        if(!missing(fac)){
          b2 <- projectRaster(tiles[[j]][[i]], res = res, crs = crs)
        }else{
          b2 <- tiles[[j]][[i]]
        }
        b <- raster::merge(b, b2)
      }

      out[[i]] <- b
    }
    out <- stack(out)
    names(out) <- layernames
  }
  out
}

