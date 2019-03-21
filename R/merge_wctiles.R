#' Merge worldclim data.
#'
#' This function merges tiles downloaded from worldclim into a single raster.

#' @import raster
#' @import sp

#' @param biotiles "List with 0.5 arc min worldclim tiles, either stacks or rasters. Variables may not vary across tiles.
#' @return raster object containing single layer or stack of worldclim variables.
#'
#' @details List object required as input is prodcued by \code{get_wctile}.
#' @author Simon Kapitza \email{kapitzas@student.unimelb.edu.au}
#' @export

merge_wctiles <- function(biotiles){

  if(!is.list(biotiles)) stop("Please provide list with stacks for each tile")

  if (length(biotiles) == 1){
    out <- biotiles[[1]]
    message("Only 1 tile detected, merging not necessary")
  }

  if(length(biotiles) > 1){
    bionames <- names(biotiles[[1]])
    out <- list()
    message("Merging tiles...")
    for (i in 1:nlayers(biotiles[[1]])){
      b <- biotiles[[1]][[i]]
      message(paste0(substr(bionames[i], 1, nchar(bionames[i])-3)," | " , i, "/", length(bionames)))

      for(j in 2:length(biotiles)){
        b <- raster::merge(b, biotiles[[j]][[i]])
      }

      out[[i]] <- b
    }
    out <- stack(out)
    names(out) <- bionames
  }
  out
}

