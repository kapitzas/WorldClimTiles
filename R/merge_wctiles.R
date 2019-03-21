#' Merge worldclim data.
#'
#' This function merges tiles downloaded from worldclim into a single raster.

#' @import raster
#' @import sp

#' @param biotiles "List with 0.5 arc min worldclim tile stacks (or single rasters) (i.e. produced by get_wctiles). Both stacks have to have the same variables in them.
#' @return raster stack (or single raster) containing merged worldclim variable(s).
#'
#' @details raster::merge is used for merging, at default settings. That means that in case of overlaps the cell values of the object that comes first in the sequence are retained. But this shouldn't matter much, because there are no overlaps between worldclim tiles.
#'
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

