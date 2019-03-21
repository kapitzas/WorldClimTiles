#' Load worldclim data.
#'
#' This function is a wrapper around raster::getData. Provide a character vector with worldclim 0.5 arcmin tile names and those tiles will be downloaded.

#' @import raster
#' @import sp

#' @param tiles Character vector containing worldclim tile names (0.5 arcmin).
#' @param var Which worldclim variable? Valid names are 'tmin', 'tmax', 'prec' and 'bio'.
#' @param path target path where data is downloaded.
#' @return raster object containing list of worldclim tiles.
#'
#' @author Simon Kapitza \email{kapitzas@student.unimelb.edu.au}
#' @export

#Download data
tile_get <- function(tiles, var, path){

  if(missing(path)){
    path <- getwd()
  }

  #WORLDCLIM
  rs <- raster(nrows = 5, ncols = 12, xmn = -180, xmx = 180,
               ymn = -60, ymx = 90)
  rs[] <- 1:length(rs)

  tiles_names <- c(paste0(0, 0:11), paste0(1, 0:11), paste0(2, 0:11), paste0(3, 0:11), paste0(4, 0:11))

  points <- xyFromCell(rs, which(tiles_names%in%tiles))

  message("Loading required worldclim tiles...")
  biotiles <- list()
  for (i in 1:nrow(points)){
    biotiles[[i]] <- getData(name = "worldclim", var = var, path = path, res = 0.5, lon = points[i,1], lat = points[i,2])
  }
  message(paste0("Data successfully downloaded to ", path))
  biotiles
}
