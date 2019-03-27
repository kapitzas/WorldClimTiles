#' Download WorldClim and SRTM data.
#'
#' This function is a wrapper for \code{raster::getData}. Provide a character vector with tile names for either WorldClim 0.5 arcmin or SRTM 90m and those tiles will be downloaded.

#' @import raster
#' @import sp

#' @param tiles Character vector. Contains WorldClim (0.5 arcmin) or SRTM (90m) tile names. Tile names can be determined with \code{tile_name()}.
#' @param var Character. Which worldclim variable? Valid values are 'tmin', 'tmax', 'prec' and 'bio'.
#' @param path target path where data is downloaded.
#' @param name Valid values are 'worldclim' and 'srtm'. Default is 'worldclim'. If \code{name = 'worldclim'}, \code{var} has to be specified.
#' @return raster object containing list of worldclim tiles.
#'
#' @author Simon Kapitza \email{simon.statecology@gmail.com}
#' @export
#' @examples
#' boundary <- getData("GADM", country = "FRA", level = 0) #Downloads France boundaries
#'
#' tilenames <- tile_name(boundary, name = 'worldclim') #Determines names of the worldclim tiles covering France
#' srtmtilenames <- tile_name(boundary, name = 'srtm') #Determines names of the srtm tiles covering France
#'
#' wctiles <- tile_get(tiles = tilenames, name = 'srtm', var = "bio") #downloads WorldClim tiles covering France
#' srtmtiles <- tile_get(tiles = srtmtilenames, name = 'srtm') #downloads SRTM tiles covering France


#Download data
tile_get <- function(tiles, var, path, name = "worldclim"){

  if(missing(path)) path <- getwd()
  #WORLDCLIM
  if(tolower(name) == "srtm"){
    rs <- raster(nrows = 24, ncols = 72, xmn = -180, xmx = 180,
                 ymn = -60, ymx = 60)
    rs[] <- 1:length(rs)
    tiles_names <- as.vector(unlist(as.data.frame(outer(paste0(1:72, "_"), 1:24, FUN = "paste0"))))
  }else{
    rs <- raster(nrows = 5, ncols = 12, xmn = -180, xmx = 180,
                 ymn = -60, ymx = 90)
    rs[] <- 1:length(rs)
    tiles_names <- c(paste0(0, 0:11), paste0(1, 0:11), paste0(2, 0:11), paste0(3, 0:11), paste0(4, 0:11))
  }

  if(all(!tiles_names%in%tiles)) stop("Supplied tile names not recognised")
  points <- xyFromCell(rs, which(tiles_names%in%tiles))

  message("Loading tiles...")

  tileout <- list()
  for (i in 1:nrow(points)){
    if(tolower(name) == "srtm"){
      tileout[[i]] <- getData(name = toupper(name), path = path, lon = points[i,1], lat = points[i,2])

    }else if(name == "worldclim"){
      tileout[[i]] <- getData(name = name,  var = var, res = 0.5, path = path, lon = points[i,1], lat = points[i,2])
    }
  }
  message(paste0("Data successfully downloaded to ", path))
  tileout
}
