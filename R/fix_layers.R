#' Load future predictions of bioclim variables.
#'
#' This function downloads GCM of bioclim variables and crops them to the extent provided by boundary file.
#'
#' @param bnd \code{boundary} object.
#' @param var Variable. Valid names are 'tmin', 'tmax', 'prec' and 'bio'
#' @param res Resolution. Valid resolutions are 0.5, 2.5, 5 and 10.
#' @param rcp Representative Concenctration Pathway. Valid values are 26, 45, 60 and 85.
#' @param model GCM model under which predictions were made. Should be one of "AC", "BC", "CC", "CE", "CN", "GF", "GD", "GS", "HD", "HG", "HE", "IN", "IP", "MI", "MR", "MC", "MP", "MG", or "NO".
#' @param year Time step of predictions. Should be 50 or 70.
#' @param ... Further parameters passed to raster::getData. See /code{?raster::getData}.
#' @return raster object containing single layer or stack of GCM predictions of bioclim variables, cropped to extent of \code{bnd}.
#'
#' @author Simon Kapitza \email{kapitzas@student.unimelb.edu.au}
#' @export

fix_layers <- function(mask_name, path){
  f <- validate_layers(mask_name, path)
  mask <- f[[2]][[which(names(f[[2]]) == mask_name)]]
  need_fixed <- which(rowSums(f[[1]][c(2,3,4)]) != 3)

  for (i in need_fixed){
    ras <- f[[2]][[i]]
    message(paste0("Processing ", names(ras)," | ", i, "/", tail(need_fixed, 1)))
    if(is.na(crs(ras))){
      crs(ras) <- crs(mask)
      message(paste0("CRS unknown for ", names(ras), ", assumed to be same as mask"))
      f[[1]][i,3] <- 1
    }
    if(f[[1]][i,2] == 0){
      ras <- projectRaster(ras, mask)
      f[[1]][i, c(2,3)] <- 1
    }
    if(f[[1]][i,4] == 0){
      ras <- crop(ras, mask)
      f[[1]][i, 4] <- 1
    }
    f[[2]][[i]] <- ras
  }
  f
}
