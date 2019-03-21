#' Load future predictions of bioclim variables.
#'
#' This function downloads GCM of bioclim variables and crops them to the extent provided by boundary file.
#'
#' @import raster
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

# mask_name <- "mask"
#
# path <- path_temp2
# type <- NULL
validate_layers <- function(mask_name, path, type = NULL){

  extens <- paste0(paste0(".", c(type, "grd", "asc", "sdat", "rst", "nc", "tif", "envi", "bil", "img"), "$"), collapse = "|")

  l <- list.files(path, full.names = T, pattern = extens)
  names <- list.files(path, full.names = F, pattern = extens)
  file_list <- list()
  for(i in 1:length(l)){
    file_list[[i]] <- raster(l[[i]])
    names(file_list)[i] <- names(raster(l[[i]]))
  }

  inds <- c(which(names(file_list) == mask_name), which(names(file_list) != mask_name))
  file_list <- file_list[inds]
  mask <- file_list[[which(names(file_list) == mask_name)]]

  df <- data.frame("var_names" = names(file_list), "res" = 0, "crs" = 0, "extent" = 0)

  #Commpare resolutions
  res_comp <- colSums(sapply(file_list, FUN = function(x) {round(res(x),9) == round(res(mask),9)}))
  df$res[which(res_comp == 2)] <- 1

  #compare crs
  crs_comp <- sapply(file_list, FUN = function(x) {as.character(crs(x)) == as.character(crs(mask))})
  df$crs[which(crs_comp == TRUE)] <- 1
  ext_comp <- sapply(file_list, FUN = function(x) {round(extent(x),5) == round(extent(mask),5)})
  df$extent[which(ext_comp == TRUE)] <- 1

  no_issues <- length(which(df == 0))
  message(paste0("There are ", no_issues, " issues. Check output table to identify which rasters need work"))
  list(df, file_list)
}

require(testthat)

