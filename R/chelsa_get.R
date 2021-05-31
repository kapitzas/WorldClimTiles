#' Download CHELSA V_2.1 data
#'
#' This function downloads CHELSA V_2.1 climate layers (Karger et al., 2017) into a specified folder.

#' @importFrom utils download.file

#' @param target_path Download folder on local machine
#' @param vars Character string containing the CHELSA variables to be downloaded, can be one or more of "pr", "tas", "tasmax", "tasmin".
#' @param years Vector containing the years from which monthly data should be downloaded (must be between 1972-2013)
#' @param months Vector containing the months to be downloaded (1-12).
#'
#' @author Simon Kapitza \email{simon.statecology@gmail.com}
#' @export
#' @examples

#' vars <- c("pr", "tas", "tasmax", "tasmin")
#' months <- 1:12
#' years <- c(1979:1981)
#' path <- "mypath/"
#' download_chelsa(target_path, years, vars, months)
#'
#' @references
#' Karger, D., Conrad, O., Böhner, J. et al. Climatologies at high resolution for the earth’s land surface areas. Sci Data 4, 170122 (2017). https://doi.org/10.1038/sdata.2017.122


download_chelsa <- function(target_path, years, vars, months){
  timeout_old <- getOption('timeout')
  options(timeout=1000)
  for(year in years){
    for(var in vars){
      for(month in months){
        month <- sprintf("%02d", month)
        name <- paste(c("CHELSA", var, month, year, "V.2.1.tif"), collapse = "_")
        source_url <- file.path("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly", var, name)
        destination <- file.path(target_path, name)
        download.file(source_url, destination)
      }
    }
  }
  options(timeout = timeout_old)
}

