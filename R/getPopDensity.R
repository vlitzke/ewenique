#' getPopDensity Function
#'
#' This function allows you to get an indice of local population density, 
#' based off of where the individual was seen in census data. 
#' 
#' @param census Takes in a data frame of census data that is probably subset beforehand if 
#' you are looking for a particular set of data (i.e. by season, year)
#'
#' @keywords population density

getPopDensity <- function(census) {
  ind_cen <- census
  ind_cen$popDenValue <- gridPopDen$Occurence[match(ind_cen$comb_coords,gridPopDen$comb_coords)]
  res <- mean(ind_cen$popDenValue)
  return(res)
}
