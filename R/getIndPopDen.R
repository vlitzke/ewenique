#' getPopDensity Function
#'
#' This function allows you to get an indice of local population density, 
#' based off of where the individual was seen in census data. 
#' 
#' @param census Takes in a data frame of census data that is probably subset beforehand if 
#' you are looking for a particular set of data (i.e. by season, year)
#'
#' @keywords population density

# Takes in an individual's census data 
getIndPopDen <- function(ind_cen, year) {
  matchYear <- subset(localpopDen_sum, localpopDen_sum$Year == year)
  ind_cen$popDenValue <- matchYear$sum_means[match(ind_cen$comb_coords, matchYear$comb_coords)]
  res <- round(mean(ind_cen$popDenValue, na.rm=TRUE))
  return(res)
}
