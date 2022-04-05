#' getMeanVeg Function
#'
#' This function allows you to get a weighted mean of the vegetation in
#' certain census coordinate points.
#' 
#' @param id Individual's ID
#' @param census A group of census data that is probably subset beforehand if 
#' you are looking for a particular set of data (i.e. by season, year)
#'
#' @keywords vegetation

# Need veg dataframe and comb_coords
getMeanVeg<-function(id, cen){
  
  # match the coordinate points between the census and vegetation data to 
  # extract out the percentage cover of each vegetation for each grid
  cen$Calluna <- veg$Calluna[match(cen$comb_coords,veg$comb_coords)]
  cen$Holcus  <- veg$H.lanatus[match(cen$comb_coords,veg$comb_coords)]
  
  # Take the mean of both of the columns 
  res<-c(mean(cen$Calluna, na.rm=TRUE),mean(cen$Holcus, na.rm=TRUE))
  
  # Rename the columns
  names(res)<-c("mean Calluna","mean Holcus") 
  
  # Returns both values 
  return(res)
}

