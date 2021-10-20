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

getMeanVeg<-function(id, choosen_cen){
  choosen_cen$Calluna <- veg$Calluna[match(choosen_cen$comb_coords,veg$comb_coords)]
  choosen_cen$Holcus  <- veg$H.lanatus[match(choosen_cen$comb_coords,veg$comb_coords)]
  res<-c(mean(choosen_cen$Calluna, na.rm=TRUE),mean(choosen_cen$Holcus, na.rm=TRUE))
  names(res)<-c("mean Calluna","mean Holcus") 
  return(res)
}

