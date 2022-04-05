#' getSurvivalInfo Function
#'
#' This function allows you to get the survival information from an individual - it returns it's 
#' survival category along with longevity.
#' 
#' @param id Takes in an individual's ID.
#'
#' @keywords survival info

getSurvivalInfo <-function(id){
  
  this_sheep <- subset(sheep,sheep$ID == id)
  by <- this_sheep$BYear
  dy <- this_sheep$DeathYear
  dm <- this_sheep$DeathMonth
  st <- this_sheep$Status # alive/dead/etc at time of database version
  
  loy <- NA  # year of last observation(s)
  this_sheep_c<-subset(census,census$ID==id)
  if(dim(this_sheep_c)[1]>0){
    last_obs<-subset(this_sheep_c,this_sheep_c$Year==max(this_sheep_c$Year))
    last_obs<-subset(this_sheep_c,this_sheep_c$Month==max(this_sheep_c$Month))
    loy <- last_obs$Year[i]
  }
  
  longevity <- NA
  
  if(is.na(dm)){
    dm <- 1
  }
  # If they do have a birth and death year
  if(is.na(by)==FALSE & is.na(dy)==FALSE){
    
    sheep_death_year <- dy
    
    # If they were over a year old, and happened to survive the winter (death 
    # month later than May, when the spring census happens)
    if(dy>by & dm<5) {
      sheep_death_year <- dy-1
    }
    longevity <- sheep_death_year-by
  }
  
  surv_cat <-NA
  
  if(is.na(longevity)==FALSE & longevity>0) {
    surv_cat <- "recruited"
    
  }
  
  if(is.na(longevity)==FALSE & longevity==0){
    if(is.na(dm)==FALSE){
      if(dy==by & dm<=8){
        surv_cat <-"neonate"
      }else{
        surv_cat <-"first_winter"
      }
    }
  }
  if(st=="Foetus") surv_cat <- "foetus"
  
  # if that has not sorted it out, assume, that the sheep died in the winter 
  # after it was last seen.  Note, this is only for assigning broad survival 
  # categories; for these sheep, longevity will be NA
  if(is.na(surv_cat)){
    
    if(is.na(loy)==FALSE && loy>by){ 
      
      surv_cat <- "recruited"
      
    }else{
      
      surv_cat <- "first_winter"
      # n.b. any missed neonatal mortalities would be mis-categorised
      # as first winter deaths by this algorithm, but these should be
      # extremely rare
    }
  }
  return(list(longevity=longevity,surv_cat=surv_cat))
}
