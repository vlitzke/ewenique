#' getOffspring Function
#'
#' This function returns a binomial if a given individual had offspring in a given year and what survival category it made it to.
#' 
#' @param id, year 
#'
#' @keywords Offspring annual

getOffspring <- function(id, year) {
  offspring_sub <- subset(sheep,sheep$MumID==id)
  offspring_sub <- subset(offspring_sub, offspring_sub$BYear == year)
  
  if (dim(offspring_sub)[1]>0) { # If she had at least one offspring in that year
    
    offspring <- 1 # Code offspring as one
    
    # Run the function survival_info on each of those offspring (she could have had twins)
    offspring_sub$surv_cat<-NA
    
    for(i in 1:dim(offspring_sub)[1]){
      
      offspring_sub$surv_cat[i]<-survival_info(offspring_sub$ID[i])$surv_cat
      offspring_sub$offspring[i] <- ifelse(grepl("neonate", offspring_sub$surv_cat[i]), "1", "0") 
      offspring_sub$weaned[i] <- ifelse(grepl("first_winter", offspring_sub$surv_cat[i]), "1", "0")
      offspring_sub$recruit[i] <- ifelse(grepl("recruit", offspring_sub$surv_cat[i]), "1", "0")
      
      if (offspring_sub$recruit[i] == "1") {
        offspring_sub$weaned[i] <- "1"
      }
    }
    
    # If there are at least one neonate that year, code as 1
    if(sum(offspring_sub$weaned >= 1)) {
      weaned <- 1
    } else {
      weaned <- 0
    }
    
    # If the ewe has at least one recruit in that year, code recruit as 1 
    if(sum(offspring_sub$recruit >= 1)) {
      recruit <- 1
    } else {
      recruit <- 0
    }
    
    if(dim(offspring_sub)[1] == 2) {
      twins <- 1
    } else {
      twins <- 0
    }
    
    if(dim(offspring_sub)[1] == 1){
      off_weight <- offspring_sub$BirthWt
    }
    
    if(dim(offspring_sub)[1] > 1) {
      off_weight <- mean(offspring_sub$BirthWt)
    }
    
    return(list(
      offspring,
      weaned,
      recruit,
      twins,
      off_weight
    ))
    
  } else {
    return(list(
      offspring = 0,
      weaned = 0,
      recruit = 0,
      twins = 0,
      off_weight = NA
    ))
  }
}
