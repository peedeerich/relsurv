# function to extract 5 (arbitrary?) year RS from an rs.surv object

rs.extract <- function(rs.obj, yr=5){
  
  has_strata <- "strata" %in% attributes(rs.obj)$names
  
  if(!has_strata){
   rs <- rs.obj$surv[which.min(abs(rs.obj$time-365.24*yr))] 
  }
  else{
    strata <- c(0,cumsum(rs.obj$strata))
    rs     <- c()
    for (i in 1:(length(strata)-1)){
      rs.time <- rs.obj$time[strata[i]+1:strata[i+1]]
      rs.surv <- rs.obj$surv[strata[i]+1:strata[i+1]]
      rs[i] <- rs.surv[which.min(abs(rs.time-365.24*yr))]
    }
  
  names(rs) <- names(rs.obj$strata)
  
  }
  
  rs
  
}


#RS_pop_5yr <- RS_pop$surv[which.min(abs(RS_pop$time-365.24*5))]
