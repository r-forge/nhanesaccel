nhanes.accel.reweight <-
function(acceldata, wave, seqn.column, include.column) {
  
  # If accel.data is not a matrix or data frame, output error
  if (!is.matrix(acceldata) & !is.data.frame(acceldata)) {
    stop("For acceldata= option, please enter matrix or data frame")
  }
  
  # If wave is not 1 or 2, output error
  if (wave!=1 & wave!=2) {
    stop("For wave= option, please enter 1 for NHANES 2003-2004 or 2 for NHANES 2005-2006")
  }
  
  # If seqn.column or include.column are out of range, output error
  if (seqn.column<1 | seqn.column>ncol(acceldata) | include.column<1 | include.column>ncol(acceldata)) {
    stop("For seqn.column= and include.column= options, please specify which columns in
         acceldata contain the variables seqn and include, respectively")
  }
  
  # Set variables to NULL to avoid notes from CRAN check
  wave1_demo=wave2_demo=NULL
  
  # Load in demographics binary file
  if (wave==1) {
    data("wave1_demo", envir=environment())
    demo = wave1_demo
    rm(wave1_demo)
  }
  else {
    data("wave2_demo", envir=environment())
    demo = wave2_demo
    rm(wave2_demo)
  }
  
  # Calculate adjusted weights
  wtmec2yr_adj = rep(0,nrow(acceldata))
  for (a in 1:2) {
    for (b in 1:3) {
      for (c in 1:9) {
        indices1 = which(demo[,2]==a&demo[,4]==b&demo[,3]==c)
        ids_demo = demo[indices1,1]
        ids_valid = ids_demo[which(ids_demo %in% acceldata[acceldata[,include.column]==1,1])]
        indices2 = which(demo[,2]==a&demo[,4]==b&demo[,3]==c&demo[,1] %in% acceldata[acceldata[,include.column]==1,1])
        indices3 = which(acceldata[,seqn.column] %in% ids_valid)
        wtmec2yr_adj[indices3] = demo[indices2,5]*sum(demo[indices1,5])/sum(demo[indices2,5])
      }
    }
  }
  
  # Add weights column to acceldata
  acceldata = cbind(acceldata,wtmec2yr_adj)
  
  # Return acceldata
  return(acceldata)
}