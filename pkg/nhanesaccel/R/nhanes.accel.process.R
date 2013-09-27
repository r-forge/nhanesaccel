nhanes.accel.process <-
function(waves=3, directory=getwd(), brevity=1, valid.days=1, 
         valid.week.days=0, valid.weekend.days=0,int.cuts=c(100,760,2020,5999),
         youth.mod.cuts=rep(int.cuts[3],12),youth.vig.cuts=rep(int.cuts[4],12),
         save.dayfile=FALSE, days.distinct=FALSE, nonwear.window=60, nonwear.tol=0,
         nonwear.tol.upper=99, nonwear.nci=FALSE, weartime.minimum=600, 
         weartime.maximum=1200, use.partialdays=FALSE, active.bout.length=10, 
         active.bout.tol=0, mvpa.bout.tol.lower=0, vig.bout.tol.lower=0, 
         active.bout.nci=FALSE, sed.bout.tol=0, sed.bout.tol.maximum=int.cuts[2]-1, 
         artifact.thresh = 25000, artifact.action = 1, weekday.weekend=FALSE) {
  
  # If waves parameter not set to 1, 2, or 3, stop function and output error message to user
  if (sum(waves==c(1,2,3))==0) {
    stop("For waves= option, please enter 1 for NHANES 2003-2004, 2 for NHANES 2005-2006, or 3 for both")
  }
  
  # If brevity out of range, output error
  if (sum(brevity==c(1,2,3))==0) {
    stop("For brevity= option, please enter 1, 2, or 3 (see documentation)")
  }
  
  # If valid.days, valid.week.days, or valid.weekend.days out of range, output error
  if (valid.days<1 | valid.days>7 | valid.week.days>5 | valid.weekend.days > 2) {
    stop("For valid.days= option, please enter value between 1 and 7; for valid.week.days= 
           and valid.weekend.days= options, please enter values no greater than 5 and 2, 
           respectively")
  }
  
  # If length of int.cuts is not 4, or if values are out of range, output error
  if (length(int.cuts)!=4 | sum(int.cuts<0)>0) {
    stop("For int.cuts= option, please enter a vector of 4 non-negative values")
  }
  
  # If length of youth.mod.cuts or youth.vig.cuts is not 12, or if values are out of range, output error
  if (length(youth.mod.cuts)!=12 | length(youth.vig.cuts)!=12 | min(youth.mod.cuts,youth.vig.cuts)<1 | max(youth.mod.cuts,youth.vig.cuts)>32767) {
    stop("For youth.mod.cuts= and youth.vig.cuts= options, please enter vector of 12 values between 1 and 32767")
  }
  
  # If save.dayfile is not a logical, output error
  if (!is.logical(save.dayfile)) {
    stop("For save.dayfile= option, please enter TRUE or FALSE")
  }
  
  # If days.distinct is not a logical, output error
  if (!is.logical(days.distinct)) {
    stop("For days.distinct= option, please enter TRUE or FALSE")
  }
  
  # If If nonwear.window is out of range, output error
  if (nonwear.window<1) {
    stop("For nonwear.window= option, please enter positive value")
  }
  
  # If nonwear.tol out of range, output error
  if (nonwear.tol<0 | nonwear.tol>=nonwear.window) {
    stop("For nonwear.tol= option, please enter non-negative value less than nonwear.window")
  }
  
  # If nonwear.tol.upper out of range, output error
  if (nonwear.tol.upper<0) {
    stop("For nonwear.tol.upper= option, please enter non-negative value")
  }
  
  # If nonwear.nci is not a logical, output error
  if (!is.logical(nonwear.nci)) {
    stop("For nonwear.nci= option, please enter TRUE or FALSE")
  }
  
  # If weartime.minimum out of range, output error
  if (weartime.minimum <=0) {
    stop("For weartime.minimum= option, please enter positive value")
  }
  
  # If weartime.maximum out of range, output error
  if (weartime.maximum<=weartime.minimum) {
    stop("For weartime.maximum= option, please enter positive value greater than weartime.minimum")
  }
  
  # If use.partialdays is not a logical, output error
  if (!is.logical(use.partialdays)) {
    stop("For use.partialdays= option, please enter TRUE or FALSE")
  }
  
  # If active.bout.length out of range, output error
  if (active.bout.length<=1) {
    stop("For active.bout.length= option, please enter value greater than 1")
  }
  
  # If active.bout.tol out of range, output error
  if (active.bout.tol<0 | active.bout.tol>=active.bout.length) {
    stop("For active.bout.tol= option, please enter non-negative value less than active.bout.tol")
  }
  
  # If mvpa.bout.tol.lower out of range, output error
  if (mvpa.bout.tol.lower<0 | mvpa.bout.tol.lower>int.cuts[3]) {
    stop("For mvpa.bout.tol.lower= option, please enter non-negative value no greater than int.cuts[3]")
  }
  
  # If vig.bout.tol.lower out of range, output error
  if (vig.bout.tol.lower<0 | vig.bout.tol.lower>int.cuts[4]) {
    stop("For vig.bout.tol.lower= option, please enter non-negative value no greater than int.cuts[4]")
  }
  
  # If active.bout.nci is not a logical, output error
  if (!is.logical(active.bout.nci)) {
    stop("For active.bout.nci= option, please enter TRUE or FALSE")
  }
  
  # If sed.bout.tol out of range, output error
  if (sed.bout.tol<0 | sed.bout.tol>=10) {
    stop("For sed.bout.tol= option, please enter non-negative value less than 10")
  }
  
  # If sed.bout.tol.maximum out of range, output error
  if (sed.bout.tol.maximum<0) {
    stop("For sed.tol.maximum= option, please enter non-negative value")
  }
  
  # If artifact.thresh out of range, output error
  if (artifact.thresh <= int.cuts[4]) {
    stop("For artifact.thresh= option, please enter value greater than int.cuts[4]")
  }
  
  # If artifact.action out of range, output error
  if (sum(artifact.action==c(1,2,3,4))==0) {
    stop("For artifact.action= option, please enter 1, 2, 3, or 4 (see documentation)")
  }
  
  # If weekday.weekend is not a logical, output error
  if (!is.logical(weekday.weekend)) {
    stop("For weekday.weekend= option, please enter TRUE or FALSE")
  }
  
  # Save user-defined int.cuts in variable int.cuts.original
  int.cuts.original = int.cuts
  
  # Set variables to NULL to avoid notes from CRAN check
  wave1_ages=wave1_demo=wave1_paxcal=wave1_paxday=wave1_paxinten=wave1_paxstat=wave1_seqn=NULL
  wave2_ages=wave2_demo=wave2_paxcal=wave2_paxday=wave2_paxinten=wave2_paxstat=wave2_paxstep=wave2_seqn=NULL

  # Compute daily physical activity variables for NHANES 2003-2004
  if (waves==1 | waves==3) {

    # Load in data for NHANES 2003-2004
    pb = tkProgressBar(title="Processing NHANES data",label="NHANES 03-04 raw data files loaded",
                       min=0,max=5,initial=0,width=300)
    data("wave1_seqn", envir=environment())
    setTkProgressBar(pb,1)
    data("wave1_paxstat", envir=environment())
    setTkProgressBar(pb,2)
    data("wave1_paxcal", envir=environment())
    setTkProgressBar(pb,3)
    data("wave1_paxday", envir=environment())
    setTkProgressBar(pb,4)
    data("wave1_paxinten", envir=environment())
    setTkProgressBar(pb,5)
    close(pb)
    
    # Finding start and end points of each ID
    indices = seq(1,length(wave1_seqn),10080)
    repeat {
      a1 = 1:length(indices)
      a2 = which(wave1_seqn[indices[a1]]!=wave1_seqn[indices[a1+1]-1])[1]
      if (is.na(a2)) {break} else {
        a3 = which(wave1_seqn[indices[a2]:(indices[a2]+10079)]!=wave1_seqn[indices[a2]])[1]
        indices = c(indices[1:a2],(indices[a2]+a3-1),seq(indices[a2]+a3-1+10080,length(wave1_seqn),10080))
      }
    }
    ids = wave1_seqn[indices]
    mat1 = matrix(NA,nrow=length(ids),ncol=3)
    mat1[,1] = wave1_seqn[indices]
    mat1[,2] = indices
    j = 1:(length(indices)-1)
    mat1[j,3]=mat1[j+1,2]-1
    mat1[max(j)+1,3]=length(wave1_seqn)
    rm(j,indices)
    
    # Drop vector that is no longer of use
    rm(wave1_seqn)
    
    # Load in vector of ages for sample
    data("wave1_ages", envir=environment())
    
    # Initializing matrix to save daily physical activity variables
    dayvars1 = matrix(NA,ncol=66,nrow=length(ids)*7)
    
    # k is the "day counter"
    k = 0
    
    # Initializing status bar
    pb = tkProgressBar(title="Processing NHANES data",label="NHANES 03-04 observations processed",
                       min=0,max=length(ids),initial=0,width=300)
    
    # Looping through accelerometer data for i participants
    for (i in 1:length(ids)) { 
      
      # Update status bar after every 50th participant's data is processed
      if (i%%50==0) {setTkProgressBar(pb,(i-1))}
      
      # Load in accelerometer data for participant i
      week.paxday = wave1_paxday[mat1[i,2]:mat1[i,3]]
      week.paxinten = wave1_paxinten[mat1[i,2]:mat1[i,3]]
      
      # Getting value for paxstat and paxcal
      stat = wave1_paxstat[mat1[i,2]]
      cal = wave1_paxcal[mat1[i,2]]
      
      # Getting number of data points for participant i
      weeklength = length(week.paxinten)
      
      # If participant has less than weartime.minimum minutes of data or status or calibration > 1, skip
      if (weeklength<weartime.minimum | stat>1 | cal>1) {
        k = k + 1
        dayvars1[k,1] = ids[i]
        dayvars1[k,2:3] = 0
        next
      }
      
      # If artifact.action = 3, replace minutes with counts >= artifact.thresh with average of surrounding minutes
      if (artifact.action==3) {week.paxinten = accel.artifacts(counts=week.paxinten,thresh=artifact.thresh)}

      # Call weartime.flag function to flag minutes valid for analysis
      week.wearflag = accel.weartime(counts=week.paxinten,
                                     window=nonwear.window,
                                     tol=nonwear.tol,
                                     tol.upper=nonwear.tol.upper,
                                     nci=nonwear.nci,
                                     days.distinct=days.distinct,
                                     skipchecks=TRUE)
      
      # If artifact.action = 2, consider minutes with counts >= artifact.thresh as non-weartime
      if (artifact.action==2) {
        artifact.locs = which(week.paxinten>=artifact.thresh)
        week.wearflag[artifact.locs] = 0
        week.paxinten[artifact.locs] = 0
      }
      
      # If total weartime is less than weartime.minimum, no chance of having 1 valid day so skip
      if (sum(week.wearflag)<weartime.minimum) {
        k = k + 1
        dayvars1[k,1] = ids[i]
        dayvars1[k,2:3] = 0
        next
      }
      
      if (brevity==2 | brevity==3) {
        
        # Assign cut-points for MVPA and vigorous PA according to age
        int.cuts = int.cuts.original
        if (wave1_ages[i]<18) {
          int.cuts[3]=youth.mod.cuts[wave1_ages[i]-5]
          int.cuts[4]=youth.vig.cuts[wave1_ages[i]-5]
        }
        
        # Identify bouts of MVPA, VPA, and sedentary time
        week.boutedMVPA = accel.bouts(counts=week.paxinten,
                                      weartime=week.wearflag,
                                      bout.length=active.bout.length,
                                      thresh.lower=int.cuts[3],
                                      tol=active.bout.tol,
                                      tol.lower=mvpa.bout.tol.lower,
                                      nci=active.bout.nci,
                                      days.distinct=days.distinct,
                                      skipchecks=TRUE)
        week.boutedvig = accel.bouts(counts=week.paxinten,
                                     weartime=week.wearflag,
                                     bout.length=active.bout.length,
                                     thresh.lower=int.cuts[4],
                                     tol=active.bout.tol,
                                     tol.lower=vig.bout.tol.lower,
                                     nci=active.bout.nci,
                                     days.distinct=days.distinct,
                                     skipchecks=TRUE)
        week.boutedsed10 = accel.bouts(counts=week.paxinten,
                                       weartime=week.wearflag,
                                       bout.length=10,
                                       thresh.upper=int.cuts[1]-1,
                                       tol=sed.bout.tol,
                                       tol.upper=sed.bout.tol.maximum,
                                       days.distinct=days.distinct,
                                       skipchecks=TRUE)
        week.boutedsed30 = accel.bouts(counts=week.paxinten,
                                       weartime=week.wearflag,
                                       bout.length=30,
                                       thresh.upper=int.cuts[1]-1,
                                       tol=sed.bout.tol,
                                       tol.upper=sed.bout.tol.maximum,
                                       days.distinct=days.distinct,
                                       skipchecks=TRUE)
        week.boutedsed60 = accel.bouts(counts=week.paxinten,
                                       weartime=week.wearflag,
                                       bout.length=60,
                                       thresh.upper=int.cuts[1]-1,
                                       tol=sed.bout.tol,
                                       tol.upper=sed.bout.tol.maximum,
                                       days.distinct=days.distinct,
                                       skipchecks=TRUE)
      }
  
      # Loop through days and generate physical activity variables
      for (j in 1:7) {
        
        # Row index for physical activity variable matrix
        k = k + 1
        
        # Loading data from weekly vectors into daily vectors
        day.paxinten = week.paxinten[(1440*j-1439):min(1440*j,weeklength)]
        day.wearflag = week.wearflag[(1440*j-1439):min(1440*j,weeklength)]
        if (brevity==2 | brevity==3) {
          day.boutedMVPA = week.boutedMVPA[(1440*j-1439):min(1440*j,weeklength)]
          day.boutedvig = week.boutedvig[(1440*j-1439):min(1440*j,weeklength)]
          day.boutedsed10 = week.boutedsed10[(1440*j-1439):min(1440*j,weeklength)]
          day.boutedsed30 = week.boutedsed30[(1440*j-1439):min(1440*j,weeklength)]
          day.boutedsed60 = week.boutedsed60[(1440*j-1439):min(1440*j,weeklength)]
        }
        
        # Calculating constants that are used more than once
        daywear = sum(day.wearflag)
        maxcount = max(day.paxinten)
        daylength = length(day.paxinten)
         
        # Outputting variables to dayvars1 matrix
        
        # NHANES ID number
        dayvars1[k,1] = ids[i]
        
        # Day of week
        dayvars1[k,2] = week.paxday[1440*j-1439]
        
        # Check whether day is valid for analysis; if not, mark as invalid and skip rest of loop
        if (daywear<weartime.minimum | daywear>weartime.maximum | (artifact.action==1 & maxcount>=artifact.thresh) |
              (use.partialdays==FALSE & daylength<1440)) {
          dayvars1[k,3] = 0
          if (weeklength<=j*1440) {break} else {next}
        } else {dayvars1[k,3] = 1}
        
        # Minutes of valid weartime
        dayvars1[k,4] = daywear
    
        # Storing day.paxinten[day.wearflag==1] into its own vector
        day.paxinten.valid = day.paxinten[day.wearflag==1]
         
        # Total counts during weartime
        dayvars1[k,5] = sum(day.paxinten.valid)
        
        # Counts per minute - calculated as total counts during weartime divided by weartime
        dayvars1[k,6] = dayvars1[k,5]/dayvars1[k,4]
        
        if (brevity==2 | brevity==3) {
          
          # Minutes in various intensity levels
          intensities = accel.intensities(counts=day.paxinten.valid,thresh=int.cuts)
          dayvars1[k,8:15] = intensities[1:8]
          
          # Proportions of daily weartime in each intensity level
          dayvars1[k,16:23] = dayvars1[k,8:15]/daywear
          
          # Counts accumulated during weartime in each intensity level  
          dayvars1[k,24:31] = intensities[9:16]
           
          # Bouted sedentary time
          dayvars1[k,32] = sum(day.boutedsed10)
          dayvars1[k,33] = sum(day.boutedsed30)
          dayvars1[k,34] = sum(day.boutedsed60)
    
          # Sedentary breaks
          dayvars1[k,35] = accel.sedbreaks(counts=day.paxinten,weartime=day.wearflag,thresh=int.cuts[1],skipchecks=TRUE)
          
          # Maximum 1-min, 5-min, 10-min, and 30-min count averages
          dayvars1[k,36] = maxcount
          dayvars1[k,37] = movingaves(x=day.paxinten,window=5,return.max=TRUE,skipchecks=TRUE)
          dayvars1[k,38] = movingaves(x=day.paxinten,window=10,return.max=TRUE,skipchecks=TRUE)
          dayvars1[k,39] = movingaves(x=day.paxinten,window=30,return.max=TRUE,skipchecks=TRUE)
          
          # MVPA and vigorous physical activity in >= 10-min bouts
          dayvars1[k,40] = sum(day.boutedMVPA)
          dayvars1[k,41] = sum(day.boutedvig)
          dayvars1[k,42] = sum(dayvars1[k,40:41])
          
          if (brevity==3) {
            
            # Hourly counts/min averages
            if (daylength==1440) {
              dayvars1[k,43:66] = blockaves(x=day.paxinten,window=60,skipchecks=TRUE)
            }
            
          }
          
        }
      
        # Break from day-to-day for loop if reached end of data
        if (weeklength<=j*1440) {break}
      }
      
    }
    
    # Close status bar
    close(pb)
    
    # Delete empty rows in dayvars1
    dayvars1 = dayvars1[1:k,]

    # Calculate per-person averages
    personaves1 = personvars(dayvars=dayvars1,rows=i,days=valid.days,wk=valid.week.days,we=valid.weekend.days)
    
    # Calculate adjusted 2-year MEC weight
    personaves1 = nhanes.accel.reweight(acceldata=personaves1,wave=1,seqn.column=1,include.column=5)
    
    # Add variables nhanes_wave and adjusted 4-year MEC weight
    personaves1 = cbind(personaves1[,1],rep(1,nrow(personaves1)),personaves1[,2:195],personaves1[,195]/2)
  
    # Clear variables   
    rm(wave1_paxstat,wave1_paxcal,wave1_paxday,wave1_paxinten,wave1_ages)
    
  }
  
  # Compute daily physical activity variables for NHANES 2003-2004
  if (waves==2 | waves==3) {

    # Load in data for NHANES 2005-2006
    pb = tkProgressBar(title="Processing NHANES data",label="NHANES 05-06 raw data files loaded",
                       min=0,max=6,initial=0,width=300)
    data("wave2_seqn", envir=environment())
    setTkProgressBar(pb,1)
    data("wave2_paxstat", envir=environment())
    setTkProgressBar(pb,2)
    data("wave2_paxcal", envir=environment())
    setTkProgressBar(pb,3)
    data("wave2_paxday", envir=environment())
    setTkProgressBar(pb,4)
    data("wave2_paxinten", envir=environment())
    setTkProgressBar(pb,5)
    data("wave2_paxstep", envir=environment())
    setTkProgressBar(pb,6)
    close(pb)
    
    # Finding start and end points of each ID
    indices = seq(1,length(wave2_seqn),10080)
    repeat {
      a1 = 1:length(indices)
      a2 = which(wave2_seqn[indices[a1]]!=wave2_seqn[indices[a1+1]-1])[1]
      if (is.na(a2)) {break} else {
        a3 = which(wave2_seqn[indices[a2]:(indices[a2]+10079)]!=wave2_seqn[indices[a2]])[1]
        indices = c(indices[1:a2],(indices[a2]+a3-1),seq(indices[a2]+a3-1+10080,length(wave2_seqn),10080))
      }
    }
    ids = wave2_seqn[indices]
    mat1 = matrix(NA,nrow=length(ids),ncol=3)
    mat1[,1] = wave2_seqn[indices]
    mat1[,2] = indices
    j = 1:(length(indices)-1)
    mat1[j,3]=mat1[j+1,2]-1
    mat1[max(j)+1,3]=length(wave2_seqn)
    rm(j,indices)
    
    # Drop vector that is no longer of use
    rm(wave2_seqn)
    
    # Load in vector of ages for sample
    data("wave2_ages", envir=environment())
    
    # Initializing matrix to save daily physical activity variables
    dayvars2 = matrix(NA,ncol=66,nrow=length(ids)*7)
    
    # k is the "day counter"
    k = 0
    
    # Initializing status bar
    pb = tkProgressBar(title="Processing NHANES data",label="NHANES 05-06 observations processed",
                       min=0,max=length(ids),initial=0,width=300)
    
    # Looping through accelerometer data for i participants
    for (i in 1:length(ids)) { 
      
      # Update status bar after every 50th participant's data is processed
      if (i%%50==0) {setTkProgressBar(pb,(i-1))}
      
      # Load in accelerometer data for participant i
      week.paxday = wave2_paxday[mat1[i,2]:mat1[i,3]]
      week.paxinten = wave2_paxinten[mat1[i,2]:mat1[i,3]]
      week.paxstep = wave2_paxstep[mat1[i,2]:mat1[i,3]]
      
      # Getting value for paxstat and paxcal
      stat = wave2_paxstat[mat1[i,2]]
      cal = wave2_paxcal[mat1[i,2]]
      
      # Getting number of data points for participant i
      weeklength = length(week.paxinten)
      
      # If participant has less than weartime.minimum minutes of data or status or calibration > 1, skip
      if (weeklength<weartime.minimum | stat>1 | cal>1) {
        k = k + 1
        dayvars2[k,1] = ids[i]
        dayvars2[k,2:3] = 0
        next
      }
      
      # If artifact.action = 3, replace minutes with counts >= artifact.thresh with average of surrounding minutes
      if (artifact.action==3) {week.paxinten = accel.artifacts(counts=week.paxinten,thresh=artifact.thresh)}
      
      # Call weartime.flag function to flag minutes valid for analysis
      week.wearflag = accel.weartime(counts=week.paxinten,
                                     window=nonwear.window,
                                     tol=nonwear.tol,
                                     tol.upper=nonwear.tol.upper,
                                     nci=nonwear.nci,
                                     days.distinct=days.distinct,
                                     skipchecks=TRUE)
      
      # If artifact.action = 2, consider minutes with counts >= artifact.thresh as non-weartime
      if (artifact.action==2) {
        artifact.locs = which(week.paxinten>=artifact.thresh)
        week.wearflag[artifact.locs] = 0
        week.paxinten[artifact.locs] = 0
      }
      
      # If total weartime is less than weartime.minimum, no chance of having 1 valid day so skip
      if (sum(week.wearflag)<weartime.minimum) {
        k = k + 1
        dayvars2[k,1] = ids[i]
        dayvars2[k,2:3] = 0
        next
      }
      
      if (brevity==2 | brevity==3) {
        
        # Assign cut-points for MVPA and vigorous PA according to age
        int.cuts = int.cuts.original
        if (wave2_ages[i]<18) {
          int.cuts[3]=youth.mod.cuts[wave2_ages[i]-5]
          int.cuts[4]=youth.vig.cuts[wave2_ages[i]-5]
        }
        
        # Identify bouts of MVPA, VPA, and sedentary time
        week.boutedMVPA = accel.bouts(counts=week.paxinten,
                                      weartime=week.wearflag,
                                      bout.length=active.bout.length,
                                      thresh.lower=int.cuts[3],
                                      tol=active.bout.tol,
                                      tol.lower=mvpa.bout.tol.lower,
                                      nci=active.bout.nci,
                                      days.distinct=days.distinct,
                                      skipchecks=TRUE)
        week.boutedvig = accel.bouts(counts=week.paxinten,
                                     weartime=week.wearflag,
                                     bout.length=active.bout.length,
                                     thresh.lower=int.cuts[4],
                                     tol=active.bout.tol,
                                     tol.lower=vig.bout.tol.lower,
                                     nci=active.bout.nci,
                                     days.distinct=days.distinct,
                                     skipchecks=TRUE)
        week.boutedsed10 = accel.bouts(counts=week.paxinten,
                                       weartime=week.wearflag,
                                       bout.length=10,
                                       thresh.upper=int.cuts[1]-1,
                                       tol=sed.bout.tol,
                                       tol.upper=sed.bout.tol.maximum,
                                       days.distinct=days.distinct,
                                       skipchecks=TRUE)
        week.boutedsed30 = accel.bouts(counts=week.paxinten,
                                       weartime=week.wearflag,
                                       bout.length=30,
                                       thresh.upper=int.cuts[1]-1,
                                       tol=sed.bout.tol,
                                       tol.upper=sed.bout.tol.maximum,
                                       days.distinct=days.distinct,
                                       skipchecks=TRUE)
        week.boutedsed60 = accel.bouts(counts=week.paxinten,
                                       weartime=week.wearflag,
                                       bout.length=60,
                                       thresh.upper=int.cuts[1]-1,
                                       tol=sed.bout.tol,
                                       tol.upper=sed.bout.tol.maximum,
                                       days.distinct=days.distinct,
                                       skipchecks=TRUE)
      }
      
      # Loop through days and generate physical activity variables
      for (j in 1:7) {
        
        # Row index for physical activity variable matrix
        k = k + 1
        
        # Loading data from weekly vectors into daily vectors
        day.paxinten = week.paxinten[(1440*j-1439):min(1440*j,weeklength)]
        day.paxstep = week.paxstep[(1440*j-1439):min(1440*j,weeklength)]
        day.wearflag = week.wearflag[(1440*j-1439):min(1440*j,weeklength)]
        if (brevity==2 | brevity==3) {
          day.boutedMVPA = week.boutedMVPA[(1440*j-1439):min(1440*j,weeklength)]
          day.boutedvig = week.boutedvig[(1440*j-1439):min(1440*j,weeklength)]
          day.boutedsed10 = week.boutedsed10[(1440*j-1439):min(1440*j,weeklength)]
          day.boutedsed30 = week.boutedsed30[(1440*j-1439):min(1440*j,weeklength)]
          day.boutedsed60 = week.boutedsed60[(1440*j-1439):min(1440*j,weeklength)]
        }
        
        # Calculating constants that are used more than once
        daywear = sum(day.wearflag)
        maxcount = max(day.paxinten)
        daylength = length(day.paxinten)
        
        # Outputting variables to dayvars2 matrix
        
        # NHANES ID number
        dayvars2[k,1] = ids[i]
        
        # Day of week
        dayvars2[k,2] = week.paxday[1440*j-1439]
        
        # Check whether day is valid for analysis; if not, mark as invalid and skip rest of loop
        if (daywear<weartime.minimum | daywear>weartime.maximum | (artifact.action==1 & maxcount>=artifact.thresh) |
              (use.partialdays==FALSE & daylength<1440)) {
          dayvars2[k,3] = 0
          if (weeklength<=j*1440) {break} else {next}
        } else {dayvars2[k,3] = 1}
        
        # Minutes of valid weartime
        dayvars2[k,4] = daywear
        
        # Storing day.paxinten[day.wearflag==1] into its own vector
        day.paxinten.valid = day.paxinten[day.wearflag==1]
        
        # Total counts during weartime
        dayvars2[k,5] = sum(day.paxinten.valid)
        
        # Counts per minute - calculated as total counts during weartime divided by weartime
        dayvars2[k,6] = dayvars2[k,5]/dayvars2[k,4]
        
        if (brevity==2 | brevity==3) {
        
          # Steps
          dayvars2[k,7] = sum(day.paxstep[day.wearflag==1])
          
          # Minutes in various intensity levels
          intensities = accel.intensities(counts=day.paxinten.valid,thresh=int.cuts)
          dayvars2[k,8:15] = intensities[1:8]
          
          # Proportions of daily weartime in each intensity level
          dayvars2[k,16:23] = dayvars2[k,8:15]/daywear
          
          # Counts accumulated during weartime in each intensity level  
          dayvars2[k,24:31] = intensities[9:16]
          
          # Bouted sedentary time
          dayvars2[k,32] = sum(day.boutedsed10)
          dayvars2[k,33] = sum(day.boutedsed30)
          dayvars2[k,34] = sum(day.boutedsed60)
          
          # Sedentary breaks
          dayvars2[k,35] = accel.sedbreaks(counts=day.paxinten,weartime=day.wearflag,thresh=int.cuts[1],skipchecks=TRUE)
          
          # Maximum 1-min, 5-min, 10-min, and 30-min count averages
          dayvars2[k,36] = maxcount
          dayvars2[k,37] = movingaves(x=day.paxinten,window=5,return.max=TRUE,skipchecks=TRUE)
          dayvars2[k,38] = movingaves(x=day.paxinten,window=10,return.max=TRUE,skipchecks=TRUE)
          dayvars2[k,39] = movingaves(x=day.paxinten,window=30,return.max=TRUE,skipchecks=TRUE)
          
          # MVPA and vigorous physical activity in >= 10-min bouts
          dayvars2[k,40] = sum(day.boutedMVPA)
          dayvars2[k,41] = sum(day.boutedvig)
          dayvars2[k,42] = sum(dayvars2[k,40:41])
          
          if (brevity==3) {
            
            # Hourly counts/min averages
            if (daylength==1440) {
              dayvars2[k,43:66] = blockaves(x=day.paxinten,window=60,skipchecks=TRUE)
            }
            
          }
          
        }
        
        # Break from day-to-day for loop if reached end of data
        if (weeklength<=j*1440) {break}
      }
      
    }
    
    # Close status bar
    close(pb)
    
    # Delete empty rows in dayvars2
    dayvars2 = dayvars2[1:k,]
    
    # Calculate per-person averages
    personaves2 = personvars(dayvars=dayvars2,rows=i,days=valid.days,wk=valid.week.days,we=valid.weekend.days)
    
    # Calculate adjusted 2-year MEC weight
    personaves2 = nhanes.accel.reweight(acceldata=personaves2,wave=2,seqn.column=1,include.column=5)
    
    # Add variables nhanes_wave and adjusted 4-year MEC weight
    personaves2 = cbind(personaves2[,1],rep(2,nrow(personaves2)),personaves2[,2:195],personaves2[,195]/2)
    
    # Clear variables
    rm(wave2_paxstat,wave2_paxcal,wave2_paxday,wave2_paxinten,wave2_paxstep,wave2_ages)

  }
  
  # Combine 2003-2004 and 2005-2006 data if necessary
  if (waves==1) {
    dayvars = dayvars1
    personaves = personaves1
  } else if (waves==2) {
    dayvars = dayvars2
    personaves = personaves2
  } else if (waves==3) {
    dayvars = rbind(dayvars1,dayvars2)
    personaves = rbind(personaves1,personaves2)
  }
  
  # Write .csv file with day-to-day variables if requested
  if (save.dayfile==TRUE) {
    
    # Add variable indicating which NHANES wave each participant is from
    dayvars = cbind(dayvars[,1],rep(NA,nrow(dayvars)),dayvars[,2:66])
    dayvars[dayvars[,1]<=31125,2] = 1
    dayvars[dayvars[,1]>31125,2] = 2
    
    # Add variable names to per-day dataset
    colnames(dayvars) = c("seqn","nhanes_wave","day","valid_day","valid_min","counts","cpm","steps","sed_min",
                          "light_min","life_min","mod_min","vig_min","lightlife_min","mvpa_min",
                          "active_min","sed_percent","light_percent","life_percent",
                          "mod_percent","vig_percent","lightlife_percent","mvpa_percent",
                          "active_percent","sed_counts","light_counts","life_counts",
                          "mod_counts","vig_counts","lightlife_counts","mvpa_counts",
                          "active_counts","sed_bouted_10min","sed_bouted_30min",
                          "sed_bouted_60min","sed_breaks","max_1min_counts","max_5min_counts",
                          "max_10min_counts","max_30min_counts","mvpa_bouted","vig_bouted",
                          "guideline_min","cpm_hour1","cpm_hour2","cpm_hour3","cpm_hour4",
                          "cpm_hour5","cpm_hour6","cpm_hour7","cpm_hour8","cpm_hour9",
                          "cpm_hour10","cpm_hour11","cpm_hour12","cpm_hour13","cpm_hour14",
                          "cpm_hour15","cpm_hour16","cpm_hour17","cpm_hour18","cpm_hour19",
                          "cpm_hour20","cpm_hour21","cpm_hour22","cpm_hour23","cpm_hour24")
    
    # Drop variables according to brevity setting
    if (brevity==1) {dayvars = dayvars[,1:7]}
    else if (brevity==2) {dayvars = dayvars[,1:43]}
    
    # Write file with day-to-day variables
    curdate = as.character(Sys.Date())
    filename = paste("NHANES_accel_days_",curdate,".csv",sep="")
    write.csv(x=dayvars,file=filename,quote=FALSE,row.names=FALSE,na="")
  }
  
  # Add variable names to per-person dataset
  varnames = c("seqn","nhanes_wave","valid_days","valid_week_days","valid_weekend_days",
               "include","valid_min","counts","cpm","steps","sed_min","light_min",
               "life_min","mod_min","vig_min","lightlife_min","mvpa_min",
               "active_min","sed_percent","light_percent","life_percent",
               "mod_percent","vig_percent","lightlife_percent","mvpa_percent",
               "active_percent","sed_counts","light_counts","life_counts",
               "mod_counts","vig_counts","lightlife_counts","mvpa_counts",
               "active_counts","sed_bouted_10min","sed_bouted_30min",
               "sed_bouted_60min","sed_breaks","max_1min_counts","max_5min_counts",
               "max_10min_counts","max_30min_counts","mvpa_bouted","vig_bouted",
               "guideline_min","cpm_hour1","cpm_hour2","cpm_hour3","cpm_hour4","cpm_hour5",
               "cpm_hour6","cpm_hour7","cpm_hour8","cpm_hour9","cpm_hour10","cpm_hour11",
               "cpm_hour12","cpm_hour13","cpm_hour14","cpm_hour15","cpm_hour16",
               "cpm_hour17","cpm_hour18","cpm_hour19","cpm_hour20","cpm_hour21",
               "cpm_hour22","cpm_hour23","cpm_hour24")
  varnames = c(varnames,paste("wk_",varnames[7:69],sep=""),paste("we_",varnames[7:69],sep=""),"wtmec2yr_adj","wtmec4yr_adj")
  colnames(personaves) = varnames
  
  # Drop variables according to brevity and weekday.weekend settings
  if (brevity==1) {
    if (weekday.weekend==TRUE) {personaves = personaves[,c(1:9,70:72,133:135,196:197)]}
    else {personaves = personaves[,c(1:9,196:197)]}
  } else if (brevity==2) {
    if (weekday.weekend==TRUE) {personaves = personaves[,c(1:45,70:108,133:171,196:197)]}
    else {personaves = personaves[,c(1:45,196:197)]}
  } else if (brevity==3) {
    if (weekday.weekend==FALSE) {personaves = personaves[,c(1:69,196:197)]}
  }
  
  # Write file with daily averages
  curdate = as.character(Sys.Date())
  filename = paste("NHANES_accel_",curdate,".csv",sep="")
  write.csv(x=personaves,file=filename,quote=FALSE,row.names=FALSE,na="")
  
  # Write .csv file with function settings
  settings = c("waves",waves,"valid.days",valid.days,"valid.week.days",valid.week.days,
    "valid.weekend.days",valid.weekend.days,"int.cuts",int.cuts,"youth.mod.cuts",youth.mod.cuts,"youth.vig.cuts",
    youth.vig.cuts,"days.distinct",days.distinct,"nonwear.window",nonwear.window,"nonwear.tol",nonwear.tol,
    "nonwear.tol.upper",nonwear.tol.upper,"nonwear.nci",nonwear.nci,"weartime.minimum",weartime.minimum,
    "weartime.maximum",weartime.maximum,"use.partialdays",use.partialdays,"active.bout.length",active.bout.length,
    "active.bout.tol",active.bout.tol,"active.bout.nci",active.bout.nci,"mvpa.bout.tol.lower",mvpa.bout.tol.lower,
    "vig.bout.tol.lower",vig.bout.tol.lower,"sed.bout.tol",sed.bout.tol,"sed.bout.tol.maximum",sed.bout.tol.maximum,
    "artifact.thresh",artifact.thresh,"artifact.action",artifact.action)
  settings = as.data.frame(settings)
  colnames(settings) = NULL
  filename = paste("NHANES_settings_",curdate,".csv",sep="")
  write.csv(x=settings,file="settings.csv",quote=FALSE,row.names=FALSE)
  
  # Return message to user
  message = paste("A dataset with physical activity variables has been created. Please see",directory,"for the .csv file(s). Thanks for using nhanesaccel.")
  return(message)
  
}