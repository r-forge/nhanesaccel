nhanes.accel.process <-
function(waves = 3, directory = getwd(), brevity = 1, valid.days = 1, 
         valid.week.days = 0, valid.weekend.days = 0,int.cuts = c(100,760,2020,5999),
         youth.mod.cuts = rep(int.cuts[3],12), youth.vig.cuts = rep(int.cuts[4],12),
         cpm.nci = FALSE, days.distinct = FALSE, nonwear.window = 60, nonwear.tol = 0,
         nonwear.tol.upper = 99, nonwear.nci = FALSE, weartime.minimum = 600, 
         weartime.maximum = 1200, use.partialdays = FALSE, active.bout.length = 10, 
         active.bout.tol = 0, mvpa.bout.tol.lower = 0, vig.bout.tol.lower = 0, 
         active.bout.nci = FALSE, sed.bout.tol = 0, sed.bout.tol.maximum = int.cuts[2]-1, 
         artifact.thresh = 25000, artifact.action = 1, weekday.weekend = FALSE,
         return.form = 1, write.csv = TRUE) {
  
  # If waves not set to 1, 2, or 3, output error
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
  
  # If cpm.nci is not a logical, output error
  if (!is.logical(cpm.nci)) {
    stop("For cpm.nci= option, please enter TRUE or FALSE")
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
  
  # If return.form is out of range, output error
  if (!return.form %in% c(1,2,3)) {
    stop("For return.form= option, please enter 1 for per-person, 2 for per-day, or 3 for both")
  }
  
  # If write.csv is not a logical, output error
  if (!is.logical(write.csv)) {
    stop("For write.csv= option, please enter TRUE or FALSE")
  }
  
  # Save user-defined int.cuts in variable int.cuts.original
  int.cuts.original = int.cuts
  
  # Set variables to NULL to avoid notes from CRAN check
  w1=wave1_ages=wave1_demo=wave1_paxcal=wave1_paxday=wave1_paxinten=wave1_paxstat=wave1_seqn=NULL
  w2=wave2_ages=wave2_demo=wave2_paxcal=wave2_paxday=wave2_paxinten=wave2_paxstat=wave2_paxstep=wave2_seqn=NULL

  # Compute daily physical activity variables for NHANES 2003-2004
  if (waves==1 | waves==3) {

    # Load in data for NHANES 2003-2004
    pb = tkProgressBar(title="Processing NHANES data",label="NHANES 03-04 raw data files loaded",
                       min=0,max=2,initial=0,width=300)
    data("w1", envir=environment())
    setTkProgressBar(pb,1)
    data("wave1_paxinten", envir=environment())
    setTkProgressBar(pb,2)
    close(pb)
    
    # Start and end points of each ID
    mat1 = w1[,1:3]
    ids = mat1[,1]
    
    # Create stat, cal, day, and age vectors
    wave1_paxstat = w1[,4]
    wave1_paxcal = w1[,5]
    wave1_paxday = w1[,6]
    wave1_ages = w1[,7]
    
    # Initialize matrix to save daily physical activity variables
    dayvars1 = matrix(NA,ncol=68,nrow=length(ids)*7)
    
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
      week.paxinten = wave1_paxinten[mat1[i,2]:mat1[i,3]]
      
      # Get value for paxstat and paxcal
      stat = wave1_paxstat[i]
      cal = wave1_paxcal[i]
      
      # Get number of data points for participant i
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
      
      if (brevity>1) {
        
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
  
      # Get day of week of first day
      curday = wave1_paxday[i]
      
      # Loop through days and generate physical activity variables
      for (j in 1:7) {
        
        # Row index for physical activity variable matrix
        k = k + 1
        
        # Loading data from weekly vectors into daily vectors
        day.paxinten = week.paxinten[(1440*j-1439):min(1440*j,weeklength)]
        day.wearflag = week.wearflag[(1440*j-1439):min(1440*j,weeklength)]
        if (brevity>1) {
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
        dayvars1[k,2] = curday
        curday = curday + 1
        if (curday==8) {
          curday = 1
        }
        
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
        
        if (brevity>1) {
          
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
          dayvars1[k,42] = sum(day.boutedMVPA)
          dayvars1[k,43] = sum(day.boutedvig)
          dayvars1[k,44] = sum(dayvars1[k,42:43])
          
          if (dayvars1[k,42]>0) {
            dayvars1[k,40] = sum(rle2(day.boutedMVPA)[,1]==1)
          } else {
            dayvars1[k,40] = 0
          }
          if (dayvars1[k,43]>0) {
            dayvars1[k,41] = sum(rle2(day.boutedMVPA)[,1]==1)
          } else {
            dayvars1[k,41] = 0
          }
          
          if (brevity==3) {
            
            # Hourly counts/min averages
            if (daylength==1440) {
              dayvars1[k,45:68] = blockaves(x=day.paxinten,window=60,skipchecks=TRUE)
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
    personaves1 = cbind(personaves1[,1],rep(1,nrow(personaves1)),personaves1[,2:201],personaves1[,201]/2)
  
    # Clear variables   
    rm(w1,wave1_paxstat,wave1_paxcal,wave1_paxday,wave1_paxinten,wave1_ages)
    
  }
  
  # Compute daily physical activity variables for NHANES 2003-2004
  if (waves==2 | waves==3) {

    # Load in data for NHANES 2005-2006
    if (brevity==1) {
      pb = tkProgressBar(title="Processing NHANES data",label="NHANES 05-06 raw data files loaded",
                         min=0,max=2,initial=0,width=300)
      data("w2", envir=environment())
      setTkProgressBar(pb,1)
      data("wave2_paxinten", envir=environment())
      setTkProgressBar(pb,2)
      close(pb)
    } else {
      pb = tkProgressBar(title="Processing NHANES data",label="NHANES 05-06 raw data files loaded",
                         min=0,max=3,initial=0,width=300)
      data("w2", envir=environment())
      setTkProgressBar(pb,1)
      data("wave2_paxinten", envir=environment())
      setTkProgressBar(pb,2)
      data("wave2_paxstep", envir=environment())
      setTkProgressBar(pb,3)
      close(pb)
    }
    
    # Start and end points of each ID
    mat1 = w2[,1:3]
    ids = mat1[,1]
    
    # Create stat, cal, day, and age vectors
    wave2_paxstat = w2[,4]
    wave2_paxcal = w2[,5]
    wave2_paxday = w2[,6]
    wave2_ages = w2[,7]
    
    # Initialize matrix to save daily physical activity variables
    dayvars2 = matrix(NA,ncol=68,nrow=length(ids)*7)
    
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
      week.paxinten = wave2_paxinten[mat1[i,2]:mat1[i,3]]
      if (brevity>1) {
        week.paxstep = wave2_paxstep[mat1[i,2]:mat1[i,3]]
      }
      
      # Get value for paxstat and paxcal
      stat = wave2_paxstat[i]
      cal = wave2_paxcal[i]
      
      # Get number of data points for participant i
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
      
      if (brevity>1) {
        
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
      
      # Get day of week of first day
      curday = wave2_paxday[i]
      
      # Loop through days and generate physical activity variables
      for (j in 1:7) {
        
        # Row index for physical activity variable matrix
        k = k + 1
        
        # Loading data from weekly vectors into daily vectors
        day.paxinten = week.paxinten[(1440*j-1439):min(1440*j,weeklength)]
        day.wearflag = week.wearflag[(1440*j-1439):min(1440*j,weeklength)]
        if (brevity>1) {
          day.paxstep = week.paxstep[(1440*j-1439):min(1440*j,weeklength)]
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
        dayvars2[k,2] = curday
        curday = curday + 1
        if (curday==8) {
          curday = 1
        }
        
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
        
        if (brevity>1) {
        
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
          dayvars2[k,42] = sum(day.boutedMVPA)
          dayvars2[k,43] = sum(day.boutedvig)
          dayvars2[k,44] = sum(dayvars2[k,42:43])
          
          if (dayvars2[k,42]>0) {
            dayvars2[k,40] = sum(rle2(day.boutedMVPA)[,1]==1)
          } else {
            dayvars2[k,40] = 0
          }
          if (dayvars2[k,43]>0) {
            dayvars2[k,41] = sum(rle2(day.boutedMVPA)[,1]==1)
          } else {
            dayvars2[k,41] = 0
          }
          
          if (brevity==3) {
            
            # Hourly counts/min averages
            if (daylength==1440) {
              dayvars2[k,45:68] = blockaves(x=day.paxinten,window=60,skipchecks=TRUE)
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
    personaves2 = cbind(personaves2[,1],rep(2,nrow(personaves2)),personaves2[,2:201],personaves2[,201]/2)
    
    # Clear variables
    rm(w2,wave2_paxstat,wave2_paxcal,wave2_paxday,wave2_paxinten,wave2_paxstep,wave2_ages)

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
  
  # Prepare data frame with day-to-day variables if requested
  if (return.form %in% c(2,3)) {
    
    # Add variable indicating which NHANES wave each participant is from
    dayvars = cbind(dayvars[,1],rep(NA,nrow(dayvars)),dayvars[,2:ncol(dayvars)])
    dayvars[dayvars[,1]<=31125,2] = 1
    dayvars[dayvars[,1]>31125,2] = 2
    
    # Add variable names to per-day dataset
    colnames(dayvars) = c("seqn","nhanes_wave","day","valid_day","valid_min","counts","cpm","steps","sed_min",
                          "light_min","life_min","mod_min","vig_min","lightlife_min","mvpa_min","active_min",
                          "sed_percent","light_percent","life_percent","mod_percent","vig_percent",
                          "lightlife_percent","mvpa_percent","active_percent","sed_counts","light_counts",
                          "life_counts","mod_counts","vig_counts","lightlife_counts","mvpa_counts","active_counts",
                          "sed_bouted_10min","sed_bouted_30min","sed_bouted_60min","sed_breaks","max_1min_counts",
                          "max_5min_counts","max_10min_counts","max_30min_counts","num_mvpa_bouts","num_vig_bouts",
                          "mvpa_bouted","vig_bouted","guideline_min","cpm_hour1","cpm_hour2","cpm_hour3","cpm_hour4",
                          "cpm_hour5","cpm_hour6","cpm_hour7","cpm_hour8","cpm_hour9","cpm_hour10","cpm_hour11",
                          "cpm_hour12","cpm_hour13","cpm_hour14","cpm_hour15","cpm_hour16","cpm_hour17","cpm_hour18",
                          "cpm_hour19","cpm_hour20","cpm_hour21","cpm_hour22","cpm_hour23","cpm_hour24")
    
    # Drop variables according to waves and brevity settings
    if (brevity==1) {
      dayvars = dayvars[,1:7]
    } else if (brevity==2) {
      if (waves==1) {
        dayvars = dayvars[,c(1:7,9:45)]
      } else {
        dayvars = dayvars[,1:45]
      }
    } else if (brevity==3) {
      if (waves==1) {
        dayvars = dayvars[,c(1:7,9:ncol(dayvars))]
      }
    }
    
  }
  
  # Prepare data frame with daily averages if requested
  if (return.form %in% c(1,3)) {
  
    # Add variable names to per-person dataset
    varnames = c("seqn","nhanes_wave","valid_days","valid_week_days","valid_weekend_days","include","valid_min",
                 "counts","cpm","steps","sed_min","light_min","life_min","mod_min","vig_min","lightlife_min",
                 "mvpa_min","active_min","sed_percent","light_percent","life_percent","mod_percent","vig_percent",
                 "lightlife_percent","mvpa_percent","active_percent","sed_counts","light_counts","life_counts",
                 "mod_counts","vig_counts","lightlife_counts","mvpa_counts","active_counts","sed_bouted_10min",
                 "sed_bouted_30min","sed_bouted_60min","sed_breaks","max_1min_counts","max_5min_counts",
                 "max_10min_counts","max_30min_counts","num_mvpa_bouts","num_vig_bouts","mvpa_bouted","vig_bouted",
                 "guideline_min","cpm_hour1","cpm_hour2","cpm_hour3","cpm_hour4","cpm_hour5","cpm_hour6","cpm_hour7",
                 "cpm_hour8","cpm_hour9","cpm_hour10","cpm_hour11","cpm_hour12","cpm_hour13","cpm_hour14","cpm_hour15",
                 "cpm_hour16","cpm_hour17","cpm_hour18","cpm_hour19","cpm_hour20","cpm_hour21","cpm_hour22","cpm_hour23",
                 "cpm_hour24")
    varnames = c(varnames,paste("wk_",varnames[7:length(varnames)],sep=""),paste("we_",varnames[7:length(varnames)],sep=""),"wtmec2yr_adj","wtmec4yr_adj")
    colnames(personaves) = varnames
    
    # Drop variables according to brevity and weekday.weekend settings
    if (brevity==1) {
      if (weekday.weekend==TRUE) {
        personaves = personaves[,c(1:9,72:74,137:139,202:203)]
      } else {
        personaves = personaves[,c(1:9,202:203)]
      }
      
    } else if (brevity==2) {
      if (waves==1) {
        if (weekday.weekend==TRUE) {
          personaves = personaves[,c(1:9,11:47,72:74,76:112,137:139,141:177,202:203)]
        } else {
          personaves = personaves[,c(1:9,11:47,202:203)]
        }
      } else {
        if (weekday.weekend==TRUE) {
          personaves = personaves[,c(1:47,72:112,137:177,202:203)]
        } else {
          personaves = personaves[,c(1:47,202:203)]
        }
      }
      
    } else if (brevity==3) {
      if (waves==1) {
        if (weekday.weekend==TRUE) {
          personaves = personaves[,c(1:9,11:74,76:139,141:203)]
        } else {
          personaves = personaves[,c(1:9,11:71,202:203)]
        }
      } else {
        if (weekday.weekend==FALSE) {
          personaves = personaves[,c(1:71,202:203)]
        }
      }
    }
    
    # If cpm.nci is TRUE, re-calculate averages for cpm
    if (cpm.nci==TRUE) {
      personaves[,"cpm"] = personaves[,"counts"]/personaves[,"valid_min"]
    }
    
  }
  
  # Write .csv file(s) according to write.csv and return.form
  if (write.csv==TRUE) {
    
    # Get date for filename
    curdate = as.character(strftime(Sys.Date(), format="%Y-%m-%d"))
    
    # Write per-day file if requested
    if (return.form %in% c(2,3)) {
      filestem = "accel_days_"
      dayfile = paste(filestem,curdate,".csv",sep="")
      if (file.exists(dayfile)) {
        reps = 1
        repeat {
          reps = reps + 1
          daytest = paste(filestem,curdate,"_",reps,".csv",sep="")
          if (!file.exists(daytest)) {
            dayfile = daytest
            break
          }
        }
      }
      write.csv(x=dayvars,file=dayfile,quote=FALSE,row.names=FALSE,na="")
    }
    
    # Write per-person file if requested
    if (return.form %in% c(1,3)) {
      filestem = "accel_aves_"
      personfile = paste(filestem,curdate,".csv",sep="")
      if (file.exists(personfile)) {
        reps = 1
        repeat {
          reps = reps + 1
          persontest = paste(filestem,curdate,"_",reps,".csv",sep="")
          if (!file.exists(persontest)) {
            personfile = persontest
            break
          }
        }
      }
      write.csv(x=personaves,file=personfile,quote=FALSE,row.names=FALSE,na="")
    }
  
    # Write .csv file with function settings
    settings = c("waves",waves,"valid.days",valid.days,"valid.week.days",valid.week.days,
                 "valid.weekend.days",valid.weekend.days,"int.cuts",int.cuts,
                 "youth.mod.cuts",youth.mod.cuts,"youth.vig.cuts",youth.vig.cuts,
                 "cpm.nci",cpm.nci,"days.distinct",days.distinct,"nonwear.window",nonwear.window,
                 "nonwear.tol",nonwear.tol,"nonwear.tol.upper",nonwear.tol.upper,
                 "nonwear.nci",nonwear.nci,"weartime.minimum",weartime.minimum,
                 "weartime.maximum",weartime.maximum,"use.partialdays",use.partialdays,
                 "active.bout.length",active.bout.length,"active.bout.tol",active.bout.tol,
                 "mvpa.bout.tol.lower",mvpa.bout.tol.lower,"vig.bout.tol.lower",vig.bout.tol.lower,
                 "active.bout.nci",active.bout.nci,"sed.bout.tol",sed.bout.tol,
                 "sed.bout.tol.maximum",sed.bout.tol.maximum,"artifact.thresh",artifact.thresh,
                 "artifact.action",artifact.action)
    settings = as.data.frame(settings)
    colnames(settings) = NULL
    filestem = "settings_"
    settingsfile = paste(filestem,curdate,".csv",sep="")
    if (file.exists(settingsfile)) {
      reps = 1
      repeat {
        reps = reps + 1
        settingstest = paste(filestem,curdate,"_",reps,".csv",sep="")
        if (!file.exists(settingstest)) {
          settingsfile = settingstest
          break
        }
      }
    }
    write.csv(x=settings,file=settingsfile,quote=FALSE,row.names=FALSE)
  
    # Print message to user
    print(paste("Please see",directory,"for .csv file(s) with NHANES physical activity variables."))
  }
  
  # Return data frame(s)
  if (return.form==1) {
    return(personaves)
  } else if (return.form==2) {
    return(dayvars)
  } else if (return.form==3) {
    retlist = list(personaves=personaves, dayvars=dayvars)
    return(retlist)
  }
  
}