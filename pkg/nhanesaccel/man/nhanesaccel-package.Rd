\name{nhanesaccel-package}
\alias{nhanesaccel-package}
\alias{nhanesaccel}
\docType{package}
\title{
Process accelerometer data from NHANES 2003-2006
}
\description{
This package contains functions for processing accelerometer data from the National Health and Nutrition Examination Survey (NHANES), years 2003-2006 (data included). The main function, nhanes.accel.process, generates a variety of physical activity variables, including indicators of physical activity volume, intensity, frequency, and duration. It also generates variables that can be used to study patterns of physical activity, i.e. weekday/weekend differences and hourly trends. A number of function parameters are available to control data processing algorithms like non-wear time classification and activity bout detection.
}
\details{
\tabular{ll}{
Package: \tab nhanesaccel\cr
Type: \tab Package\cr
Version: \tab 2.1.1\cr
Date: \tab 2014-08-23\cr
License: \tab GPL-2\cr
}
The following functions are included in 'nhanesaccel':

\code{\link{nhanes.accel.process}},
\code{\link{nhanes.accel.reweight}}

This package relies heavily on the 'accelerometry' package [3], which contains various functions for processing minute-to-minute accelerometer data. Some additional information on both packages and their functions can be found on Dane's website, \url{https://sites.google.com/site/danevandomelen/}.
}
\author{
Dane R. Van Domelen, W. Stephen Pittard, and Tamara B. Harris

Maintainer: Dane R. Van Domelen <vandomed@gmail.com>
}
\references{
1. Centers for Disease Control and Prevention (CDC). National Center for Health Statistics (NCHS). National Health and Nutrition Examination Survey Data. Hyattsville, MD: US Department of Health and Human Services, Centers for Disease Control and Prevention, 2003-6 \url{http://www.cdc.gov/nchs/nhanes/nhanes_questionnaires.htm}. Accessed July 31, 2014.

2. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed July 31, 2014.

3. Dane R. Van Domelen (2014). accelerometry: Functions for processing minute-to-minute accelerometer data. R package version 2.2.2. \url{http://CRAN.R-project.org/package=accelerometry}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\keyword{ package }
\seealso{
\code{\link[accelerometry:accelerometry-package]{accelerometry}}
}
\examples{
# Process NHANES 2003-2006 data using default settings
nhanes1 <- nhanes.accel.process()

# Process NHANES 2003-2006 data with following non-default settings: require 4 valid days 
# of monitoring for participants to be considered to have valid data for analysis; use 
# 90- rather than 60-minute window for non-wear algorithm; and request that physical 
# activity averages are calculated for all days as well as weekdays and weekend days 
# separately.
nhanes2 <- nhanes.accel.process(valid.days = 4, nonwear.window = 90, 
                                weekday.weekend = TRUE)

# Process NHANES 2003-2006 data while replicating methods used in the NCI's SAS programs 
# [2]. One way to do this is to explicitly set each function input in order to replicate
# the NCI's methods.
youthmod <- c(1400, 1515, 1638, 1770, 1910, 2059, 2220, 2393, 2580, 2781, 3000, 3239)
youthvig <- c(3758, 3947, 4147, 4360, 4588, 4832, 5094, 5375, 5679, 6007, 6363, 6751)
nhanes3 <- nhanes.accel.process(waves = 3, brevity = 2, valid.days = 4, 
                                youth.mod.cuts = youthmod, youth.vig.cuts = youthvig, 
                                cpm.nci = TRUE, days.distinct = TRUE, nonwear.tol = 2, 
                                nonwear.tol.upper = 100, nonwear.nci = TRUE, 
                                weartime.maximum = 1440, active.bout.tol = 2, 
                                active.bout.nci = TRUE, artifact.thresh = 32767, 
                                artifact.action = 3)

# The easier way is to use the nci.methods input as shown here
nhanes4 <- nhanes.accel.process(waves = 3, brevity = 2, nci.methods = TRUE)

# They give equivalent results
all(nhanes3 == nhanes4, na.rm = TRUE)

# The variables in nhanes3 and nhanes4 correspond to the variables created by 
# the NCI's SAS programs as follows: valid_days/valdays, include/valid_person, 
# mvpa_bouted/allmean_mv, mvpa_min/allmean_mv1, mod_min/allmean_m1, vig_bouted/allmean_v, 
# vig_min/allmean_v1, and cpm/allmean_cnt_wr. All values are identical using either 
# program. The only difference is that the SAS programs include data on nine more 
# participants who had potentially unreliable data. Excluding these participants is 
# considered acceptable by the NCI [2].
}