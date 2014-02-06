\name{nhanesaccel-package}
\alias{nhanesaccel-package}
\alias{nhanesaccel}
\docType{package}
\title{
Process accelerometer data from NHANES 2003-2006
}
\description{
This package contains functions for processing accelerometer data from the National Health and Nutrition Examination Survey (NHANES), years 2003-2006 (data included). The main function, nhanes.accel.process, generates a variety of physical activity variables, including indicators of physical activity volume, intensity, frequency, and duration. It also generates variables that can be used to study patterns of physical activity, i.e. weekday/weekend differences and hourly trends. A number of function parameters are available to control data processing algorithms like non-wear time classification and activity bout detection. A call to this function produces a .csv file for statistical analysis.
}
\details{
\tabular{ll}{
Package: \tab nhanesaccel\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2014-02-01\cr
License: \tab GPL-2\cr
}
The following functions are contained in the package nhanesaccel:

\code{\link{nhanes.accel.process}},
\code{\link{nhanes.accel.reweight}}

Some additional information on the package nhanesaccel and its functions can be found on Dane's website, \url{https://sites.google.com/site/danevandomelen/}.
}
\author{
Dane R. Van Domelen, W. Stephen Pittard, and Tamara B. Harris

Maintainer: Dane R. Van Domelen <vandomed@gmail.com>
}
\references{
1. Centers for Disease Control and Prevention (CDC). National Center for Health Statistics (NCHS). National Health and Nutrition Examination Survey Data. Hyattsville, MD: US Department of Health and Human Services, Centers for Disease Control and Prevention, 2003-6 \url{http://www.cdc.gov/nchs/nhanes/nhanes_questionnaires.htm}. Accessed February 1, 2014.

2. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed February 1, 2014.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\keyword{ package }
\seealso{
\code{\link[accelerometry:accelerometry-package]{accelerometry}}
}
\examples{
# Process NHANES 2003-2006 data using default settings
acceldata1 <- nhanes.accel.process(write.csv = FALSE)

# Require 4+ valid days, increase non-wear window to 90, and get weekday/weekend averages
acceldata2 <- nhanes.accel.process(valid.days = 4, nonwear.window = 90, 
                                   weekday.weekend = TRUE, write.csv = FALSE)

# Use settings that replicate the NCI's SAS programs
youthmod <- c(1400,1515,1638,1770,1910,2059,2220,2393,2580,2781,3000,3239)
youthvig <- c(3758,3947,4147,4360,4588,4832,5094,5375,5679,6007,6363,6751)
acceldata3 <- nhanes.accel.process(waves = 3, brevity = 2, valid.days = 4, 
                                   youth.mod.cuts = youthmod, youth.vig.cuts = youthvig, 
                                   cpm.nci = TRUE, days.distinct = TRUE, nonwear.tol = 2, 
                                   nonwear.tol.upper = 100, nonwear.nci = TRUE, 
                                   weartime.maximum = 1440, active.bout.tol = 2, 
                                   active.bout.nci = TRUE, artifact.thresh = 32767, 
                                   artifact.action = 3, write.csv = FALSE)
}