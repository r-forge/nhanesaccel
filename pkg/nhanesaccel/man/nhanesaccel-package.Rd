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
Version: \tab 2.2.1\cr
Date: \tab 2014-08-12\cr
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
NA
}