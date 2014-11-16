\name{nhanes.accel.reweight}
\alias{nhanes.accel.reweight}
\title{
Calculate Adjusted Mobile Exam Center (MEC) Weights
}
\description{
This function calculates adjusted Mobile Exam Center (MEC) weights for the subsample of NHANES 2003-2004 and 2005-2006 participants with valid accelerometer data, making the subsample nationally representative. This function is used internally by \code{\link{nhanes.accel.process}}, but could also be used independently.
}
\usage{
nhanes.accel.reweight(acceldata, wave, seqn.column, include.column)
}
\arguments{
  \item{acceldata}{
Matrix or data frame with one row per participant. It can have any number of columns but one must have respondent sequence numbers (NHANES ID variable 'seqn') and another must indicate whether each participant has valid data (1 if valid, 0 if not valid).
}
  \item{wave}{
1 for NHANES 2003-2004, 2 for NHANES 2005-2006.
}
  \item{seqn.column}{
Column number that has respondent sequence numbers (NHANES ID variable 'seqn').
}
  \item{include.column}{
Column number that indicates whether participant has valid data (1 if valid, 0 if not valid). 
}
}
\details{
The adjusted MEC weights should be used in statistical analysis, along with the survey design features `strata' and `primary sampling unit (PSU)' from the NHANES Demographics files [1]. This is necessary to account for the complex survey design of NHANES. 

The function generates a variable called wtmec2yr_adj. This is the appropriate weight variable if only one wave of NHANES data is used for analysis. If both NHANES 2003-2004 and NHANES 2005-2006 are used for analysis, the user will have to call this function once for each wave, and calculate 4-year weights for analysis. 4-year weights can be calculated by the user simply as wtmec4yr_adj = wtmec2yr_adj/2.

The vast majority of users do not need to worry about this function, as nhanes.accel.process calls it internally. The data frame(s) returned by a function call to nhanes.accel.process will include the variables wtmec2yr_adj and wtmec4yr_adj.
}
\value{
Matrix identical to input object, but with an extra variable, wtmec2yr_adj.
}
\references{
1. Centers for Disease Control and Prevention (CDC). National Center for Health Statistics (NCHS). National Health and Nutrition Examination Survey Data. Hyattsville, MD: US Department of Health and Human Services, Centers for Disease Control and Prevention, 2003-6 \url{http://www.cdc.gov/nchs/nhanes/nhanes_questionnaires.htm}. Accessed July 31, 2014.

2. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed July 31, 2014.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen, W. Stephen Pittard, and Tamara B. Harris
}
\note{
This function essentially replicates the NCI SAS program reweight.pam [2].

Some additional information on the package nhanesaccel and its functions can be found on Dane's website, \url{https://sites.google.com/site/danevandomelen/}.
}
\seealso{
\code{\link{nhanes.accel.process}}
}
\examples{
# Process NHANES 2003-2004 data using default settings
nhanes0304 <- nhanes.accel.process(waves = 1)

# Create new 'include' variable - 1 if participant has 4 or more valid days, otherwise 0
nhanes0304$include2 <- ifelse(nhanes0304$valid_days >= 4, 1, 0)

# Rename previous 2-year weight variable so new one does not overwrite it
names(nhanes0304)[10] = "wtmec2yr_adj_old"

# Calculate new 2-year weights for subset of participants with at least 4 valid days
nhanes0304 <- nhanes.accel.reweight(acceldata = nhanes0304, wave = 1, seqn.column = 1, 
              include.column = 12)
              
# Notice that new weights (wtmec2yr_adj) differ from the old ones (wtmec2yr_adj_old),
# and that participants with 1-3 valid days are now assigned zero weight.
nhanes0304[1:20, ]

# This is just an example to illustrate the usage of nhanes.accel.reweight. In practice,
# it would be easier to add `valid.days = 4' to the initial nhanes.accel.process function
# call. Then the adjusted weights for participants with at least 4 valid days would be 
# calculated automatically by nhanes.accel.process.
}
\keyword{ NHANES }
\keyword{ survey weights }