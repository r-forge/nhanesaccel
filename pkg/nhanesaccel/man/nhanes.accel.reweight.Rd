\name{nhanes.accel.reweight}
\alias{nhanes.accel.reweight}
\title{
Calculate adjusted MEC weights
}
\description{
This function calculates adjusted MEC weights for the subsample of NHANES 2003-2004 and 2005-2006 participants with valid accelerometer data, making the subsample nationally representative. This function is used internally by \code{\link{nhanes.accel.process}}, but could also be used independently.
}
\usage{
nhanes.accel.reweight(acceldata, wave, seqn.column, include.column)
}
\arguments{
  \item{acceldata}{
Matrix or data frame with one row per participant. It can have any number of columns but one must have respondent sequence numbers (i.e. NHANES ID variable 'seqn') and another must indicate whether each participant has valid data (1 if valid, 0 if not valid).
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
The adjusted MEC weights should be used in statistical analysis, along with the survey design features `strata' and `primary sampling unit (PSU)' from the NHANES Demographics files. This is necessary to account for the complex survey design of NHANES. 

The function generates a variable called wtmec2yr_adj. This is the appropriate weight variable if only one wave of NHANES data is used for analysis. If both NHANES 2003-2004 and NHANES 2005-2006 are used for analysis, the user will have to call this function once for each wave, and calculate 4-year weights for analysis. 4-year weights can be calculated by the user simply as wtmec4yr_adj = wtmec2yr_adj/2.
}
\value{
Matrix identical to input object, but with an extra variable, wtmec2yr_adj.
}
\references{
1. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed July 31, 2014.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen, W. Stephen Pittard, and Tamara B. Harris
}
\note{
This function essentially replicates the NCI SAS program reweight.pam [1].

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
              
# Notice that participants with 1-3 valid days are now assigned zero weight
nhanes0304[1:20, ]

# This is just an example to illustrate the usage of nhanes.accel.reweight. In practice,
# it would be easier to add `valid.days = 4' to the initial nhanes.accel.process function
# call. Then the 4-day weights would be calculated automatically.
}
\keyword{ NHANES }
\keyword{ survey weights }