\name{nhanes.accel.process}
\alias{nhanes.accel.process}
\title{
Process NHANES 2003-2006 accelerometer data
}
\description{
This function calculates a variety of physical activity variables from the time-series accelerometer data in NHANES 2003-2006. The function produces a summary .csv file, which by default is written to the user's working directory. A data dictionary for the variables generated is available on Dane's website, \url{https://sites.google.com/site/danevandomelen/}.
}
\usage{
nhanes.accel.process(waves = 3, directory = getwd(), brevity = 1, valid.days = 1, 
valid.week.days = 0, valid.weekend.days = 0, int.cuts = c(100, 760, 2020, 5999), 
youth.mod.cuts = rep(int.cuts[3], 12), youth.vig.cuts = rep(int.cuts[4], 12), 
save.dayfile = FALSE, cpm.nci=FALSE, days.distinct = FALSE, nonwear.window = 60, 
nonwear.tol = 0, nonwear.tol.upper = 99, nonwear.nci = FALSE, weartime.minimum = 600, 
weartime.maximum = 1200, use.partialdays = FALSE, active.bout.length = 10, 
active.bout.tol = 0, mvpa.bout.tol.lower = 0, vig.bout.tol.lower = 0, 
active.bout.nci = FALSE, sed.bout.tol = 0, sed.bout.tol.maximum = int.cuts[2] - 1, 
artifact.thresh = 25000, artifact.action = 1, weekday.weekend = FALSE)
}
\arguments{
  \item{waves}{
1 for NHANES 2003-2004, 2 for NHANES 2005-2006, 3 for both.
}
  \item{directory}{
Location that the user would like the .csv file(s) to be written to. The default is the current working directory.
}
  \item{brevity}{
Controls the number of physical activity variables returned. If 1, returns basic indicators of physical activity volume; if 2, also returns indicators of activity intensities, activity bouts, sedentary behavior, and peak activity; if 3, also returns hourly count averages.
}
  \item{valid.days}{
Minimum number of valid days required to be considered valid for analysis.
}
  \item{valid.week.days}{
Minimum number of valid weekdays required to be considered valid for analysis.
}
  \item{valid.weekend.days}{
Minimum number of valid weekend days required to be considered valid for analysis.
}
  \item{int.cuts}{
Vector of four cut-points from which five intensity ranges are derived. For example, if thresh=c(100,760,2020,5999), minutes with 0-99 counts are classified as intensity level 1, minutes with 100-759 counts are classified as intensity level 2, ... , and minutes with 5999 or greater counts are classified as intensity level 5. Intensities 1-5 typically correspond to sedentary, light, lifestyle, moderate, and vigorous.
}
  \item{youth.mod.cuts}{
Vector of 12 count cut-points for moderate physical activity in youth. The 1st value is for participants age 6, 2nd is for participants age 7, ... , and 12th is for participants age 17. If agreement with NCI SAS programs (see reference 2) is desired, user should enter youth.mod.cuts=c(1400,1515,1638,1770,1910,2059,2220,2393,2580,2781,3000,3239).
}
  \item{youth.vig.cuts}{
Vector of 12 count cut-points for vigorous physical activity in youth. The 1st value is for participants age 6, 2nd is for participants age 7, ... , and 12th is for participants age 17. If agreement with NCI SAS programs (see reference 2) is desired, user should enter youth.vig.cuts=c(3758,3947,4147,4360,4588,4832,5094,5375,5679,6007,6363,6751).
}
  \item{save.dayfile}{
If TRUE, function writes a .csv file with physical activity variables for each day of monitoring for each participant (in addition to the .csv file with daily averages). If FALSE, function only writes the .csv file with daily averages.
}
  \item{cpm.nci}{
If TRUE, average counts per minute is calculated by dividing average daily counts by average daily weartime, as opposed to averaging each day's counts per minute value. In general, leave as FALSE unless you want to replicate the NCI SAS programs (see reference 2).
  }
  \item{days.distinct}{
If TRUE, treat each day of data as distinct, i.e. identify non-wear time and activity bouts in day 1, then day 2, etc.; If FALSE, apply algorithms on continuous basis for full monitoring period.
}
  \item{nonwear.window}{
Minimum length of a non-wear interval.
}
  \item{nonwear.tol}{
Number of minutes with non-zero counts allowed during a non-wear interval.
}
  \item{nonwear.tol.upper}{
Maximum count value for a minute with non-zero counts during a non-wear interval.
}
  \item{nonwear.nci}{
If TRUE, use non-wear algorithm from the NCI SAS programs (see reference 2); if FALSE, use regular algorithm.
}
  \item{weartime.minimum}{
Minimum number of wear time minutes for a day of monitoring to be considered valid.
}
  \item{weartime.maximum}{
Maximum number of wear time minutes for a day of monitoring to be considered valid.
}
  \item{use.partialdays}{
If TRUE, function will consider a day of data for analysis even if it contains fewer than 1440 minutes; if FALSE, function will ignore days of data that contain fewer than 1440 minutes.
}
  \item{active.bout.length}{
Minimum length of moderate-to-vigorous physical activity (MVPA) and vigorous physical activity (VPA) bouts.
}
  \item{active.bout.tol}{
Number of minutes with counts below the required intensity level allowed during MVPA and VPA bouts.
}
  \item{mvpa.bout.tol.lower}{
Lower cut-off for count values outside of MVPA intensity range during an MVPA bout.
}
  \item{vig.bout.tol.lower}{
Lower cut-off for count values outside of VPA intensity range during a VPA bout.
}
  \item{active.bout.nci}{
If TRUE, use activity bouts algorithm from the NCI SAS programs (see reference 2); if FALSE, use regular algorithm.
}
  \item{sed.bout.tol}{
Number of minutes with counts outside sedentary range allowed during sedentary bouts.
}
  \item{sed.bout.tol.maximum}{
Upper cut-off for count values outside sedentary range during a sedentary bout.
}
  \item{artifact.thresh}{
Lower cut-off for counts that are abnormally high and should be considered artifacts.
}
  \item{artifact.action}{
If 1, exclude days that have one or more artifacts; if 2, consider artifacts as non-wear time; if 3, replace artifacts with average of neighboring count values; if 4, take no action.
}
  \item{weekday.weekend}{
If TRUE, function computes physical activity averages for weekdays and weekend days separately (in addition to daily averages for all valid days, which are computed regardless). If FALSE, function only computes averages for all valid days.
}
}
\details{
NA
}
\value{
No object is returned. Function writes .csv file(s) to specified directory (by default, the current working directory).
}
\references{
1. Centers for Disease Control and Prevention (CDC). National Center for Health Statistics (NCHS). National Health and Nutrition Examination Survey Data. Hyattsville, MD: US Department of Health and Human Services, Centers for Disease Control and Prevention, 2003-6 \url{http://www.cdc.gov/nchs/nhanes/nhanes_questionnaires.htm}. Accessed August 16, 2013.

2. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed August 16, 2013.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen, W. Stephen Pittard, and Tamara B. Harris.
}
\note{
Leaving days.distinct set to FALSE is strongly encouraged. If set to TRUE, activity bouts and non-wear periods that span the end of one day and the beginning of the next may be missed. This option is included because it is necessary to reproduce the algorithm used in the NCI SAS programs (see reference 2).

The default for weartime.maximum is 1200. Daily wear time greater than 1200 minutes corresponds to less than 4 hours of sleep. In such cases, it seems more likely that the participant slept while wearing the device, and as a result had small movements overnight show up as wear time. However, wear time maximums are not uncommon. Users who wish to use more traditional data processing methodology can set weartime.maximum to 1440. Also, if participants in the study were not instructed to remove the accelerometer during sleeping, weartime.maximum should be set to 1440.

For use.partialdays, one might prefer to include days of monitoring that do not have a full 1440 minutes of data, as long as the wear time criteria are met. However, it seems likely that missing segments of data might tend to occur at a particular time of day (e.g. evening), and physical activity is strongly associated with time of day. Therefore, including such days could introduce bias.

There is no perfect solution for dealing with abnormally high count values, also known as artifacts. The NCI SAS programs (see reference 2) replace artifacts (which they define as the ActiGraph AM-7164 maximum, 32767) with the mean of neighboring count values. This can be done by specifying artifact.thresh=32767 and artifact.action=3. This method may work well, but in many cases count values that are artifacts are surrounded by count values that are only slightly lower, bringing into question whether the entire group of counts is plausible or implausible. Count values at or around the cut-point defined by artifact.thresh can contribute greatly to daily counts. Therefore the default settings, artifact.thresh=25000 and artifact.action=1, simply excludes days of monitoring with one or more count values of 25000 or greater. As this solution is clearly not ideal, users are welcome to choose their own preferred setting for artifact.thresh and artifact.action.

If the user allows for a tolerance in bout detection (e.g. active.bout.tol=2) and does not use the NCI algorithm (active.bout.nci=FALSE), specifying non-zero values for mvpa.bout.tol.lower and vig.bout.tol.lower is highly recommended. Otherwise the algorithm will tend to classify minutes immediately before and after an activity bout as being part of the bout.

NHANES uses a complex, multi-stage, probability sampling design, and all statistical analysis should account for its design features. The dataset produced by nhanes.accel.process includes the variables wtmec2yr_adj and wtmec4yr_adj. If only data from one wave of NHANES is used (i.e. 2003-2004 OR 2005-2006), the appropriate weight variable is wtmec2yr_adj. If data from both NHANES 2003-2004 and NHANES 2005-2006 is used, the appropriate weight variable is wtmec4yr_adj. Note that analyses must also include the variables strata and primary sampling unit (PSU), which are in the NHANES Demographics files along with other important variables like gender, age, race/ethnicity, etc.

There are essentially three strategies for using nhanes.accel.process to obtain a dataset for statistical analysis:

1. Use default settings for all data processing parameters. Just type the following code in R:

nhanes.accel.process()

2. Adjust certain data processing parameters. For example, only include participants with 4 or more valid days of monitoring, and request that physical activity averages are calculated for all days as well as weekdays and weekend days:

nhanes.accel.process(valid.days=4, weekday.weekend=TRUE)

3. Replicate the NCI SAS programs (see reference 2). The following code produces a .csv file with the same variables that the NCI SAS programs generate (plus some extras), using the exact same data processing methods:

youthmod = c(1400,1515,1638,1770,1910,2059,2220,2393,2580,2781,3000,3239)
youthvig = c(3758,3947,4147,4360,4588,4832,5094,5375,5679,6007,6363,6751)
nhanes.accel.process(waves=1, brevity=2, youth.mod.cuts=youthmod, youth.vig.cuts=youthvig, cpm.nci=TRUE, days.distinct=TRUE,nonwear.tol=2, nonwear.tol.upper=100, nonwear.nci=TRUE, weartime.maximum=1440, active.bout.tol=2, active.bout.nci=TRUE,artifact.thresh=32767,artifact.action=3))

Of course you could set waves=3 to process 2003-2004 and 2005-2006 data rather than just 2003-2004.

Some additional information on the package nhanesaccel and its functions can be found on Dane's website, \url{https://sites.google.com/site/danevandomelen/}.
}
\seealso{
\code{\link{nhanes.accel.reweight}}, \code{\link{accel.artifacts}}, \code{\link{accel.bouts}}, \code{\link{accel.intensities}}, \code{\link{accel.sedbreaks}}, \code{\link{accel.weartime}}, \code{\link{movingaves}}, \code{\link{blockaves}}
}
\examples{
NA
}
\keyword{ NHANES }
\keyword{ accelerometry }
\keyword{ physical activity}