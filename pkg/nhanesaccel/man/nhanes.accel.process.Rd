\name{nhanes.accel.process}
\alias{nhanes.accel.process}
\title{
Process NHANES 2003-2006 accelerometer data
}
\description{
This function calculates a variety of physical activity variables from the time-series accelerometer data in NHANES 2003-2006. A data dictionary for the variables generated is available on Dane's website, \url{https://sites.google.com/site/danevandomelen/r-package-nhanesaccel/data-dictionary}.
}
\usage{
nhanes.accel.process(waves = 3, directory = getwd(), nci.methods = FALSE, brevity = 1, 
                     valid.days = 1, valid.week.days = 0, valid.weekend.days = 0, 
                     int.cuts = c(100, 760, 2020, 5999), 
                     youth.mod.cuts = rep(int.cuts[3], 12), 
                     youth.vig.cuts = rep(int.cuts[4], 12), cpm.nci = FALSE, 
                     days.distinct = FALSE, nonwear.window = 60, nonwear.tol = 0,
                     nonwear.tol.upper = 99, nonwear.nci = FALSE, weartime.minimum = 600,
                     weartime.maximum = 1200, partialday.minimum = 1440, 
                     active.bout.length = 10, active.bout.tol = 0, 
                     mvpa.bout.tol.lower = 0, vig.bout.tol.lower = 0, 
                     active.bout.nci = FALSE, sed.bout.tol = 0, 
                     sed.bout.tol.maximum = int.cuts[2]-1, artifact.thresh = 25000, 
                     artifact.action = 1, weekday.weekend = FALSE, return.form = 1, 
                     write.csv = FALSE)
}
\arguments{
  \item{waves}{
1 for NHANES 2003-2004, 2 for NHANES 2005-2006, 3 for both.
}
  \item{directory}{
Location that the user would like the .csv file(s) to be written to. The default is the current working directory.
}
  \item{nci.methods}{
If TRUE, inputs are set to replicate the data processing methods used by the NCI's SAS programs [2]. See Examples for further details. Even if nci.methods is set to TRUE, the user can specify non-default values for waves, brevity, weekday.weekend, return.form, and write.csv.
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
Vector of 12 count cut-points for moderate physical activity in youth. The 1st value is for participants age 6, 2nd is for participants age 7, ... , and 12th is for participants age 17. If agreement with NCI's SAS programs [2] is desired, user should enter youth.mod.cuts = c(1400, 1515, 1638, 1770, 1910, 2059, 2220, 2393, 2580, 2781, 3000, 3239).
}
  \item{youth.vig.cuts}{
Vector of 12 count cut-points for vigorous physical activity in youth. The 1st value is for participants age 6, 2nd is for participants age 7, ... , and 12th is for participants age 17. If agreement with NCI's SAS programs [2] is desired, user should enter youth.vig.cuts = c(3758, 3947, 4147, 4360, 4588, 4832, 5094, 5375, 5679, 6007, 6363, 6751).
}
  \item{cpm.nci}{
If TRUE, average counts per minute is calculated by dividing average daily counts by average daily weartime, as opposed to averaging each day's counts per minute value. In general, leave as FALSE unless you want to replicate the NCI's SAS programs [2].
}
  \item{days.distinct}{
If TRUE, treat each day of data as distinct, i.e. identify non-wear time and activity bouts in day 1, then day 2, etc.; If FALSE, apply algorithms on continuous basis for full monitoring period. Strongly recommend setting to FALSE to capture non-wear periods that start between 11 pm and midnight, when participants removes accelerometer to go to sleep.
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
If TRUE, use non-wear algorithm from the NCI's SAS programs [2]; if FALSE, use regular algorithm (see Details).
}
  \item{weartime.minimum}{
Minimum number of wear time minutes for a day of monitoring to be considered valid.
}
  \item{weartime.maximum}{
Maximum number of wear time minutes for a day of monitoring to be considered valid.
}
  \item{partialday.minimum}{
Minimum number of minutes for a partial day of monitoring to be processed and potentially considered valid for analysis (generally applies only to the first and last days of monitoring, which may not cover full 24-hour periods). This input is included because some researchers may prefer to exclude a day that only has data from, say, 1 pm to midnight. Even though there may be sufficient wear time during that period to be classified as a valid day, the missing chunk of data prior to 1 pm may result in the day not being representative of the participant's usual physical activity.
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
If TRUE, use activity bouts algorithm from the NCI's SAS programs [2]; if FALSE, use regular algorithm (see Details).
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
  \item{return.form}{
If 1, function returns physical activity variables on per-person basis, i.e. daily averages for each participant; if 2, function returns variables on per-day basis; if 3, function returns both via a list.
}
  \item{write.csv}{
If TRUE, function writes .csv file(s) to user-specified directory (by default, the current working directory). The files written are those requested by return.form. If FALSE, no .csv files are written.
}
}
\details{
The algorithm used to identify non-wear time is defined by function inputs nonwear.window, nonwear.tol, nonwear.tol.upper, and nonwear.nci. If nonwear.nci is set to FALSE, a `regular' non-wear algorithm is used. This algorithm classifies as non-wear time any interval of length nonwear.window in which no more than nonwear.tol counts are non-zero, and those counts are all less than nonwear.tol.upper. If nonwear.nci is set to TRUE, the non-wear algorithm from the NCI's SAS programs [2] is used. This algorithm classifies as non-wear time any interval of length nonwear.window that starts with a count value of 0, does not contain any periods with (nonwear.tol + 1) consecutive non-zero count values, and does not contain any counts greater than nonwear.tol.upper. Once a non-wear bout is established, it continues until there are (nonwear.tol + 1) consecutive non-zero count values or a single count value greater than nonwear.tol.upper. 

The activity bout algorithm operates similarly to the non-wear algorithm. If active.bout.nci is set to FALSE, a `regular' algorithm is used. To illustrate, any interval of length active.bout.length where no more than active.bout.tol minutes have counts less than int.cuts[3], and the counts below int.cuts[3] are all mvpa.bout.tol.lower or greater, is considered an MVPA bout. If active.bout.nci is set to TRUE, the NCI's algorithm is used. This algorithm defines an MVPA bout as an interval that starts with ten consecutive count values greater than or equal to int.cuts[3], allowing for up to active.bout.tol minutes with counts below int.cuts[3]. The first minute of the bout cannot be below int.cuts[3]. Once the MVPA bout is established, it continues until there are (active.bout.tol + 1) consecutive minutes with counts less than int.cuts[3]. The parameters mvpa.bout.tol.lower and vig.bout.tol.lower are not used in the NCI bout algorithm.
}
\value{
A single matrix or a list of two matrices, depending on return.form. If write.csv is set to TRUE, the function also writes one or two .csv files to user-specified directory.
}
\references{
1. Centers for Disease Control and Prevention (CDC). National Center for Health Statistics (NCHS). National Health and Nutrition Examination Survey Data. Hyattsville, MD: US Department of Health and Human Services, Centers for Disease Control and Prevention, 2003-6 \url{http://www.cdc.gov/nchs/nhanes/nhanes_questionnaires.htm}. Accessed July 31, 2014.

2. National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data. Available at: \url{http://riskfactor.cancer.gov/tools/nhanes_pam}. Accessed July 31, 2014.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen, W. Stephen Pittard, and Tamara B. Harris.
}
\note{
Leaving days.distinct set to FALSE is strongly encouraged. If set to TRUE, activity bouts and non-wear periods that span the end of one day and the beginning of the next may be missed. This option is included because it is necessary to reproduce the algorithm used in the NCI's SAS programs [2].

The default for weartime.maximum is 1200. Daily wear time greater than 1200 minutes corresponds to less than 4 hours of sleep. In such cases, it seems more likely that the participant slept while wearing the device, and as a result had small movements overnight show up as wear time. However, wear time maximums are not uncommon. Users who wish to use more traditional data processing methodology can set weartime.maximum to 1440. Also, if participants in the study were not instructed to remove the accelerometer during sleeping, weartime.maximum should be set to 1440.

For partialday.minimum, one might prefer to include days of monitoring that do not have a full 1440 minutes of data, as long as the wear time criteria are met. We recommend requiring the full 1440 minutes, because a day of monitoring with a big chunk of missing data is probably not representative of a typical day of physical activity. Also, it seems likely that missing segments of data might tend to occur at a particular time of day (e.g. evening), and physical activity is strongly associated with time of day. Therefore, including such days could introduce bias.

There is no perfect solution for dealing with abnormally high count values, also known as artifacts. The NCI's SAS programs replace artifacts (which they define as the ActiGraph AM-7164 maximum, 32767) with the mean of neighboring count values [2]. This can be done by specifying artifact.thresh = 32767 and artifact.action = 3. This method may work well, but in many cases count values that are artifacts are surrounded by count values that are only slightly lower, bringing into question whether the entire group of counts is plausible or implausible. Count values at or around the cut-point defined by artifact.thresh can contribute greatly to daily counts. Therefore the default settings, artifact.thresh = 25000 and artifact.action = 1, simply excludes days of monitoring with one or more count values of 25000 or greater. As this solution is clearly not ideal, users are welcome to choose their own preferred setting for artifact.thresh and artifact.action.

If the user allows for a tolerance in bout detection (e.g. active.bout.tol = 2) and does not use the NCI algorithm (active.bout.nci = FALSE), specifying non-zero values for mvpa.bout.tol.lower and vig.bout.tol.lower is highly recommended. Otherwise the algorithm will tend to classify minutes immediately before and after an activity bout as being part of the bout.

NHANES uses a complex, multi-stage, probability sampling design, and all statistical analysis should account for its design features. The dataset produced by nhanes.accel.process includes the variables wtmec2yr_adj and wtmec4yr_adj. If only data from one wave of NHANES is used (i.e. 2003-2004 OR 2005-2006), the appropriate weight variable is wtmec2yr_adj. If data from both NHANES 2003-2004 and NHANES 2005-2006 is used, the appropriate weight variable is wtmec4yr_adj. Note that analyses must also include the variables strata and primary sampling unit (PSU), which are in the NHANES Demographics files along with other important variables like gender, age, race/ethnicity, etc.

Some additional information on the package nhanesaccel and its functions can be found on Dane's website, \url{https://sites.google.com/site/danevandomelen/}.
}
\seealso{
\code{\link{nhanes.accel.reweight}}, \code{\link{accel.artifacts}}, \code{\link{accel.bouts}}, \code{\link{accel.intensities}}, \code{\link{accel.sedbreaks}}, \code{\link{accel.weartime}}, \code{\link{movingaves}}, \code{\link{blockaves}}
}
\examples{
# Process NHANES 2003-2006 data using default settings
acceldata1 <- nhanes.accel.process()

# Process NHANES 2003-2006 data with following non-default settings: require 4 valid days 
# of monitoring for participants to be considered to have valid data for analysis; use 
# 90- rather than 60-minute window for non-wear algorithm; and request that physical 
# activity averages are calculated for all days as well as weekdays and weekend days 
# separately.
acceldata2 <- nhanes.accel.process(valid.days = 4, nonwear.window = 90, 
                                   weekday.weekend = TRUE)

# Process NHANES 2003-2006 data while replicating methods used in the NCI's SAS programs 
# [2]. One way to do this is to explicitly set each function input in order to replicate
# the NCI's methods.

youthmod <- c(1400, 1515, 1638, 1770, 1910, 2059, 2220, 2393, 2580, 2781, 3000, 3239)
youthvig <- c(3758, 3947, 4147, 4360, 4588, 4832, 5094, 5375, 5679, 6007, 6363, 6751)
acceldata3 <- nhanes.accel.process(waves = 3, brevity = 2, valid.days = 4, 
                                   youth.mod.cuts = youthmod, youth.vig.cuts = youthvig, 
                                   cpm.nci = TRUE, days.distinct = TRUE, nonwear.tol = 2, 
                                   nonwear.tol.upper = 100, nonwear.nci = TRUE, 
                                   weartime.maximum = 1440, active.bout.tol = 2, 
                                   active.bout.nci = TRUE, artifact.thresh = 32767, 
                                   artifact.action = 3)

# The easier way is to use the nci.methods input as shown below
acceldata4 <- nhanes.accel.process(waves = 3, brevity = 2, nci.methods = TRUE)

# They give equivalent results
all(acceldata3 == acceldata4)

# The variables in acceldata3 and acceldata4 correspond to the variables created by 
# the NCI's SAS programs as follows: valid_days/valdays, include/valid_person, 
# mvpa_bouted/allmean_mv, mvpa_min/allmean_mv1, mod_min/allmean_m1, vig_bouted/allmean_v, 
# vig_min/allmean_v1, and cpm/allmean_cnt_wr. All values are identical using either 
# program. The only difference is that the SAS programs include data on nine more 
# participants who had potentially unreliable data. Excluding these participants is 
# considered acceptable by the NCI [2].
}
\keyword{ NHANES }
\keyword{ accelerometry }
\keyword{ physical activity}