\name{dem}
\alias{dem}
\docType{data}
\title{
NHANES 2003-2006 demographics data
}
\description{
Includes 45 demographics variables for participants in NHANES 2003-2006.
}
\usage{data(dem)}
\format{
  A data frame with 20470 observations on the following 45 variables.
  \describe{
    \item{\code{seqn}}{Respondent sequence number (i.e. participant ID).}
    \item{\code{sddsrvyr}}{Data release number (3 if participant is from NHANES 2003-2004, 4 if participant is from NHANES 2005-2006).}
    \item{\code{ridstatr}}{Interview/examination status (1 if participant was interviewed only, 2 if participant was both interviewed and Mobile Examination Center examined).}
    \item{\code{ridexmon}}{Six month time period when examination was performed (1 for Nov. 1-April 30, 2 for May 1-Oct. 31).}
    \item{\code{riagendr}}{Gender (1 = Male, 2 = Female).}
    \item{\code{ridageyr}}{Age in years at time of household screening. Values are whole numbers from 0 to 85, with 85 indicating that the participant was 85 years of age or older.}
    \item{\code{ridagemn}}{Age in months at time of household screening for participants younger than 85 years.}
    \item{\code{ridageex}}{Age in months at time of examination for participants younger than 85 years.}
    \item{\code{ridreth1}}{Race/ethnicity (1 = Mexican American, 2 = Other Hispanic, 3 = Non-Hispanic White, 4 = Non-Hispanic Black, 5 = Other Race - Including Multi-Racial).}
    \item{\code{dmqmilit}}{Veteran/military status. Did you ever serve in the Armed Forces of the United States? (1 = Yes, 2 = No, 7 = Refused, 9 = Don't know)}
    \item{\code{dmdborn}}{Country of birth (1 = Born in 50 US States or Washington, DC, 2 = Born in Mexico, 3 = Born Elsewhere, 7 = Refused, 9 = Don't know).}
    \item{\code{dmdcitzn}}{Citizenship status (1 = Citizen by birth or naturalization, 2 = Not a citizen of the US, 7 = Refused, 9 = Don't know).}
    \item{\code{dmdyrsus}}{Length of time in US (1 = Less than 1 yr., 2 = 1 yr., less than 5 yrs., 3 = 5 yrs., less than 10 yrs., 4 = 10 yrs., less than 15 yrs., 5 = 15 yrs., less than 20 yrs., 6 = 20 yrs., less than 30 yrs., 7 = 30 yrs., less than 40 yrs., 8 = 40 yrs., less than 50 yrs., 9 = 50 yrs. or more, 77 = Refused, 88 = Could not determine, 99 = Don't know).}
    \item{\code{dmdeduc3}}{Education level for children/youth age 6-19 years. What is the highest grade or level of school you completed or the highest degree you have received? (0 = Never attended/kindergarten only, 1 = 1st grade, 2 = 2nd grade, 3 = 3rd grade, 4 = 4th grade, 5 = 5th grade, 6 = 6th grade, 7 = 7th grade, 8 = 8th grade, 9 = 9th grade, 10 = 10th grade, 11 = 11th grade, 12 = 12th grade, no diploma, 13 = High school graduate, 14 = GED or equivalent, 15 = More than high school, 55 = Less than 5th grade, 66 = Less than 9th grade, 77 = Refused, 99 = Don't know)}
    \item{\code{dmdeduc2}}{Education level for adults age 20 years or older. What is the highest grade or level of school you have completed or the highest degree you have received? (1 = Less than 9th Grade, 2 = 9-11th grade (Includes 12th grade with no diploma), 3 = High school grad/GED or equivalent, 4 = Some college or AA degree, 5 = College graduate or above, 7 = Refused, 9 = Don't know)}
    \item{\code{dmdschol}}{Now attending school? (1 = In school, 2 = On vacation from school (between grades), 3 = Neither in school or on vacation from school (between grades), 7 = Refused, 9 = Don't know)}
    \item{\code{dmdmartl}}{Marital status (1 = Married, 2 = Widowed, 3 = Divorced, 4 = Separated, 5 = Never married, 6 = Living with partner, 77 = Refused, 99 = Don't know).}
    \item{\code{dmdhhsiz}}{Total number of people in the household (7 indicates 7 or more people).}
    \item{\code{indhhinc}}{Annual household income (1 = $0 to $4,999, 2 = $5,000 to $9,999, 3 = $10,000 to $14,999, 4 = $15,000 to $19,999, 5 = $20,000 to $24,999, 6 = $25,000 to $34,999, 7 = $35,000 to $44,999, 8 = $45,000 to $54,999, 9 = $55,000 to $64,999, 10 = $65,000 to $74,999, 11 = $75,000 and Over, 12 = Over $20,000, 13 = $Under 20,000, 77 = Refused, 99 = Don't know).}
    \item{\code{indfminc}}{Annual family income (1 = $0 to $4,999, 2 = $5,000 to $9,999, 3 = $10,000 to $14,999, 4 = $15,000 to $19,999, 5 = $20,000 to $24,999, 6 = $25,000 to $34,999, 7 = $35,000 to $44,999, 8 = $45,000 to $54,999, 9 = $55,000 to $64,999, 10 = $65,000 to $74,999, 11 = $75,000 and Over, 12 = Over $20,000, 13 = $Under 20,000, 77 = Refused, 99 = Don't know).}
    \item{\code{indfmpir}}{Family poverty income ratio (PIR) (5 indicates PIR of 5 or greater).}
    \item{\code{ridexprg}}{Pregnancy status at the time of MEC exam (1 = Yes, positive lab pregnancy test or self-reported pregnant at exam, 2 = Not pregnant at exam, 3 = Cannot ascertain if pregnant at exam).}
    \item{\code{dmdhrgnd}}{Gender of the household reference person (1 = Male, 2 = Female).}
    \item{\code{dmdhrage}}{Age in years of the household reference person at the time of screening. Values are whole numbers from 16 to 85, with 85 indicating that the household reference person was 85 years of age or older.}
    \item{\code{dmdhrbrn}}{Household reference person country of birth (1 = Born in 50 US States or Washington, DC, 2 = Born in Mexico, 3 = Born Elsewhere, 7 = Refused, 9 = Don't know).}
    \item{\code{dmdhredu}}{Education level for household reference person. What is the highest grade or level of school you have received? (1 = Less than 9th grade, 2 = 9-11th grade (Includes 12th grade with no diploma), 3 = High school grad/GED or equivalent, 4 = Some college or AA degree, 5 = College graduate or above, 7 = Refused, 9 = Don't know)}
    \item{\code{dmdhrmar}}{Marital status of household reference person (1 = Married, 2 = Widowed, 3 = Divorced, 4 = Separated, 5 = Never married, 6 = Living with partner, 77 = Refused, 99 = Don't know).}
    \item{\code{dmdhsedu}}{Education level for spouse of household reference person. What is the highest grade level you have completed or the highest degree you have received? (1 = Less than 9th grade, 2 = 9-11th grade (Includes 12th grade with no diploma), 3 = High school grad/GED or equivalent, 4 = Some college or AA degree, 5 = College graduate or above, 7 = Refused, 9 = Don't know)}
    \item{\code{sialang}}{Language used for interview (1 = English, 2 = Spanish).}
    \item{\code{siaproxy}}{Whether a proxy respondent was used in conducting the interview (1 = Yes, 2 = No).}
    \item{\code{siaintrp}}{Whether an interpreter was used in conducting the interview (1 = Yes, 2 = No).}
    \item{\code{fialang}}{Language used for the Family Interview Instrument (1 = English, 2 = Spanish).}
    \item{\code{fiaproxy}}{Whether a proxy respondent was used in conducting the Family Interview (1 = Yes, 2 = No).}
    \item{\code{fiaintrp}}{Whether an interpreter was used in conducting the Family Interview (1 = Yes, 2 = No).}
    \item{\code{mialang}}{Language used for the MEC Interview (1 = English, 2 = Spanish). }
    \item{\code{miaproxy}}{Whether a proxy respondent was used in conducting the MEC Interview (1 = Yes, 2 = No).}
    \item{\code{miaintrp}}{Whether an interpreter was used in conducting the MEC Interview (1 = Yes, 2 = No).}
    \item{\code{aialang}}{Language used for the MEC Audio-Computer-Assisted Self-Interviewing interview (1 = English, 2 = Spanish).}
    \item{\code{wtint2yr}}{Full sample 2-year interview weight.}
    \item{\code{wtmec2yr}}{Full sample 2-year MEC exam weight.}
    \item{\code{sdmvpsu}}{Masked variance unit pseudo-primary sampling unit variable for variance estimation.}
    \item{\code{sdmvstra}}{Masked variance unit pseudo-stratum variable for variance estimation.}
    \item{\code{ridreth2}}{Linked NHANES III race/ethnicity (1 = Non-Hispanic White, 2 = Non-Hispanic Black, 3 = Mexican American, 4 = Other Race - Including Multi-Racial, 5 = Other Hispanic).}
    \item{\code{dmdeduc}}{Education level for participants age 6 years or older. What is the highest grade or level of school you have completed or the highest degree you have received? (1 = Less than high school, 2 = High school diploma (including GED), 3 = More than high school, 7 = Refused, 9 = Don't Know)}
    \item{\code{dmdfmsiz}}{Total number of people in the family (7 indicates 7 or more people).}
  }
}
\details{
The variable descriptions above are taken directly from the NHANES website [1].
}
\source{
National Health and Nutrition Examination Survey 2003-2006 [1].
}
\references{
1. Centers for Disease Control and Prevention (CDC). National Center for Health Statistics (NCHS). National Health and Nutrition Examination Survey Data. Hyattsville, MD: US Department of Health and Human Services, Centers for Disease Control and Prevention, 2003-6 \url{http://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2003}. Accessed July 31, 2014.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\keyword{datasets}