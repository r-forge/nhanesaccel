\name{dem}
\alias{dem}
\docType{data}
\title{
NHANES 2003-2006 demographics data
}
\description{
Includes 45 demographic variables from NHANES 2003-2006.
}
\usage{data(dem)}
\format{
  A data frame with 20470 observations on the following 45 variables.
  \describe{
    \item{\code{SEQN}}{a numeric vector}
    \item{\code{SDDSRVYR}}{a numeric vector}
    \item{\code{RIDSTATR}}{a numeric vector}
    \item{\code{RIDEXMON}}{a numeric vector}
    \item{\code{RIAGENDR}}{a numeric vector}
    \item{\code{RIDAGEYR}}{a numeric vector}
    \item{\code{RIDAGEMN}}{a numeric vector}
    \item{\code{RIDAGEEX}}{a numeric vector}
    \item{\code{RIDRETH1}}{a numeric vector}
    \item{\code{DMQMILIT}}{a numeric vector}
    \item{\code{DMDBORN}}{a numeric vector}
    \item{\code{DMDCITZN}}{a numeric vector}
    \item{\code{DMDYRSUS}}{a numeric vector}
    \item{\code{DMDEDUC3}}{a numeric vector}
    \item{\code{DMDEDUC2}}{a numeric vector}
    \item{\code{DMDSCHOL}}{a numeric vector}
    \item{\code{DMDMARTL}}{a numeric vector}
    \item{\code{DMDHHSIZ}}{a numeric vector}
    \item{\code{INDHHINC}}{a numeric vector}
    \item{\code{INDFMINC}}{a numeric vector}
    \item{\code{INDFMPIR}}{a numeric vector}
    \item{\code{RIDEXPRG}}{a numeric vector}
    \item{\code{DMDHRGND}}{a numeric vector}
    \item{\code{DMDHRAGE}}{a numeric vector}
    \item{\code{DMDHRBRN}}{a numeric vector}
    \item{\code{DMDHREDU}}{a numeric vector}
    \item{\code{DMDHRMAR}}{a numeric vector}
    \item{\code{DMDHSEDU}}{a numeric vector}
    \item{\code{SIALANG}}{a numeric vector}
    \item{\code{SIAPROXY}}{a numeric vector}
    \item{\code{SIAINTRP}}{a numeric vector}
    \item{\code{FIALANG}}{a numeric vector}
    \item{\code{FIAPROXY}}{a numeric vector}
    \item{\code{FIAINTRP}}{a numeric vector}
    \item{\code{MIALANG}}{a numeric vector}
    \item{\code{MIAPROXY}}{a numeric vector}
    \item{\code{MIAINTRP}}{a numeric vector}
    \item{\code{AIALANG}}{a numeric vector}
    \item{\code{WTINT2YR}}{a numeric vector}
    \item{\code{WTMEC2YR}}{a numeric vector}
    \item{\code{SDMVPSU}}{a numeric vector}
    \item{\code{SDMVSTRA}}{a numeric vector}
    \item{\code{RIDRETH2}}{a numeric vector}
    \item{\code{DMDEDUC}}{a numeric vector}
    \item{\code{DMDFMSIZ}}{a numeric vector}
  }
}
\details{
NA
}
\source{
National Health and Nutrition Examination Survey 2003-2006 (see reference).
}
\references{
Centers for Disease Control and Prevention (CDC). National Center for Health Statistics (NCHS). National Health and Nutrition Examination Survey Data. Hyattsville, MD: US Department of Health and Human Services, Centers for Disease Control and Prevention, 2003-6 \url{http://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2003}. Accessed January 26, 2014.
}
\examples{
data(dem)
## maybe str(dem) ; plot(dem) ...
}
\keyword{datasets}