\name{dem}
\alias{dem}
\docType{data}
\title{
NHANES 2003-2006 demographics data
}
\description{
Includes 45 demographic variables.
}
\usage{data(dem)}
\format{
  A data frame with 20470 observations on the following 45 variables.
  \describe{
    \item{\code{seqn}}{a numeric vector}
    \item{\code{sddsrvyr}}{a numeric vector}
    \item{\code{ridstatr}}{a numeric vector}
    \item{\code{ridexmon}}{a numeric vector}
    \item{\code{riagendr}}{a numeric vector}
    \item{\code{ridageyr}}{a numeric vector}
    \item{\code{ridagemn}}{a numeric vector}
    \item{\code{ridageex}}{a numeric vector}
    \item{\code{ridreth1}}{a numeric vector}
    \item{\code{dmqmilit}}{a numeric vector}
    \item{\code{dmdborn}}{a numeric vector}
    \item{\code{dmdcitzn}}{a numeric vector}
    \item{\code{dmdyrsus}}{a numeric vector}
    \item{\code{dmdeduc3}}{a numeric vector}
    \item{\code{dmdeduc2}}{a numeric vector}
    \item{\code{dmdschol}}{a numeric vector}
    \item{\code{dmdmartl}}{a numeric vector}
    \item{\code{dmdhhsiz}}{a numeric vector}
    \item{\code{indhhinc}}{a numeric vector}
    \item{\code{indfminc}}{a numeric vector}
    \item{\code{indfmpir}}{a numeric vector}
    \item{\code{ridexprg}}{a numeric vector}
    \item{\code{dmdhrgnd}}{a numeric vector}
    \item{\code{dmdhrage}}{a numeric vector}
    \item{\code{dmdhrbrn}}{a numeric vector}
    \item{\code{dmdhredu}}{a numeric vector}
    \item{\code{dmdhrmar}}{a numeric vector}
    \item{\code{dmdhsedu}}{a numeric vector}
    \item{\code{sialang}}{a numeric vector}
    \item{\code{siaproxy}}{a numeric vector}
    \item{\code{siaintrp}}{a numeric vector}
    \item{\code{fialang}}{a numeric vector}
    \item{\code{fiaproxy}}{a numeric vector}
    \item{\code{fiaintrp}}{a numeric vector}
    \item{\code{mialang}}{a numeric vector}
    \item{\code{miaproxy}}{a numeric vector}
    \item{\code{miaintrp}}{a numeric vector}
    \item{\code{aialang}}{a numeric vector}
    \item{\code{wtint2yr}}{a numeric vector}
    \item{\code{wtmec2yr}}{a numeric vector}
    \item{\code{sdmvpsu}}{a numeric vector}
    \item{\code{sdmvstra}}{a numeric vector}
    \item{\code{ridreth2}}{a numeric vector}
    \item{\code{dmdeduc}}{a numeric vector}
    \item{\code{dmdfmsiz}}{a numeric vector}
  }
}
\details{
NA
}
\source{
National Health and Nutrition Examination Survey 2003-2006 (see reference).
}
\references{
Centers for Disease Control and Prevention (CDC). National Center for Health Statistics (NCHS). National Health and Nutrition Examination Survey Data. Hyattsville, MD: US Department of Health and Human Services, Centers for Disease Control and Prevention, 2003-6 \url{http://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2003}. Accessed February 1, 2014.
}
\examples{
data(dem)
## maybe str(dem) ; plot(dem) ...
}
\keyword{datasets}