% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/SynthMeanEffect.R
\name{SynthMeanEffect}
\alias{SynthMeanEffect}
\title{Calculate the Mean Outcome Gap in a Synthetic Control Analysis}
\usage{
SynthMeanEffect(dataprep, synth, begin_time, end_time)
}
\arguments{
\item{dataprep}{The output of a call to \code{dataprep}}

\item{synth}{The output of a call to \code{synth}}

\item{begin_time}{A numeric of length one. The beginning of the period to calculate the mean gap.}

\item{end_time}{A numeric of length one. The end of the period to calculate the mean gap.}
}
\value{
A scalar containing the mean difference in outcomes.
}
\description{
This function calculates the mean gap between the observed outcome and a synthetic control's outcome over a user specified time period.  When the user specified time period corresponds to the post-treatment period, this quantity is the average treatment effect (ATE).
}
\examples{
##Example: Hainmueller and Diamond's Toy panel dataset

#load data
data(synth.data)

## create matrices from panel data that provide inputs for fitMultiSynth()
dataprep.out<-
 dataprep(
   foo = synth.data,
   predictors = c("X1", "X2", "X3"),
   predictors.op = "mean",
   dependent = "Y",
   unit.variable = "unit.num",
   time.variable = "year",
   special.predictors = list(
     list("Y", 1991, "mean"),
     list("Y", 1985, "mean"),
     list("Y", 1980, "mean")
   ),
   treatment.identifier = 7,
   controls.identifier = c(29, 2, 13, 17, 32, 38),
   time.predictors.prior = c(1984:1989),
   time.optimize.ssr = c(1984:1990),
   unit.names.variable = "name",
   time.plot = 1984:1996
 )

 synth.out <- synth(dataprep.out)

 SynthMeanEffect(dataprep.out, synth.out, 1991,1996)
}
\author{
Dalston G. Ward: \email{ward.dalston@wustl.edu}
}
\references{
\itemize{
\item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
\item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
}
}
\seealso{
\code{\link{synth}}

\code{\link{dataprep}}
}

