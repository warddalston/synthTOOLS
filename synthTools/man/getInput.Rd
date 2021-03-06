% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/getFunctions.R
\docType{methods}
\name{getInput}
\alias{getInput}
\alias{getInput,MultiSynth-method}
\title{Extract the Dataprep Output Used as the Base of a MultiSynth Analysis}
\usage{
getInput(input)

\S4method{getInput}{MultiSynth}(input)
}
\arguments{
\item{input}{An object of class "MultiSynth"}
}
\value{
The output of the call to \code{\link{dataprep}} that forms the base of the MultiSynth object
}
\description{
Returns the output of a call to dataprep that forms the basis of a MultiSynth analysis.  This allows the user to investigate the X0, X1, Z0, Z1, Y0plot, and Y1plot of the actual treated/full donor pool/full covariate set case.
}
\author{
Dalston G. Ward: \email{ward.dalston@wustl.edu}
}
\seealso{
\code{\link{MultiSynth}}

\code{\link{getPreps}}

\code{\link{getFits}}

\code{\link{getTreated}}

\code{\link{getTreatmentTime}}

\code{\link{getCase}}

\code{\link{getStats}}
}

