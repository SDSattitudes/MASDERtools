% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IRT-functions.R
\name{comparer}
\alias{comparer}
\title{Compare PCM, GPCM, and GRM models}
\usage{
comparer(
  dat,
  scale.names = NULL,
  drop.items = NULL,
  pval = 0.01,
  verbose = TRUE
)
}
\arguments{
\item{dat}{Data from items; usually a data.frame.}

\item{scale.names}{Vector of scale names; must be based on common part of item names.}

\item{drop.items}{A list of vectors of item numbers to be dropped WITHIN each scale. These are the column numbers within each scale's sub-data.frame, not the overall column numbers.}

\item{pval}{The p-value to use in the chi-squared test for nest models (comparing PCM and GPCM). Default value 0.01.}

\item{verbose}{Logical; debug purposes.}
}
\description{
Compare PCM, GPCM, and GRM models
}
