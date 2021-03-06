% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{get_best_predictions}
\alias{get_best_predictions}
\title{Get best predictions}
\usage{
get_best_predictions(trainobj, rep = NA, ncomp = NA, keepX = NA,
  keepY = NA)
}
\arguments{
\item{trainobj}{The \code{train} object from \code{caret}, fit using mixOmics spls.}

\item{rep}{If trainobj was fit using repeated cross-validation, choose a repeat (e.g. "Rep1") or leave as NA for all repeats (default).}

\item{ncomp}{Manually select CV predictions with this parameter, instead of the best one from \code{train}.}

\item{keepX}{Manually select CV predictions with this parameter, instead of the best one from \code{train}.}

\item{keepY}{Manually select CV predictions with this parameter, instead of the best one from \code{train}.}
}
\value{
A \code{data.frame} with the variables
\itemize{
  \item \code{pred} - the predicted values
  \item \code{obs} - the observed values
  \item \code{ncomp} - Tuning variable
  \item \code{keepX} - Tuning variable
  \item \code{keepY} - Tuning variable
  \item \code{fold} - Cross-validation fold
  \item \code{rep} - Repeat number (for repeated CV)
}
}
\description{
Returns the CV predictions associated with the best performing tuning parameters. If there are multiple CV repeats, these are separated.
}
\examples{
library(caret)
x <- data.frame(matrix(rnorm(1000),nrow = 100))
y <- rnorm(100)
PLS <- train(x = x, y = y, method = get_mixOmics_spls())
getBestPredictions(PLS)
}
