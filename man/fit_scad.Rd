% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1_1_model.R
\name{fit_scad}
\alias{fit_scad}
\title{Model selection for high-dimensional Cox models with SCAD penalty}
\usage{
fit_scad(
  x,
  y,
  nfolds = 5L,
  gammas = c(2.01, 2.3, 3.7, 200),
  eps = 1e-04,
  max.iter = 10000L,
  seed = 1001,
  trace = FALSE,
  parallel = FALSE
)
}
\arguments{
\item{x}{Data matrix.}

\item{y}{Response matrix made by \code{\link[survival]{Surv}}.}

\item{nfolds}{Fold numbers of cross-validation.}

\item{gammas}{Gammas to tune in \code{\link[ncvreg]{cv.ncvsurv}}.}

\item{eps}{Convergence threshhold.}

\item{max.iter}{Maximum number of iterations.}

\item{seed}{A random seed for cross-validation fold division.}

\item{trace}{Output the cross-validation parameter tuning
progress or not. Default is \code{FALSE}.}

\item{parallel}{Logical. Enable parallel parameter tuning or not,
default is \code{FALSE}. To enable parallel tuning, load the
\code{doParallel} package and run \code{registerDoParallel()}
with the number of CPU cores before calling this function.}
}
\description{
Automatic model selection for high-dimensional Cox models
with SCAD penalty, evaluated by penalized partial-likelihood.
}
\examples{
\donttest{
data("smart")
x <- as.matrix(smart[, -c(1, 2)])
time <- smart$TEVENT
event <- smart$EVENT
y <- survival::Surv(time, event)

fit <- fit_scad(
  x, y,
  nfolds = 3, gammas = c(3.7, 5),
  max.iter = 15000, seed = 1010
)

nom <- as_nomogram(
  fit, x, time, event,
  pred.at = 365 * 2,
  funlabel = "2-Year Overall Survival Probability"
)

plot(nom)
}
}
