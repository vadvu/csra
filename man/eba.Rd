% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eba.R
\name{eba}
\alias{eba}
\title{Extreme boundary analysis for binary outcome}
\usage{
eba(y, x, const, control, data, nvar, model = "logit", cl = NULL)
}
\arguments{
\item{y}{Character. Dependent variable.}

\item{x}{Character. Independent variable.}

\item{const}{Array with characters. Set of constant controls.}

\item{control}{Array with characters. Set of controls.}

\item{data}{Dataframe.}

\item{nvar}{Number. Number of control variables (not including constant controls) to make combinations.}

\item{model}{Model type. Now only "logit" for logistic regression is supported.}

\item{cl}{Number. Number of clusters to use for computations. Default is \code{NULL} for 1 cluster.}
}
\value{
Dataframe with results
}
\description{
Extreme boundary analysis for binary outcome
}
