% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotResults.R
\name{plotResults}
\alias{plotResults}
\title{Plot results}
\usage{
plotResults(path, x, n, taskname, ...)
}
\arguments{
\item{path}{path to store the plot}

\item{x}{a vector of counts of successes; could be obtained from getResults()}

\item{n}{a vector of counts of trials}

\item{taskname}{the name of the task for labeling, e.g., Word Intrusion, Optimal Label.}
}
\description{
Plot results
}
\details{
Visualize the accuracy rate (proportion correct) for a specified batch
}
