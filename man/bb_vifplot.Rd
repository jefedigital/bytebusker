% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bb_vifplot.R
\name{bb_vifplot}
\alias{bb_vifplot}
\title{bb_vifplot}
\usage{
bb_vifplot(lm, title = NULL)
}
\arguments{
\item{lm}{a linear regression lm object}

\item{title}{a title}
}
\value{
a VIF plot
}
\description{
Takes an lm object and makes a nice Variance Inflation Factor (VIF) plot using car::vif()
}
