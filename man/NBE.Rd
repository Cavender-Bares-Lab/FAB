% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NBE.R
\name{NBE}
\alias{NBE}
\title{Net Biodiversity Effect}
\usage{
NBE(experiment_design, measured_feature)
}
\arguments{
\item{experiment_design}{A \code{data.frame} with columns representing species
and rows representing plots. Each cell contains the relative abundance of species
per plot. Plot IDs should be embedded as \code{rownames} otherwise row numbers
will be returned as plot IDs.}

\item{measured_feature}{If it is a \code{data.frame} it should
describe measurements at the species level (e.g., biomass, growth rates), with
columns as species and rows as plots similar to \code{experiment_design}. Each
cell must contain the measured feature; use \code{NA} if the feature was not measured.
If it is a numeric vector it should describe the measured feature at the
community level.}
}
\value{
A data.table with the estimations of the Net Biodiversity Effect (NBE) and
its two components, Complementary Effect (CE) and Selection Effect (SE) per
plot.
}
\description{
Estimates the Net Biodiversity Effect (NBE) and its two components:
Complementary Effect (CE) and Selection Effect (SE). The function is based
on Loreau & Hector (2001) <doi:10.1038/35083573>.
}
\references{
Loreau, M., Hector, A. 2001. Partitioning selection and
complementarity in biodiversity experiments. Nature 412, 72–76 (2001).
 <doi:10.1038/35083573>
}
\author{
J. Antonio Guzmán Q. and Laura Williams
}
