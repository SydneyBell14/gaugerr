% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/point_estimate.R
\name{point_estimate}
\alias{point_estimate}
\title{Point Estimates for Gauge R&R Analysis}
\usage{
point_estimate(data, part = P, operator = O, measurement = Y)
}
\arguments{
\item{data}{a data frame consisting of gauge R&R data to run the analysis}

\item{part}{a column of the data frame that has the part labels for the
measurements (this is can be specified by just the column name i.e. P)}

\item{operator}{a column of the data frame that has the operator labels
for the measurements (this is can be specified by just the column name i.e. O)}

\item{measurement}{a column of the data frame that has measurements of the
object collected (this is can be specified by just the column name i.e. Y)}
}
\value{
a list of the point estimates
s_p: the point estimate for parts
s_o: the point estimate for operators
s_po the point estimate for the parts and operators
s_tot: the total point estimate
s_repro: the point estimate for reproducibility
s_gauge:
pg_ratio: the part/gauge ratio (s2_p/s2_gauge)
gt_ratio: the gauge/total ratio (s2_gauge/s2_tot)
pr_ratio: the part/reproducibility ratio (s2_p/s2_repro)
}
\description{
Point Estimates for Gauge R&R Analysis
}
\examples{
mydata <- data.frame(P=c(1,2), O=c(1,1), Y=c(5, 4))
point_estimate(mydata)
point_estimate(mydata, part=P, operator=O, measurement=Y)

}
\references{
Burdick, Richard K., Connie M. Borror, and Douglas C. Montgomery. Design and Analysis of Gauge R&R Studies: Making Decisions with Confidence Intervals in Random and Mixed ANOVA Models. Society for Industrial and Applied Mathematics, 2005.
}
