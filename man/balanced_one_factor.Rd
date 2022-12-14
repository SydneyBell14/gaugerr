% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/balanced_one_factor.R
\name{balanced_one_factor}
\alias{balanced_one_factor}
\title{Unbalanced One Factor}
\usage{
balanced_one_factor(data, part = P, measurement = Y, alpha = 0.05)
}
\arguments{
\item{data}{a data frame that contains measurements, operators and parts
for a Gauge R&R analysis}

\item{part}{a column of the data frame that has the part labels for the
measurements (this is can be specified by just the column name i.e. P)}

\item{measurement}{a column of the data frame that has measurements of the
object collected (this is can be specified by just the column name i.e. Y)}

\item{alpha}{the value for the confidence interval calculation (i.e. 95\% CI
would have alpha=0.05)}
}
\value{
a data frame of the values of the point estimates and the upper and
lower bounds for each estimate
}
\description{
Unbalanced One Factor
}
\examples{
mydata <- data.frame(P=c(1,1,2,2,3,3),
O=c(1,1,1,1,1,1),Y=c(5.3, 6.5, 5.4, 6.4, 6.9, 5.8))
balanced_one_factor(mydata, alpha=0.01)

}
\references{
Burdick, Richard K., Connie M. Borror, and Douglas C. Montgomery. Design and Analysis of Gauge R&R Studies: Making Decisions with Confidence Intervals in Random and Mixed ANOVA Models. Society for Industrial and Applied Mathematics, 2005.
}
