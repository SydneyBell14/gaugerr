% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/balanced_no_interaction.R
\name{balanced_no_interaction}
\alias{balanced_no_interaction}
\title{Balanced Without Interaction}
\usage{
balanced_no_interaction(
  data,
  part = P,
  operator = O,
  measurement = Y,
  alpha = 0.05,
  conf_type = "mls"
)
}
\arguments{
\item{data}{a data frame that contains measurements, operators and parts
for a Gauge R&R analysis}

\item{part}{a column of the data frame that has the part labels for the
measurements (this is can be specified by just the column name i.e. P)}

\item{operator}{a column of the data frame that has the operator labels
for the measurements (this is can be specified by just the column name i.e. O)}

\item{measurement}{a column of the data frame that has measurements of the
object collected (this is can be specified by just the column name i.e. Y)}

\item{alpha}{the value for the confidence interval calculation (i.e. 95\% CI
would have alpha=0.05)}

\item{conf_type}{this parameter specifies the confidence interval type that is
desired by the function and there are two options ("mls" and "gpq")}
}
\value{
a data frame of the values of the point estimates and the upper and
lower bounds for each estimate
}
\description{
Balanced Without Interaction
}
\examples{
mydata <- data.frame(P=c(1,1,1,2,2,2),
O=c(1,2,3,1,2,3), Y=c(2,2.1,2.2,1.9,2.3,1.8))
balanced_no_interaction(mydata, alpha=0.05)

}
\references{
Burdick, Richard K., Connie M. Borror, and Douglas C. Montgomery. Design and Analysis of Gauge R&R Studies: Making Decisions with Confidence Intervals in Random and Mixed ANOVA Models. Society for Industrial and Applied Mathematics, 2005.
}
