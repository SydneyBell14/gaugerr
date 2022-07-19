#' General Gauge R&R Analysis Function
#'
#' @param data a data frame that contains measurements, operators and parts
#' for a Gauge R&R analysis
#' @param part a column of the data frame that has the part labels for the
#' measurements
#' @param operator a column of the data frame that has the operator labels
#' for the measurements
#' @param measurement a column of the data frame that has measurements of the
#' object collected
#'
#' @return a data frame with the confidence and point estimator values, for the
#' appropriate analysis (balanced/unbalanced and one factor/two factor.)
#' @export
#'
#' @import dplyr
#'
#' @examples
gauge_rr <- function(data, part=P, operator=O, measurement=Y) {
  # Function to determine if it is balanced
  # Function to check if there is interaction? not sure how to do this
  # Function to determine how many factors
  # Then run the appropriate function for the data and the gauge R&R
  # study

  # output the study results
}
