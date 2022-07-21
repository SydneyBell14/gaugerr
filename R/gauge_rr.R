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
  part_var <- rlang::enquo(part)
  oper_var <- rlang::enquo(operator)
  y_var <- rlang::enquo(measurement)
  # Function to determine if it is balanced
  is_balanced <- function(data, part, operator) {
    total <- count(data, part, operator)

    for (i in length(total$n)){
      if (sum(total$n)/length(total$n) == total$n[i]){
        result <- TRUE
      }else{
        result <- FALSE
        return(result)
      }
    }
    #what determines if it is balanced
  }
  if (is_balanced == TRUE){
    #check for interaction term

  }else{
    #check for one factor or two factors

  }

  # Function to check if there is interaction? not sure how to do this
  # Function to determine how many factors
  # Then run the appropriate function for the data and the gauge R&R
  # study

  # output the study results
}
