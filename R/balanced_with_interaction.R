#' Balanced With Interaction Analysis
#'
#' @param data a data frame that contains measurements, operators and parts
#' for a Gauge R&R analysis
#' @param part a column of the data frame that has the part labels for the
#' measurements
#' @param operator a column of the data frame that has the operator labels
#' for the measurements
#' @param measurement a column of the data frame that has measurements of the
#' object collected
#' @param alpha the value for the confidence interval calculation (i.e. 95% CI
#' would have alpha=0.05)
#' @param conf_type specifying the type of confidence interval wanted to calculation
#'
#' @return returns a data frame with the confidence intervals of the data that is
#' passed into the function
#' @export
#'
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' mydata <- data.frame(P=c(1,1,1,2,2,2), O=c(1,2,3,1,2,3), Y=c(2,2.1,2.2,1.9,2.3,1.8))
#' balanced_with_interaction(mydata, part=P, operator=O, measurement=Y, alpha=0.01)
balanced_with_interaction <- function(data, part=P, operator=O,
                                      measurement=Y, alpha=0.05,
                                      conf_type = "mls") {

  # the model Y_{ijk} = mu_Y + P_i + O_j + (PO)_{ij} + E_{ijk}

  part_var <- rlang::enquo(part)
  oper_var <- rlang::enquo(operator)
  y_var <- rlang::enquo(measurement)

  #if statement for the different types of confidence intervals
  if (conf_type == "mls"){
    table <- conf_intervals(data, !!part_var, !!oper_var, !!y_var, alpha)
  }else if(conf_type == "gpq"){
<<<<<<< HEAD
    table <- conf_intervals_gpq(data, part, operator, measurement, alpha)
=======
    table <- conf_intervals_gpq(data, !!part_var, !!oper_var, !!y_var, alpha)
>>>>>>> 35184bcc763c28cde0e4a20a228f9bc1706258b7
  }

  # returning the output of the function with the estimates and the CIs
  return(table)
}
