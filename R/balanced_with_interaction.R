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
#' @importFrom stringr str_c
#'
#' @examples
#' mydata <- data.frame(P=c(1,1,1,2,2,2), O=c(1,2,3,1,2,3), Y=c(2,2.1,2.2,1.9,2.3,1.8))
balanced_with_interaction <- function(data, part=P, operator=O,
                                      measurement=Y, alpha=0.05,
                                      conf_type = "mls") {

  #This can be a combination of the conf_intervals and the conf_intervals_gpq

  # the model Y_{ijk} = mu_Y + P_i + O_j + (PO)_{ij} + E_{ijk}


  # runs all the functions and should create some sort of output
  # not sure what this would look like
  mean_ss(data, part, operator,measurement)
  if (conf_type == "mls"){
    conf_intervals(data, part, operator, measurement, alpha)
  }else{
    conf_intervals_gpq(data,part, operator, measurement, alpha)
  }

  gauge_variance_per(data,  part,  operator,  measurement)
  gauge_variance(data,  part,  operator,  measurement)

  return(stringr::str_c("plot", data))


  # output will be a tibble with quantity, estimate, lower, and upper columns.



}
