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
#' @param conf_type specifying the type of confidence interval wanted to calculation,
#' and there are two options ("mls" or "gpq")
#'
#' @return returns a data frame with the confidence intervals of the data that is
#' passed into the function
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import rlang
#'
#' @examples
#' mydata <- data.frame(P=c(1,1,1,2,2,2),
#' O=c(1,2,3,1,2,3), Y=c(2,2.1,2.2,1.9,2.3,1.8))
#' balanced_with_interaction(mydata, part=P, operator=O, measurement=Y, alpha=0.01)
#' balanced_with_interaction(mydata, conf_type = "gpq")
#'
#' @references Burdick, Richard K., Connie M. Borror, and Douglas C. Montgomery. Design and Analysis of Gauge R&R Studies: Making Decisions with Confidence Intervals in Random and Mixed ANOVA Models. Society for Industrial and Applied Mathematics, 2005.
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
    bal_interact_mls <- structure(table, class = c("intervals_table", "data.frame"))
    return(bal_interact_mls)
  }else if(conf_type == "gpq"){
    table <- conf_intervals_gpq(data, !!part_var, !!oper_var, !!y_var, alpha)
    bal_interact_gpq <- structure(table, class = c("intervals_table", "data.frame"))
    return(bal_interact_gpq)
  }


}
