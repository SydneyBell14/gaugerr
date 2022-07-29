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
#' @param alpha the value for the confidence interval calculation (i.e. 95% CI
#' would have alpha=0.05)
#' @param interaction this is a binary parameter where the user can specify if
#' there is or is not interaction in the data set. (using T/F or TRUE/FALSE)
#' @param factor1 this tells the function that there is a factor in the data set
#' @param factor2 this tells the function that there are multiple factors in the
#' data set.
#' @param formula the user is able to input a formula that determines how many
#' factors the model has.
#'
#' @return a data frame with the confidence and point estimator values, for the
#' appropriate analysis (balanced/unbalanced, one factor/two factor, and
#' interaction/no interaction)
#' @export
#'
#' @import dplyr
#'
#' @examples
#' mydata <- data.frame(P=c(1,2,3,4,1,2,3,4), O=c(1,2,1,2,1,2,1,2), Y=c(2,2,2,3,3,2,3,2))
#' gauge_rr(mydata, P, O, Y, interaction=FALSE, factor1=mydata$P, factor2=mydata$O)
gauge_rr <- function(data, part=P, operator=O, measurement=Y, alpha=0.05,
                     interaction=FALSE, factor1 = NULL, factor2 = NULL,
                     formula = NULL) {
  part_var <- rlang::enquo(part)
  oper_var <- rlang::enquo(operator)
  y_var <- rlang::enquo(measurement)
  # Function to determine if it is balanced
  if (is_balanced(data, !!part_var, !!oper_var) == TRUE){
    #check if it has one or two factors
    if(!is.null(factor1) && !is.null(factor2)){
      #two factor and check for interaction
      if(is_interaction(interaction) == TRUE){
        #this is balanced_with_interaction
        balanced_with_interaction(data, !!part_var, !!oper_var, !!y_var)
      }else if(is_interaction(interaction) == FALSE)
        #this is balanced_no_interaction
        balanced_no_interaction(data, !!part_var , !!oper_var, !!y_var)
    }else if (!is.null(factor1) | !is.null(factor2)){
      #this is balanced_one_factor
      balanced_one_factor(data, !!part_var, !!oper_var, !!y_var)
    }else{
      #this is an error
      print("error")
    }
  }else{
    #check if it has one or two factors
    if(!is.null(factor1) && !is.null(factor2)){
      #two factor and check for interaction
      if(is_interaction(interaction) == TRUE){
        #this is unbalanced_with_interaction
        unbalanced_with_interaction(data, !!part_var, !!oper_var, !!y_var)
      }else if(is_interaction(interaction) == FALSE)
        #this is unbalanced_no_interaction
        unbalanced_no_interaction(data, !!part_var , !!oper_var, !!y_var)
    }else if (!is.null(factor1) | !is.null(factor2)){
      #this is unbalanced_one_factor
      unbalanced_one_factor(data, !!part_var, !!oper_var, !!y_var)
    }else{
      #this is an error
      print("error")
    }
  }
}

#this function determines if the data is balanced or unbalanced
is_balanced <- function(data, part=P, operator=O) {
  total <- dplyr::count(data, {{part}}, {{operator}})
  for (i in 1:length(total$n)){
    if (sum(total$n)/length(total$n) == total$n[i]){
      result <- TRUE
    }else{
      result <- FALSE
      break
    }
  }
  return(result)
}

#this function determines if there is interaction
is_interaction <- function(interaction){
  if(interaction == TRUE){
    interact <- TRUE
  }else if(interaction == FALSE){
    interact <- FALSE
  }
  return(interact)
}

#this determines if is one or two factors in the model and runs an error otherwise
num_factors <- function(data, part=NULL, operator=NULL){
  if (!is.null(part) && !is.null(operator)){
    factors <- "two"
    return(factors)
  }else if(!is.null(part) && is.null(operator)){
    factors <- "one"
    return(factors)
  }else if(is.null(part) && !is.null(operator)){
    factors <- "one"
    return(factors)
  }else{
    factors <- "error"
    return(factors)
  }
}
