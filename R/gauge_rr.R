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
#' @param formula the user is able to input a formula that determines how many
#' factors the model has. This formula must have the lme4 format (i.e. y ~ (1 | P) + (1 | O))
#'
#' @return a data frame with the confidence and point estimator values, for the
#' appropriate analysis (balanced/unbalanced, one factor/two factor, and
#' interaction/no interaction)
#' @export
#'
#' @import dplyr, lme4
#'
#' @examples
#' mydata <- data.frame(P=c(1,2,3,4,1,2,3,4), O=c(1,2,1,2,1,2,1,2), Y=c(2,2,2,3,3,2,3,2))
#' gauge_rr(mydata, P, operator = mydata$O, Y, interaction=FALSE)
#'
#' @references Burdick, Richard K., Connie M. Borror, and Douglas C. Montgomery. Design and Analysis of Gauge R&R Studies: Making Decisions with Confidence Intervals in Random and Mixed ANOVA Models. Society for Industrial and Applied Mathematics, 2005.
#'
gauge_rr <- function(data, part=P, operator=NULL, measurement=Y,
                     alpha=0.05, interaction=FALSE, formula = NULL) {

  if (is.null(operator)){ #one factor
    part_var <- rlang::enquo(part)
    y_var <- rlang::enquo(measurement)
    if(is_balanced_one(data, !!part_var)){
      balanced_one_factor(data, part = !!part_var, measurement = !!y_var, alpha = alpha)
    }else{
      unbalanced_one_factor(data, part = !!part_var, measurement = !!y_var, alpha = alpha)
    }
  }else{ #two factor
    part_var <- rlang::enquo(part)
    oper_var <- rlang::enquo(operator)
    y_var <- rlang::enquo(measurement)
    if(is_balanced_two(data, !!part_var, !!oper_var)){
      if(!is.null(formula)){
        len <- length(lme4::findbars(formula))
        if (len == 3){
          balanced_with_interaction(data, part = !!part_var,
                                    operator = !!oper_var,
                                    measurement = !!y_var,
                                    alpha = alpha)
        }else if(len == 2){
          balanced_no_interaction(data, part = !!part_var,
                                  operator = !!oper_var,
                                  measurement = !!y_var,
                                  alpha = alpha)
        }
      }else if(is_interaction(interaction) == TRUE){
        balanced_with_interaction(data, part = !!part_var,
                                  operator = !!oper_var,
                                  measurement = !!y_var,
                                  alpha = alpha)
      }else if (is_interaction(interaction) == FALSE){
        balanced_no_interaction(data, part = !!part_var,
                                operator = !!oper_var,
                                measurement = !!y_var,
                                alpha = alpha)
      }
    }else{
      if(!is.null(formula)){
        len <- length(lme4::findbars(formula))
        if (len == 3){
          unbalanced_with_interaction(data, part = !!part_var,
                                      operator = !!oper_var,
                                      measurement = !!y_var,
                                      alpha = alpha)
        }
        else if (len == 2){
          unbalanced_no_interaction(data, part = !!part_var,
                                    operator = !!oper_var,
                                    measurement = !!y_var,
                                    alpha = alpha)
        }
      }
      if(is_interaction(interaction) == TRUE){
        unbalanced_with_interaction(data, part = !!part_var,
                                    operator = !!oper_var,
                                    measurement = !!y_var,
                                    alpha = alpha)
      }else if(is_interaction(interaction) == FALSE){
        unbalanced_no_interaction(data, part = !!part_var,
                                  operator = !!oper_var,
                                  measurement = !!y_var,
                                  alpha = alpha)
      }
    }
  }
}

# this function determines if the data is balanced or unbalanced
# a two factor model
is_balanced_two <- function(data, part=P, operator=O) {
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

# this function determines if the data is balanced or unbalanced
# for a one factor model
is_balanced_one <- function(data, part=P){
  total <- dplyr::count(data, {{part}})
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
