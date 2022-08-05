#' General Gauge R&R Analysis Function
#'
#' @param data a data frame that contains measurements, operators and parts
#' for a Gauge R&R analysis
#' @param part a column of the data frame that has the part labels for the
#' measurements (this is can be specified by just the column name i.e. P)
#' @param operator a column of the data frame that has the operator labels
#' for the measurements (this is can be specified by just the column name i.e. O)
#' @param measurement a column of the data frame that has measurements of the
#' object collected (this is can be specified by just the column name i.e. Y)
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
#' @import dplyr
#' @importFrom lme4 findbars
#' @importFrom rlang enquo
#'
#' @examples
#' mydata <- data.frame(P=c(1,2,3,4,1,2,3,4), Y=c(2,2,2,3,3,2,3,2))
#' gauge_rr(mydata, part = P, measurement = Y, interaction=FALSE)
#'
#' @references Burdick, Richard K., Connie M. Borror, and Douglas C. Montgomery. Design and Analysis of Gauge R&R Studies: Making Decisions with Confidence Intervals in Random and Mixed ANOVA Models. Society for Industrial and Applied Mathematics, 2005.
#'
gauge_rr <- function(data, part=P, operator=NULL, measurement=Y,
                     alpha=0.05, interaction=FALSE, formula = NULL) {

  operator <- rlang::enquo(operator)
  if (rlang::quo_is_null(operator)){ #one factor
    #setting to part to something that can be passed into a function
    part_var <- rlang::enquo(part)
    #setting measurement to something that can be passed into a function
    y_var <- rlang::enquo(measurement)
    if(is_balanced_one(data, !!part_var)){ #balanced one factor
      balanced_one_factor(data, part = !!part_var, measurement = !!y_var, alpha = alpha)
    }else{ #unbalanced one factor
      unbalanced_one_factor(data, part = !!part_var, measurement = !!y_var, alpha = alpha)
    }
  }else{ #two factor
    #setting to part to something that can be passed into a function
    part_var <- rlang::enquo(part)
    #setting to operator to something that can be passed into a function
    oper_var <- rlang::enquo(operator)
    #setting to measurement to something that can be passed into a function
    y_var <- rlang::enquo(measurement)
    if(is_balanced_two(data, !!part_var, !!oper_var)){
      #balanced two factor
      if(!is.null(formula)){ #parsing formula
        len <- length(lme4::findbars(formula))
        if (len == 3){ #interaction present
          balanced_with_interaction(data, part = !!part_var,
                                    operator = !!oper_var,
                                    measurement = !!y_var,
                                    alpha = alpha)
        }else if(len == 2){ #no interaction present
          balanced_no_interaction(data, part = !!part_var,
                                  operator = !!oper_var,
                                  measurement = !!y_var,
                                  alpha = alpha)
        }
      }else if(is_interaction(interaction) == TRUE){ #no formula given, interaction present
        balanced_with_interaction(data, part = !!part_var,
                                  operator = !!oper_var,
                                  measurement = !!y_var,
                                  alpha = alpha)
      }else if (is_interaction(interaction) == FALSE){ #no formula given, no interaction present
        balanced_no_interaction(data, part = !!part_var,
                                operator = !!oper_var,
                                measurement = !!y_var,
                                alpha = alpha)
      }
    }else{ #unbalanced two faactor
      if(!is.null(formula)){ #parsing the formula
        len <- length(lme4::findbars(formula))
        if (len == 3){ #interaction present
          unbalanced_with_interaction(data, part = !!part_var,
                                      operator = !!oper_var,
                                      measurement = !!y_var,
                                      alpha = alpha)
        }
        else if (len == 2){ #no interaction present
          unbalanced_no_interaction(data, part = !!part_var,
                                    operator = !!oper_var,
                                    measurement = !!y_var,
                                    alpha = alpha)
        }
      }
      if(is_interaction(interaction) == TRUE){ #no formula given, interaction present
        unbalanced_with_interaction(data, part = !!part_var,
                                    operator = !!oper_var,
                                    measurement = !!y_var,
                                    alpha = alpha)
      }else if(is_interaction(interaction) == FALSE){ #no formula given, no interaction present
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

# this function determines if there is interaction
is_interaction <- function(interaction){
  if(interaction == TRUE){
    interact <- TRUE
  }else if(interaction == FALSE){
    interact <- FALSE
  }
  return(interact)
}
