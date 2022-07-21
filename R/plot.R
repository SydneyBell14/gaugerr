#' Plotting Function
#'
#' @param df is a data frame object with class type "intervals_table"
#' @param ... other arguments that can be passed into the function
#'
#' @return a ggplot of the confidence intervals defined by the table.
#' @export plot.intervals_table
#'
#' @import ggplot2
#'
#' @examples
#' mydf <- structure(data.frame(quantity = c("part", "operator"),
#' estimate = c(12, 15), lower = c(3, 5), upper = c(20, 18)), class= "intervals_table")
#' mydf.plot
plot.intervals_table <- function(df, ...) {
  ggplot(data=df, mapping= aes(x= df$quantity, y= df$estimate)) +
    geom_pointrange(mapping = aes(ymin = df$lower, ymax = df$upper))

  NextMethod("plot")
}

plot <- function(df, ...){
  UseMethod("plot")
}
