#' Plotting Function
#'
#' @param x is a data frame object with class type "intervals_table"
#' @param ... other arguments that can be passed into the function
#'
#' @return a ggplot of the confidence intervals defined by the table.
#' @export
#'
#' @import ggplot2
#' @method plot intervals_table
#'
#' @examples
#' mydf <- structure(data.frame(quantity = c("part", "operator"),
#' estimate = c(12, 15), lower = c(3, 5), upper = c(20, 18)),
#' class= c("intervals_table", "data.frame"))
#' plot(mydf)
plot.intervals_table <- function(x,...) {

  ggplot(data=x, mapping= aes(x=quantity, y= estimate)) +
    geom_pointrange(mapping = aes(ymin = lower, ymax = upper))

}

