library(dplyr)
data_renaming <- function(data, part=P, operator=O, measurement=Y){
  n <- nrow(data)
  p <- nrow(data %>%
              group_by({{part}}) %>%
              summarize(ybar = mean({{measurement}})))
  o <- nrow(data %>%
              group_by({{operator}}) %>%
              summarize(ybar = mean({{measurement}})))
  r <- n/(o*p)

  return(c(n,r,o,p))
}
