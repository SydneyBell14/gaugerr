balanced_with_interaction <- function(data, part=P, operator=O,
                                      measurement=Y, alpha=0.05,
                                      conf_type = "mls") {

  # runs all the functions and should create some sort of output
  # not sure what this would look like
  mean_ss(data, part, operator, measurement)
  if (conf_type == "mls"){
    conf_intervals(data, part, operator, measurement, alpha)
  }else{
    conf_intervals_gpq(data, part, operator, measurement, alpha)
  }

  gauge_variance_per(data, part, operator, measurement)
  gauge_variance(data, part, operator, measurement)

  # also thinking about changing the name conventions to have them
  # be consistent with the balanced_no_interaction function naming
  # conventions

}
