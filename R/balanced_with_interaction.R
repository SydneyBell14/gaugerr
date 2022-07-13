balanced_with_interaction <- function(data, part=P, operator=O,
                                      measurement=Y, alpha=0.05,
                                      conf_type = "mls") {

  # the model Y_{ijk} = mu_Y + P_i + O_j + (PO)_{ij} + E_{ijk}


  # runs all the functions and should create some sort of output
  # not sure what this would look like
  mean_ss(data, {{part}}, {{operator}}, {{measurement}})
  if (conf_type == "mls"){
    conf_intervals(data, {{part}}, operator, measurement, alpha)
  }else{
    conf_intervals_gpq(data,  part,  operator,  measurement, alpha)
  }

  gauge_variance_per(data,  part,  operator,  measurement)
  gauge_variance(data,  part,  operator,  measurement)

  return(stringr::str_c("plot", data))


  # this function produces an error which is due to the naming of part, operator and measurement.


}
