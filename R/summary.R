summary <- function(data, part=P, operator=O, measurement=Y, conf=T, conf_gpq=F,
                    var=F, var_percentage=F, mean=F, point_est=F) {
  if(conf==T){
    conf_intervals(data, part, operator, measurement)
  }
  if(conf_gpq == T){
    conf_intervals_gpq(data, part, operator, measurement)
  }
  if(var==T){
    gauge_variance(data, part, operator, measurement)
  }
  if(var_percentage==T){
    gauge_variance_per(data, part, operator, measurement)
  }
  if(mean==T){
    mean_ss(data, part, operator, measurement)
  }
  if(point_est==T){
    point_estimate(data, part, operator, measurement)
  }

}
