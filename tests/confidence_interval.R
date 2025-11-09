

# two sided

intervalTwoSided = function(critical_value,standard_error,mean_value ){
  critical_value = abs(critical_value)
  upper_bound = mean_value + critical_value * standard_error
  lower_bound = mean_value - critical_value * standard_error
  return(list(lower_bound = lower_bound, upper_bound = upper_bound))
}

# right sided/lower confidence bounds

intervalRightSided = function(critical_value,standard_error,mean_value ){
  # rejection region on the right
  critical_value = abs(critical_value)
  lower_bound = mean_value - critical_value * standard_error
  upper_bound = Inf
  return (list(lower_bound = lower_bound, upper_bound = upper_bound))
}

# left sided

intervalLeftSided = function(critical_value,standard_error,mean_value ){
  # rejection region on the left
  critical_value = abs(critical_value)
  lower_bound = -Inf
  upper_bound = mean_value + critical_value * standard_error
  return (list(lower_bound = lower_bound, upper_bound = upper_bound)) 
}
