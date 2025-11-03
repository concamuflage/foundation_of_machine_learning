# helper functions to compare a statistic and critical value

# for two sided test.
compareTwoSided = function(statistic,critical){
  cat("two sided test\n")
  if (abs(statistic) > abs(critical) ){
    cat("reject H0 by statistic\n")
  } else {
    cat("fail to reject H0 by statistic \n")
  }
}

# for right sided test
compareRightSided = function(statistic,critical){
  cat("right sided test\n")
  
  if (statistic > critical){
    cat("reject H0 by statistic\n")
  } else {
    cat("fail to reject H0 by statistic \n")
  }
}

# for left sided test
compareLeftSided = function(statistic,critical){

  cat("left sided test \n")
  
  if (statistic < critical){
    cat("reject H0 by statistic\n")
  } else {
    cat("fail to reject H0 by statistic \n")
  }
}

comparePvalueAlpha = function(p_value,alpha) {
  if (p_value < alpha){
    cat("reject H0 by p_value\n")
  } else {
    cat("fail to reject H0 by p_value\n")
  }
}