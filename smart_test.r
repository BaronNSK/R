smart_test <- function(x){
  x <- table(x)
  if (min(x) < 5){
    return(fisher.test(x)$p.value)
  }
  else{
    chi <- chisq.test(x)
    return(c(chi$statistic, chi$parameter, chi$p.value))
  }
}