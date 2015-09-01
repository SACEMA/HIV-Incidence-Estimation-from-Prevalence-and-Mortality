print.hincest <-
function(x,...)
{
  cat("Call:\n")
  print(x$call)
  cat("\n Incidence rate estimates and ", x$level,"% Confidence Intervals\n")
  print(data.frame(Age=x$Age,Estimate=x$inciEst,LowerBound=x$LowerBound,UpperBound=x$UpperBound))
}

