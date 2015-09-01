plot.hincest <-
function(x,xpch=4,xxlab="Age", xylab="Incidence rates (%)",...)
{  
  plot(x$Age,x$inciEst*100,pch=xpch,xlab=xxlab, ylab=xylab,ylim=c(min(x$LowerBound*80),max(x$UpperBound[x$UpperBound<1]*120)))
  plotCI(x$Age,x$inciEst*100,ui=x$UpperBound*100,li=x$LowerBound*100,add=TRUE,pch=xpch)
}

