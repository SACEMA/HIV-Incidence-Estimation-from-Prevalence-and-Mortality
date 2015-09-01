myconfint <-
function(mat, level=.95)
{
  if(level>1&level<=100) level=level/100 else if(level>100|level<0){
   level =.95; warning("The level should be between 0 and 1, the default level=.95 has been used")}
  x1=(1-level)/2; x2=1-(1-level)/2; 
  intcf = function(x)
   { y = x; y[x<=0]=0;
     unlist(quantile(y,probs=c(.5,x1,x2),na.rm=T))
   }
  apply(mat,2,intcf)
}

