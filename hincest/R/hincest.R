hincest <-
function(age,inst, Age, HIV, ti, Mort, r=5, sizeboot=1000, cilevel=.95, ntrials=10)
{
  intime = (max(ti)+min(ti))/2;
  indtime= which(ti<intime)
  Aget1 = Age[indtime]; Aget2 = Age[-c(indtime)]; 
  HIVt1 = HIV[indtime]; HIVt2 = HIV[-c(indtime)];
  t1=ti[indtime]; t2=ti[-c(indtime)]
  
  Estim = fincest(age,inst,hivt1=HIVt1, aget1=Aget1, hivt2=HIVt2, aget2=Aget2, ti1=t1, ti2=t2, fincw=r, fMort=Mort, fntrials=ntrials)
  res = data.frame(matrix(,ncol=3),nrow=length(age))
  names(res) = c("estimate", "LowerBound", "UpperBound")
  BOOT.INCI = matrix(,nrow=sizeboot,ncol=length(age));
  nsim=1
  while(nsim<=sizeboot)
  {
    sampleboot1=sample(1:length(HIVt1), replace=TRUE)
    sampleboot2=sample(1:length(HIVt2), replace=TRUE)
    if(length(Mort)==1) Mort=rep(Mort,length(age));
    Mortboot=Mort
    Estimboot= fincest(age,inst, hivt1=HIVt1[sampleboot1], aget1=Aget1[sampleboot1],hivt2=HIVt2[sampleboot2], aget2=Aget2[sampleboot2], ti1=t1[sampleboot1], ti2=t2[sampleboot2], fincw=r, fMort=Mortboot, fntrials=ntrials)
    BOOT.INCI[nsim,] = unlist(Estimboot[1,])
    nsim=nsim+1
  }
  out.inci = myconfint(BOOT.INCI,level=cilevel)
  res = list(Age=unlist(Estim[3,]), inciEst=unlist(Estim[1,]), LowerBound=out.inci[2,],UpperBound=out.inci[3,],
  level=cilevel, Method="boostrap")
  res$call=match.call()
  class(res)<- "hincest"
  res
}

