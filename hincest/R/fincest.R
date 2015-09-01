fincest <-
function(fage,finst, hivt1, aget1, hivt2, aget2, ti1, ti2, fincw, fMort, fntrials)
{
  sortind = sort(fage, index.return=T)$ix
  fage=fage[sortind]; 
  if(length(fMort)==1) fMort = rep(fMort,length(fage)) else fMort=fMort[sortind];
  if(length(finst)==1) finst = rep(finst,length(fage)) else finst=finst[sortind];
  if(length(fincw)==1) fincw = rep(fincw,length(fage));
  if(length(ti1)==1) ti1 = rep(ti1,length(hivt1))
  if(length(ti2)==1) ti2 = rep(ti2,length(hivt2))
  res = sapply(1:length(fage),function(aa) ifincest(fprevEst(fage[aa],finst[aa], hivt1, aget1, hivt2, aget2, ti1[aa], ti2[aa], fincw[aa], fntrials),fMort[aa]) ) 
  res;
}

