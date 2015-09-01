\name{fprevEst}
\alias{fprevEst}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Not important1
}
\description{
Not important1
}
\usage{
fprevEst(age, inst, hivt1, Aget1, hivt2, Aget2, t1, t2, incw, ntrials)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{age}{
Not important1
}
  \item{inst}{
Not important1
}
  \item{hivt1}{
Not important1
}
  \item{Aget1}{
Not important1
}
  \item{hivt2}{
Not important1
}
  \item{Aget2}{
Not important1
}
  \item{t1}{
Not important1
}
  \item{t2}{
Not important1
}
  \item{incw}{
Not important1
}
  \item{ntrials}{
Not important1
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Work ongoing
}
\author{
GM
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
fprevEst = function(age,inst, hivt1, Aget1, hivt2, Aget2, t1, t2, incw, ntrials)
{
   hivt1pos = which(hivt1==1& Aget1<=age+incw & Aget1>=age-incw)
   hivt1neg = which(hivt1==0& Aget1<=age+incw & Aget1>=age-incw)
   hivt2pos = which(hivt2==1& Aget2<=age+incw & Aget2>=age-incw)
   hivt2neg = which(hivt2==0& Aget2<=age+incw & Aget2>=age-incw)
   pos.age.t1 = Aget1[hivt1pos]; pos.age.t2 = Aget2[hivt2pos] 
   neg.age.t1 = Aget1[hivt1neg]; neg.age.t2 = Aget2[hivt2neg]
   ut1neg = length(neg.age.t1)!=0; ut2neg = length(neg.age.t2)!=0
   ut1pos = length(pos.age.t1)!=0; ut2pos = length(pos.age.t2)!=0
   minaget1 = min(Aget1)+3; minaget2 = min(Aget2)+3
   maxaget1 = max(Aget1)+4; maxaget2 = max(Aget2)+4
  lv = function(mpar)
  {
    p0 = exp(mpar[1]) ; p01 = mpar[2]; p10 = mpar[3];
    lv.pos.t1 = function(w)
    { res = p0 + p01*(w-age)+ p10*(t1-inst)
      u = res <= 0; res[u]=10^(-25)
      u = res >= 1; res[u]=1-10^(-25)
      log(res)
    }
    lv.neg.t1 = function(w)
    { res = 1-(p0 + p01*(w-age)+ p10*(t1-inst))
      u = res <= 0; res[u]=10^(-25)
      u = res >= 1; res[u]=1-10^(-25)
      log(res)
    }
    lv.pos.t2 = function(w)
    { res = p0 + p01*(w-age)+ p10*(t2-inst)
      u = res <= 0; res[u]=10^(-25)
      u = res >= 1; res[u]=1-10^(-25)
      log(res)
    }
    lv.neg.t2 = function(w)
    { res = 1-(p0 + p01*(w-age)+ p10*(t2-inst))
      u = res <= 0; res[u]=10^(-25)
      u = res >= 1; res[u]=1-10^(-25)
      log(res)
    }
    res = 0
    if (ut1neg) {
      zz = table(sort(neg.age.t1)); zzage = unique(sort(neg.age.t1))
      res = res - sum(sapply(zzage, lv.neg.t1)*zz)
    }
    if(ut1pos){ 
      zz = table(sort(pos.age.t1)) ; zzage = unique(sort(pos.age.t1))
      res = res - sum(sapply(zzage, lv.pos.t1)*zz)
    }
    if(ut2neg){ 
      zz = table(sort(neg.age.t2 )) ; zzage = unique(sort(neg.age.t2 ))
      res = res - sum(sapply(zzage, lv.neg.t2)*zz)
    }
    if(ut2pos){
      zz=table(sort(pos.age.t2)); zzage = unique(sort(pos.age.t2))
      res = res - sum(sapply(zzage, lv.pos.t2)*zz)
    }  
    res
  }
  
  init.p0.t1 = sum(ut1pos)/(sum(ut1pos)+sum(ut1neg))
  n01.t1 = sum(hivt1==1& Aget1<=max(age,minaget1)) 
  n00.t1 = sum(hivt1==0& Aget1<=max(age,minaget1))
  n11.t1 = sum(hivt1==1& Aget1>=min(age,maxaget1))
  n10.t1 = sum(hivt1==0& Aget1>=min(age,maxaget1))
  
  init.p1.t1 = 1/incw/2*(n11.t1/(n11.t1+n10.t1)-n01.t1/(n01.t1+n00.t1))
  
  init.p0.t2 = sum(ut2pos)/(sum(ut2pos)+sum(ut2neg))
  n01.t2 =  sum(hivt2==1& Aget2<=max(age,minaget2)); 
  n00.t2 =  sum(hivt2==0& Aget2<=max(age,minaget2))
  n11.t2 =  sum(hivt2==1& Aget2>=min(age,maxaget2)); 
  n10.t2 =  sum(hivt2==0& Aget2>=min(age,maxaget2))
  
  init.p1.t2 = 1/incw/2*(n11.t2/(n11.t2+n10.t2)-n01.t2/(n01.t1+n00.t1))

  init.p0 =  1/2*(init.p0.t1+init.p0.t2); u=init.p0==0; init.p0[u]=10^-25
  init.p1 =  1/2*(init.p1.t1+init.p1.t2)
  init.p2 = 1/(t2-t1)*(init.p0.t2-init.p0.t1)
  
  logistic = function(x) log((x+10^-25)/(1+10^-25-x))
  
  res = nlm(lv,c(log(init.p0),init.p1,init.p2),gradtol=10e-5)

  F.res = res$estimate

  if(res$code==1) { F.res[1] = exp(F.res[1]); F.res = list(age=age, prev=F.res[1],partial.prev.a=F.res[2],partial.prev.t=F.res[3]) 
  } else
  { 
     res = optim(c(log(init.p0),init.p1,init.p2),lv)
     if(res$convergence==0) 
     {
       F.res = res$par; F.res[1] = exp(F.res[1]); F.res = list(age=age, prev=F.res[1],partial.prev.a=F.res[2],partial.prev.t=F.res[3]) 
     } else 
     {
         
         k = 0
      
         R.Values = numeric()
         R.Estimate = list()
      
         for(j in 1:ntrials)
         {
           res = optim(c(log(runif(1+10^-25)),runif(2)),lv) # I no longer use nlm
           if(is.list(res))
             { k=k+1; R.Estimate[[k]] = res$par
                      R.Values[k]=res$value
             }
         }
         {
          mink = which (abs((R.Values-min(R.Values))/min(R.Values))<10^-1)
          fail = length(mink)/k ; crit = min(3/ntrial,1)
          if(fail>=crit)
          {
            imink = which (R.Values==min(R.Values))[1]
            F.res = R.Estimate[[imink]]
            F.res[1] = exp(F.res[1])
            F.res = list(age=age, prev=F.res[1],partial.prev.a=F.res[2],partial.prev.t=F.res[3])      
          } else 
          {
             for(j in (ntrial+1):3*ntrials)
             {
               res = optim(c(log(runif(1+10^-25)),runif(2)),lv)
               if(is.list(res))
                 { k=k+1; R.Estimate[[k]] = res$par
                          R.Values[k]=res$value
                 }
             }
             imink = which (R.Values==min(R.Values))[1]
             F.res = R.Estimate[[imink]]
             F.res[1] = exp(F.res[1])
             F.res = list(age=age, prev=F.res[1],partial.prev.a=F.res[2],partial.prev.t=F.res[3])
          }
       }
     } 
  }
  class(F.res)<- 'prevEst'
  F.res
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
