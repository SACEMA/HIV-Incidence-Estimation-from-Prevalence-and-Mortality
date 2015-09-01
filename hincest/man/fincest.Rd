\name{fincest}
\alias{fincest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Nothing6
}
\description{
Nothing6
}
\usage{
fincest(fage, finst, hivt1, aget1, hivt2, aget2, ti1, ti2, fincw, fMort, fntrials)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fage}{
Nothing6
}
  \item{finst}{
Nothing6
}
  \item{hivt1}{
Nothing6
}
  \item{aget1}{
Nothing6
}
  \item{hivt2}{
Nothing6
}
  \item{aget2}{
Nothing6
}
  \item{ti1}{
Nothing6
}
  \item{ti2}{
Nothing6
}
  \item{fincw}{
Nothing6
}
  \item{fMort}{
Nothing6
}
  \item{fntrials}{
Nothing6
}
}
\details{
Nothing6
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
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
function(fage,finst, hivt1, aget1, hivt2, aget2, ti1, ti2, fincw, fMort, fntrials)
{
  sortind = sort(fage, index.return=T)$ix
  fage=fage[sortind]; 
  if(length(fMort)==1) fMort = rep(fMort,length(fage)) else fMort=fMort[sortind];
  if(length(finst)==1) finst = rep(finst,length(fage)) else finst=finst[sortind];
  if(length(fincw)==1) fincw = rep(fincw,length(fage));
  if(length(ti1)==1) ti1 = rep(ti1,length(HIVt1))
  if(length(ti2)==1) ti2 = rep(ti2,length(HIVt2))
  res = sapply(1:length(fage),function(aa) fincEst(fprevEst(fage[aa],finst[aa], hivt1, aget1, hivt2, aget2, ti1[aa], ti2[aa], fincw[aa], fntrials),fMort[aa]) ) 
  res;
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
