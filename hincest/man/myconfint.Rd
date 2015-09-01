\name{myconfint}
\alias{myconfint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Not important2
}
\description{
Not important2
}
\usage{
myconfint(mat, level = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
Not important2
}
  \item{level}{
Not important2
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Not important2
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
myconfint = function(mat, level=.95)
{
  if(level>1&level<=100) level=level/100 else if(level>100|level<0){
   level =.95; warning('The level should be between 0 and 1, the default level=.95 has been used')}
  x1=(1-level)/2; x2=1-(1-level)/2; 
  intcf = function(x)
   { y = x; y[x<=0]=0;
     unlist(quantile(y,probs=c(.5,x1,x2)))
   }
  apply(mat,2,intcf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~ important2 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
