\name{ifincest}
\alias{ifincest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Nothing 7
}
\description{
Nothing 7
}
\usage{
ifincest(prevest.a, mortrate.a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prevest.a}{
Nothing 7
}
  \item{mortrate.a}{
Nothing 7
}
}
\details{
Nothing 7
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Nothing 7
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
function(prevest.a, mortrate.a)
{
  incid = (prevest.a$partial.prev.a+prevest.a$partial.prev.t)/(1-prevest.a$prev)+mortrate.a*prevest.a$prev
  inc = list(i=pmax(incid,0),p=prevest.a$prev,age=prevest.a$age)
  class(inc) <- 'incEst'
  inc
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
