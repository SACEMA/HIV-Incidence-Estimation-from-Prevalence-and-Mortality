\name{print.hincest}
\alias{print.hincest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Estimation of the age and time specific incidence rates
}
\description{
 Prints estimates of the incidence rates and confidence intervals for a given inclusion window
}
\usage{
print.hincest(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ object of the class 'hincest'
}  
\item{...}{ other parameters to be passed through to printing functions
}
\details{
%% Not important5
}
}
\value{
%% Not important5
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
S. Guy Mahiane
}
\note{
%%  ~~further notes~~
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(hincdata)
dd=hincdata;
#xx=hincest(20:30,inst=15, Age=c(dd$dsurv$aget1,dd$dsurv$aget2),
#HIV=c(dd$dsurv$hivt1,dd$dsurv$hivt2), 
#ti=c(rep(dd$tsurv[1],length(dd$dsurv$aget1)), rep(dd$tsurv[2],length(dd$dsurv$aget2))), 
#Mort=dd$Mort, incw=5, sizeboot=400,cilevel=.95, ntrials=10)

#plot(xx) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~print }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
