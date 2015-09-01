\name{plot.hincest}
\alias{plot.hincest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Plot diagnostic of an object 'hincest'
}
\description{
 Plots the estimated incidence rates and its bootstrapped confidence intervals
}
\usage{
plot.hincest(x, xpch = 4, xxlab = 'Age', xylab = 'Incidence rates (\%)',...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ object of class 'hincest'
}
  \item{xpch}{ either an integer specifying a symbol or a single character to be used as the default in plotting points. 
}
  \item{xxlab}{  a title for the x axis
 
}
  \item{xylab}{ a title for the y axis
}
  \item{...}{ other parameters to be passed through to plotting functions
}
}
\details{
 %%
}
\value{
%%Not important4
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Guy Mahiane
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

#require(plotrix)
#plot(xx)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~plot }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
