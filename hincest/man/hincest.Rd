\name{hincest}
\alias{hincest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimation of the age and time specific incidence rates
}
\description{
Estimates the incidence rates and confidence intervals for a given inclusion window
}
\usage{
hincest(age, inst, Age, HIV, ti, Mort, r = 5, sizeboot = 1000, cilevel = 0.95, ntrials = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{age}{ vector of ages at which the incidence rates are to be estimated
}
  \item{inst}{ time point at which the incidence is to be estimated; should be greater 
  than the smallest time individuals were observed and smaller than the largest time individuals were surveyed
}
  \item{Age}{ vector of ages of individuals surveyed
}
  \item{HIV}{ vector of HIV statuses of individuals surveyed; should be 
  coded in the following way: 0 for HIV-negative and 1 for HIV positive individuals
}
  \item{ti}{ vector of times when individuals where surveyed
}
  \item{Mort}{ vector of equal length as age containing the corresponding estimates of
   the excess mortality rates for infected individuals, AT THE TIME 'inst'.
}
  \item{r}{ vector of inclusion windows - one value for each age at which incidence is to be calculated
}
  \item{sizeboot}{ number of replications of the bootstrap for confidence interval estimation. The default is 1000
}
  \item{cilevel}{ level of the confidence intervals, a number between 0 and 1. It is optional, the default is 0.95, which leads to 95\% confidence intervals.

}
  \item{ntrials}{ number of random initiations of the algorithm for maximising the likelihood, it is set to 10 by 
  default.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
List containging the ages at which the incidence rates are estimated, the estimate 
of the incidence rates, the lower and upper bounds of the confidence intervals.
  \item{Age }{ vector of age(s) at which the incidence rates are estimated}
  \item{inciEst }{ estimates of the incidence rates}
  \item{LowerBound }{ lower bound of the confidence intervals} 
  \item{UpperBound }{ upper bound of the confidence intervals}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
S. Guy Mahiane and A. Welte 
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

#xx;
#require(plotrix)
#plot(xx)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
