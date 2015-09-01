\name{hincEst-package}
\alias{hincEst-package}
\alias{hincEst}
\docType{package}
\title{
Estimation of incidence rate of any infection with differential moratality such as HIV
}
\description{
Estimates incidence rate of any infection with differential mortality using individual serostatus data
 observed at successive surveys and excess mortality due to the infection at a given time between the surveys.
}
\details{
\tabular{ll}{
Package: \tab hincEst\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-07-05\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
The main fuctions are:

hincest: A wrapper function to estimate incidence rate as function of age at a given
 time between two surveys.
 
plot.hincest: a function for plotting objects of the class 'hincest' 

choicer: a function for studying the sensitivity of the incidence with respect to the
inclusion window. It provides incidence rate estimates and confidence intervals at given
ages and times as a function of the inclusion window.

plot.choicer: a function for plotting objects of the class 'choicer' 
}
\author{
S. Guy Mahiane

Maintainer: S. Guy Mahiane <g.mahiane@gmail.com>
}
\references{
The paper is in review
}
\keyword{ package }
\seealso{
}
\examples{
data(hincdata)
dd=hincdata;
#xx=hincest(20:30,inst=15, Aget1=dd$dsurv$aget1, Aget2=dd$dsurv$aget2,
#HIVt1=dd$dsurv$hivt1, HIVt2=dd$dsurv$hivt2, t1=dd$tsurv[1], t2=dd$tsurv[2], Mort=dd$Mort, r=5, sizeboot=400,
#cilevel=.95, ntrials=10, sdmort=0)

#xx;
#require(plotrix)
#plot(xx)

#xxx=choicer(c(23,19,30),incw=1:5,inst=15, Aget1=dd$dsurv$aget1, Aget2=dd$dsurv$aget2, 
#HIVt1=dd$dsurv$hivt1, HIVt2=dd$dsurv$hivt2, t1=dd$tsurv[1], t2=dd$tsurv[2], Mort=dd$Mort, 
#sizeboot=400,cilevel=.95, ntrials=10)
#xxx;
#plot(xxx)

}