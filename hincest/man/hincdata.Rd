\name{hincdata}
\alias{hincdata}
\docType{data}
\title{
Example of simulated data for the HIV epidemic
}
\description{
List containing: a) one data frame of 4 variables giving age and HIV statuses (0 for HIV-negative,
 1 for HIV positive) of individuals in a population in which two surveys were
 conducted at two times; b) times at which the surveys were conducted and c) the excess
  mortality among infected individuals
}
\usage{data(hincdata)}
\format{
  The format is:
List of 3
 $ dsurv:'data.frame':	5000 obs. of  4 variables:
  ..$ aget1: int [1:5000] 15 15 15 15 15 15 15 15 15 15 ...
  ..$ aget2: int [1:5000] 15 15 15 15 15 15 15 15 15 15 ...
  ..$ hivt1: num [1:5000] 0 0 0 0 0 0 0 0 0 0 ...
  ..$ hivt2: num [1:5000] 0 0 0 0 0 0 0 0 0 0 ...
 $ tsurv: num [1:2] 13 17
 $ Mort : num [1:35] 0.000411 0.002833 0.005444 0.007795 0.009303 ...
}
\details{
dsurv data frame containing data from two surveys. aget1 and aget2 are ages of
individuals sureveyed at the first (t1) and second (t2) surveys rectively; hivt1 and hivt2
 are HIV statuses of individuals surveyed at the first (t1) and second (t2) surveys.
tsurv time at which the surveys were conducted. The first coordonate indicates t1
Mort the excess mortality rates at time t0=15 among for HIV infected individuals
}
\source{
An HIV epidemic was simulated and a survey was conducted in the simulated population
 at times 13 and 17 after the infection was introduced in the population.
}
\references{
The paper is in review
}
\examples{
data(hincdata)
str(hincdata) ; #plot(hincdata) ...
summary(hincdata)
}
\keyword{datasets}
