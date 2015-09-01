ifincest <-
function(prevest.a, mortrate.a)
{
  incid = (prevest.a$partial.prev.a+prevest.a$partial.prev.t)/(1-prevest.a$prev)+mortrate.a*prevest.a$prev
  inc = list(i=pmax(incid,0),p=prevest.a$prev,age=prevest.a$age)
  class(inc) <- "incEst"
  inc
}

