\name{crit2cost}
\alias{crit2cost}
\title{
Cost function from seriation criterion}
\description{
	Returns a cost function from seriation criterion (package seriation)
}
\usage{
crit2cost(crit)
}
\arguments{
  \item{crit}{
One of seriation::list_criterion_methods("dist")
}

}
\examples{
	 			
require(DendSer) 			

d<- dist(iris[1:20,-5])
h <- hclust(d)
DendSer(h,d,cost=function(x,o,...) criterion(as.dist(x),o,method="AR_deviations"))
DendSer(h,d,cost=crit2cost("AR_deviations")) # short version of above
DendSer(h,d,cost=crit2cost("ME"))

# use DendSer methods via seriate
# get_order(seriate(d,method="Dendser", control=list(hclust=h,cost=costARc))) 
DendSer(h,d,cost=costARc)




}
