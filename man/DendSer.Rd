\name{DendSer}
\alias{DendSer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Implements dendrogram seriation
}
 \description{
 Implements dendrogram seriation. 
}
\usage{
DendSer(h, ser_weight, cost = costBAR, node_op = NULL, costArg = NULL, 
maxloops = NULL, saveinfo = FALSE, direction = NULL,  GW=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{
     An object of class hclust
}
  \item{ser_weight}{
     Used by cost function to evaluate ordering. For cost=costLS, this is a vector of object weights. Otherwise is a dist or  symmetric matrix.
}
  \item{cost}{
    Function used to evaluate permutation.Current choices are costLS, costPL, costLPL, costED, costARc, costBAR.}
  \item{node_op}{
     Function used to reorder branches at a dendrogram node. DendSer picks default depending on cost function. NULL means use default depending on cost.
}
  \item{costArg}{
     Other args for cost function.}
  \item{maxloops}{
    Maximum number of iterations allowed. NULL means use default depending on cost.
}
  \item{saveinfo}{
    Logical, whether info associated with search is saved.
}
  \item{direction}{
     Order of visiting nodes. Values are "up" or "down", for nodes in order of increasing or decreasing height.NULL means use default depending on cost.}
     \item{GW}{
    Logical, initial GW step or not. NULL means use default depending on cost.}
  \item{\dots}{
     Not used.
}
}
 \details{
 	costED uses the Gruvaeus and Wainer 1972 algorithm, as provided by package gclus.
 }
\value{
	Numeric vector giving an optimal dendrogram order
}
\references{
Gruvaeus, G. \& Wainer, H. (1972), ``Two additions to hierarchical cluster analysis'', British Journal of
Mathematical and Statistical Psychology, 25, 200-206.
	
	
}
\author{
	Catherine Hurley & Denise Earle
}
% \note{
% %%  ~~further notes~~
% }

% %% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{dser}}
 }
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line


\examples{
	 			
require(DendSer) 			

d<- dist(iris[,-5])
h <- hclust(d,method="average")
ob<- DendSer(h,d)
opl<- DendSer(h,d,cost=costPL)
plotAsColor(d,ob)

w <- rowSums(iris[,-5])
ow <- DendSer(h,w,cost=costLS) # arranges cases by size, within hclust
stars(iris[ow,-5],labels=NULL, col.stars=cutree(h,3)[ow]) # and color by cluster
#stars(iris[ow,-5],labels=NULL, col.stars=iris[ow,5]) # or by species


}

