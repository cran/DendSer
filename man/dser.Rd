\name{dser}
\alias{dser}
\alias{dser.hclust}
\alias{dser.data.frame}
\alias{dser.matrix}
\alias{dser.dist}
\title{
Implements dendrogram seriation. Interface to DendSer.
}
 \description{
 Implements dendrogram seriation. Interface to DendSer. }
\usage{
dser(x,ser_weight,cost=costBAR, ...)

\method{dser}{data.frame}(x,ser_weight,cost=costBAR,...)

\method{dser}{matrix}(x,ser_weight,cost=costBAR,scale=TRUE,dmethod="euclidean",...)

\method{dser}{dist}(x,ser_weight,cost=costBAR,hmethod="average",...)

\method{dser}{hclust}(x,ser_weight,cost=costBAR,...)
}
\arguments{
  \item{x}{Used to select method.}
   \item{ser_weight}{Seriation weights. For cost=costLS, defaults to first column of matrix x, otherwise to symmetric matrix version of dist d.}
   \item{cost}{Current choices are costLS, costPL, costLPL, costED, costARc, costBAR.}
    \item{scale}{Logical value,controls whether matrix x should be scaled prior to forming dist.}
  \item{dmethod}{Method of dist calculation. See function \code{dist}.}
  \item{hmethod}{Method of hclust calculation. See function \code{hclust}.}

  \item{\dots}{Other args}

}

\details{
	When x is a matrix or data.drame, forms a dist of rows using function dist with method = dmethod. 
When x is a dist, forms a hclust with method = hmethod which is then reordered.  }
 
\value{
	Numeric vector giving an optimal dendrogram order
}
% \references{}
\author{
	Catherine Hurley & Denise Earle
}
% \note{
% %%  ~~further notes~~
% }

% %% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\examples{
	 			
require(DendSer) 			

iriss <- scale(iris[,-5])
plotAsColor(iriss,order.row=dser(iriss))



w <- prcomp(iris[,-5],scale=TRUE)$x[,1]
h<- hclust(dist(iriss))
h$order <- ow <- dser(h,w,cost=costLS) # arranges cases along first PC, within dendrogram


# compare re-rordered dendrogram to PC scores, w
dev.new(width=10,height=5)
par(mar=c(0,2,1,1))
layout(matrix(1:2, nrow = 2), heights = c(4,1.5) )
par(cex=.7)
plot(h,main="",xlab="",hang=-1,labels=FALSE)
u <- par("usr")
par(mar=c(1,2,0,1))
 

plot.new()
par(usr=c(u[1:2],min(w),max(w)))

x<- 1:length(w)
rect(x-.5,0,x+.5,w[ow],col=cutree(h,3)[ow]+1)

}

