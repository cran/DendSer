#' Implements dendrogram seriation. Interface to DendSer.
#' 
#' Implements dendrogram seriation. Interface to DendSer.
#' 
#' When x is a matrix or data.drame, forms a dist of rows using function dist
#' with method = dmethod.  When x is a dist, forms a hclust with method =
#' hmethod which is then reordered.
#' 
#' @aliases dser dser.hclust dser.data.frame dser.matrix dser.dist
#' @param x Used to select method.
#' @param ser_weight Seriation weights. For cost=costLS, defaults to first
#' column of matrix x, otherwise to symmetric matrix version of dist d.
#' @param cost Current choices are costLS, costPL, costLPL, costED, costARc,
#' costBAR.
#' @param scale Logical value,controls whether matrix x should be scaled prior
#' to forming dist.
#' @param dmethod Method of dist calculation. See function \code{dist}.
#' @param hmethod Method of hclust calculation. See function \code{hclust}.
#' @param \dots Other args
#' @return Numeric vector giving an optimal dendrogram order
#' @author Catherine Hurley & Denise Earle
#' @examples
#' 
#' 	 			
#' 
#' iriss <- scale(iris[,-5])
#' plotAsColor(iriss,order.row=dser(iriss))
#' 
#' 
#' 
#' w <- prcomp(iris[,-5],scale=TRUE)$x[,1]
#' h<- hclust(dist(iriss))
#' h$order <- ow <- dser(h,w,cost=costLS) # arranges cases along first PC, within dendrogram
#' 
#' 
#' # compare re-rordered dendrogram to PC scores, w
#' dev.new(width=10,height=5)
#' par(mar=c(0,2,1,1))
#' layout(matrix(1:2, nrow = 2), heights = c(4,1.5) )
#' par(cex=.7)
#' plot(h,main="",xlab="",hang=-1,labels=FALSE)
#' u <- par("usr")
#' par(mar=c(1,2,0,1))
#'  
#' 
#' plot.new()
#' par(usr=c(u[1:2],min(w),max(w)))
#' 
#' x<- 1:length(w)
#' rect(x-.5,0,x+.5,w[ow],col=cutree(h,3)[ow]+1)
#' 
#' 
#' @export dser
dser <- function(x,ser_weight,cost=costBAR, ...)
         UseMethod("dser")
         
#' @describeIn dser dser method
#' @export 
dser.data.frame <- function(x,ser_weight,cost=costBAR,...) {
	dser.matrix(as.matrix(x),ser_weight,cost=cost,...)
	}
	
#' @describeIn dser dser method
#' @export 
dser.matrix <-function(x,ser_weight,cost=costBAR,scale=TRUE,dmethod="euclidean",...) {
	if (!is.matrix(x) || !is.numeric(x))
	stop("'x' must be 2d numeric matrix")
	if (scale) x <- scale(x)
	 d <- dist(x,method=dmethod)
	if (missing(ser_weight))
	 if (isTRUE(all.equal(cost, costLS)))
	    ser_weight <- x[,1]
    	 else ser_weight <- as.matrix(d)

     dser.dist(d,ser_weight,cost=cost,...)
	}

#' @describeIn dser dser method
#' @export 
dser.dist <- function(x,ser_weight,cost=costBAR,hmethod="average",...) {
    h <- hclust(x,method=hmethod)
    if (missing(ser_weight) && !isTRUE(all.equal(cost, costLS)))
    	ser_weight <- as.matrix(x)
    DendSer(h,ser_weight,cost=cost,...)
	}

#' @describeIn dser dser method
#' @export 
dser.hclust <- function(x,ser_weight,cost=costBAR,...) {
     DendSer(x,ser_weight,cost=cost,...)
	}

