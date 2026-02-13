## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(DendSer)

## -----------------------------------------------------------------------------
set.seed(123)
iris1 <- iris[sample(nrow(iris),10), -5]
d <- dist(scale(iris1))
h <- hclust(d, method="average")
DendSer(h,d, cost=costPL)
DendSer(h,d, cost=costLPL)
DendSer(h,d, cost=costARc)
DendSer(h,d, cost=costBAR)
DendSer(h,1:10, cost=costLS) # a dendrogram ordering "most" consistent with 1...10
DendSer(h,d, cost=costED) # same as gclus::reorder.hclust

## -----------------------------------------------------------------------------
dser(iris1,d, cost=costPL, scale=TRUE)
dser(iris1,d, cost=costLPL, scale=TRUE)
dser(iris1,d, cost=costARc, scale=TRUE)
dser(iris1,d, cost=costBAR, scale=TRUE)
dser(iris1,1:10, cost=costLS, scale=TRUE) # a dendrogram ordering "most" consistent with 1...10
dser(iris1,d, cost=costED, scale=TRUE) # same as gclus::reorder.hclust

## ----fig.width=6--------------------------------------------------------------
iriss <- scale(iris[sample(nrow(iris),150),-5])
cols <- hcl.colors(12,"PuBuGn" )
par(mar=c(1,1,1,1))
par(mfrow=c(1,2))
plotAsColor(iriss, col=cols)
newOrd <- dser(iriss)
plotAsColor(iriss, col=cols,order.row=newOrd)

## ----fig.width=8--------------------------------------------------------------
w <- prcomp(iris[,-5],scale=TRUE)$x[,1]
h<- hclust(dist(scale(iris[,-5]))) # complete linkage by default
oh <- h$order

# the display
par(mar=c(0,2,1,1))
layout(matrix(1:2, nrow = 2), heights = c(4,1.5) )
par(cex=.7)
plot(h,main="",xlab="",hang=-1,labels=FALSE)
u <- par("usr")
par(mar=c(1,2,0,1))
plot.new()
par(usr=c(u[1:2],min(w),max(w)))
x<- 1:length(w)
rect(x-.5,0,x+.5,w[oh],col=cutree(h,3)[oh]+1)

## ----fig.width=8--------------------------------------------------------------
 h$order <- ow <- DendSer(h,w,cost=costLS) # arranges cases along first PC, within dendrogram

# the display
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


