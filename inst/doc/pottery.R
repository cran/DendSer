## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(DendSer)

## ----message=FALSE------------------------------------------------------------

if (!("HSAUR2" %in% rownames(installed.packages()))) install.packages("HSAUR2")


library(HSAUR2)

data(pottery)
sitename <- c("Gloucester","Llanedryn","Caldicot","IsleThorns","AshleyRails")[pottery[,"kiln"]]
pottery$Site <- factor(sitename)
reg <- c("Gloucester","Wales","Wales","NewForest","NewForest")[pottery[,"kiln"]]
pottery$Region <- factor(reg)


## -----------------------------------------------------------------------------
panelMerit <- function(dats,clus,...){
  
  gmean <- aggregate(dats,list(clus),mean)[,-1]
  nv <- ncol(dats)
  dv <- matrix(0,nv,nv)
  
  for (i in 2:nv) {
    for (j in 1:(i-1)){
      cij <- gmean[,c(i,j)]
      dv[i,j] <- sum(dist(cij))
      dv[j,i] <- dv[i,j]
    }
  }
  dv
}

# copied from PairZiz::pcp to avoid dependency
pcp <- 
function (data, order = NULL, panel.colors = NULL, col = 1, lty = 1, 
    horizontal = TRUE, mar = NULL, scale=TRUE,axis.width=0,axis.grid.col="grey70",connect=TRUE,...) 
{
    if (is.null(mar)) 
        if (horizontal == TRUE) 
            mar <- c(2, 2, 2, 2) + 0.1
        else mar <- c(2, 5, 2, 2) + 0.1
    par("mar"=mar)
    if (!is.null(order)) {
    	axis.labels <- colnames(data)[order]
        data <- data[, order]
        if (is.matrix(panel.colors)) 
            panel.colors <- panel.colors[order, order]
            }
    else axis.labels <- colnames(data)
    if (is.matrix(panel.colors)) 
        panel.colors <- panel.colors[col(panel.colors) == row(panel.colors) + 1]
    if (is.vector(panel.colors)) 
        if (ncol(data) - 1 != length(panel.colors)) 
            stop("dimensions do not match")
    if (scale==TRUE) x <- apply(data, 2, function(x) (x - min(x))/(max(x) - min(x)))
    else x <- data
    p <- ncol(x)
     indx <- 1:p
    if ((length(axis.width)==1) && (axis.width == 0))
      bx <- x
    else {
      bx <- x[,rep(1:p,times=rep(2,times=p))]
      if (length(axis.width)==1)
        indx <- as.vector(sapply(indx,function(x)c(x-axis.width/2,x+axis.width/2)))
      else indx <- as.vector(t(cbind(indx-axis.width/2,indx+axis.width/2)))
    	}
     linesr <- range(bx)
     pts <- rep(1,length.out=p)
     if (horizontal == TRUE) {
        matplot(indx, t(bx), xlab = "", ylab = "", axes = FALSE, 
            type = "n", xaxs="i",pch=pts,...)
        axis(1, at = 1:p, labels = axis.labels)
        if (!(is.null(panel.colors))) 
            for (i in 1:(p - 1)) rect(i, 0, i + 1, 1, lty = 0, 
                col = panel.colors[i])
         if (!is.null(axis.grid.col))
        for (i in 1:p) lines(c(i, i), linesr, col = axis.grid.col)
        if (connect)
        matpoints(indx, t(bx), type = "l", col = col, lty = lty, pch=pts,...)
        else if (axis.width == 0)
        matpoints(indx, t(bx),  col = col,  pch=pts,...)
         else for (i in seq(1,length(indx),2))
                  matpoints(indx[i:(i+1)], t(bx[,i:(i+1)]), type = "l", col = col, lty = lty, pch=pts,...)
         
            
            }
    else {
        matplot(t(bx), rev(indx), xlab = "", ylab = "", axes = FALSE, 
            type = "n",yaxs="i",pch=pts, ...)
        axis(2, at = p:1, labels = axis.labels, las = 2)
        if (!(is.null(panel.colors))) 
            for (i in 1:(p - 1)) rect(0, i, 1, i + 1, lty = 0, 
                col = panel.colors[p - i])
        if (!is.null(axis.grid.col))
          for (i in 1:p) lines(linesr, c(i, i), col = axis.grid.col)
        if (connect)
        matpoints(t(bx), rev(indx), type = "l", col = col, lty = lty, pch=pts, ...)
        else if (axis.width == 0)
        matpoints(t(bx), rev(indx),  col = col,  pch=pts,...)
        else for (i in seq(1,length(indx),2))
                matpoints(t(bx[,i:(i+1)]), rev(indx[i:(i+1)]), type = "l", col = col, lty = lty, pch=pts, ...)
            
            
    }
    invisible()
}


## ----fig.width=7--------------------------------------------------------------

cols <- hcl.colors(12,"Blues" )
par(mar=c(1,1,1,1))
par(mfrow=c(1,3))

od <- c(2,9,6,1,3,7,4,5,8) # random order of numeric variables
dat <- scale(pottery[,od])

d <- dist(dat) 

plotAsColor(as.matrix(d), col=cols, main="",rank=TRUE) # the rank option ranks the distances before maping to colour

h <- hclust(d, method="average")
o <- DendSer(h, d, costBAR)
plotAsColor(as.matrix(d), o, col=cols, main="",rank=T) # Figure 9(a)

clus <- cutree(h, 3)
gcols <- c("#89b7e5","#B2F0A2","red3")  

stars(dat[o,], col.stars=gcols[clus[o]],  lwd=.05,labels=NULL, cex=1,radius=F) # Figure 9(b)


## ----fig.width=7--------------------------------------------------------------
pcp(dat,  col=gcols[clus], horiz = TRUE, mar=c(3,2,1,2),
        main="", xaxs = "i") # Figure 10(a)

# Order vars to highlight cluster separation
dats <-   apply(dat, 2, function(x) (x - min(x))/(max(x) - 
            min(x)))
            
merit <- panelMerit(dats,clus)            
ov<- dser(as.dist(max(merit)-merit),cost=costLPL)

pcp(dat[,ov],  col=gcols[clus], horiz = TRUE, mar=c(3,2,1,2),
        main="", xaxs = "i")  # Figure 10(b)

