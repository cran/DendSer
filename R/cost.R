





defaultcostArg <- function(costfn,sw){
  if (identical(costfn,costBAR)) as.integer(max(2,floor(nrow(sw)/5)))
  else if (identical(costfn,costARc)) nrow(sw)-1
  else if (identical(costfn,costLPL)) as.numeric((nrow(sw)-1):1)
  else if (identical(costfn,costLS)) seq_along(sw)
  else NULL
}



AR_target <- function(n) {
  #generates target matrix for ARc cost function.
  mat <- matrix(0L, nrow=n, ncol=n)
  targ <- n-abs(col(mat)-row(mat))
  targ[targ<0] <- 0L
  diag(targ) <- 0L
  mode(targ) <- "integer"
  targ
}




#' Cost functions for DendSer
#' Each of these functions evaluates the cost of an ordering.
#' @param sw For cost=costLS, this is a vector of object weights. Otherwise is
#' a symmetric matrix.
#' @param o An ordering vector.
#' @param node The node
#' @param se Extra info
#' @param target Parameter used by cost function.
#' @param \dots Other args.
#' @return Result of cost
#' @author Catherine Hurley & Denise Earle
#' @name cost

#' @export costLS
#' @rdname cost
costLS <- function(sw, o, target=seq_along(sw),...){
   -sum(sw[o]*target)
}


#' @export costARc
#' @rdname cost
costARc <- function(sw, o,target=nrow(sw)-1,...){
  if (is.matrix(target))
    .Call(carct,sw,o,as.numeric(target))
  else .Call(cbar,sw,o,as.integer(target))
}



#' @export costED
#' @rdname cost
costED <- function(sw, o,node,se,...){ 
   sw[o[se[2,node]],o[se[3,node]]]
}

#' @export costPL
#' @rdname cost
costPL <- function(sw, o,...){
  .Call(cpl,sw,o)
}

#' @export costLPL
#' @rdname cost
costLPL <- function(sw, o,target=(nrow(sw)-1):1,...){ 	
  .Call(clpl,sw,o,as.numeric(target))
}

#' @export costBAR
#' @rdname cost
costBAR <- function(sw, o,target=max(2,floor(nrow(sw)/5)),...){
  .Call(cbar,sw,o,as.integer(target))
}










