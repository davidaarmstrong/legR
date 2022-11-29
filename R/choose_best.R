#' Choose Best Dimension
#'
#' Chooses latent dimension that best predicts the outcome.
#'
#' The algorithm chooses the best dimension by identifying for each vote the
#' situation where the pre for variable j is greater than \code{bestmin} and
#' the pre for all other variables not equal to j is less than \code{othermin}.
#'
#' @param x A #Votes x #Dimensions matrix of PRE values. 
#' @param bestmin Scalar indicating the minimum PRE value to be
#' considered "well predicted" bythe model.
#' @param othermax Scalar indicating the maximum PRE of the inferior dimension.
#' If \code{NULL}, the algorithm just chooses the first highest PRE, regardless
#' of the other PRE values.
#' @param ndim Scalar giving the number of dimensions to use.  The algorithm
#' will use the first \code{ndim} columns of \code{x} in the calculation.
#' @param terms Vector indicating the term to which each bill belongs. 
#' @param nperterm Scalar giving the number of bills to use per term. 
#' If \code{NULL}, all dimensions are used.
#' @param ... Other arguments to be passed down - currently not implemented.
#'
#' @return A vector identifying which dimension was best or \code{NA}
#' if no dimension met both criteria.
#' @export
choose_best <- function(x, 
                        bestmin=0, 
                        othermax=0, 
                        ndim=NULL, 
                        terms = NULL, 
                        nperterm = NULL,
                        ...){
  rowmax <- function(x)apply(x, 1, max)
  if(!is.null(ndim)){
    if(ndim > ncol(x)){
      ndim <- NULL
    }
  }
  if(!is.null(ndim)){
    x <- x[,1:ndim]
  }
  if(is.null(othermax)){
    best <- apply(x, 2, \(z)as.numeric(z == rowmax(x)))
  }else{
    best <- matrix(NA, nrow= nrow(x), ncol=ncol(x))
    for(i in 1:ncol(x)){
      tmp <- apply(x, 1, function(x)ifelse(x[i] > bestmin & all(x[-i] <= othermax), 1, NA))
      best[,i] <- ifelse(is.na(best[,i]), tmp, best)
    }
  }
  if(!is.null(nperterm)){
  tabs <- lapply(1:ncol(best), \(x)table(best[,x], terms))
  tabs <- lapply(tabs, \(x)colnames(x)[which(x["1", ] < nperterm)])
  w <- NULL
  if(any(sapply(tabs, length) > 0)){
    for(j in 1:length(tabs)){
      if(length(tabs[[j]]) > 0){
        w <- rbind(w, cbind(j, as.numeric(tabs[[j]])))  
      }
    }
    
  }
  if(nrow(w) > 0){
    crit <- NULL
    for(i in 1:ncol(x)){
      crit <- cbind(crit, x[,i] * (1-rowmax(x[,-i, drop=FALSE])))
    }
    for(i in 1:nrow(w)){
      trm_inds <- which(terms == w[i,2])
      qtl <- nperterm/length(trm_inds)
      qtl <- min(1,qtl)
      qtl <- max(0, qtl)
      c_qtl <- quantile(crit[trm_inds, w[i,1]], 1-qtl)
      best[trm_inds[which(crit[trm_inds, w[i,1]] >= c_qtl)], w[i,1]] <- 1
      }
    }
  }
  return(best)
}


