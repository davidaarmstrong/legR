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
  if(!is.null(ndim)){
    if(ndim > ncol(x)){
      ndim <- NULL
    }
  }
  if(!is.null(ndim)){
    x <- x[,1:ndim]
  }
  if(is.null(nperterm)){
    if(is.null(othermax)){
      best <- apply(x, 1, which.max)
    }else{
      best <- rep(NA, nrow(x))
      for(i in 1:ncol(x)){
        tmp <- apply(x, 1, function(x)ifelse(x[i] > bestmin & all(x[-i] <= othermax), i, NA))
        best <- ifelse(is.na(best), tmp, best)
      }
    }
  }else{
    diffs <- sapply(1:ncol(x), function(i)x[,i] - apply(x[,-i, drop=FALSE], 1, max, na.rm=TRUE))
    best <- matrix(0, ncol=ncol(x), nrow=nrow(x))
    for(j in 1:ncol(x)){
      dd <- data.frame(col = 1:nrow(x), d= diffs[,j], terms=terms) %>% filter(d > 0)
      nb <- dd %>% group_by(terms) %>% slice_max(order_by=d, n=nperterm)  
      best[cbind(nb$col, j)] <- j
    }
    best <- rowSums(best)
  }
  return(best)
}

