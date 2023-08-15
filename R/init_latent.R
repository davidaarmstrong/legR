#' Initialize Latent Variable
#'
#' Initialize latent variable with GLRM or PCA.
#'
#' This function initializes a static latent variable for each observation
#' on a given number of dimensions.  This can use a principal components
#' analysis (PCA) model or a generalized low-rank model (GLRM).  The latter
#' requires you to have the \code{h2o} package installed.
#'
#' @param X A matrix of observations that are 0, 1 or \code{NA}.
#' @param minprop The proportion voting in the minority that serves as the
#' cutoff.  Votes with fewer than n times \code{minprop} votes in the minority
#' will be removed from the vote matrix.
#' @param terms A vector of values identifying the term in which the
#' vote was taken.
#' @param method A character - either "pca" or "glrm" identifying the
#' default method for initializing the latent variable.
#' @param k Scalar giving the number of dimensions to be estimated.  Note,
#' this does not necessarily have to be the same as the number of dimensions
#' estimated in the final model.
#' @param nRounds Number of rounds of GLRM.  If this is 1, then all bills are 
#' fed into the GLRM.  If ths is greater than 1, then \code{nRounds}/\code{ncol(X)} 
#' bills are fed into the GLRM each time and the results of the initialized latent 
#' variable are averaged over the different runs.  The right value will depend 
#' on the computational resources at hand, but we suggest the smalles value such that
#' \code{ncol(X)}/\code{nRounds} < 5000 as a good place to start.  If set to \code{NULL}, 
#' the algorithm uses \code{ceiling(ncol(X)/5000)}. 
#' @param nRand Number of random samples from the input matrix to use 
#' in producing the output. 
#' @param nMax Maximum number of bills to take per term if \code{nRand} is bigger than 1. 
#' @param h2o.init.args A list of arguments to be passed to \code{h2o.init}.
#' @param h2o.glrm.args A list or arguments to be passed to \code{h2o.glrm}.
#' @param seed Random number generator seed. 
#' @param ... Other arguments to be passed down - currently unimplemented.
#'
#' @return A list with the reduced set of votes, their corresponding terms
#' and an n x k matrix of latent variable estimates.
#' @importFrom stats princomp binomial coef glm na.omit plogis sd cov
#' @importFrom dplyr group_by sample_n n
#' @importFrom utils installed.packages
#' @importFrom magrittr `%>%`
#' @importFrom RSpectra eigs_sym
#'
#'
#' @export
init_lv <- function(X,
                    minprop=.2,
                    terms,
                    method=c("pca", "glrm"),
                    k=5,
                    nRounds = 1,
                    nMax = NULL,
                    nRand = 1, 
                    h2o.init.args,
                    h2o.glrm.args,
                    seed=NULL,
                    ...){
  meth <- match.arg(method)
  if(length(terms) != ncol(X)){
    stop("The length of terms has to be the same as the number of columns in X\n")
  }
  rlout <- remove_lop(X, terms, minprop)
  X <- rlout$X
  terms <- rlout$terms
  if(is.null(nRounds)){
    nRounds <- ceiling(ncol(X)/5000)
  }
  naX <- max(apply(X, 2, function(x)sum(is.na(x))))
  if(any(naX > 0) & meth == "pca"){
    X <- impute_pca(X)
  }
  if(meth == "glrm" & !("h2o" %in% installed.packages()[,"Package"])){
    stop("To use method='glrm', you must first install the h2o package\n")
  }
  if(meth == "pca"){
    u <- matrix(1, ncol=1, nrow=nrow(X))
    i <- length(u)
    Xt <- X - u %*% t(u) %*% X*(1/i)
    # S <- t(Xt)%*%Xt*(1/(i-1))
    ## or
    S <- cov(Xt)
    eS <- eigs_sym(S, k=k, which="LM")
    L <- eS$values
    g <- eS$vectors

    ## The L1, L2, ... values in the article are just the eigen values

    x <- Xt %*%g %*% diag(1/sqrt(3*L))
  }else{
    do.call(h2o::h2o.init, h2o.init.args)  # connect to H2O instance
    if(nRounds > 1 & nRand > 1)stop("Only one of nRounds or nRand can be bigger than 1\n")
    if(nRounds > 1){
      set.seed(seed)
      cols <- sample(1:nRounds, ncol(X), replace=TRUE)
      splitX <- by(1:ncol(X), list(cols), function(i)X[,i])      
    }
    if(nRand > 1){
      set.seed(seed)
      splitX <- list()
      for(j in 1:nRand){
        tmp <- data.frame(col=1:ncol(X), terms=terms)
        tmp %>% group_by(terms) %>%
          sample_n(min(nMax, n()))
        splitX[[j]] <- X[,tmp$col]
      }
    }      
    if(nRand == 1 & nRounds == 1){
      splitX <- list()
      splitX[[1]] <- X
    }
    arr <- array(dim=c(nrow(X), length(splitX), k))
    for(i in 1:length(splitX)){
      cat("\nEstimating GLRM Round ", i, " of ", length(splitX), "\n", sep="")
      Xl <- lapply(as.data.frame(splitX[[i]]), as.factor)
      Xn <- do.call(data.frame, Xl)
      hX <- h2o::as.h2o(Xn)
      h2o.glrm.args$training_frame = hX
      h2o.glrm.args$k <- k
      h2o.glrm.args$seed <- seed
      out <- do.call(h2o::h2o.glrm, h2o.glrm.args)
      gframe <- h2o::h2o.getFrame(out@model$representation_name)
      gp <- princomp(gframe)
      for(j in 1:k){
        arr[,i,j] <- gp$scores[,j]
      }
    }
    pp <- NULL
    for(i in 1:k){
      pp <- cbind(pp, princomp(arr[,,i])$scores[,1])
    }
    x <- princomp(pp)$scores
  }
  return(list(votes = X, terms=terms, lv= x))
}






