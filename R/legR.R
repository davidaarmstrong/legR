#' Multi-dimensional Independent Dynamic Item Response Theory
#'
#' Estimates the multi-dimensional dynamic IRT model.
#'
#' @param X An \eqn{n\times p}{nxp} matrix of votes with values 1, 0 or \code{NA}.
#' @param terms A length \eqn{p}{p} vector of values identifying the term in which the
#' vote was taken. The first term should be coded as 1 and increase by consecutive integers.
#' @param est_model Logical indicating whether model should be estimated.
#' If \code{FALSE}, the initial latent variable is estimated, the pres
#' calculated and the best dimension identified.  A table is printed of the
#' best dimension by term.  In this case, just the PREs, best dimension
#' and the data with lop-sided votes removed are returned.
#' @param priors User specified priors to be passed to the \code{dynIRT} function.  Must be a
#' list of priors of the same length as the dimensionality being estimated.
#' @param legis_data A data frame giving information about the legislators whose
#' votes are in \code{X}.  The only requirement is that the variable identifying the observations
#' be called \code{"name"}.
#' @param k Number of dimensions for the GLRM to estimate. 
#' @param ndim Number of dimensions of the dynamic IRT model to be estimated. 
#' @param dynIRT_control A list containing control parameters for the dynamic
#' IRT model.  See the \code{\link{dynIRT}} documentation from the \code{emIRT} package.
#' @param ... Other arguments to be passed down to \code{init_lv}, \code{calc_pres},
#' \code{remove_lop}.
#'
#' @return Depending on \code{est_model} either a list with the IRT inputs
#' for evaluation purposes or a list of models and a dataset of latent variable
#' estimates.
#'
#' @importFrom emIRT dynIRT
#' @export
#'
legR <- function(X, terms, est_model=FALSE,
                 legis_data=NULL, priors = NULL,
                 k = 5, ndim = 2, 
                 dynIRT_control = list(threads = 1,verbose = TRUE, thresh = 1e-6, maxit=500), ...){
  if(!all(c(as.matrix(X)) %in% c(0,1,NA)))stop("Voting matrix can only conatin 0, 1 or NA\n")
  if(is.null(legis_data)){
    if(is.null(rownames(X))){
      legis_data <- data.frame(name = paste0("obs_", 1:nrow(X)))
    }else{
      legis_data <- data.frame(name = rownames(X))
    }
  }
    ilv <- init_lv(X,
                   k=k,
                   terms=terms,
                   h2o.glrm.args = h2o.glrm.ctrl(...),
                   h2o.init.args = h2o.init.ctrl(...),
                   ...)
    X <- ilv$votes
    terms <- ilv$terms
    trm_ord <- order(terms)
    terms <- terms[trm_ord]
    X <- X[,trm_ord]
    pres <- calc_pres(X, ilv$lv, ...)
    best <- choose_best(pres$pres[,-1], ndim=ndim, ...)
  Xs <- lapply(1:max(best, na.rm=TRUE),
               function(i)list(X[, which(best == i)], terms[which(best == i)]))

  dats <- lapply(Xs, function(x)make_data(x[[1]], x[[2]], legis_data))
  if(is.null(priors)){
    priors <- lapply(dats, function(x)make_priors(x$dat$rc))
  }
  b <- lapply(1:max(best, na.rm=TRUE), function(i){
    pres$b[cbind(which(best == i), i)]})
  starts <- lapply(1:length(dats), function(i)make_starts(dats[[i]]$dat, priors[[i]], b[[i]]))
  if(!est_model){
    ret <- list(dats=dats, priors=priors, starts=starts, ilv=ilv, pres=pres, best=best, legis_data=legis_data)
    return(ret)
  }
mods <- lapply(1:length(dats), function(i){
   try(dynIRT(.data = dats[[i]]$dat,
                  .starts = starts[[i]],
                  .priors = priors[[i]],
                  .control = dynIRT_control))

  })
  ret <- list(mods=mods, dats=dats, priors=priors, starts=starts, ilv=ilv, pres=pres, best=best, legis_data=legis_data)
  class(ret) <- "legR"
  return(ret)
}
