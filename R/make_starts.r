#' Make Starts for MI-DIRT
#'
#' Makes starting values to pass to the dynamic IRT function.
#'
#' @param data The input data to a \code{dynIRT} model.
#' @param priors The input priors to a \code{dynIRT} model.
#' @param b The coefficients from the \code{calc_pres} function.
#' @param ... Other arguments to be passed down - currently not implemented.
#'
#' @return A list that can be passed to the \code{dynIRT()} function from \code{emIRT}.
#' @export
#'
#'
make_starts <- function(data, priors, b, ...){
  if(any(is.na(b))){
    b <- ifelse(is.na(b), 0, b)
  }
  xb <- c(Matrix(data$rc) %*% b)
  xb <- xb[[1]][,1]
  anchors <- which(priors$x.mu0 != 0)
  if(sign(xb[anchors[1]]) != sign(priors$x.mu0[anchors[1]]))xb <- -xb
  rsxb <- rs(xb)
  start.legis <- matrix(0, nrow=nrow(data$rc), ncol=data$T)
  for(i in 1:nrow(data$rc)){
    sq <- (data$startlegis[i]+1):(data$endlegis[i]+1)
    nq <- length(sq)
    #    start.legis[i, sq] <- runif(nq, -2, 2)
    if(!(i %in% anchors)){
      start.legis[i, sq] <- rsxb[i] + runif(nq, -.25, .25)
    }else{
      start.legis[i, sq] <- priors$x.mu0[i] + c(0, runif(nq-1, -.25, .25))
    }
  }

  starts <- list(
    alpha = matrix(0, nrow=ncol(data$rc), ncol=1),
    beta = matrix(1, nrow=ncol(data$rc), ncol=1),
    x = start.legis
  )
  starts
}
