#' Agreement Scores
#'
#' Calculate agreement scores among voters/legislators.
#'
#' @param X an \eqn{n\times p}{nxp} matrix of votes, which can include \code{NA}.
#'
#' @return A data frame containing agreement scores and number of joint votes
#' for every pair of voters.
#'
#' @importFrom Matrix tcrossprod Matrix
#' @export
agree_score <- function(X){
  X1n <- X
  X1n[which(X1n == -1, arr.ind=TRUE)] <- 1
  Xm <- Matrix(X)
  X1nm <- Matrix(X1n)
  cp_a <- Matrix::tcrossprod(Xm)
  cp_n <- Matrix::tcrossprod(X1nm)
  out <- data.frame(
    row = rep(1:nrow(X), nrow(X)),
    col= rep(1:nrow(X), each=nrow(X)),
    a = c(as.matrix(cp_a)),
    n = c(as.matrix(cp_n))
  )
  out <- out[which(out$row < out$col), ]
  out
}
