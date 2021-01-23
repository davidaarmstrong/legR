#' Make Priors for MI-DIRT
#'
#' Makes priors to pass to the dynamic IRT function.
#'
#' @param X Matrix of votes.
#' @param agree_n_thresh Scalar indicating how many common votes legislators must have to be anchors.
#' @param ... Other arguments to be passed down - currently not implemented.
#'
#' @importFrom dplyr filter arrange case_when
#' @importFrom stats runif
#' @importFrom rlang .data
#' @return A list that can be passed to the \code{dynIRT()} function from \code{emIRT}.
#' @export
make_priors <- function(X, agree_n_thresh = 50, ...){
  xm0 <- rep(0, nrow(X))
  a <- agree_score(X)
  a <- a %>% filter(.data$n > agree_n_thresh) %>% arrange(a)
  xm0[a$row[1]] <- 1
  xm0[a$col[1]] <- -1
  xs0 <- ifelse(xm0 == 0, 1, .01)
  priors <- list(
    beta.mu = matrix(c(0,0), ncol=1),
    beta.sigma = diag(2),
    x.mu0 = matrix(xm0, ncol=1),
    x.sigma0 = matrix(xs0, ncol=1),
    omega2 = matrix(rep(.1, nrow(X)), ncol=1))
  return(priors)
}
