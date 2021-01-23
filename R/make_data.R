#' Make Data for MI-DIRT
#'
#' Makes data to pass to the dynamic IRT function.
#'
#' @param X Matrix of votes.
#' @param terms Vector of term numbers corresponding to each of the columns in \code{X}.
#' @param legis_data A data frame minimally giving an observation identifier for the legislators.
#' @param ... Other arguments to be passed down - currently not implemented.
#'
#' @importFrom pscl rollcall
#' @importFrom emIRT convertRC
#'
#' @return A list that can be passed to the \code{dynIRT()} function from \code{emIRT}.
#' @export
make_data <- function(X, terms, legis_data=NULL,...){
  rc <- rollcall(as.data.frame(X), yea=1, nay=0, missing=NA)
  cX <- convertRC(rc)
  l1 <- t(apply(X, 1, function(x)range(terms[which(!is.na(x))])))
  inds1 <- which(is.finite(l1[,1]))
  ld1 <- legis_data[which(is.finite(l1[,1])), , drop=FALSE]
  v1 <- cX$votes[which(is.finite(l1[,1])), ]
  l1 <- l1[which(is.finite(l1[,1])), ]

  dat <- list(
    rc = apply(v1, 2, as.numeric),
    startlegis = matrix(l1[,1]-1, ncol=1),
    endlegis=matrix(l1[,2]-1, ncol=1),
    bill.session = matrix(terms-1, ncol=1),
    T = max(terms)
  )
  return(list(dat = dat, id= ld1))
}
