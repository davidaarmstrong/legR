#' Remove Lopsided Votes
#'
#' Removes lopsided votes from the voting matrix
#'
#' @param X A vote matrix
#' @param terms A vector of values identifying the term in which the
#' vote was taken.
#' @param minprop The proportion voting in the minority that serves as the
#' cutoff.  Votes with fewer than n times \code{minprop} votes in the minority
#' will be removed from the vote matrix.
#' @param ... other arguments to be passed down - currently not implemented.
#'
#' @return A vote matrix
#' @export
remove_lop <- function(X, terms, minprop=.2, ...){
  props <- apply(as.matrix(X), 2, mean, na.rm=TRUE)
  wout <- which(props < minprop | (1-props) < minprop)
  if(length(wout) > 0){
    terms <- terms[-wout]
    X <- X[,-wout]
  }
  return(list(X=X, terms=terms))
}
