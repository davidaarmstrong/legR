#' PCA Imputation
#'
#' Use Principal Components to Impute Voting Data.
#'
#' Uses the method discussed in Potthoff (2018) to impute missing data in a
#' binary voting data matrix.
#'
#' @param Y A binary data matrix with missing data.
#'
#' @return A matrix with missing data imputed.
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
impute_pca <- function(Y){
  Yimp <- apply(Y, 2, function(x)ifelse(is.na(x), mean(x, na.rm=TRUE), x))
  sy <- svd(Yimp)
  tmpX <- sy$u%*% diag(sy$d)
  ab <- matrix(NA, nrow=2, ncol=ncol(Yimp))
  cat("Imputing Vote Matrix\n")
  pb = txtProgressBar(min = 0, max = ncol(Yimp), initial = 0)
  for(i in 1:ncol(Yimp)){
    ab[,i] <- coef(suppressWarnings(glm(Yimp[,i] ~ tmpX[,1], family=binomial)))
    setTxtProgressBar(pb,i)
  }
  wg <- which(ab[2,] >0)
  wl <- which(ab[2,] < 0)
  if(length(wg) > 0){
    cond1 <- apply(Y[,wg], 1, function(y)sd(na.omit(y)) == 0)
  }else{
    cond1 <- rep(TRUE, nrow(Y))
  }
  if(length(wl) > 0){
    cond2 <- apply(Y[,wl], 1, function(y)sd(na.omit(y)) == 0)
  }else{
    cond2 <- rep(TRUE, nrow(Y))
  }
  sgn <- ifelse(Y[,wg[1]] == 1, 1, -1)
  newX <- ifelse(cond1 & cond2, 3*sgn, NA)
  wna <- which(is.na(newX))
  newX[wna] <- sapply(wna, function(i)coef(glm(Y[i,] ~ ab[2,]-1, offset=ab[1,], family=binomial))[1])
  xb <- plogis(cbind(1, newX) %*% ab)
  Y[which(is.na(Y), arr.ind=TRUE)] <- xb[which(is.na(Y), arr.ind=TRUE)]
  Y
}
