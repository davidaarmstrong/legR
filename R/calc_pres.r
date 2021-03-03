#' Calculate PRE
#'
#' Calculates PRE for initialized latent variables.
#'
#' Calculates the PRE for each indicator.  This is used to identify
#' which indicator is best predicted by which latent variable.
#'
#' @param votes A matrix of votes
#' @param initlv Initial estimate of latent variables
#' @param ... Other arguments to be passed down
#'
#' @return A list with a matrix of PRE values and matrix of coefficients
#' from the various GLMs estimated to calculate PRE.
#' @importFrom stats as.formula
#' @importFrom glue glue
#' @importFrom dplyr mutate
#' @export
calc_pres <- function(votes, initlv, ...){
  if(!is.data.frame(initlv)){
    initlv <- as.data.frame(initlv)
  }
  if(!is.data.frame(votes)){
    votes <- as.data.frame(votes)
  }
  forms <- glue("vote ~ {names(initlv)}")
  tmp <- initlv %>%
    mutate(vote = NA)


  pres <- epres <- NULL
  cat("Calculating PREs\n")
  pb_pre = txtProgressBar(min = 0, max = ncol(votes), initial = 0)
  b <- p <- NULL
  for(i in 1:ncol(votes)){
    tmp$vote <- votes[[i]]
    mods <- lapply(forms, function(f){
      try(glm(as.formula(f), data=tmp, family=binomial))
    })
    pres <- rbind(pres, sapply(mods, function(x)getPRE(x)$pre))
    b <- rbind(b, sapply(mods, function(x)getCoef(x)))
    p <- rbind(p, sapply(mods, function(x)getP(x)))
    setTxtProgressBar(pb_pre,i)
  }
  pres <- as.data.frame(pres)
  names(pres) <- glue("Dim{1:ncol(pres)}")
  out <- cbind(data.frame(proposal = colnames(votes)),
               pres)
  list(pres=out, b = b, p=p)
}
