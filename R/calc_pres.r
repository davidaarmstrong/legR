#' Calculate PRE
#'
#' Calculates PRE for initialized latent variables.
#'
#' Calculates the PRE for each indicator.  This is used to identify
#' which indicator is best predicted by which latent variable.
#'
#' @param votes A matrix of votes
#' @param initlv Initial estimate of latent variables
#' @param glm_method Use either binomial glm or firth logistic regression to calculate coefficients and PRE. 
#' @param ... Other arguments to be passed down
#'
#' @return A list with a matrix of PRE values and matrix of coefficients
#' from the various GLMs estimated to calculate PRE.
#' @importFrom stats as.formula
#' @importFrom glue glue
#' @importFrom dplyr mutate
#' @importFrom logistf logistf
#' @export
calc_pres <- function(votes, initlv, glm_method=c("glm", "firth"), ...){
  glm_method=match.arg(glm_method)
  if(!is.data.frame(initlv)){
    initlv <- as.data.frame(initlv)
  }
  if(!is.data.frame(votes)){
    votes <- as.data.frame(votes)
  }
  forms <- glue("vote ~ scale({names(initlv)})")
  tmp <- initlv %>%
    mutate(vote = NA)
  glmfun <- switch(glm_method, 
                   glm = glm, 
                   firth = firth_fun)

  pres <- epres <- NULL
  cat("Calculating PREs\n")
  pb_pre = txtProgressBar(min = 0, max = ncol(votes), initial = 0)
  b <- p <- NULL
  for(i in 1:ncol(votes)){
    tmp$vote <- votes[[i]]
    sink(tempfile())
    mods <- lapply(forms, function(f){
      try(suppressWarnings(glmfun(as.formula(f), data=tmp, family=binomial)))
    })
    sink()
    pres <- rbind(pres, sapply(mods, function(x)getPRE(x, data=tmp)$pre))
    b <- rbind(b, sapply(mods, function(x)getCoef(x)))
    setTxtProgressBar(pb_pre,i)
  }
  pres <- as.data.frame(pres)
  names(pres) <- glue("Dim{1:ncol(pres)}")
  out <- cbind(data.frame(proposal = colnames(votes)),
               pres)
  list(pres=out, b = b)
}
