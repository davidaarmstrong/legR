#' Proportional and Expected Proportional Reductions in Error
#'
#' Calculates proportional reduction in error (PRE) and expected proportional
#' reduction in error (epre) from Herron (1999).
#'
#' Proportional reduction in error is calculated as a function of correct and
#' incorrect predictions (and the probabilities of correct and incorrect
#' predictions for ePRE).
#' @param mod1 A model of class \code{glm} (with family \code{binomial}),
#' \code{polr} or \code{multinom} for which (e)PRE will be calculated.
#' @param data Data frame to be passed down to the function. 
#' @return An object of class \code{pre}, which is a list with the following
#' elements: \item{pre}{The proportional reduction in error} \item{epre}{The
#' expected proportional reduction in error} \item{m1form}{The formula for
#' model 1} \item{m2form}{The formula for model 2} \item{pcp}{The percent
#' correctly predicted by model 1} \item{pmc}{The percent correctly predicted
#' by model 2} \item{epcp}{The expected percent correctly predicted by model 1}
#' \item{epmc}{The expected percent correctly predicted by model 2}
#' @author Dave Armstrong
#' @references Herron, M.  1999.  Postestimation Uncertainty in Limited
#' Dependent Variable Models.  Political Analysis 8(1): 83--98.
#'
#' @importFrom stats model.frame predict update
#' @export
#'
#'
getPRE <- function (mod1, data){
  if(!inherits(mod1, "try-error")){
    y <- mod1[["y"]]
    mod2 <- glm(vote ~ 1, data=data, family=binomial)
    pred.mod2 <- as.numeric(predict(mod2, type = "response") >=
                              0.5)
    pmc <- mean(mod2$y == pred.mod2)
    pred.y <- as.numeric(predict(mod1, type = "response") >=
                           0.5)
    pcp <- mean(pred.y == mod1$y)
    pre <- (pcp - pmc)/(1 - pmc)
    pred.prob1 <- predict(mod1, type = "response")
    pred.prob2 <- predict(mod2, type = "response")
    epcp <- (1/length(pred.prob1)) * (sum(pred.prob1[which(mod1$y ==
                                                             1)]) + sum(1 - pred.prob1[which(mod1$y == 0)]))
    epmc <- (1/length(pred.prob2)) * (sum(pred.prob2[which(mod2$y ==
                                                             1)]) + sum(1 - pred.prob2[which(mod2$y == 0)]))
    epre <- (epcp - epmc)/(1 - epmc)
    ret <- list(pre=pre, epre=epre)
  }else{
    ret <- list(pre = NA, epre=NA)
  }
  return(ret)
}
