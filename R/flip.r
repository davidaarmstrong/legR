#' Reflect Latent Variable Estimates
#'
#' Identifies reflections that minimize the distance of  adjacent 
#' time-points on the same latent variable
#' 
#' @param data A data frame or tibble produced with \code{gather_data}.
#' @param id Character string giving the name(s) of the ID variable(s) 
#' in the data. 
#' @param vars Variables to be evaluated for reflection.  Generally, 
#' these would be all of the \code{DimX} variables. 
#' @param time Character string giving the time variable in the 
#' data set. 
#' @param ... Other arguments to be passed down, currently not implemented. 
#'
#' @return A data frame with time and a flip indicator for each 
#' evaluated dimension.  This can be merged with the original data and
#' can be used to make the appropriate reflections of the latent variables. 
#'
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr slice_max 
#' @importFrom stats cor model.matrix model.response
#' @export
#' 
flip <- function(data, id = "name", vars, time, ...){
  flip_inds <- list()
  for(j in 1:length(vars)){
    dl <- data %>% 
      select(all_of(c(id, time, vars[j]))) %>% 
      pivot_wider(names_from=all_of(time), 
                  values_from=all_of(vars[j]))
    flip <- 1
    for(i in 3:ncol(dl)){
      d1 <- dl %>% 
        select(c(i-1, i)) %>% 
        as.matrix() 
      d1 <- apply(d1, 1, function(x)(x[1]-x[2])^2) 
      d1 <- sum(d1, na.rm=TRUE)
      d2 <- dl %>% 
        select(c(i-1, i)) %>% 
        as.matrix() 
      d2 <- apply(d2, 1, function(x)(x[1]+x[2])^2) 
      d2 <- sum(d2, na.rm=TRUE)
      dl[,i][[1]] <- dl[,i][[1]] * ifelse(d2 < d1, -1, 1)
      flip <- c(flip, ifelse(d2 < d1, -1, 1))
    }
    flip_inds[[j]] <- flip
  }
  names(flip_inds) <- paste("flip", 1:length(flip_inds), sep="")
  flip_dat <- do.call(data.frame, flip_inds)
  flip_dat[[time]] <- setdiff(colnames(dl), id)
  return(flip_dat)
}
