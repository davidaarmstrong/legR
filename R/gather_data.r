#' Collect Data from Estimated Models
#'
#' Collects data from model and transforms it from wide- to long-form.  Also estimates bootstrapped posterior
#' standard deviations for the latent point estimates.
#'
#' @param x An object of class \code{legR}.
#' @param ... Other arguments to be passed down
#' @param boot_sd Logical indicating whether bootstrapped posterior standard deviations should be produced.
#' @param Nboot If \code{boot_sd = TRUE}, then how many bootstrap samples should be used to calculate the
#' posterior standard deviation.
#' @param dynIRT_control A list of control parameters for the dynamic IRT models.
#'
#' @return A data frame with latent variable estimates.
#' @export
#' @importFrom MASS mvrnorm
#' @importFrom emIRT boot_emIRT
#' @importFrom dplyr left_join select all_of bind_cols full_join
#' @importFrom glue glue_collapse
#' @importFrom stats lm pnorm vcov
#' @importFrom tidyr pivot_longer as_tibble
gather_data <- function(x, ...,
                        boot_sd=FALSE,
                        Nboot=100,
                        dynIRT_control = list(threads = 1,verbose = TRUE, thresh = 1e-6, maxit=500)){
  out_dats <- list()
  for(i in 1:length(x$mods)){
    x1 <- x$mods[[i]]$means$x
    colnames(x1) <- 1:ncol(x1)
    rownames(x1) <- x$dats[[i]]$id$name
    x1 <- as_tibble(x1, rownames="name")
    x1 <- x1 %>% pivot_longer(-c("name"), names_to="session", values_to=paste0("Dim",i)) %>% 
      mutate(session = as.numeric(.data$session))
    out_dats[[i]] <- x1
  }
  out_dat <- out_dats[[1]]
  if(length(x$mods) > 1){
   for(i in 2:length(x$mods)){
     out_dat <- full_join(out_dat, out_dats[[i]])
   } 
  }
  if(boot_sd){
    boot_res <- list()
    for(i in 1:length(x$mods)){
      cat("\n\nCalculating Bootstrap SD for Dimension", i, "\n")
      tmp <- boot_emIRT(x$mods[[i]],
                        .data=x$dats[[i]]$dat,
                        .starts = x$starts[[i]],
                        .priors = x$priors[[i]],
                        .control = dynIRT_control,
                        Ntrials=Nboot)
      x1 <- tmp$bse$x
      colnames(x1) <- 1:ncol(x1)
      rownames(x1) <- x$dats[[i]]$id$name
      x1 <- as_tibble(x1, rownames="name")
      x1 <- x1 %>% pivot_longer(-c("name"), names_to="session", values_to=paste0("Dim",i, "_sd")) %>% 
        mutate(session = as.numeric(.data$session))
      boot_res[[i]] <- x1      
    }
    out_boot <- boot_res[[1]]
    if(length(x$mods) > 1){
      for(i in 2:length(x$mods)){
        out_boot <- full_join(out_boot, boot_res[[i]])
      } 
    }
    out_dat <- left_join(out_dat, out_boot)
  }
  return(out_dat)
}
