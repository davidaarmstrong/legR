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
#' @param orthogonalize Character string giving the kind of orthogonalization to be done,
#' including \code{"none"}, \code{"gs"} for Gram-Schmidt (i.e., residualization)
#' or \code{"pca"} indicating a principal components orthogonalization.  Both orthogonalization options
#' are done on the posterior means and that same transform is applied to all iterations to get
#' appropriate uncertainty estimates for the orthogonalized variables.
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
                        dynIRT_control = list(threads = 1,verbose = TRUE, thresh = 1e-6, maxit=500),
                        orthogonalize=c("none", "gs", "pca")){
  ortho <- match.arg(orthogonalize, several.ok = TRUE)
  out_dats <- lapply(1:length(x$mods), function(i){
    d <- x$mods[[i]]$means$x
    colnames(d) <- 1:ncol(d)
    base <- x$dats[[i]]$id %>% 
      select("name") %>% 
      mutate(start = c(x$dats[[1]]$dat$startlegis+1), 
             end = c(x$dats[[1]]$dat$endlegis+1))
    d <- bind_cols(base, as_tibble(d)) %>%
      pivot_longer(-c("name", "start","end"), names_to="session", values_to = paste0("Dim_",i)) %>%
      mutate(session = as.numeric(.data$session)) %>% 
      filter(!(.data$session < .data$start | .data$session > .data$end))
    d    
  })
  out_dat <- out_dats[[1]]
  for(i in 2:length(out_dats)){
    out_dat <- full_join(out_dat, out_dats[[i]])
  }
  if(!boot_sd){
    if("gs" %in% ortho){
      out_orth <-out_dat
      out_orth <- out_orth %>% 
        mutate(Dim_1_gs = .data$Dim_1) %>% 
        select(-"Dim_1")
      for(m in 2:length(x$mods)){
        ivs <- glue_collapse(glue("Dim_{1:max(1, (m-1))}_gs"), "+")
        form <- glue("Dim_{m} ~ {ivs}")
        tmp_mod <- lm(form, data=out_orth)
        out_orth[[glue("Dim_{m}_gs")]] <- out_dat[[glue("Dim_{m}")]] - predict(tmp_mod, newdata=out_orth)
        out_orth <- out_orth %>% select(-glue("Dim_{m}"))
        
      }
      out_dat <- left_join(out_dat, out_orth)
    }
    if("pca" %in% ortho){
      out_orth <- out_dat
      sel <- grep("^Dim_\\d+$", names(out_dat), value=TRUE)
      dims <- out_dat %>% dplyr::select(all_of(sel))
      pc_dim <- princomp(na.omit(dims))
      scores <- as.matrix(dims) %*% pc_dim$loadings
      for(m in 1:length(x$mods)){
        out_orth[[glue("Dim_{m}_pca")]] <- scores[,m]
        out_orth <- out_orth %>% select(-glue("Dim_{m}"))
      }
      out_dat <- left_join(out_dat, out_orth)
    }
  }
  # if(boot_sd){
  #   boot.res <- list()
  #   for(i in 1:length(x$mods)){
  #     cat("\n\nCalculating Bootstrap SD for Dimension", i, "\n")
  #     boot.res[[i]] <- boot_emIRT(x$mods[[i]],
  #                          .data=x$dats[[i]]$dat,
  #                          .starts = x$starts[[i]],
  #                          .priors = x$priors[[i]],
  #                          .control = dynIRT_control,
  #                          Ntrials=Nboot)
  #   }
  #   boot_long <- list()
  #   for(i in 1:length(boot.res)){
  #     boot_long[[i]] <- data.frame(name = rep(ids[[i]][,1], ncol(lats[[i]])),
  #                                 time = rep(1:ncol(lats[[i]]), each=nrow(ids[[i]])),
  #                                 latent = c(boot.res[[i]]$bse$x))
  #     names(boot_long[[i]])[3] <- paste0("Dim_", i, "_sd")
  #   }
  #   boot_out <- x$legis_data
  #   for(i in 1:length(boot_long)){
  #     boot_out <- left_join(boot_out, boot_long[[i]])
  #   }
  #   allNA <-  apply(boot_out[,grep("^Dim", names(boot_out))], 1, function(z)all(is.na(z)))
  #   boot_out <- boot_out %>% filter(!allNA)
  #   out_dat <- left_join(out_dat, boot_out)
  #   if(ortho %in% c("gs", "pca")){
  #     draws <- list()
  #     for(i in 1:length(lats)){
  #       wna <- which(is.na(out_dat[[glue("Dim_{i}")]]) | is.na(out_dat[[glue("Dim_{i}_sd")]]))
  #       mu <- out_dat[[glue("Dim_{i}")]]
  #       sig <- out_dat[[glue("Dim_{i}_sd")]]
  #       mu[wna] <- 0
  #       sig[wna] <- 0
  #       draws[[i]] <- t(MASS::mvrnorm(1500, mu, diag(sig)))
  #     }
  #     if(ortho == "gs"){
  #       out_orth <- out_dat
  #       for(m in 2:length(lats)){
  #         ivs <- glue_collapse(glue("Dim_{1:max(1, (m-1))}"), "+")
  #         form <- glue("Dim_{m} ~ {ivs}")
  #         tmp_mod <- lm(form, data=out_orth)
  #         fit <-matrix(coef(tmp_mod)[1], ncol=ncol(draws[[1]]), nrow=nrow(draws[[1]]))
  #         for(j in 2:length(coef(tmp_mod))){
  #           fit <- fit + coef(tmp_mod)[j]*draws[[(j-1)]]
  #         }
  #         res <- draws[[m]] - fit
  #         out_orth[[glue("Dim_{m}")]] <- rowMeans(res)
  #         out_orth[[glue("Dim_{m}_sd")]] <- apply(res, 1, sd)
  #       }
  #     }
  #     if(ortho == "pca"){
  #       out_orth <- out_dat
  #       sel <- grep("^Dim_\\d+$", names(out_dat), value=TRUE)
  #       dims <- out_dat %>% dplyr::select(all_of(sel))
  #       pc_dim <- princomp(na.omit(dims))
  #       nd <- lapply(1:ncol(draws[[1]]), function(x)sapply(draws, function(i)i[,x]))
  #       nd <-lapply(nd, function(x)as.matrix(x) %*% pc_dim$loadings)
  #       nd2 <- lapply(1:ncol(nd[[1]]), function(i)sapply(nd, function(x)x[,i]))
  #       for(m in 1:length(nd2)){
  #         out_orth[[glue("Dim_{m}")]] <- rowMeans(nd2[[m]])
  #         out_orth[[glue("Dim_{m}_sd")]] <- apply(nd2[[m]], 1, sd)
  #       }
  #     }
  #     for(i in 1:length(lats)){
  #       wna <- which(is.na(out_dat[[glue("Dim_{i}")]]) | is.na(out_dat[[glue("Dim_{i}_sd")]]))
  #       if(length(wna) > 0){
  #         out_orth[[glue("Dim_{i}")]][wna] <- NA
  #         out_orth[[glue("Dim_{i}_sd")]][wna] <- NA
  #       }
  #     }
  #   }
  # }
  return(out_dat)
}
