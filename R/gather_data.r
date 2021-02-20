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
      mutate(start = c(x$dats[[i]]$dat$startlegis+1), 
             end = c(x$dats[[i]]$dat$endlegis+1))
    d <- bind_cols(base, as_tibble(d)) %>%
      pivot_longer(-c("name", "start","end"), names_to="session", values_to = paste0("Dim_",i)) %>%
      mutate(session = as.numeric(.data$session)) %>% 
      filter(!(.data$session < .data$start | .data$session > .data$end))
    d    
  })
  out_dat <- out_dats[[1]]
  for(i in 2:length(out_dats)){
    out_dat <- full_join(out_dat, out_dats[[i]] %>% select(name, contains("Dim")))
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
  if(boot_sd){
    boot.res <- list()
    for(i in 1:length(x$mods)){
      cat("\n\nCalculating Bootstrap SD for Dimension", i, "\n")
      boot.res[[i]] <- boot_emIRT(x$mods[[i]],
                           .data=x$dats[[i]]$dat,
                           .starts = x$starts[[i]],
                           .priors = x$priors[[i]],
                           .control = dynIRT_control,
                           Ntrials=Nboot)
    }
    out_ses <- lapply(1:length(boot.res), function(i){
      d <- boot.res[[i]]$bse$x
      colnames(d) <- 1:ncol(d)
      base <- x$dats[[i]]$id %>% 
        select("name") %>% 
        mutate(start = c(x$dats[[i]]$dat$startlegis+1), 
               end = c(x$dats[[i]]$dat$endlegis+1))
      d <- bind_cols(base, as_tibble(d)) %>%
        pivot_longer(-c("name", "start","end"), names_to="session", values_to = paste0("Dim_",i)) %>%
        mutate(session = as.numeric(.data$session)) %>% 
        filter(!(.data$session < .data$start | .data$session > .data$end))
      d    
    })
    out_se <- out_ses[[1]]
    for(i in 2:length(out_ses)){
      out_se <- full_join(out_se, out_ses[[i]] %>% select(name, contains("Dim")))
    }
    names(out_se) <- gsub("(Dim_\\d+)", "\\1_sd", names(out_se))
    out_dat <- full_join(out_dat, out_se)
    if("gs" %in% ortho){
      out_orth <-out_dat
      out_orth <- out_orth %>% 
        mutate(Dim_1_gs = .data$Dim_1, 
               dim_1_gs_sd = out_se$Dim_1_sd) %>% 
        select(-c("Dim_1", "Dim_1_sd"))
      for(m in 2:length(x$mods)){
        ivs <- glue_collapse(glue("Dim_{1:max(1, (m-1))}_gs"), "+")
        form <- glue("Dim_{m} ~ {ivs}")
        tmp_mod <- lm(form, data=out_orth)
        out_orth[[glue("Dim_{m}_gs")]] <- out_dat[[glue("Dim_{m}")]] - predict(tmp_mod, newdata=out_orth)
        a <- coef(tmp_mod)
        s2 <- out_se[[glue("Dim_{m}_sd")]]^2
        for(j in 1:length(a)){
          s2 <- s2 + a[j]^2*out_se[[glue("Dim_{j}_sd")]]^2
        }
        out_orth[[glue("Dim_{m}_gs_sd")]] <- sqrt(s2)
        out_orth <- out_orth %>% select(-c(glue("Dim_{m}"), glue("Dim_{m}_sd")))
        
      }
      out_dat <- left_join(out_dat, out_orth)
    }
    if("pca" %in% ortho){
      out_orth <- out_dat
      ggs <- grep("_gs", names(out_orth), value=TRUE)
      if(length(ggs) > 0){
        out_orth <- out_orth %>% select(-all_of(ggs))
      }
      selx <- grep("^Dim_\\d+$", names(out_dat), value=TRUE)
      sels <- grep("^Dim_\\d+_sd$", names(out_dat), value=TRUE)
      dims <- out_dat %>% dplyr::select(all_of(selx))
      sds <- out_dat %>% dplyr::select(all_of(sels))
      pc_dim <- princomp(na.omit(dims))
      scores <- as.matrix(dims) %*% pc_dim$loadings
      vars <- as.matrix(sds)^2 %*% pc_dim$loadings^2
      for(m in 1:length(x$mods)){
        out_orth[[glue("Dim_{m}_pca")]] <- scores[,m]
        out_orth[[glue("Dim_{m}_pca_sd")]] <- sqrt(vars[,m])
        out_orth <- out_orth %>% select(-c(glue("Dim_{m}"), glue("Dim_{m}_sd")))
      }
      out_dat <- left_join(out_dat, out_orth)
    }
  }
  return(out_dat)
}
