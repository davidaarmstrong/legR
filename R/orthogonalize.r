#' Orthogonalize Collect Data from Estimated Models
#'
#' Orthogonalizes the data collected using \code{gather_data} using 
#' the Gram-Schmidt method. 
#' 
#' @param x A data frame or tibble produced with \code{gather_data}.
#' @param ... Other arguments to be passed down, currently not implemented. 
#'
#' @return A data frame with latent variable estimates.
#' @export
orthogonalize <- function(x, ...){
  dims <- grep("^Dim\\d$", names(x), value=TRUE)
  has_sd <- length(grep("sd$", names(x))) > 0
  out_orth <- x %>% 
    mutate(Dim1_gs = .data$Dim1) %>% 
    select(-"Dim1")
  if(has_sd){
    out_orth <- out_orth %>% 
      mutate(Dim1_gs_sd = .data$Dim1_sd) %>% 
      select(-"Dim1_sd")
  }
  for(m in 2:length(dims)){
    ivs <- glue_collapse(glue("Dim{1:max(1, (m-1))}_gs"), "+")
    form <- glue("Dim{m} ~ {ivs}")
    tmp_mod <- lm(form, data=out_orth)
    out_orth[[glue("Dim{m}_gs")]] <- out_orth[[glue("Dim{m}")]] - predict(tmp_mod, newdata=out_orth)
    out_orth <- out_orth %>% select(-glue("Dim{m}"))
    if(has_sd){
      a <- coef(tmp_mod)
      s2 <- x[[glue("Dim{m}_sd")]]^2
      for(j in 1:length(a)){
        s2 <- s2 + a[j]^2*x[[glue("Dim{j}_sd")]]^2
      }
      out_orth[[glue("Dim{m}_gs_sd")]] <- sqrt(s2)
      out_orth <- out_orth %>% select(-glue("Dim{m}_sd"))
    }    
  }
  out_dat <- left_join(x, out_orth)
  return(out_dat)
}
  
