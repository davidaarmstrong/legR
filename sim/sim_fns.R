make_body <- function(n_per_term, incumb_prop=.8, nterms, s_range, w_range, dim){
  # n_per_term: number of legislators per term. 
  # incumb_prop: incumbency percentage (prob of staying in legislature)
  # nterms: number of terms in the legislature
  # s_range: range of starting values starts will come from U(s_range[1], s_range[2])
  # w_range: range of widths of uniform distributions of disturbances.  Individual 
  #   disturbances will be drawn as follows: w ~ U(w_range[1], w_range[2]) and e ~ U(-w/2, w/2)
  require(dplyr)
  out <- list()
  tmp <- data.frame(
    id = 1:n_per_term, 
    term = 1)
  x <- matrix(runif(n_per_term*dim, s_range[1], s_range[2]), ncol=dim)
  x <- setNames(as.data.frame(x), paste0("Dim_", 1:dim))
  out[[1]] <- bind_cols(tmp, x)
  for(i in 2:nterms){
    outs <- rbinom(n_per_term, 1, (1-incumb_prop))
    w_out <- which(outs == 1)
    mx_id <- max(out[[(i-1)]]$id)
    new_id <- (mx_id+1):(mx_id+length(w_out))
    old_id <- out[[(i-1)]]$id
    old_id <- old_id[-w_out]
    old_tmp <- data.frame(
      id = c(old_id, new_id), 
      term = (i-1))
    old_tmp <- suppressMessages(left_join(old_tmp, out[[(i-1)]]))
    delta <- matrix(runif(n_per_term*dim, w_range[1], w_range[2]), ncol=dim)
    s <- matrix(runif(n_per_term*dim, s_range[1], s_range[2]), ncol=dim)
    new_tmp <- data.frame(
      id = c(old_id, new_id), 
      term = (i))
    for(j in 1:dim){
      new_tmp[[paste0("Dim_", j)]] <- ifelse(is.na(old_tmp[[paste0("Dim_", j)]]), 
                                             s[,j], 
                                             old_tmp[[paste0("Dim_", j)]] + delta[,j])
    }
    out[[i]] <- new_tmp
  }  
  out_all <- do.call(bind_rows, out)
  for(i in 2:dim){
    td <- out_all %>% select("Dim_1":paste0("Dim_", i))
    form <- as.formula(paste0("Dim_", i, " ~ ."))
    out_all[[paste0("Dim_", i)]] <- residuals(lm(form, data=td))
  }
  out_all <- out_all %>% mutate(across(contains("Dim"), ~c(scale(.x))))
  out_all
}
make_votes <- function(n_votes, data, n_irrel = 0, a_range, b_range){
  require(tidyr)
  if(length(n_votes) != length(grep("^Dim", names(data)))){
    stop("The n_votes vector has to have as many values as there are dimensions in the data\n")
  }
  v <- vi <- NULL
  for(i in 1:length(n_votes)){
    ab <- rbind(runif(n_votes[i], a_range[1], a_range[2]), 
                runif(n_votes[i], b_range[1], b_range[2]))
    p <- plogis(cbind(1, data[[paste0("Dim_", i)]]) %*% ab)
    y <- apply(p, 2, function(x)rbinom(length(x), 1, x))
    v <- cbind(v, y)
  }
  if(n_irrel > 0){
    drg <- data %>% select(contains("Dim")) %>% as.matrix %>% c(.) %>% range(.)
    abi <- rbind(runif(n_irrel, a_range[1], a_range[2]), 
                 runif(n_irrel, a_range[1], a_range[2]))
    for(j in 1:n_irrel){
      yi <- rbinom(nrow(data), 1, 
                   plogis(cbind(1, runif(nrow(data), drg[1], drg[2])) %*% abi[,j]))
      vi <- cbind(vi, yi)
    }
    v <- cbind(v, vi)
  }
  colnames(v) <- paste0("V", 1:ncol(v))
  vf <- paste(colnames(v)[c(1, ncol(v))], collapse=":")
  data <- bind_cols(data, as.data.frame(v))
  dataw <- data %>% select(-contains("Dim")) %>% 
    pivot_wider(names_from="term", 
                values_from=contains("V"))
  return(dataw)
}
apre.legR <- function(out, data){
  X <- as.matrix(out$ilv$votes)
  trm <- out$ilv$term
  tmp <- out$legis_data
  data <- data %>% select(name, session, Dim_1, Dim_2)
  res <- NULL
  for(i in 1:max(trm)){
    w <- which(trm == i)
    tmp <- suppressMessages(left_join(tmp %>% select(name), data %>% filter(session == i)))
    for(j in 1:length(w)){
      tmp$vote <- X[,w[j]]
      m1 <- glm(vote ~ Dim_1, data=tmp)
      m2 <- glm(vote ~ Dim_1 + Dim_2, data=tmp)
      mv <- min(table(tmp$vote))
      err1 <- sum(model.response(model.frame(m1)) != as.numeric(m1$fitted > .5))
      err2 <- sum(model.response(model.frame(m2)) != as.numeric(m2$fitted > .5))
      vec <- c(mv, err1, err2)
      names(vec) <- c("minvote", "err1", "err2")
      res <- rbind(res, vec)
    }
    
  }
  ap1 <- sum(res[,1] - res[,2])/sum(res[,1])
  ap2 <- sum(res[,1] - res[,3])/sum(res[,3])
  p <- c("apre1" = ap1, "apre2" = ap2)
  return(p)
}

run_sim <- function(n_per_term = ceiling(runif(1, 30,150)), 
                     incumb_prop = runif(1, .4, .9), 
                     nterms = 10, 
                     s_range = c(-2,2), 
                     w_range = sort(runif(2, .05, 2.5), decreasing = FALSE), 
                     dim = 2, 
                     n_votes = runif(1, 100, 1000),
                     prop_votes = NULL,
                     prop_irrel = NULL, 
                     a_range = c(-1.5, -.5), 
                     b_range = c(.75, 1.5), 
                    ...){
  
  # n_per_term = ceiling(runif(1, 30,150))
  # incumb_prop = runif(1, .4, .9)
  # nterms = 10
  # s_range = c(-2,2)
  # w_range = sort(runif(2, .05, 2.5), decreasing = FALSE)
  # dim = 2
  # n_votes = 500
  # prop_votes = .65
  # prop_irrel = 0
  # a_range = c(-1.5, -.5)
  # b_range = c(.75, 1.5)
  # nRounds <- 2

  
  
  if(is.null(prop_votes)){
    p1 <- runif(1, .5, .85)
    p2 <- 1-p1
  }else{
    p1 <- prop_votes
    p2 <- 1-p1
  }
  props <- c(p1, p2)
  n_votes <- ceiling(n_votes*props)
  if(is.null(prop_irrel)){
    n_irrel <- ceiling(runif(1, .1, .6)*sum(n_votes))
  }else{
    n_irrel <- prop_irrel*sum(n_votes)
  }
  
  
  

  
  
  leg <- make_body(n_per_term=n_per_term, 
                   incumb_prop=incumb_prop,
                   nterms=nterms,
                   s_range= s_range, 
                   w_range=w_range, 
                   dim=dim)
  
  votes <- make_votes(n_votes=n_votes, 
                      data=leg, 
                      n_irrel=n_irrel, 
                      a_range=a_range, 
                      b_range=b_range)
  
  ld <- leg %>% 
    group_by(id) %>% 
    summarise(across(contains("Dim"), ~mean(.x, na.rm=TRUE))) %>% 
    rename("name" = "id")
  
  trm <- as.numeric(gsub(".*_(\\d+)", "\\1", names(votes)[-1]))
  out <- legR(
    votes[,-1], 
    terms=trm, 
    legis_data=ld, 
    minprop=.1, 
    k=2, 
    ndim=2, 
    method="glrm", 
    max_mem_size="4g", 
    est_model = TRUE, 
    ...
  )
  n_kept <- sapply(out$dats, function(x)ncol(x$dat$rc))
  if(!any(sapply(out$mods, inherits, "try-error"))){
  sim_rcl <- list()
  for(i in 1:10){
    tmpv <- votes[, -1]
    tmpv <- tmpv[, which(trm == i)]
    wna <- which(apply(tmpv, 1, function(x)all(is.na(x))))
    tmpv <- tmpv[-wna, ]
    tmpl <- as.data.frame(ld)[-wna, ]
    tmpl$party <- 1
    sim_rcl[[i]] <- pscl::rollcall(as.data.frame(tmpv), 
                                   legis.names=as.character(tmpl$name), 
                                   legis.data=tmpl)  
  }
 
  pols <- sapply(out$priors, function(x)which(x$x.mu0 == 1))
  require(dwnominate)
  dwn <- dwnominate(sim_rcl, polarity = pols)
  dleg <- dwn$legislators %>% select(session, name, coord1D, coord2D)
  
  
  
  
    o <- gather_data(out, orthogonalize=c("gs", "pca"))
    leg <- setNames(leg, c("name", "session", "true1", "true2"))
    all <- left_join(leg, o)
    all <- left_join(all, dleg %>% mutate(name = as.integer(name)))
    save(out, all, file="sim_inter.rda")
    
    r1 <- all %>% select(true1, Dim_1, Dim_1_gs, Dim_1_pca, coord1D) %>% cor(., use="pair") %>% .[,1]
    r2 <- all %>% select(true2, Dim_2, Dim_2_gs, Dim_2_pca, coord2D) %>% cor(., use="pair") %>% .[,1]
    a1 <- apre.legR(out, o)
    a2 <- apre.legR(out, all %>% 
                      select(name, session, coord1D, coord2D) %>% 
                      rename("Dim_1" = "coord1D", "Dim_2" = "coord2D"))

    res <- list(r1 = r1, 
                r2 = r2, 
                params = c("n_per_term" = n_per_term, 
                           "incumb_prop" = incumb_prop, 
                           "w_low" = w_range[1], 
                           "w_high" = w_range[2], 
                           "n1" = n_votes[1], 
                           "n2" = n_votes[2], 
                           "nk1" = n_kept[1], 
                           "nk2" = n_kept[2], 
                           "aprel1" = a1[1], 
                           "aprel2" = a1[2],
                           "apred1" = a2[1], 
                           "apred2" = a2[2], 
                           "n_irrel" = n_irrel)
    )
  }else{
    res <- list(r1 = rep(NA, 5), 
                r2 = rep(NA, 5), 
                params = c("n_per_term" = n_per_term, 
                           "incumb_prop" = incumb_prop, 
                           "w_low" = w_range[1], 
                           "w_high" = w_range[2], 
                           "n1" = n_votes[1], 
                           "n2" = n_votes[2], 
                           "nk1" = n_kept[1], 
                           "nk2" = n_kept[2], 
                           "aprel1" = NA, 
                           "aprel2" = NA,
                           "apred1" = NA, 
                           "apred2" = NA, 
                           "n_irrel" = n_irrel))
    
  }
  h2o::h2o.shutdown(prompt = FALSE)
  return(res)
}
