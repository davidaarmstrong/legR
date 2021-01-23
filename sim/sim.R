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


leg <- make_body(n_per_term=50, 
                 incumb_prop=.8,
                 nterms=10,
                 s_range= c(-2,2), 
                 w_range=c(.5, 1), 
                 dim=2)


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

votes <- make_votes(n_votes=c(500, 200), 
                    data=leg, 
                    n_irrel=25, 
                    a_range=c(-1.5, -.5), 
                    b_range=c(.75, 1.5))


library(legR)
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
  nRounds = 2, 
  k=2, 
  ndim=2, 
  method="glrm", 
  max_mem_size="8g", 
  est_model = TRUE
)

o <- gather_data(out, orthogonalize=c("gs", "pca"))
leg <- setNames(leg, c("name", "session", "true1", "true2"))
all <- left_join(leg, o)

all %>% select(true1, Dim_1, Dim_1_gs, Dim_1_pca) %>% cor(., use="pair")
all %>% select(true2, Dim_2, Dim_2_gs, Dim_2_pca) %>% cor(., use="pair")

ld
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

load("sim/sim_dwnom.rda")
l1 <- sim_dwnom$legislators %>%
  select(session, ID, coord1D, coord2D) %>% 
  rename("name" = "ID", 
         "coord1D_1" = "coord1D", 
         "coord2D_1" = "coord2D") %>% 
  mutate(name = as.integer(name))
l2 <- sim_dwnom2$legislators %>% 
  select(session, ID, coord1D, coord2D) %>% 
  rename("name" = "ID", 
         "coord1D_2" = "coord1D", 
         "coord2D_2" = "coord2D") %>% 
  mutate(name = as.integer(name))

l3 <- sim_dwnom3$legislators %>% 
  select(session, ID, coord1D, coord2D) %>% 
  rename("name" = "ID", 
         "coord1D_3" = "coord1D", 
         "coord2D_3" = "coord2D") %>% 
  mutate(name = as.integer(name))

all <- left_join(all, l1)
all <- left_join(all, l2)
all <- left_join(all, l3)

all %>% select(true1, Dim_1, Dim_1_gs, Dim_1_pca, coord1D_1, coord1D_2, coord1D_3) %>% 
  cor(., use="pair")

all %>% select(true2, Dim_2, Dim_2_gs, Dim_2_pca, coord2D_1, coord2D_2, coord2D_3) %>% 
  cor(., use="pair")


