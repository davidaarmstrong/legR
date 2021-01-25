source("sim/sim_fns.r")
library(legR)
library(tidyverse)
debugonce(run_sim)

out1 <- run_sim(nRounds = 2, prop_irrel = 0, n_votes=500, prop_votes=.65)





sim_res <- list()
for(i in 1:2500){
  sim_res[[i]] <- run_sim(prop_irrel = 0)
}


# r1 <- r2 <- NULL
out <- legR(
  votes[,-1], 
  terms=trm, 
  legis_data=ld, 
  minprop=.25, 
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

r1 <- rbind(r1, all %>% select(true1, Dim_1, Dim_1_gs, Dim_1_pca) %>% cor(., use="pair") %>% .[,1])
r2 <- rbind(r2, all %>% select(true2, Dim_2, Dim_2_gs, Dim_2_pca) %>% cor(., use="pair") %>% .[,1])

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





  X <- as.matrix(out$ilv$votes)
  trm <- out$ilv$term
  tmp <- out$legis_data %>% select(name)
  data <- all %>% select(name, session, Dim_1, Dim_2, coord1D, coord2D)
  res <- NULL
  for(i in 1:max(data$session)){
    w <- which(trm == i)
    tmp <- suppressMessages(left_join(tmp, data %>% filter(session == i)))
    for(j in 1:length(w)){
      tmp$vote <- X[,w[j]]
      m1 <- glm(vote ~ Dim_1, data=tmp)
      m2 <- glm(vote ~ Dim_1 + Dim_2, data=tmp)
      m3 <- glm(vote ~ coord1D, data=tmp)
      m4 <- glm(vote ~ coord1D + coord2D, data=tmp)
      
      mv <- min(table(tmp$vote))
      err1 <- sum(model.response(model.frame(m1)) != as.numeric(m1$fitted > .5))
      err2 <- sum(model.response(model.frame(m2)) != as.numeric(m2$fitted > .5))
      err3 <- sum(model.response(model.frame(m3)) != as.numeric(m3$fitted > .5))
      err4 <- sum(model.response(model.frame(m4)) != as.numeric(m4$fitted > .5))
      vec <- c(mv, err1, err2, err3, err4)
      names(vec) <- c("minvote", "err1", "err2", "err3", "err4")
      res <- rbind(res, vec)
    }
    
  }
  ap1 <- sum(res[,1] - res[,2])/sum(res[,1])
  ap2 <- sum(res[,1] - res[,3])/sum(res[,1])
  ap3 <- sum(res[,1] - res[,4])/sum(res[,1])
  ap4 <- sum(res[,1] - res[,5])/sum(res[,1])
  p <- c("apre1" = ap1, "apre2" = ap2)
  return(p)
}


