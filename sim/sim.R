source("sim/sim_fns.r")
library(legR)
library(tidyverse)

out1 <- run_sim(prop_irrel = 0)


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


