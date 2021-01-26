setwd("Z:/Dropbox (DaveArmstrong)/RA_Toronto roll call votes")
load("~/Dropbox/RA_Toronto roll call votes/torontoX.rda")
library(pscl)
library(tidyverse)
h <- h_all %>% select(name)
h <- h %>% mutate(party=1)
xl <- list()
for(i in 1:6){
  tmpx <- X[, which(trms == i)]
  tmpn <- as.character(h$name)
  wna <- which(apply(tmpx, 1, function(x)all(is.na(x))))
  tmpx <- tmpx[-wna, ]
  tmpn <- tmpn[-wna]
  tmph <- as.data.frame(h[-wna, ])
  xl[[i]] <- pscl::rollcall(tmpx, legis.data=tmph)
}

library(dwnominate)
dx <- dwnominate(xl)

load("senate_rcl.rda")
sen_dwnom <- dwnominate(rcl)
sim_dwnom <- dwnominate(sim_rcl)
sim_dwnom2 <- dwnominate(sim_rcl, model=2)
sim_dwnom3 <- dwnominate(sim_rcl, model=3, polar=c(24,53))


library(legR)

l <- legR(X, trms, est_model = TRUE, nRounds=2, max_mem_size="8g", method="glrm", k=3, ndim=3, legis_data = h)
lo <- gather_data(l, orthogonalize = "gs",)
