library(legR)
library(dwnominate)
library(tidyverse)
source("sim_fns.r")
out1 <- list()
for(i in 1:2){
  out1[[i]] <- run_sim()
  save(out, file="out1_sim.res")
}
quit('no')
