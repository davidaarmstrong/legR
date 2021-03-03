library(rio)
library(tidyverse)
sc <- import("applied/SCDB_2020_01.dta")


sc <- sc %>% select(justice, justiceName, docketId, term, majority) %>% 
  mutate(vote = majority-1) %>% 
  select(-majority)

library(legR)

unjust <- unique(sc$justiceName)
undock <- unique(sc$docketId)
legdat <- data.frame(name = unjust)
votes <- matrix(nrow=length(unjust), ncol=length(undock))
votes <- sc %>% select(justiceName, docketId, vote) %>% 
  pivot_wider(names_from="docketId", values_from="vote")
votes <- as.data.frame(votes)
rownames(votes) <- votes[,1]
votes <- votes[,-1]
votes <- as.matrix(votes)
colnames(votes) <- paste0("V", 1:ncol(votes))
term <- sc %>% 
  group_by(docketId) %>% 
  summarise(term=first(term)) %>% 
  ungroup %>% 
  select(term) %>% 
  pull 

term <- term-1945

const <- apply(votes, 2, sd, na.rm=TRUE) == 0
votes <- votes[,-which(const)]
term <- term[-which(const)]


out <- legR(votes, term, est_model=TRUE, 
            legis_data=legdat, k=1, ndim=1, 
            method="glrm",
            minprop=.05, 
            nRounds=1, 
            max_mem_size="8g")

g <- gather_data(out, orthogonalize="none")


mq <- import("applied/justices.dta")
mq <- mq %>% 
  mutate(term = term - 1945) %>% 
  select(justiceName, term, post_med) %>% 
  setNames(c("name", "session", "mqs"))

g <- g %>% left_join(mq)



