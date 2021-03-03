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

allna <- apply(votes,2, function(x)all(is.na(x)))
if(any(allna)){
  votes <- votes[,-which(allna)]
  term <- term[-which(allna)]
}



out <- legR(votes, term, est_model=TRUE, 
            legis_data=legdat, k=2, ndim=2, 
            method="glrm",
            glm_method="glm", 
            minprop=.1, 
            nRounds=1, 
            nRand =1, 
            nperterm=15,
#            seed=4324, # r= 0.703
            seed=519, # r = 0.83
            max_mem_size="8g")

x <- gather_data(out)
x <- orthogonalize(x)
flipx <- flip(x, id="name", vars=c("Dim1_gs", "Dim2_gs"), time="session")
x <- left_join(x, flipx %>% mutate(session = as.numeric(session)))
## use the - in front of Dim1_gs*flip1 to make it positively related to mqs
x <- x %>% mutate(Dim1_gs = -Dim1_gs*flip1,
                  Dim2_gs = Dim2_gs*flip2) %>% 
  select(-contains("flip"))


mq <- import("applied/justices.dta")
mq <- mq %>% 
  mutate(term = term - 1945) %>% 
  select(justiceName, term, post_med) %>% 
  setNames(c("name", "session", "mqs"))

g <- x %>% left_join(mq)

cor(g[,5:7], use="pair")


r <- g %>% group_by(session) %>% 
  summarise(r1 = cor(Dim1_gs, mqs, use="pair"), 
            r2 = cor(Dim2_gs, mqs, use="pair"))



v <- out$ilv$votes
trm <- out$ilv$terms
vs <- by(1:ncol(votes), trm, function(j)v[,j])
vstats <- NULL
for(i in 1:length(vs)){
  tmp <- as_tibble(vs[[i]], rownames="name")
  sub <- g %>% filter(session == i)
  sub$vote <- NA
  tmp <- tmp[match(sub$name, tmp$name), ]
  for(j in 2:ncol(tmp)){
    sub$vote <- c(tmp[,j][[1]])
    sub1 <- na.omit(sub)
    minvotes <- min(table(sub$vote))
    m1 <- logistf(vote ~ Dim1_gs + Dim2_gs, data=sub1, family=binomial)
    m1a <- logistf(vote ~ Dim1_gs , data=sub1, family=binomial)
    m2 <- logistf(vote ~ mqs, data=sub1, family=binomial)
    err1 <- sum(m1$y != as.numeric(m1$linear.predictors > 0))
    err1a <- sum(m1a$y != as.numeric(m1a$linear.predictors > 0))
    err2 <- sum(m2$y != as.numeric(m2$linear.predictors > 0))
    vstats <- rbind(vstats, c(min = minvotes, err1 = err1, err1a =err1a, err2=err2)) 
  }
}


sum(vstats[,1]-vstats[,2])/sum(vstats[,1])
sum(vstats[,1]-vstats[,3])/sum(vstats[,1])
sum(vstats[,1]-vstats[,4])/sum(vstats[,1])


