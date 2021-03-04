library(rio)
library(tidyverse)
library(glue)
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
#            seed=519, # r = 0.83
            seed=734, # 
            max_mem_size="8g")

x <- gather_data(out)
x <- x %>% filter(!((Dim1 == 0 | is.na(Dim1)) & 
                     Dim2 ==0 | is.na(Dim2)))
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



### Plot Trajectories

locs <- function(quadrant, plotObj, xloc = NULL, yloc=NULL){
  if(!inherits(plotObj, "ggplot_built")){
    plotObj <- try(ggplot_build(plotObj))
  }
  if(inherits(plotObj, "try-error"))stop("plotObj must be a ggplot or the output from ggplot_build()\n")
  if(is.null(xloc)){
    xloc <- switch(quadrant, 
                   q1 = c(.05, .45), 
                   q2 = c(.55, .95), 
                   q3 = c(.55, .95), 
                   q4 = c(.05, .45))
  }
  if(is.null(yloc)){
    yloc <- switch(quadrant, 
                   q1 = c(.55, .95), 
                   q2 = c(.55, .95), 
                   q3 = c(.05, .45), 
                   q4 = c(.05, .45))
  }
  rx <- plotObj$layout$panel_params[[1]]$x.range
  ry <- plotObj$layout$panel_params[[1]]$y.range
  drx <- diff(rx)
  dry <- diff(ry)
  xmin <- rx[1] + xloc[1]*drx
  xmax <- rx[1] + xloc[2]*drx
  ymin <- ry[1] + yloc[1]*dry
  ymax <- ry[1] + yloc[2]*dry
  c(xmin, xmax, ymin, ymax)
}


tmp <- g %>% filter(name == "RBGinsburg") %>% 
  arrange(session) %>% 
  mutate(l1 = lag(Dim1_gs), 
         l2 = lag(Dim2_gs))

p1 <- ggplot(tmp) + 
  geom_segment(aes(x = l1, xend=Dim1_gs, 
                   y=l2, yend=Dim2_gs)) + 
  geom_vline(xintercept=0) + 
  geom_hline(yintercept=0) + 
  theme_bw() + 
  theme(panel.grid=element_blank()) + 
  coord_cartesian(xlim=range(g$Dim1_gs, na.rm=TRUE), 
                  ylim=range(g$Dim2_gs, na.rm=TRUE)) + 
  labs(x="", y="") 
p2 <- 
  ggplot(tmp) + 
  geom_segment(aes(x = l1, xend=Dim1_gs, 
                   y=l2, yend=Dim2_gs)) + 
  geom_point(aes(x=Dim1_gs, y=Dim2_gs), size=5) +
  geom_text(aes(x=Dim1_gs, y=Dim2_gs,
                label=session), col="white", size=3) +
  theme_classic() + 
  labs(x="Dimension 1", y="Dimension 2") + 
  ggtitle(tmp$name[1])



l <- locs("q4", p2, xloc=c(0,.4), yloc=c(.1,.5))

p2 + annotation_custom(ggplotGrob(p1), 
                       xmin=l[1], 
                       xmax=l[2], 
                       ymin=l[3], 
                       ymax=l[4])

ggsave(glue("applied/sc_plots/{tmp$name[1]}.png"), 
      height=8, width=8, units="in", dpi=150)





library(pROC)

m1 <- glm(vote ~ Dim1_gs + Dim2_gs, data=sub1, family=binomial)
m1a <- glm(vote ~ Dim1_gs , data=sub1, family=binomial)
m2 <- glm(vote ~ mqs, data=sub1, family=binomial)

auc_mod <- function(mod){
  roc_obj <- pROC::roc(mod$y, mod$fitted)
  pROC::auc(roc_obj)
}

auc_mod(m1)
auc_mod(m1a)
auc_mod(m2)





