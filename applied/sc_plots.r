library(ggrepel)
plot_justice <- function(jname, quadrant, xloc=NULL, yloc=NULL){
  tmp <- g %>% filter(name == jname) %>% 
    arrange(session) %>% 
    mutate(l1 = lag(Dim1_gs), 
           l2 = lag(Dim2_gs))
  
  p1 <- ggplot(tmp) + 
    geom_vline(xintercept=0, col="gray50", lty=2) + 
    geom_hline(yintercept=0, col="gray50", lty=2) + 
    geom_segment(aes(x = l1, xend=Dim1_gs, 
                     y=l2, yend=Dim2_gs)) + 
    theme_bw() + 
    theme(panel.grid=element_blank(), 
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA)) +  # bg of the plot) + 
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
  
  
  
  l <- locs(quadrant, p2, xloc=xloc, yloc=yloc)
  
  p12 <- p2 + annotation_custom(ggplotGrob(p1), 
                                xmin=l[1], 
                                xmax=l[2], 
                                ymin=l[3], 
                                ymax=l[4])
  
  p3 <- ggplot(tmp, aes(x=session, y=mqs)) + 
    geom_line() + 
    geom_point() + 
    theme_classic() +
    labs(x="Term", y="Martin-Quinn Score") + 
    coord_cartesian(ylim = range(g$mqs, na.rm=TRUE))
  
  
  png(glue("applied/sc_plots/{tmp$name[1]}.png"), 
      height=6, width=12, units="in", res=150)
  gridExtra::grid.arrange(p12, p3, nrow=1)
  dev.off()
  
}



plot_justice("HHBurton", "q4", xloc=c(0,.4), yloc=c(.1,.5))
plot_justice("RBGinsburg", "q2")
plot_justice("AScalia", "q4")
plot_justice("CThomas", "q4")
plot_justice("RHJackson", "q1")
plot_justice("WODouglas", "q3", xloc=c(.4, .8), yloc=c(0, .4))
plot_justice("FFrankfurter", "q1", xloc=c(0,.4))
plot_justice("SFReed", "q2")
plot_justice("HLBlack", "q4")
plot_justice("WBRutledge", "q4")
plot_justice("FMurphy", "q3")
plot_justice("FMVinson", "q2")
plot_justice("TCClark", "q2", xloc=c(.6,1), yloc=c(.6,1))
plot_justice("SMinton", "q1")
plot_justice("EWarren", "q3")
plot_justice("JHarlan2", "q1", xloc=c(0,.4), yloc=c(.4,.8))
plot_justice("WJBrennan", "q3", xloc=c(.6,1), yloc=c(.4,.8))
plot_justice("CEWhittaker", "q4", yloc=c(.2,.6))
plot_justice("PStewart", "q3")
plot_justice("BRWhite", "q3")
plot_justice("AJGoldberg", "q2")
plot_justice("AFortas", "q2")
plot_justice("TMarshall", "q2", xloc=c(.6,1), yloc=c(.6,1))
plot_justice("WEBurger", "q3", xloc=c(.6,1))
plot_justice("HABlackmun", "q3", xloc=c(.5,.9), yloc=c(.2,.6))
plot_justice("LFPowell", "q2", yloc=c(.4,.8))
plot_justice("WHRehnquist", "q4")
plot_justice("JPStevens", "q3", yloc=c(0,.4))
plot_justice("SDOConnor", "q4", yloc=c(.15,.55))
plot_justice("AMKennedy", "q4")
plot_justice("DHSouter", "q3")
plot_justice("SGBreyer", "q4", xloc=c(.6,1), yloc=c(-.025,.375))
plot_justice("JGRoberts", "q3")
plot_justice("SAAlito", "q2")
plot_justice("SSotomayor", "q2")
plot_justice("EKagan", "q3")
plot_justice("NMGorsuch", "q4")
plot_justice("BMKavanaugh", "q4")

plot_court <- function(term){
  tmp <- g %>% filter(session==term) 
  p1 <- ggplot(tmp, aes(x=Dim1_gs, y=Dim2_gs)) + 
    geom_vline(xintercept=0, col="gray50", lty=2) + 
    geom_hline(yintercept=0, col="gray50", lty=2) + 
    geom_point() + 
    geom_text_repel(aes(label=name)) + 
    theme_classic() + 
    coord_cartesian(xlim=range(g$Dim1_gs), 
                    ylim = range(g$Dim2_gs)) + 
    labs(x="Dimension 1", y="Dimension 2")
    
  p2 <- ggplot(tmp, aes(x=Dim1_gs, y=reorder(name, Dim1_gs, mean))) + 
    geom_point() + 
    geom_vline(xintercept=0, lty=2, col="gray50") + 
    geom_linerange(aes(xmin=0, xmax=Dim1_gs)) + 
    theme_classic() + 
    labs(x="Dimension 1", y="")
  
  p3 <- ggplot(tmp, aes(x=Dim2_gs, y=reorder(name, Dim2_gs, mean))) + 
    geom_point() + 
    geom_vline(xintercept=0, lty=2, col="gray50") + 
    geom_linerange(aes(xmin=0, xmax=Dim2_gs)) + 
    theme_classic() + 
    labs(x="Dimension 2", y="")

  p4 <- ggplot(tmp, aes(x=mqs, y=reorder(name, mqs, mean))) + 
    geom_point() + 
    geom_vline(xintercept=0, lty=2, col="gray50") + 
    geom_linerange(aes(xmin=0, xmax=mqs)) + 
    theme_classic() + 
    labs(x="Martin-Quinn Score", y="")
png(glue("applied/sc_plots/{tmp$session[1]+1945}.png"), 
         height=10, width=10, units="in", res=150)  
gridExtra::grid.arrange(p4, p2, p3, p1, ncol=2, top=glue("{tmp$session[1]+1945}"))
dev.off()  
}

for(i in 1:74){
  plot_court(i)  
}


library(Gmedian)
Gmedian(g[,c("Dim1_gs", "Dim2_gs")])
gmfun <- function(x){
  out <- Gmedian::Gmedian(x)
  colnames(out) <- paste0("Dim", 1:ncol(out))
  out <- as.data.frame(out)
  out
}

gmeds <- g %>% 
  group_by(session) %>% 
  summarise(gmfun(cbind(Dim1_gs, Dim2_gs))) %>% 
  mutate(lab = case_when(session == 1 ~ "1946", 
                         session == 74 ~ "2019", 
                         TRUE ~ NA_character_))

cents <- g %>% 
  group_by(session) %>% 
  summarise(Dim1 = mean(Dim1_gs), 
            Dim2 = mean(Dim2_gs)) %>% 
  mutate(lab = case_when(session == 1 ~ "1946", 
                       session == 74 ~ "2019", 
                       TRUE ~ NA_character_))

gmeds <- gmeds %>% 
  mutate(l1 = lag(Dim1), 
         l2 = lag(Dim2))
dimave <- g %>% 
  group_by(session) %>% 
  summarise(med1 = median(Dim1_gs), 
            med2 = median(Dim2_gs), 
            mn1 = mean(Dim1_gs), 
            mn2 = mean(Dim2_gs))

p1 <- ggplot(gmeds) + 
  geom_vline(xintercept=0, col="gray50", lty=2) + 
  geom_hline(yintercept=0, col="gray50", lty=2) + 
  geom_point(aes(x=Dim1, y=Dim2, colour=session+1945)) + 
  theme_classic() + 
  scale_colour_viridis_c() + 
  labs(x="Dimension 1", y="Dimension 2", colour="Term") + 
  ggtitle("Geometric Median")

p1a <- ggplot() + 
  geom_line(data = dimave, aes(x=session+1945, y=med1, colour="Median")) + 
  geom_line(data = dimave, aes(x=session+1945, y=mn1, colour="Mean")) + 
  geom_line(data = gmeds, aes(x=session+1945, y=Dim1, colour="Geometric Mean")) + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="Term", y="Dimension 1", colour="") 

p1b <- ggplot() + 
  geom_line(data = dimave, aes(x=session+1945, y=med2, colour="Median")) + 
  geom_line(data = dimave, aes(x=session+1945, y=mn2, colour="Mean")) + 
  geom_line(data = gmeds, aes(x=session+1945, y=Dim2, colour="Geometric Mean")) + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="Term", y="Dimension 2", colour="") 




p2 <- ggplot(cents) + 
  geom_vline(xintercept=0, col="gray50", lty=2) + 
  geom_hline(yintercept=0, col="gray50", lty=2) + 
  geom_point(aes(x=Dim1, y=Dim2, colour=session+1945)) + 
  theme_classic() + 
  scale_colour_viridis_c() + 
  labs(x="Dimension 1", y="Dimension 2", colour="Term") + 
  ggtitle("Mean")


mqave <- g %>% 
  group_by(session) %>% 
  summarise(med = median(mqs, na.rm=TRUE), 
            mn = mean(mqs, na.rm=TRUE))

p3 <- ggplot(mqave, aes(x=session+1945)) + 
  geom_line(aes(y=med, colour="Median")) + 
  geom_line(aes(y=mn, colour="Mean")) + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="Term", y="Martin-Quinn Score", colour="") 

png("applied/sc_plots/centers.png", height=12, width=7, units="in", res=150)
gridExtra::grid.arrange(p1, p2, p1a, p1b, p3, ncol=2)
dev.off()
