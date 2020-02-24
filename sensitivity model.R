library(tidyverse)

sensitivity <- readr::read_csv("sensitivity-down-metric2.csv", na = "NA")

as.character(sensitivity$rotation)
sensitivity$rotation <-  as.factor(sensitivity$rotation)
sensitivity <- sensitivity %>% mutate(ilevofactor=ilevofactor*100)

#sensitivity.summ <- Rmisc::summarySE(sensitivity, "ilevofactor")
install.packages("ggthemes") 
library(ggthemes)

ggplot(sensitivity, aes(ilevofactor, netreturnchg, color=rotation))+
  geom_point(size=3)+
  geom_smooth(se=FALSE, method="lm")+
  geom_vline(xintercept = 0, linetype="dashed")+
  geom_hline(yintercept = 0, linetype="dashed")+
  ggrepel::geom_text_repel(aes(label=ident),
                           nudge_y=-20, direction="y", color="black")+
  facet_grid(.~year, labeller=labeller(rotation=rotalbl))+
  ylab('change in economic return ($/ha)')+
  xlab("proportion of yield change (%)")+
  #ggtitle("Sensitivity analysis",
  #       subtitle="effect of fluopyram to soybean yield and economic return\nin different crop rotations")+
  scale_color_discrete(name = "Rotation", labels = c("2-yr", "3-yr", "4-yr"))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=315, hjust=0))

  

