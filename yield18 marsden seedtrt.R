library(tidyverse)
library(dplyr)
library(plyr)
library(readxl)
yld_ilevo_marsden_18 <- read_excel("NIFA 2018 ILeVo seed treatment yields harvested 102918.xlsx", 
                                   sheet = "data to r", na = "NA")
View(yld_ilevo_marsden_18)
detach(package:plyr)

summaryYld <- summarySE(yld_ilevo_marsden_18, measurevar="yield_bupAc", groupvars=c("seedtrt","rotation"),na.rm=TRUE)
View(summaryYld)

ggplot(summaryYld, aes(x=rotation,y=yield_bupAc, fill=seedtrt)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=yield_bupAc-se, ymax=yield_bupAc+se), width=.2, position=position_dodge(.9))+
  ylab("Yield (bu/Ac)")+
  expand_limits(y=50)+
  theme(axis.line = element_line(colour = "black",size = 1, linetype = "solid"))

library(agricolae) 
mod.yield <- aov(yield_bupAc~rotation*seedtrt+blk, data=yld_ilevo_marsden_18)
summary(mod.yield)
LSD.test(mod.yield, c("rotation","seedtrt"),console=TRUE)

#w/o blk 4
Yld_woBlk4 <- yld_ilevo_marsden_18 %>% filter(blk!=4) 

summaryYld_woBlk4 <- summarySE(Yld_woBlk4, measurevar="yield_bupAc", groupvars=c("seedtrt","rotation"),na.rm=TRUE)
ggplot(summaryYld_woBlk4, aes(x=rotation,y=yield_bupAc, fill=seedtrt)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=yield_bupAc-se, ymax=yield_bupAc+se), width=.2, position=position_dodge(.9))+
  ylab("Yield (bu/Ac)")+
  expand_limits(y=30)+
  theme(axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  scale_fill_manual(values=c("#FF6600","#3399FF"))

mod.yield.woblk4 <- aov(yield_bupAc~rotation*seedtrt+blk, data=Yld_woBlk4)
summary(mod.yield.woblk4)
LSD.test(mod.yield.woblk4, c("rotation","seedtrt"),console=TRUE)
