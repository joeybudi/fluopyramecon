library(tidyverse)
library(readr)
#preprocessing ####

yield <- read_csv("yields SB wwo ilevo.csv")
yield <- read_csv("C:/Users/budij/Box/ILeVO/yields SB wwo ilevo.csv")

yield$plot <- as.factor(yield$plot)
yield$block <- as.factor(yield$block)
yield$year <- as.factor(yield$year)

yieldx <- Rmisc::summarySE(yield, "yield1", c("rotation","seedtrt", "year"))
arrange(yieldx, seedtrt, year, rotation)
#write.csv(yieldb, "yields SB wwo ilevo og.csv")

yield1 <- gather(yield, "seedtrt", "yield", 4:5)
yield1 <- pivot_wider(yield, names_from=seedtrt, values_from=yield)
yield1
#yield proportion due to   

yield2 <- yield1 %>%
  mutate(factor= fluopyram/base, fluopyramdiff=fluopyram-base)

#yield summaries EDA ####
library(Rmisc)
yield2
yield.summ2 <- summarySE(yield2, measurevar="factor", groupvars=c("rotation","year"), na.rm=TRUE)
yield.summ2
yield.summ3 <- summarySE(yield2, measurevar="fluopyramdiff", groupvars=c("rotation","year"), na.rm=TRUE)
yield.summ3


ggplot(yield.summ2, aes(x=rotation,y=factor, fill=rotation)) +
  geom_bar(position="dodge", stat="identity", size=1, color="#000000") +
  geom_errorbar(aes(ymin=factor-se, ymax=factor+se), width=.2, size=1, position=position_dodge(.9))+
  geom_jitter(data=yield2, aes(x=rotation, y=factor, stroke=.5), alpha=0.3)+
  ylab("Yield factor, fluopyram/base")+
  xlab("Rotation")+
  facet_grid(.~year, labeller=label_both)+
  theme(axis.line = element_line(colour = "black",size = 1, linetype = "solid"),
        text= element_text(size=16))+
  scale_y_continuous(breaks=seq(0,1,0.2))+
  theme_bw()

ggplot(yield.summ3, aes(x=rotation,y=fluopyramdiff, fill=rotation)) +
  geom_bar(position="dodge", stat="identity", size=1, color="#000000") +
  geom_errorbar(aes(ymin=fluopyramdiff-se, ymax=fluopyramdiff+se), width=.2, size=1, position=position_dodge(.9))+
  geom_jitter(data=yield2, aes(x=rotation, y=fluopyramdiff, stroke=.5), alpha=0.3)+
  ylab("Yield difference, fluopyram vs base (bu/Ac)")+
  xlab("Rotation")+
  facet_grid(.~year, labeller=label_both)+
  theme(axis.line = element_line(colour = "black",size = 1, linetype = "solid"),
        text= element_text(size=16))+
  #scale_y_continuous(breaks=seq(0,1,0.2))+
  theme_bw()

yield3<- select(yield2, -factor)
yield4 <- gather(yield3, "seedtrt", "yield", 3:4)
arrange(yield4, year, rotation)

#PLOT for adjusted yields, adjusted from Liebman main plot yields ####
yield
summ.yield <- Rmisc::summarySE(yield, "yield", c("rotation", "seedtrt"))
summ.yield

lblyield <- c("c","c","abc","bc","ab","a", 
                  "b","b","ab","ab","a","a",
                  "c","c","b","c","ab","a") #label from LSD from line 77, year 2017, 2018, 2019

lblyield2 <- c("c","c","b","bc","a","a")
rotalbl <- c("2-yr rtn","3-yr rtn","4-yr rtn")
names(rotalbl) <- c("S2","S3","S4")

ggplot(summ.yield, aes(x=seedtrt,y=yield, fill=seedtrt)) +
  geom_bar(position="dodge", stat="identity", size=1, color="#000000") +
  scale_fill_brewer(palette="Set3")+
  geom_errorbar(aes(ymin=yield-se, ymax=yield+se), width=.2, size=1, position=position_dodge(.9))+
  geom_jitter(data=yield, aes(x=seedtrt, y=yield, stroke=.5, shape=year, color=year), alpha=0.6, show.legend=TRUE)+
  ylab("Yield (Mg/ha)")+
  xlab("Seed treatment")+
  facet_grid(.~rotation, labeller=labeller(rotation=rotalbl))+
  #ggtitle("Estimated yield, 2017-2019", subtitle="adjusted from main plot*fluopyram yield factor")+
  theme_bw()+
  geom_text(position = position_dodge(0.9),vjust = -2,hjust=0.25, size=4, 
            label=lblyield2, aes(fontface=2))+  
  theme(text=element_text(size=12),
        axis.text.x=element_text(angle = 315, hjust = 0),
        strip.text.x = element_text(size = 10,  face = "bold"),
        legend.position ="right",
        legend.title=element_blank(),
        # legend.background = element_rect(fill="lightblue", 
        #                                  size=0.5, linetype="solid"),
        panel.background = element_rect(fill = "#F0F0F0",
                                        colour = "white",
                                        size = 0.2, linetype = "solid"),
        panel.grid.major = element_line(size = 0, linetype = 'solid',
                                        colour = "#E2E2E2"), 
        panel.grid.minor = element_line(size = 0, linetype = 'solid',
                                        colour = "#E2E2E2")
  )
  #scale_y_continuous(breaks=seq(0,1,0.2))+
 
#stats1 ####

yield.lm <- lm(yield~rotation*seedtrt + seedtrt*year+  block + year +block:year, data=yield)
TukeyHSD(aov(yield.lm))
ya <- anova(yield.lm)
ya 
lsdyld <- agricolae::LSD.test(yield.lm, c("rotation","seedtrt"))
lsdyld
anova(yield~rotation*seedtrt + block + year, data=yield)

#WITH OG PLOT YIELDS, not adjusted compared to Liebman main plot yields ####
yieldb$block <-  as.character(yield$block)
yieldb$year <-  as.character(yield$year)
yieldb$rotation <- as.character(yield$rotation)

yieldbspread <- spread(yieldb, seedtrt, yield)
yieldbspread
yieldbspread1 <- yieldbspread %>% mutate(fluopyramdiff=fluopyram-base, fluopyramfactor=fluopyram/base)
summ.yieldbspread2 <- summarySE(yieldbspread1, measurevar = "fluopyramdiff", 
                                groupvars=c("rotation","year"), na.rm=TRUE)
summ.yieldbspread3 <- summarySE(yieldbspread1, measurevar = "fluopyramfactor", 
                                groupvars=c("rotation","year"), na.rm=TRUE)
summ.yieldbspread3
ggplot(summ.yieldbspread2, aes(x=rotation,y=fluopyramdiff, fill=rotation)) +
  geom_bar(position="dodge", stat="identity", size=1, color="#000000") +
  geom_errorbar(aes(ymin=fluopyramdiff-se, ymax=fluopyramdiff+se), width=.2, size=1, position=position_dodge(.9))+
  geom_jitter(data=yieldbspread1, aes(x=rotation, y=fluopyramdiff, stroke=.5), alpha=0.3)+
  ylab("Yield diff, fluopyram-base")+
  xlab("Rotation")+
  facet_grid(.~year, labeller=label_both)+
  ggtitle("Yield difference, fluopyram-base x rotation, 2017-2019")+
  theme(axis.line = element_line(colour = "black",size = 1, linetype = "solid"),
        text= element_text(size=16))+
  #scale_y_continuous(breaks=seq(0,1,0.2))+
  theme_bw()

ggplot(summ.yieldbspread3, aes(x=rotation,y=fluopyramfactor, fill=rotation)) +
  geom_bar(position="dodge", stat="identity", size=1, color="#000000") +
  geom_errorbar(aes(ymin=fluopyramfactor-se, ymax=fluopyramfactor+se), width=.2, size=1, position=position_dodge(.9))+
  geom_jitter(data=yieldbspread1, aes(x=rotation, y=fluopyramfactor, stroke=.5), alpha=0.3)+
  ylab("Yield factor, fluopyram/base")+
  xlab("Rotation")+
  facet_grid(.~year, labeller=label_both)+
  ggtitle("Ratio of yield, /base x rotation, 2017-2019")+
  theme(axis.line = element_line(colour = "black",size = 1, linetype = "solid"),
        text= element_text(size=16))+
  #scale_y_continuous(breaks=seq(0,1,0.2))+
  theme_bw()

#LSD test
library(agricolae)
mod.yield17 <- aov(yield ~ block + rotation + seedtrt + rotation:seedtrt, data=subset(yield, year=="2017"))
summary(mod.yield17)
lsd17<- LSD.test(mod.yield17, c("rotation","seedtrt"))
lsd17
TukeyHSD(mod.yield17)
                 
mod.yield18 <- aov(yield ~ block + rotation + seedtrt , data=subset(yield, year=="2018"))
summary(mod.yield18)
lsd18<- LSD.test(mod.yield18, c("rotation","seedtrt"))
lsd18
TukeyHSD(mod.yield18)

mod.yield19 <- aov(yield ~ block + rotation + seedtrt , data=subset(yield, year=="2019"))
summary(mod.yield19)
lsd19<- LSD.test(mod.yield18, c("rotation","seedtrt"))
lsd19
TukeyHSD(mod.yield19)

#plots
yieldb.summ <- summarySE(yieldb, measurevar="yield", 
                         groupvars=c("rotation","year","seedtrt"), na.rm=TRUE)

ggplot(yieldb.summ, aes(x=rotation,y=yield, fill=seedtrt)) +
  geom_bar(position="dodge", stat="identity", size=1, color="#000000") +
  geom_errorbar(aes(ymin=yield-se, ymax=yield+se), width=.2, size=1, position=position_dodge(.9))+
  geom_jitter(data=yieldb, aes(x=rotation, y=yield, stroke=.5), alpha=0.3)+
  ylab("Yield (bu/Ac)")+
  xlab("Seed treatment")+
  facet_grid(.~year)+
  theme(axis.line = element_line(colour = "black",size = 1, linetype = "solid"),
        text= element_text(size=12))+
  theme_bw()

#correlation with sds #2018 only
yield18 <- subset(yield1, year=="2018")
yield_sds <- left_join(yield18, sds18a)
yield_sds <- yield_sds %>% select(-c(6:8))
yield_sds

mod.yield_sds <- lm(yield~DI, data=yield_sds)
summary(mod.yield_sds)
plot(mod.yield_sds)

ggplot(yield_sds, aes(DI, yield))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE) +
  theme_classic()
