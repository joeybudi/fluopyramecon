library(readr)
library(tidyverse)
#STAND COUNT 18 and 19 combined together

#the Vc stage ####

RowsAvg2 <- read_csv("standcountedit2018.csv") #stand count, Vc stage, 2018, from ILeVO/stand count/stand count 2018-2
stdct19vd <- read_csv("standcountedit2019.csv")  #stand count, Vc stage. 2019, from ILeVO/sdsrating/stand count 19
names(stdct19vd)


stdct18 <- RowsAvg2 %>% select(2:6) %>% dplyr::rename(stdct=sum_stdct_ha) %>% mutate(year="2018")  
stdct18
str(stdct18)

stdct19ve <- select(stdct19vd, -c(inj,X1))
stdct19ve$plot <- as.character(stdct19ve$plot)
stdct19ve$block <- as.character(stdct19ve$block)
stdct18$plot <- as.character(stdct18$plot)
stdct18$block <- as.character(stdct18$block)

levels(stdct19ve$rotation) <- c("S2","S3","S4")
stdct19ve

stdct1819 <- bind_rows(stdct18, stdct19ve)
stdct1819
str(stdct1819)
write.csv(stdct1819, "standcount1819.csv")
stdct1819.summ <- Rmisc::summarySE(stdct1819, "stdct", c("rotation","seedtrt","year"))
stdct1819.summ

#Vc data viz and stats ####
ggplot(stdct1819.summ, aes(rotation, stdct, fill=seedtrt))+
  geom_bar(stat="identity", position="dodge", color="#000000")+
  geom_errorbar(aes(ymin=stdct-se, ymax=stdct+se), width=.2, position=position_dodge(.9)) +
  geom_jitter(data=stdct1819, aes(rotation, stdct, stroke=.2), alpha=0.3)+
  scale_color_identity()+
  #geom_abline(y=0, face=2)+
  facet_grid(.~ year, labeller=label_both)+
  theme(text=element_text(size=14))+
  geom_text(position = position_dodge(0.9),vjust = -1.25,hjust=0.5, size=4, label=lblstdct1819, aes(fontface=2))+
  scale_y_continuous(labels = scales::comma)+
  #scale_x_discrete(labels=c("2-yr","3-yr","4-yr"))+
  ylab("stand count (plants/hectare)")+
  ggtitle("Stand count V1 - '18 &'19",
          subtitle="")+
  theme_bw()


library(agricolae)

stdct18.lm <- lm(stdct~rotation*seedtrt + block, data=subset(stdct1819, year=="2018"))
LSD.test(stdct18.lm, c("rotation","seedtrt"), console=TRUE)               

stdct19.lm <- lm(stdct~rotation*seedtrt + block, data=subset(stdct1819, year=="2019a"))
LSD.test(stdct19.lm, c("rotation","seedtrt"), console=TRUE)           

lblstdct1819 <- c("ab","b","a","ab","a","ab",
                  "ab","b","ab","ab","ab","a")    

stdct1819.lm <- lm(stdct~ rotation* seedtrt + block + year, data=stdct1819)
stdct1819.lmm <- lmerTest::lmer(stdct~ rotation+ seedtrt +rotation*seedtrt + (1|block:year) ,
                                  data=stdct1819)
summary(stdct1819.lm)
anova(stdct1819.lm)
anova(stdct1819.lmm)

#the R8 stage ####

stdct18r8 <- readr::read_csv("standcountedit2018R8.csv") #stand count, Vc stage, 2018, from ILeVO/stand count/stand count 2018-2
stdct19r8 <- readr::read_csv("standcountedit2019R8.csv")  #stand count, Vc stage. 2019, from ILeVO/sdsratingilevo/standcount 19

stdct18r8
stdct19r8

stdct18r8 <- stdct18r8 %>% select(2:6) %>% dplyr::rename(stdct=sum_stdct_ha) %>% mutate(year="2018")  
stdct18r8
stdct19r8 <- stdct19r8 %>%  select(2:6) %>% mutate(year="2019") 
str(stdct18r8)
str(stdct19r8)
stdct19r8$rotation

stdct19r8$plot <- as.character(stdct19r8$plot)
stdct19r8$block <- as.character(stdct19r8$block)
stdct18r8$plot <- as.character(stdct18r8$plot)
stdct18r8$block <- as.character(stdct18r8$block)

stdct18r8
stdct19r8

levels(stdct1819r8$rotation) <- c("S2","S3","S4")

stdct1819r8 <- bind_rows(stdct18r8, stdct19r8)
stdct1819r8
View(stdct1819r8)
str(stdct1819r8)
write.csv(stdct1819r8, "standcount1819r8.csv")
stdct1819r8.summ <- Rmisc::summarySE(stdct1819r8, "stdct", c("rotation","seedtrt","year"))
stdct1819r8.summ

#R8 data viz and stats ####
ggplot(stdct1819r8.summ, aes(rotation, stdct, fill=seedtrt))+
  geom_bar(stat="identity", position="dodge", color="#000000")+
  geom_errorbar(aes(ymin=stdct-se, ymax=stdct+se), width=.2, position=position_dodge(.9)) +
  geom_jitter(data=stdct1819r8, aes(rotation, stdct, stroke=.2), alpha=0.3)+
  scale_color_identity()+
  #geom_abline(y=0, face=2)+
  facet_grid(.~ year, labeller=label_both)+
  theme(text=element_text(size=14))+
  geom_text(position = position_dodge(0.9),vjust = -1.25,hjust=0.5, size=4, label=lblstdct1819r8, aes(fontface=2))+
  scale_y_continuous(labels = scales::comma)+
  #scale_x_discrete(labels=c("2-yr","3-yr","4-yr"))+
  ylab("stand count (plants/hectare)")+
  ggtitle("Stand count V1 - '18 &'19",
          subtitle="")+
  theme_bw()


library(agricolae)

stdct18r8.lm <- lm(stdct~rotation*seedtrt + block, data=subset(stdct1819r8, year=="2018"))
LSD.test(stdct18r8.lm, c("rotation","seedtrt"), console=TRUE)               

stdct19r8.lm <- lm(stdct~rotation*seedtrt + block, data=subset(stdct1819r8, year=="2019"))
LSD.test(stdct19r8.lm, c("rotation","seedtrt"), console=TRUE)           

lblstdct1819r8 <- c("ab","b","a","ab","a","ab",
                  "ab","b","ab","ab","ab","a")    

stdct1819r8.lmm <- lmerTest::lmer(stdct~ rotation+ seedtrt +rotation*seedtrt + (1|block:year) ,
                                 data=stdct1819r8)
summary(stdct1819r8.lm)
anova(stdct1819r8.lm)

