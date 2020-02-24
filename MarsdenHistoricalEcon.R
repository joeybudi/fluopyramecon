library(tidyverse)
library(Rmisc)
getwd()
setwd("C:/Users/joshu/Box/ILeVO")
histMarsdenEcon <- read_csv("Plot by plot econ 2008-2018_2_JB.csv", na="NA")

histMarsdenEcon$Rotation <- as.factor(histMarsdenEcon$Rotation)
histMarsdenEcon$Block <- as.factor(histMarsdenEcon$Block)

#gather, flip/gather, crops
histRevenue<- histMarsdenEcon %>% 
  select(Year:GRevRotations)
colnames(histRevenue)[6:10] <- c("corn","soy","oat","alfalfa","rotation")  
histRevenue <- histRevenue %>% gather(6:10, key="crop",value="revenue")

histReturn<- histMarsdenEcon %>% 
  select(-c(GRevCorn:Land))
colnames(histReturn)[6:10] <- c("corn","soy","oat","alfalfa","rotation")  
histReturn <- histReturn %>% gather(6:10, key="crop",value="return")

#join revenue and return
histMarsdenEcon2 <- full_join(histRevenue, histReturn)
histMarsdenEcon2$crop <- factor(histMarsdenEcon2$crop , levels=c("corn","soy","oat","alfalfa","rotation"))
histMarsdenEcon2$Year <- factor(histMarsdenEcon2$Year)

#
hist.rota.crop.summ <- summarySE(histMarsdenEcon2, measurevar="return", groupvars=c("Rotation","crop","Year"), na.rm=TRUE)
hist.rota.crop.summ

#bar graphs
ggplot(hist.rota.crop.summ, 
       aes(x=crop, y=return, 
           fill=crop, 
           #color=Fungic, 
           label=sprintf("%0.1f", round(return, digits = 1)
                         ))) + #https://stackoverflow.com/questions/38369855/how-to-put-exact-number-of-decimal-places-on-label-ggplot-bar-chart
  geom_line(stat="identity", position="dodge", color="#000000", alpha=0.5) +
  geom_errorbar(aes(ymin=return-se, ymax=return+se), width=.2, position=position_dodge(.9)) +
  geom_jitter(data=hist.rota.crop.summ, 
              aes(x=crop, y=return, stroke=.5), 
              alpha=0.7)+ #put label on plot number
  #geom_label(position="nudge", label.size=.05, label.padding=.05)+
  geom_text(position = position_dodge(0.9),
            vjust = -3, hjust=-.1,
            size=4, 
            aes(fontface=2))+
  ylab('Return to land and management ($/acre)')+
  facet_grid(Rotation ~ Year, labeller=label_both)+
  theme(strip.background = element_rect(colour="black", fill="white", size=1.5, linetype="solid"))+
  theme_gray() #just need to label the x axis

#presented in line time series
  #filtered for soy and rotations
  
  #return
#cropnameslbl <- c('soy'="soybean",
 #              'rotation'="average: corn-soy-oat-alfalfa")

ggplot(data=hist.rota.crop.summ, 
       aes(x=Year, y=return, group=crop, color=crop))+
           #label=sprintf("%0.1f", round(return, digits = 1) #https://stackoverflow.com/questions/38369855/how-to-put-exact-number-of-decimal-places-on-label-ggplot-bar-chart
  geom_point(size=1)+
  geom_line(size=1) +
  geom_errorbar(aes(ymin=return-se, ymax=return+se), width=.8, position=position_dodge(0)) +
  geom_point(data=dplyr::filter(hist.rota.crop.summ, crop=="soy"|crop=="rotation"), size=2)+
  geom_line(data=dplyr::filter(hist.rota.crop.summ, crop=="soy"|crop=="rotation"), size=2)+
  facet_grid(.~Rotation)+
  ggtitle('Economic return - Marsden farm economics 2008-2018')+
  ylab('Return to land and management ($/acre)')+
  theme(text=element_text(size=14))+
  scale_x_discrete(breaks=c("2008","2010","2012","2014","2016","2018"))+
  #scale_color_manual("Rotation \nsystem", 
  #                   values=c("#F8766D","#00BA38","#619CFF"),
   #                  labels=c("2-year","3-year","4-year"))+
  theme_bw()


  #revenue
hist.rota.crop.reve.summ <- summarySE(histMarsdenEcon2, measurevar="revenue", groupvars=c("Rotation","crop","Year"), na.rm=TRUE)
ggplot(data=hist.rota.crop.reve.summ, aes(x=Year, y=revenue, group=crop, color=crop))+
  geom_point(size=1)+
  geom_line(size=1) +
  geom_errorbar(aes(ymin=revenue-se, ymax=revenue+se), width=.2, position=position_dodge(0)) +
  geom_point(data=dplyr::filter(hist.rota.crop.reve.summ, crop=="soy"|crop=="rotation"), size=2)+
  geom_line(data=dplyr::filter(hist.rota.crop.reve.summ, crop=="soy"|crop=="rotation"), size=2)+
  facet_grid(.~Rotation, labeller=label_both)+
  ggtitle('Gross Revenue - Marsden farm economics 2008-2018')+
  ylab('Gross Revenue ($/acre)')+
  theme(text=element_text(size=14), 
        axis.text.x= element_text(angle=45))+
  scale_x_discrete(breaks=c("2008","2010","2012","2014","2016","2018"))+
  scale_y_continuous(breaks=seq(0,2000,200))+
  theme_bw()

#return and revenue together
ggplot(hist.rota.crop.reve.summ, aes(x=Year, y=revenue, group=crop, color=crop))+
  geom_point()+
  geom_line() +
  geom_point(data=hist.rota.crop.summ, aes(Year,return))+
  geom_line(data=hist.rota.crop.summ, aes(Year, return))+
  geom_errorbar(aes(ymin=revenue-se, ymax=revenue+se), width=.2, position=position_dodge(0)) +
  geom_point(data=dplyr::filter(hist.rota.crop.reve.summ, crop=="soy"|crop=="rotation"), size=2)+
  geom_line(data=dplyr::filter(hist.rota.crop.reve.summ, crop=="soy"|crop=="rotation"), size=2)+
  facet_grid(.~Rotation, labeller=label_both)+
  ggtitle('Gross Revenue - Marsden farm economics 2008-2018')+
  ylab('Gross Revenue to land and management ($/acre)')+
  theme(text=element_text(size=14))+
  scale_x_discrete(breaks=c("2008","2010","2012","2014","2016","2018"))+
  scale_y_continuous(breaks=seq(0,2000,200))
