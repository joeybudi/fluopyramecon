library(tidyverse)
library(dplyr)
library(plyr)
library(agricolae)
library(readxl)
#stdct_phytox <- read_excel("U:/Ilevo seed trt/stand count_phytotoxicity_seedtrt_2018.xlsx")
#View(stdct_phytox)

sds_dis_rate18 <- read_excel("sds rating data 2018.xlsx", 
                             sheet = "data edit to r", na = "NA")
View(sds_dis_rate18)



sds_dis_rate18_L <- gather(sds_dis_rate18, key="identifier",value="dis_misc", DI_r34_073118:DX_r56_090718) 
View(sds_dis_rate18_L)

#separate identifier of all the disease identifier...then spread the DI DS DX DSX
basedOnDate <- sds_dis_rate18_L%>%
  separate(identifier, c("dis","seedtrt_row","date"), "_")%>%
  spread(dis,dis_misc) 
View(basedOnDate)

#summarise the 10 subplot quadrats, remove unnecessary columns, put name to ilevo vs base seedtrt
byPlot <- basedOnDate %>% group_by(Plot, seedtrt_seq, seedtrt_row, Blk, rotation, date) %>% summarise_if(is.numeric,mean,na.rm=TRUE) %>%
  select(-(Qdrt:std_ct_row6_R8)) %>%
  mutate(seedtrt=ifelse(seedtrt_seq=="BIB" & seedtrt_row=="r34" | seedtrt_seq=="IBB" & seedtrt_row=="r56", "ilevo","base"))
View(byPlot)

#rotation and seedtrt gets a column 
IleVRota18 <- byPlot %>% unite(rotaIlev,seedtrt,rotation,sep="-", remove=FALSE)
View(IleVRota18)
as.factor(seedtrt, rotation, Blk)

#gather std ct from column to rows
# sdsRateMarsd18 <- byPlot %>%
#   gather(key="std_ct_row",value="std_ct",std_ct_row34_R8:std_ct_row56_R8)
# View(sdsRateMarsd18)

#old codes just in case  
#filter(str_detect(identifier, "egDI_%")) %>%
#mutate(date=str_sub(identifier, -6,-1))

# ggplot(byPlot) +
#   geom_point(mapping=aes(x=date,y=DI, color=rotation)) +
#   geom_smooth(mapping=aes(x=date, y=DI, group=rotation)) +
#   ylab("SDS Incidence (%)")
as.factor(byPlot$rotation)
as.factor(byPlot$Blk)
as.factor(byPlot$seedtrt)

byPlotPeak <- byPlot %>% filter(date=="090718")
View(byPlotPeak)

#_______________
#disease incidence
summDIProgRota <- summarySE(byPlot, measurevar = c("DI"), groupvars=c("date","rotation"))
View(summDIProgRota)
summDIProgIlev <- summarySE(byPlot, measurevar = c("DI"), groupvars=c("date","seedtrt"))
View(summDIProgIlev)
summDIProgIlevRota <- summarySE(IleVRota18, measurevar = c("DI"), groupvars=c("date","rotaIlev"))
View(summDIProgIlevRota)
summDIProgIlevRotaBar <- summarySE(byPlot, measurevar=c("DI"), groupvars=c("date","seedtrt","rotation"))%>% filter(date=="090718")
View(summDIProgIlevRotaBar)


#DI progression by rotation (n=6)
ggplot(summDIProgRota, aes(x=date, y=DI, group=rotation)) +
  geom_point(aes(color=rotation))+
  geom_errorbar(aes(ymin=DI-se, ymax=DI+se), width=.2)+
  geom_smooth( aes(color=rotation), se=FALSE, method="loess") + 
  #scale_colour_brewer(type = "div", palette = "Spectral") +
  ylab("SDS Incidence (%)")+
  expand_limits(y=30)+
  scale_x_discrete(name="date/DOY",labels=c("073118\n212","080918\n221","081718\n229","082918\n241","090718\n250"))+
  theme( axis.line = element_line(colour = "black",size = 1, linetype = "solid"))

#DI progression by seedtrt (n=9)
ggplot(summDIProgIlev, aes(x=date, y=DI, group=seedtrt)) +
  geom_point(aes(color=seedtrt))+
  geom_errorbar(aes(ymin=DI-se, ymax=DI+se), width=.2)+
  geom_smooth( aes(color=seedtrt), se=FALSE, method="loess") + 
  ylab("SDS Incidence (%)")+
  expand_limits(y=30)+
  scale_x_discrete(name="date/DOY",labels=c("073118\n212","080918\n221","081718\n229","082918\n241","090718\n250"))+
  theme( axis.line = element_line(colour = "black",size = 1, linetype = "solid"))

#DI progression by seedtrt and rotation (n=3)
ggplot(summDIProgIlevRota, aes(x=date, y=DI, group=rotaIlev)) +
  geom_point(aes(color=rotaIlev))+
  geom_errorbar(aes(ymin=DI-se, ymax=DI+se), width=.2)+
  geom_smooth( aes(color=rotaIlev), se=FALSE, method="loess") + 
  ylab("SDS Incidence (%)")+
  expand_limits(y=30)+
  scale_x_discrete(name="date/DOY",labels=c("073118\n212","080918\n221","081718\n229","082918\n241","090718\n250"))+
  theme( axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  scale_color_manual(values=c("#FF0000","#FF6600","#FFCC00", "#6633FF","#3399FF","#00CCFF"))

#barplot for last date only
ggplot(summDIProgIlevRotaBar, aes(x=rotation,y=DI, fill=seedtrt)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=DI-se, ymax=DI+se), width=.2, position=position_dodge(.9))+
  ylab("SDS Incidence (%)")+
  expand_limits(y=30)+
  theme(axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  scale_fill_manual(values=c("#FF6600","#3399FF"))+
  labs(title= "09/07/18")


mod.seedtrt.rota<-aov(DI~seedtrt*rotation+Blk, data=byPlotPeak)
summary(mod.seedtrt.rota)
LSD.test(mod.seedtrt.rota,c("rotation","seedtrt"),console=TRUE)


#___________________________
#disease severity
summDSProgRota <- summarySE(byPlot, measurevar = c("DS"), groupvars=c("date","rotation"))
View(summDIProgRota)
summDSProgIlev <- summarySE(byPlot, measurevar = c("DS"), groupvars=c("date","seedtrt"))
View(summDSProgIlev)
summDSProgIlevRota <- summarySE(IleVRota18, measurevar = c("DS"), groupvars=c("date","rotaIlev"))
View(summDSProgIlevRota)
summDSProgIlevRotaBar <- summarySE(byPlot, measurevar=c("DS"), groupvars=c("date","seedtrt","rotation"))%>% filter(date=="090718")
View(summDSProgIlevRotaBar)

#DS progression by rotation (n=6)
ggplot(summDSProgRota, aes(x=date, y=DS, group=rotation)) +
  geom_point(aes(color=rotation))+
  geom_errorbar(aes(ymin=DS-se, ymax=DS+se), width=.2)+
  geom_smooth( aes(color=rotation), se=FALSE, method="loess") + 
  ylab("SDS Severity (%)")+
  expand_limits(y=30)+
  scale_x_discrete(name="date/DOY",labels=c("073118\n212","080918\n221","081718\n229","082918\n241","090718\n250"))+
  theme( axis.line = element_line(colour = "black",size = 1, linetype = "solid"))

#DS progression by seedtrt and rotation (n=3)
ggplot(summDSProgIlevRota, aes(x=date, y=DS, group=rotaIlev)) +
  geom_point(aes(color=rotaIlev))+
  geom_errorbar(aes(ymin=DS-se, ymax=DS+se), width=.2)+
  geom_smooth( aes(color=rotaIlev), se=FALSE, method="loess") + 
  ylab("SDS Severity (%)")+
  expand_limits(y=30)+
  scale_x_discrete(name="date/DOY",labels=c("073118\n212","080918\n221","081718\n229","082918\n241","090718\n250"))+
  theme(axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  scale_color_manual(values=c("#FF0000","#FF6600","#FFCC00", "#6633FF","#3399FF","#00CCFF"))

#barplot for last date only DS
ggplot(summDSProgIlevRotaBar, aes(x=rotation,y=DS, fill=seedtrt)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=DS-se, ymax=DS+se), width=.2, position=position_dodge(.9))+
  ylab("SDS Severity (%)")+
  expand_limits(y=30)+
  theme(axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  scale_fill_manual(values=c("#FF6600","#3399FF"))+
  labs(title= "09/07/18")

mod.seedtrt.rota.DS<-aov(DS~seedtrt*rotation+Blk, data=byPlotPeak)
summary(mod.seedtrt.rota.DS)
LSD.test(mod.seedtrt.rota.DS,c("rotation","seedtrt"),console=TRUE)
#___________________________
#foliar disease index
summDXProgRota <- summarySE(byPlot, measurevar = c("DX"), groupvars=c("date","rotation"))
View(summDXProgRota)
summDXProgIlev <- summarySE(byPlot, measurevar = c("DX"), groupvars=c("date","seedtrt"))
View(summDXProgIlev)
summDXProgIlevRota <- summarySE(IleVRota18, measurevar = c("DX"), groupvars=c("date","rotaIlev"))
View(summDXProgIlevRota)
summDXProgIlevRotaBar <- summarySE(byPlot, measurevar=c("DX"), groupvars=c("date","seedtrt","rotation"))%>% filter(date=="090718")
View(summDXProgIlevRotaBar)

#DX progression by rotation (n=6)
ggplot(summDXProgRota, aes(x=date, y=DX, group=rotation)) +
  geom_point(aes(color=rotation))+
  geom_errorbar(aes(ymin=DX-se, ymax=DX+se), width=.2)+
  geom_smooth( aes(color=rotation), se=FALSE, method="loess") + 
  ylab("SDS Foliar disease index (1-100)")+
  expand_limits(y=30)+
  scale_x_discrete(name="date/DOY",labels=c("073118\n212","080918\n221","081718\n229","082918\n241","090718\n250"))+
  theme( axis.line = element_line(colour = "black",size = 1, linetype = "solid"))

#DX progression by seedtrt (n=9)
ggplot(summDXProgIlev, aes(x=date, y=DX, group=seedtrt)) +
  geom_point(aes(color=seedtrt))+
  geom_errorbar(aes(ymin=DX-se, ymax=DX+se), width=.2)+
  geom_smooth( aes(color=seedtrt), se=FALSE, method="loess") + 
  #scale_colour_brewer(type = "div", palette = "Spectral") +
  ylab("SDS Foliar disease index (1-100)")+
  expand_limits(y=30)+
  scale_x_discrete(name="date/DOY",labels=c("073118\n212","080918\n221","081718\n229","082918\n241","090718\n250"))+
  theme( axis.line = element_line(colour = "black",size = 1, linetype = "solid"))

#DS progression by seedtrt and rotation (n=3)
ggplot(summDXProgIlevRota, aes(x=date, y=DX, group=rotaIlev)) +
  geom_point(aes(color=rotaIlev))+
  geom_errorbar(aes(ymin=DX-se, ymax=DX+se), width=.2)+
  geom_smooth( aes(color=rotaIlev), se=FALSE, method="loess") + 
  ylab("SDS Foliar disease index (1-100)")+
  expand_limits(y=30)+
  scale_x_discrete(name="date/DOY",labels=c("073118\n212","080918\n221","081718\n229","082918\n241","090718\n250"))+
  theme(axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  scale_color_manual(values=c("#FF0000","#FF6600","#FFCC00", "#6633FF","#3399FF","#00CCFF"))

#barplot for last date only DX
ggplot(summDXProgIlevRotaBar, aes(x=rotation,y=DX, fill=seedtrt)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=DX-se, ymax=DX+se), width=.2, position=position_dodge(.9))+
  ylab("Foliar disease index (1-100)")+
  expand_limits(y=30)+
  theme(axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  scale_fill_manual(values=c("#FF6600","#3399FF"))+
  labs(title= "09/07/18")

mod.seedtrt.rota.DX<-aov(DX~seedtrt*rotation+Blk, data=byPlotPeak)
summary(mod.seedtrt.rota.DX)
LSD.test(mod.seedtrt.rota.DX,c("rotation","seedtrt"),console=TRUE)


#_________________

#example 
summDI34_d<-summarySE(sds_dis_rate18,measurevar = c("DI_%_34_090718"),groupvars=c("rotation"),na.rm = "TRUE")
View(summDI34_d)


#statistics
as.factor(Rows1$rotation)

mod.rota.stdct <- aov(std_ct ~ rotation+block, data=Rows1)
summary(mod.rota.stdct)

install.packages("agricolae")
library(agricolae)

cv.model(mod.rota.stdct)
LSD.test(mod.rota.stdct,"rotation", console=TRUE)

#byseedtrt
mod.ilevo.stdct<- aov(std_ct~seedtrt+block, data=Rows1)
summary(mod.ilevo.stdct)
cv.model(mod.ilevo.stdct)
LSD.test(mod.ilevo.stdct,"seedtrt",console=TRUE)

#rotation and seed trt as factors
mod.ilevorota.stdct <- aov(std_ct~rotation*seedtrt+block, data=Rows1)
summary(mod.ilevorota.stdct)
cv.model(mod.ilevorota.stdct)
LSD.test(mod.ilevorota.stdct, c("rotation","seedtrt"), console=TRUE)







# 
# #per rotation basis
# S2<- filter(stdct_phytox, rotation=="S2")
# View(S2)
# S4 <- filter(stdct_phytox, rotation=="S4")
# View(S4)
# S3 <- filter(stdct_phytox, rotation=="S3")
# View(S3)
# ggplot(data=S2) + 
#   geom_freqpoly(mapping=aes(x=std_ct3), position="identity", stat="bin", color="blue") +
#   geom_freqpoly(mapping=aes(x=std_ct4), position="identity", stat="bin", color="blue") +
#   geom_freqpoly(mapping=aes(x=std_ct5), position="identity", stat="bin") +
#   geom_freqpoly(mapping=aes(x=std_ct6), position="identity", stat="bin")
# 
# View(S4)
# ggplot(data=S4) + 
#   geom_histogram(mapping=aes(x=std_ct5), position="identity", stat="bin", fill="red") +
#   geom_histogram(mapping=aes(x=std_ct6), position="identity", stat="bin", fill="dark red")
# 
# ggplot(data=S4) +
#   geom_histogram(mapping=aes(x=std_ct3), position="identity", stat="bin", fill="green") +
#   geom_histogram(mapping=aes(x=std_ct4), position="identity", stat="bin", fill="dark green")
# 
# 
# ggplot(data=stdct_phytox) + 
#   geom_histogram(mapping=aes(x=std_ct4), binwidth=0.75)
# 
# TwoRowsAvg34 <- gather(stdct_phytox, "rows34", "std_ct34", 5:6)
# View(TwoRowsAvg34)
# 
# TwoRowsAvg <- gather(TwoRowsAvg34, "rows56", "std_ct56", 5:6)
# View(TwoRowsAvg)
# 
# #gathering rows 34, 56 to same column
# RowsAvg <- gather(TwoRowsAvg, "rows", "std_ct", 10,12) 
# View(RowsAvg)
# 
# Rows <- select(RowsAvg, plot, block, rotation, subplot, rows, std_ct,IBB, phytox_ct, phytox_sev)
# View(Rows)
# 
# #mutate seed trt column classification BIB=Ilevo on rows 34
# Rows1 <- Rows %>% 
#   mutate(Rows,seedtrt=ifelse(IBB=="BIB" & rows=="std_ct34" | IBB=="IBB" & rows=="std_ct56", "ilevo","base"))
# View(Rows1)
# 
# as.numeric(Rows$std_ct)
# as.factor(Rows$plot)
# as.factor(Rows$block)
# detach(package:plyr)
# Rows1 %>% 
#   group_by(plot) %>%
#   summarise(avg_stdct=mean(std_ct))
# 
# Rows1 %>% 
#   group_by(IBB)%>%
#   summarise(avg_stdct_ilevo=mean(std_ct))
# 
# Rows1 %>% 
#   group_by(rotation)%>%
#   summarise(avg_stdct_ilevo=mean(std_ct))
# 
# Rows1 %>%
#   group_by(block)%>%
#   summarise(avg_stdct_ilevo=mean(std_ct))
# 
# summRowsIBB<-summarySE(Rows1,measurevar = "std_ct",groupvars=c("seedtrt","rotation"),na.rm = "TRUE")
# View(summRowsIBB)
# 
# summRowsIlevo <- summarySE(Rows1, measurevar = "std_ct", groupvars="seedtrt")
# View(summRowsIlevo)
# 
# summRowsRota<-summarySE(Rows1,measurevar = "std_ct",groupvars=c("rotation"),na.rm = "TRUE")
# View(summRowsRota)
# 
# ggplot(summRowsIBB, aes(x=seedtrt, y=std_ct, fill=rotation)) +
#   geom_bar(position="dodge", stat="identity") +
#   geom_errorbar(aes(ymin=std_ct-se, ymax=std_ct+se), width=.2, position=position_dodge(.9))+
#   scale_y_continuous(breaks=c(2,4,6,8,10))
# 
# ggplot(summRowsRota, aes(x=rotation, y=std_ct, fill=rotation)) +
#   geom_bar(position="dodge", stat="identity") +
#   geom_errorbar(aes(ymin=std_ct-se, ymax=std_ct+se), width=.2, position=position_dodge(.9))+
#   scale_y_continuous(breaks=c(2,4,6,8,10,12))
# 
# ggplot(summRowsIlevo, aes(x=seedtrt, y=std_ct, fill=seedtrt)) +
#   geom_bar(position="dodge", stat="identity") +
#   geom_errorbar(aes(ymin=std_ct-se, ymax=std_ct+se), width=.2, position=position_dodge(.9))+
#   scale_y_continuous(breaks=c(2,4,6,8,10,12))