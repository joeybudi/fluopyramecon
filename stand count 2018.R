library(tidyverse)
library(dplyr)
library(plyr)
stdct_phytox <- read_excel("stand count_phytotoxicity_seedtrt_2018.xlsx", 
                           sheet = "Sheet1")
View(stdct_phytox)

#sds_dis_rate18 <- read_excel("sds rating data 2018.xlsx", 
 #                            sheet = "data edit", na = "NA")

#example std ct
summStdCt<-summarySE(stdct_phytox,measurevar = "std_ct3",groupvars=c("rotation"),na.rm = "TRUE")
View(summStdCt)

#per rotation basis
S2<- filter(stdct_phytox, rotation=="S2")
View(S2)
S4 <- filter(stdct_phytox, rotation=="S4")
View(S4)
S3 <- filter(stdct_phytox, rotation=="S3")
View(S3)
ggplot(data=S2) + 
  geom_freqpoly(mapping=aes(x=std_ct3), position="identity", stat="bin", color="blue") +
  geom_freqpoly(mapping=aes(x=std_ct4), position="identity", stat="bin", color="blue") +
  geom_freqpoly(mapping=aes(x=std_ct5), position="identity", stat="bin") +
  geom_freqpoly(mapping=aes(x=std_ct6), position="identity", stat="bin")
  
View(S4)
ggplot(data=S4) + 
  geom_histogram(mapping=aes(x=std_ct5), position="identity", stat="bin", fill="red") +
  geom_histogram(mapping=aes(x=std_ct6), position="identity", stat="bin", fill="dark red")
 
ggplot(data=S4) +
  geom_histogram(mapping=aes(x=std_ct3), position="identity", stat="bin", fill="green") +
  geom_histogram(mapping=aes(x=std_ct4), position="identity", stat="bin", fill="dark green")
  
  
ggplot(data=stdct_phytox) + 
  geom_histogram(mapping=aes(x=std_ct4), binwidth=0.75)

TwoRowsAvg34 <- gather(stdct_phytox, "rows34", "std_ct34", 5:6)
View(TwoRowsAvg34)

TwoRowsAvg <- gather(TwoRowsAvg34, "rows56", "std_ct56", 5:6)
View(TwoRowsAvg)

#gathering rows 34, 56 to same column
RowsAvg <- gather(TwoRowsAvg, "rows", "std_ct", 10,12) 
View(RowsAvg)

Rows <- select(RowsAvg, plot, block, rotation, subplot, rows, std_ct,IBB, phytox_ct, phytox_sev)
View(Rows)

#mutate seed trt column classification BIB=Ilevo on rows 34
Rows1 <- Rows %>% 
  mutate(Rows,seedtrt=ifelse(IBB=="BIB" & rows=="std_ct34" | IBB=="IBB" & rows=="std_ct56", "ilevo","base"))
View(Rows1)

as.numeric(Rows$std_ct)
as.factor(Rows$plot)
as.factor(Rows$block)
detach(package:plyr)
Rows1 %>% 
  group_by(plot) %>%
  summarise(avg_stdct=mean(std_ct))

Rows1 %>% 
  group_by(IBB)%>%
  summarise(avg_stdct_ilevo=mean(std_ct))

Rows1 %>% 
  group_by(rotation)%>%
  summarise(avg_stdct_ilevo=mean(std_ct))

Rows1 %>%
  group_by(block)%>%
  summarise(avg_stdct_ilevo=mean(std_ct))

summRowsIBB<-summarySE(Rows1,measurevar = "std_ct",groupvars=c("seedtrt","rotation"),na.rm = "TRUE")
View(summRowsIBB)

summRowsIlevo <- summarySE(Rows1, measurevar = "std_ct", groupvars="seedtrt")
View(summRowsIlevo)

summRowsRota<-summarySE(Rows1,measurevar = "std_ct",groupvars=c("rotation"),na.rm = "TRUE")
View(summRowsRota)

ggplot(summRowsIBB, aes(x=seedtrt, y=std_ct, fill=rotation)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=std_ct-se, ymax=std_ct+se), width=.2, position=position_dodge(.9))+
  scale_y_continuous(breaks=c(2,4,6,8,10))+
  ylab("Stand count/m at V1")+
  xlab("Seed treatment")+
  theme(axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  theme(text = element_text(size=14))


ggplot(summRowsRota, aes(x=rotation, y=std_ct, fill=rotation)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=std_ct-se, ymax=std_ct+se), width=.2, position=position_dodge(.9))+
  scale_y_continuous(breaks=c(2,4,6,8,10,12))

ggplot(summRowsIlevo, aes(x=seedtrt, y=std_ct, fill=seedtrt)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=std_ct-se, ymax=std_ct+se), width=.2, position=position_dodge(.9))+
  scale_y_continuous(breaks=c(2,4,6,8,10,12))

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

#__________________________________________________
#STAND COUNT R8

stdct_phytox_R8 <- read_excel("stand count_phytotoxicity_seedtrt_2018.xlsx", 
                           sheet = "stdct_R8")


TwoRowsAvg34b <- gather(stdct_phytox, "rows34", "std_ct34", 5:6)
View(TwoRowsAvg34b)

TwoRowsAvgb <- gather(TwoRowsAvg34b, "rows56", "std_ct56", 5:6)
View(TwoRowsAvgb)

#gathering rows 34, 56 to same column
RowsAvgb <- gather(TwoRowsAvgb, "rows", "std_ct", 10,12) 
View(RowsAvgb)

Rowsb <- select(RowsAvg, plot, block, rotation, subplot, rows, std_ct,IBB, phytox_ct, phytox_sev)
View(Rows)



#mutate seed trt column classification BIB=Ilevo on rows 34
Rows1b <- Rowsb %>% 
  mutate(Rowsb,seedtrt=ifelse(IBB=="BIB" & rows=="std_ct34" | IBB=="IBB" & rows=="std_ct56", "ilevo","base"))
View(Rows1)

as.numeric(Rowsb$std_ct)
as.factor(Rowsb$plot)
as.factor(Rowsb$block)
detach(package:plyr)
Rows1b %>% 
  group_by(plot) %>%
  summarise(avg_stdctb=mean(std_ct))

Rows1 %>% 
  group_by(IBB)%>%
  summarise(avg_stdct_ilevob=mean(std_ct))

Rows1 %>% 
  group_by(rotation)%>%
  summarise(avg_stdct_ilevob=mean(std_ct))

Rows1 %>%
  group_by(block)%>%
  summarise(avg_stdct_ilevob=mean(std_ct))

summRowsIBBb<-summarySE(Rows1,measurevar = "std_ct",groupvars=c("seedtrt","rotation"),na.rm = "TRUE")
View(summRowsIBBb)

summRowsIlevob <- summarySE(Rows1, measurevar = "std_ct", groupvars="seedtrt")
View(summRowsIlevob)

summRowsRotab<-summarySE(Rows1,measurevar = "std_ct",groupvars=c("rotation"),na.rm = "TRUE")
View(summRowsRotab)

ggplot(summRowsIBBb, aes(x=seedtrt, y=std_ct, fill=rotation)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=std_ct-se, ymax=std_ct+se), width=.2, position=position_dodge(.9))+
  scale_y_continuous(breaks=c(2,4,6,8,10))+
  ylab("Stand count/m at R8")+
  xlab("Seed treatment")+
  theme(axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  theme(text = element_text(size=14))
View(summRowsIBB)


ggplot(summRowsRotab, aes(x=rotation, y=std_ct, fill=rotation)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=std_ct-se, ymax=std_ct+se), width=.2, position=position_dodge(.9))+
  scale_y_continuous(breaks=c(2,4,6,8,10,12))

ggplot(summRowsIlevob, aes(x=seedtrt, y=std_ct, fill=seedtrt)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=std_ct-se, ymax=std_ct+se), width=.2, position=position_dodge(.9))+
  scale_y_continuous(breaks=c(2,4,6,8,10,12))

#statistics
as.factor(Rows1b$rotation)

mod.rota.stdctb <- aov(std_ct ~ rotation+block, data=Rows1)
summary(mod.rota.stdctb)

install.packages("agricolae")
library(agricolae)

cv.model(mod.rota.stdctb)
LSD.test(mod.rota.stdctb,"rotation", console=TRUE)

#byseedtrt
mod.ilevo.stdctb<- aov(std_ct~seedtrt+block, data=Rows1b)
summary(mod.ilevo.stdctb)
cv.model(mod.ilevo.stdctb)
LSD.test(mod.ilevo.stdctb,"seedtrt",console=TRUE)

#rotation and seed trt as factors
mod.ilevorota.stdctb <- aov(std_ct~rotation*seedtrt+block, data=Rows1)
summary(mod.ilevorota.stdctb)
cv.model(mod.ilevorota.stdctb)
LSD.test(mod.ilevorota.stdctb, c("rotation","seedtrt"), console=TRUE)





