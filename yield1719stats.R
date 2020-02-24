#yield 1719 model stats
getwd()
yield <- readr::read_csv("yields SB wwo ilevo.csv")

yield

#initial checks
yield.lm <- lm(yield~rotation*seedtrt + block, data=yield)
plot(predict(yield.lm), resid(yield.lm))

#linear, mixed model
library(lme4)
library(emmeans)
library(lmerTest)
library(pbkrtest)

ylm <- lm(yield ~ rotation + seedtrt +  rotation:seedtrt + block + year, data=yield)
ylm2 <- lm(yield ~ rotation + seedtrt + rotation:seedtrt + block:year, data=yield)
ylm3 <- lm(yield ~ rotation + seedtrt + block:year, data=yield)

summary(aov(ylm)) #p for seed trt=0.06
summary(aov(ylm2)) #p for seed trt=0.03
TukeyHSD(aov(ylm))

  #mixed model with lme4
ymm1 <- lmer(yield ~ rotation + seedtrt + rotation:seedtrt + (1|block:year), data=yield)
ymm2 <- lmer(yield ~ rotation + seedtrt + rotation:seedtrt +  (1|year) + (1|block) , data=yield)
ymm2b <- lmer(yield ~ rotation + seedtrt + rotation:seedtrt +  (1|year) + block , data=yield)
ymm2c <- lmer(yield ~ rotation + seedtrt + rotation:seedtrt +  year + (1|block) , data=yield)
ymm3 <- lmer(yield ~ rotation + seedtrt + (1|year), data=yield)

summary(ymm1)

  #using lmerTest package
ymm1b <- lmerTest::lmer(yield ~ rotation + seedtrt + rotation:seedtrt + (1|block:year), data=yield)
summary(ymm1b)
anova(ymm1b)

step(ymm1) #suggests (1|block:year) error term
step(ymm2) #suggests (1|year) error term
step(ymm3)

anova(ymm1, ymm2) #there is difference between ymm1 and ymm2

emm.ymm1 <- emmeans(ymm1, c("rotation","seedtrt"))
emm.ymm1
joint_tests(emm.ymm1)
pairs(emm.ymm1)
confint(pairs(emm.ymm1))

lmerTest::plot.ls_means(step(ymm1))

# LSD test, agricolae
library(agricolae)
#block effect is usually strong, so it does not get treated as random factor
yield17.lm <- lm(yield~rotation*seedtrt + block, data=subset(yield, year=="2017"))
LSD.test(yield17.lm, c("rotation","seedtrt"), console=TRUE)   
summary(aov(yield17.lm))

yield18.lm <- lm(yield~rotation*seedtrt + block, data=subset(yield, year=="2018"))
LSD.test(yield18.lm, c("rotation","seedtrt"), console=TRUE)               
summary(aov(yield18.lm))

yield19.lm <- lm(yield~rotation*seedtrt + block, data=subset(yield, year=="2019"))
LSD.test(yield19.lm, c("rotation","seedtrt"), console=TRUE)               
summary(aov(yield19.lm))

#print(yield.lmer, cor=F)
# quantile(residuals(yield.lmer, "pearson", scaled=TRUE))
# summary(yield.lmer)
# nobs(yield.lmer)
# ngrps(yield.lmer)
# sigma(yield.lmer)
# VarCorr(yield.lmer)
# fixef(yield.lmer)
# coef(summary(yield.lmer))

