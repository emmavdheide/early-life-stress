#This file contains analysis of longest leaf length in the week immediately before mowing
#to try to get at the mechanism by which rosette damage causes earlier death

#load packages
library(multcompView)
library(emmeans)
library(multcomp)

#Combined####
#load data
LLL_combined<-read.csv("Combined Pre Mowing LLL Data.csv")

#Make treatment a factor
LLL_combined$Treatment<-as.factor(LLL_combined$Treatment)

#Make year a factor
LLL_combined$Year<-as.factor(LLL_combined$Year)

#Exclude treatments that include both early and late mow
LLL_filtered <- LLL_combined[!(LLL_combined$Treatment == "Rosette Damage/Early Mow/Late Mow"|LLL_combined$Treatment=="Early Mow/Late Mow"), ]

#run an ANOVA
anova_combined<-aov(lm(PreMowLLL~Treatment, data=LLL_combined))

anova_combined_filt<-aov(lm(PreMowLLL~Treatment, data=LLL_filtered))

#check residuals
plot(lm(PreMowLLL~Treatment, data=LLL_combined)) #looks fine

plot(lm(PreMowLLL~Treatment, data=LLL_filtered))

#summarize anova
summary(anova_combined)
#at least one group is significantly different

summary(anova_combined_filt)
#at least one group is significantly different

#Post-hoc comparisons
emmeans(anova_combined, specs = pairwise~Treatment)
cld(emmeans(anova_combined, specs = pairwise~Treatment), Letters=letters)

#RD+LM significantly shorter than LM
#RD+EM significantly shorter than EM
#RD+EM+LM significantly shorter than EM+LM

emmeans(anova_combined_filt, specs = pairwise~Treatment)
cld(emmeans(anova_combined_filt, specs = pairwise~Treatment), Letters=letters)
#RD+LM significantly shorter than LM
#RD+EM significantly shorter than EM

#Alternatively, t test rosette damage vs. no rosette damage
#separate rosette damaged and non-rosette damaged plants
#DATA IS SET UP DIFFERENTLY, SO THIS DOESN'T WORK YET
LLL_RD<-LLL_combined[LLL_combined$RD=="Y",]
LLL_NoRD<-LLL_combined[LLL_combined$RD=="N",]

t.test(LLL_RD$PreMowLLL, LLL_NoRD$PreMowLLL)
#Again, this shows that rosette damaged plants have shorter LLL before mowing

#And t-test RD+LM vs. RD+EM for effect of timing
LLL_RDLM<-LLL_combined[LLL_combined$Treatment=="Rosette Damage/Late Mow",]
LLL_RDEM<-LLL_combined[LLL_combined$Treatment=="Rosette Damage/Early Mow",]
t.test(LLL_RDEM$PreMowLLL, LLL_RDLM$PreMowLLL)
#This shows that RD+LM plants did not have significantly shorter LLL before mowing than RD+EM plants
  #One sided t-test is also not significant

#2025####
#load data
LLL2025<-read.csv("2025 Pre Mowing LLL Data.csv")

#Make treatment a factor
LLL2025$Treatment<-as.factor(LLL2025$Treatment)

#run an ANOVA
anova2025<-aov(lm(Pre.Mow.LLL~Treatment, data=LLL2025))

#check residuals
plot(lm(Pre.Mow.LLL~Treatment, data=LLL2025)) #looks fine

#summarize anova
summary(anova2025)
#at least one group is significantly different

#Post-hoc comparisons
emmeans(anova2025, specs = pairwise~Treatment)
cld(emmeans(anova2025, specs = pairwise~Treatment), Letters=letters)

#RD+LM significantly shorter than LM
#RD+EM significantly shorter than EM
#RD+EM+LM is not significantly shorter than EM+LM

#2024####
#load data
LLL2024<-read.csv("2024 Pre Mowing LLL Data.csv")

#Make treatment a factor
LLL2024$Treatment<-as.factor(LLL2024$Treatment)

#run an ANOVA
anova2024<-aov(lm(PreMowLLL~Treatment, data=LLL2024))

#check residuals
plot(lm(PreMowLLL~Treatment, data=LLL2024)) #looks fine

#summarize anova
summary(anova2024)
#at least one group is significantly different

#Post-hoc comparisons
emmeans(anova2024, specs = pairwise~Treatment)
cld(emmeans(anova2024, specs = pairwise~Treatment), Letters=letters)

#RD+LM significantly shorter than LM
#RD+EM is not significantly shorter than EM
#RD+EM+LM is not significantly shorter than EM+LM