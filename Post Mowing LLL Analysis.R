#This file contains analysis of longest leaf length in the week immediately after mowing

#load packages
library(multcompView)
library(emmeans)
library(multcomp)

#Combined####
#load data
LLL_combined<-read.csv("Combined Post Mowing LLL Data.csv")

#Make treatment a factor
LLL_combined$Treatment<-as.factor(LLL_combined$Treatment)

#Make year a factor
LLL_combined$Year<-as.factor(LLL_combined$Year)

#Exclude treatments that include both early and late mow
LLL_filtered <- LLL_combined[!(LLL_combined$Treatment == "Rosette Damage/Early Mow/Late Mow"|LLL_combined$Treatment=="Early Mow/Late Mow"), ]

#run an ANOVA
anova_combined_filt<-aov(lm(PostMowLLL~Treatment, data=LLL_filtered))

#check residuals
plot(lm(PostMowLLL~Treatment, data=LLL_filtered))

#summarize anova
summary(anova_combined_filt)
#at least one group is significantly different

#Post-hoc comparisons
emmeans(anova_combined_filt, specs = pairwise~Treatment)
cld(emmeans(anova_combined_filt, specs = pairwise~Treatment), Letters=letters)
#RD+LM significantly shorter than LM
#RD+EM significantly shorter than EM

#2025####
#load data
LLL2025<-read.csv("2025 Post Mowing LLL Data.csv")

#Make treatment a factor
LLL2025$Treatment<-as.factor(LLL2025$Treatment)

#Exclude treatments that include both early and late mow
LLL2025_filtered <- LLL2025[!(LLL2025$Treatment == "Rosette Damage/Early Mow/Late Mow"|LLL2025$Treatment=="Early Mow/Late Mow"), ]

#run an ANOVA
anova2025<-aov(lm(Post.Mow.LLL~Treatment, data=LLL2025_filtered))

#check residuals
plot(lm(Post.Mow.LLL~Treatment, data=LLL2025_filtered)) #looks fine

#summarize anova
summary(anova2025)
#at least one group is significantly different

#Post-hoc comparisons
emmeans(anova2025, specs = pairwise~Treatment)
cld(emmeans(anova2025, specs = pairwise~Treatment), Letters=letters)

#RD+LM significantly shorter than LM
#RD+EM significantly shorter than EM

#2024####
#load data
LLL2024<-read.csv("2024 Post Mowing LLL Data.csv")

#Make treatment a factor
LLL2024$Treatment<-as.factor(LLL2024$Treatment)

#Exclude treatments that include both early and late mow
LLL2024_filtered <- LLL2024[!(LLL2024$Treatment == "Rosette Damage/Early Mow/Late Mow"|LLL2024$Treatment=="Early Mow/Late Mow"), ]

#run an ANOVA
anova2024<-aov(lm(PostMowLLL~Treatment, data=LLL2024_filtered))

#check residuals
plot(lm(PostMowLLL~Treatment, data=LLL2024_filtered)) #looks fine

#summarize anova
summary(anova2024)
#at least one group is significantly different

#Post-hoc comparisons
emmeans(anova2024, specs = pairwise~Treatment)
cld(emmeans(anova2024, specs = pairwise~Treatment), Letters=letters)

#RD+LM significantly shorter than LM
#RD+EM significantly shorter than EM
