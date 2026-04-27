#This file contains the life history data analysis for Rosette Damage x Mowing Experiments
#Including analyses of single year data and of combined data

#Load Packages####
library(lme4)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(ggplot2)
library(dplyr)
library(multcomp)
library(ggpattern)
library(FSA)
library(rcompanion)
library(svglite)

#Load Data####
#And make data types as desired
dat_combined <- read.csv("Combined ELS Data.csv")
dat_combined$Year<-as.factor(dat_combined$Year)
dat_combined$Block <- as.factor(dat_combined$Block)
dat_combined$Treatment <- as.factor(dat_combined$Treatment)
dat_combined$RosDam <- as.factor(dat_combined$RosDam)
dat_combined$EarlyMow <- as.factor(dat_combined$EarlyMow)
dat_combined$LateMow <- as.factor(dat_combined$LateMow)

dat_2025 <- read.csv("2025 ELS Data.csv")
dat_2025$Block <- as.factor(dat_2025$Block)
dat_2025$Treatment <- as.factor(dat_2025$Treatment)
dat_2025$RosDam <- as.factor(dat_2025$RosDam)
dat_2025$EarlyMow <- as.factor(dat_2025$EarlyMow)
dat_2025$LateMow <- as.factor(dat_2025$LateMow)

dat_2024 <- read.csv("2024 ELS Data.csv")
dat_2024$Block <- as.factor(dat_2024$Block)
dat_2024$Treatment <- as.factor(dat_2024$Treatment)
dat_2024$RosDam <- as.factor(dat_2024$RosDam)
dat_2024$EarlyMow <- as.factor(dat_2024$EarlyMow)
dat_2024$LateMow <- as.factor(dat_2024$LateMow)

#get pixel dimensions I want to use for graphics export
dims<-dev.size("px") 

#Flowering Date####
#Analysis of date of first flowering (day # = day of the experiment) 
#defined as the date on which each individual plant had its first anthesed capitulum 
#after that plant had received all physical damage treatments.

##Combined####
#Fit model, with block nested within year
fitFD_combined <- glmmTMB(FloweringDate_doe ~ RosDam*EarlyMow*LateMow + LLLSpring + (1|Year/Block), data = dat_combined, family = "gaussian")
summary(fitFD_combined)

#check residuals
sim.fitFD_combined<-simulateResiduals(fitFD_combined)
plot(sim.fitFD_combined) #not perfect, but okay (#poisson is similar)

#calculate treatment means (estimated marginal means) and pairwise comparisons by treatment
emmFD_combined <- emmeans(fitFD_combined, specs=pairwise~RosDam*EarlyMow*LateMow, type="response")
emmFD_combined

#Put emmeans output into a data frame with compact letter display for pairwise comparisons
FlDate_combined<-as.data.frame(cld(emmFD_combined, Letters=letters))
#Add Treatment Name to Data Frame
FlDate_combined<-mutate(FlDate_combined, Treatment=case_when(
  RosDam == "n" & EarlyMow == "n" & LateMow == "n" ~ "Control",
  RosDam == "y" & EarlyMow == "n" & LateMow == "n" ~ "RD",
  RosDam == "y" & EarlyMow == "y" & LateMow == "n" ~ "RD+EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "n" ~ "EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "y" ~ "EM+LM",
  RosDam == "n" & EarlyMow == "n" & LateMow == "y" ~ "LM",
  RosDam == "y" & EarlyMow == "y" & LateMow == "y" ~ "RD+EM+LM",
  RosDam == "y" & EarlyMow == "n" & LateMow == "y" ~ "RD+LM",
))

#make a plot
FlDate_combinedPlot<-ggplot(FlDate_combined,aes(y=emmean, x=Treatment))+
  geom_bar_pattern(stat = "identity", aes(fill=Treatment, pattern=Treatment), pattern_fill = "black",
                   pattern_density = 0.05,
                   pattern_spacing = 0.05,
                   pattern_angle = 45,
                   pattern_key_scale_factor = 0.6)+
  coord_flip()+
  scale_x_discrete(limits = rev) +
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(legend.position = "none", panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.x = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30))+
  labs(x="Treatment", y="Day of Experiment")+
  geom_text(aes(label = .group, y = upper.CL + 5), #+ number specifies how high the cld appears
            position = position_dodge(width = 0.75), size=7)+
  ylim(0,90)+
  ggtitle("Flowering Date")+
  geom_errorbar(aes(x=Treatment, ymin=lower.CL, ymax=upper.CL))
FlDate_combinedPlot

#ggsave("ELS Figure 5.svg",
    #plot=FlDate_combinedPlot,
    #width=dims[1],
    #height=dims[2],
    #units="px",
    #dpi=72)

##2025####
#fit model and summarize
fitFD2025 <- glmmTMB(FloweringDate_doe ~ RosDam*EarlyMow*LateMow + LLLApr30 + (1|Block), data = dat_2025, family = "gaussian", control = glmmTMBControl(optimizer = optim, optArgs=list(method="BFGS")))
summary(fitFD2025)

#check residuals
sim.fitFD2025<-simulateResiduals(fitFD2025)
plot(sim.fitFD2025) #visual inspection of residuals: looks good

#calculate treatment means (estimated marginal means) and pairwise comparisons by treatment
emmFD2025 <- emmeans(fitFD2025, specs=~RosDam*EarlyMow*LateMow, type="response")
emmFD2025

#Put emmeans output into a data frame along with compact letter display for pairwise comparisons
FlDate2025<-as.data.frame(cld(emmFD2025, Letters=letters))

#Add Treatment Name to Data Frame
FlDate2025<-mutate(FlDate2025, Treatment=case_when(
  RosDam == "n" & EarlyMow == "n" & LateMow == "n" ~ "Control",
  RosDam == "y" & EarlyMow == "n" & LateMow == "n" ~ "RD",
  RosDam == "y" & EarlyMow == "y" & LateMow == "n" ~ "RD+EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "n" ~ "EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "y" ~ "EM+LM",
  RosDam == "n" & EarlyMow == "n" & LateMow == "y" ~ "LM",
  RosDam == "y" & EarlyMow == "y" & LateMow == "y" ~ "RD+EM+LM",
  RosDam == "y" & EarlyMow == "n" & LateMow == "y" ~ "RD+LM",
))

#make a plot
FlDate2025Plot<-ggplot(FlDate2025,aes(y=emmean, x=Treatment))+
  geom_bar_pattern(stat = "identity", aes(fill=Treatment, pattern=Treatment), pattern_fill="black",
                   pattern_density=0.05, pattern_spacing=0.05, pattern_angle=45, pattern_key_scale_factor=0.6)+
  coord_flip()+
  scale_x_discrete(limits = rev) +
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(legend.position = "none", panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.x = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30))+
  labs(x="Treatment", y="Day of Experiment")+
  geom_text(aes(label = .group, y = upper.CL + 5), size=7, #+ number specifies how high the cld appears
            position = position_dodge(width = 0.75))+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+ #angle text 45 degrees
  ylim(0,90)+
  ggtitle("Flowering Date (2025)")+
  geom_errorbar(aes(x=Treatment, ymin=lower.CL, ymax=upper.CL))
FlDate2025Plot

#ggsave("ELS Figure S52025.svg",
       #plot=FlDate2025Plot,
       #width=dims[1],
       #height=dims[2],
       #units="px",
       #dpi=72)

##2024####
#fit model and summarize
fitFD2024 <- glmmTMB(FloweringDate_doe ~ RosDam*EarlyMow*LateMow + LLLMay2 + (1|Block), data = dat_2024, family = "gaussian")
summary(fitFD2024)

#check residuals
sim.fitFD2024<-simulateResiduals(fitFD2024)
plot(sim.fitFD2024) #visual inspection of residuals: looks good

#calculate treatment means (estimated marginal means) and pairwise comparisons by treatment
emmFD2024 <- emmeans(fitFD2024, specs=~RosDam*EarlyMow*LateMow, type="response")
emmFD2024

#Put emmeans output into a data frame along with compact letter display for pairwise comparisons
FlDate2024<-as.data.frame(cld(emmFD2024, Letters=letters))

#Add Treatment Name to Data Frame
FlDate2024<-mutate(FlDate2024, Treatment=case_when(
  RosDam == "n" & EarlyMow == "n" & LateMow == "n" ~ "Control",
  RosDam == "y" & EarlyMow == "n" & LateMow == "n" ~ "RD",
  RosDam == "y" & EarlyMow == "y" & LateMow == "n" ~ "RD+EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "n" ~ "EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "y" ~ "EM+LM",
  RosDam == "n" & EarlyMow == "n" & LateMow == "y" ~ "LM",
  RosDam == "y" & EarlyMow == "y" & LateMow == "y" ~ "RD+EM+LM",
  RosDam == "y" & EarlyMow == "n" & LateMow == "y" ~ "RD+LM",
))

#make a plot
FlDate2024Plot<-ggplot(FlDate2024,aes(y=emmean, x=Treatment))+
  geom_bar_pattern(stat = "identity", aes(fill=Treatment, pattern=Treatment), pattern_fill="black",
                   pattern_density=0.05, pattern_spacing=0.05, pattern_angle=45, pattern_key_scale_factor=0.6)+
  coord_flip()+
  scale_x_discrete(limits = rev) +
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(legend.position = "none", panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.x = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30))+
  labs(x="Treatment", y="Day of Experiment")+
  geom_text(aes(label = .group, y = upper.CL + 5), size=7, #+ number specifies how high the cld appears
            position = position_dodge(width = 0.75))+
  ylim(0,90)+
  ggtitle("Flowering Date (2024)")+
  geom_errorbar(aes(x=Treatment, ymin=lower.CL, ymax=upper.CL))
FlDate2024Plot

#ggsave("ELS Figure S52024.svg",
       #plot=FlDate2024Plot,
       #width=dims[1],
       #height=dims[2],
       #units="px",
       #dpi=72)

#Capitulum Production####
#This is the maximum number of capitula + buds ever present on the plant post-treatment
#minus those still immature at harvest
#plus those that produced seeds before all treatments were completed (this only happened in 2024)

##Combined####
#fit model and summarize
fitCap_combined <- glmmTMB(MaxCapitulaIncPreTrtSeedProducers ~ RosDam*EarlyMow*LateMow + LLLSpring + (1|Year/Block), data = dat_combined, family = "nbinom2") 
#poisson error distribution chosen in 2024 because this is count data
#nbinom2 is better than poisson for combined data
summary(fitCap_combined)

##check residuals
sim.fitCap_combined <- simulateResiduals(fitCap_combined)
plot(sim.fitCap_combined) #Visual inspection: this looks fine

#calculate treatment means (estimated marginal means) and pairwise comparisons by treatment
emmCap_combined <- emmeans(fitCap_combined, specs=pairwise~RosDam*EarlyMow*LateMow, type="response")
emmCap_combined

#Put emmeans output into a data frame along with compact letter display for pairwise comparisons
Cap_combined<-as.data.frame(cld(emmCap_combined, Letters=letters))

#Add Treatment Name to Data Frame
Cap_combined<-mutate(Cap_combined, Treatment=case_when(
  RosDam == "n" & EarlyMow == "n" & LateMow == "n" ~ "Control",
  RosDam == "y" & EarlyMow == "n" & LateMow == "n" ~ "RD",
  RosDam == "y" & EarlyMow == "y" & LateMow == "n" ~ "RD+EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "n" ~ "EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "y" ~ "EM+LM",
  RosDam == "n" & EarlyMow == "n" & LateMow == "y" ~ "LM",
  RosDam == "y" & EarlyMow == "y" & LateMow == "y" ~ "RD+EM+LM",
  RosDam == "y" & EarlyMow == "n" & LateMow == "y" ~ "RD+LM",
))

#make a plot
Cap_combinedPlot<-ggplot(Cap_combined,aes(y=response, Treatment))+
  geom_bar_pattern(stat = "identity", aes(fill=Treatment, pattern=Treatment), pattern_fill = "black",
                   pattern_density = 0.05,
                   pattern_spacing = 0.05,
                   pattern_angle = 45,
                   pattern_key_scale_factor = 0.6)+ 
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(legend.position = "none", panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1),axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30))+
  labs(x="Treatment", y="Capitulum Production")+
  geom_text(aes(label = .group, y = asymp.UCL + 5), #+ number specifies how high the cld appears
            position = position_dodge(width = 0.75), size=7)+
  ylim(0, 35)+
  ggtitle("Maximum Number of Capitula Present on Plant")+
  geom_errorbar(aes(x=Treatment, ymin=asymp.LCL, ymax=asymp.UCL))
Cap_combinedPlot

#ggsave("ELS Figure 2.svg",
       #plot=Cap_combinedPlot,
       #width=dims[1],
       #height=dims[2],
       #units="px",
       #dpi=72)

##2025####
#fit model and summarize
fitCap2025 <- glmmTMB(MaxCapitulaIncPreTrtSeedProducers ~ RosDam*EarlyMow*LateMow + LLLApr30 + (1|Block), data = dat_2025, family = "nbinom2") 
#poisson error distribution chosen in 2024 because this is count data
#nbinom1 fits 2025 data well
summary(fitCap2025)

#check residuals
sim.fitCap2025 <- simulateResiduals(fitCap2025)
plot(sim.fitCap2025) 

#calculate treatment means (estimated marginal means) and pairwise comparisons by treatment
emmCap2025 <- emmeans(fitCap2025, specs=~RosDam*EarlyMow*LateMow, type="response")
emmCap2025

#Put emmeans output into a data frame along with compact letter display for pairwise comparisons
Cap2025<-as.data.frame(cld(emmCap2025, Letters=letters))

#Add Treatment Name to Data Frame
Cap2025<-mutate(Cap2025, Treatment=case_when(
  RosDam == "n" & EarlyMow == "n" & LateMow == "n" ~ "Control",
  RosDam == "y" & EarlyMow == "n" & LateMow == "n" ~ "RD",
  RosDam == "y" & EarlyMow == "y" & LateMow == "n" ~ "RD+EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "n" ~ "EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "y" ~ "EM+LM",
  RosDam == "n" & EarlyMow == "n" & LateMow == "y" ~ "LM",
  RosDam == "y" & EarlyMow == "y" & LateMow == "y" ~ "RD+EM+LM",
  RosDam == "y" & EarlyMow == "n" & LateMow == "y" ~ "RD+LM",
))

#make a plot
Cap2025Plot<-ggplot(Cap2025,aes(y=response, Treatment))+
  geom_bar_pattern(stat = "identity", aes(fill=Treatment, pattern=Treatment), pattern_fill="black", pattern_density=0.05, 
                   pattern_spacing=0.05, pattern_angle=45, pattern_key_scale_factor=0.6)+
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(legend.position = "none", panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1), 
        axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30))+
  labs(x="Treatment", y="No. Capitula")+
  geom_text(aes(label = .group, y = asymp.UCL + 5), #+ number specifies how high the cld appears
            position = position_dodge(width = 0.75), size=7)+
  ylim(0, 50)+
  ggtitle("Capitulum Production (2025)")+
  geom_errorbar(aes(x=Treatment, ymin=asymp.LCL, ymax=asymp.UCL))
Cap2025Plot

#ggsave("ELS Figure S22025.svg",
       #plot=Cap2025Plot,
       #width=dims[1],
       #height=dims[2],
       #units="px",
       #dpi=72)

##2024####
#fit model and summarize
fitCap2024 <- glmmTMB(MaxCapitulaIncPreTrtSeedProducers ~ RosDam*EarlyMow*LateMow + LLLMay2 + (1|Block), data = dat_2024, family = "nbinom2") #poisson error distribution also works, using nbinom2 here for consistency with 2025 and combined data
summary(fitCap2024)

#checkresiduals
sim.fitCap2024 <- simulateResiduals(fitCap2024)
plot(sim.fitCap2024) #Visual inspection: this looks good

#calculate treatment means (estimated marginal means) and pairwise comparisons by treatment
emmCap2024 <- emmeans(fitCap2024, specs=~RosDam*EarlyMow*LateMow, type="response")
emmCap2024

#Put emmeans output into a data frame along with compact letter display for pairwise comparisons
Cap2024<-as.data.frame(cld(emmCap2024, Letters=letters))

#Add Treatment Name to Data Frame
Cap2024<-mutate(Cap2024, Treatment=case_when(
  RosDam == "n" & EarlyMow == "n" & LateMow == "n" ~ "Control",
  RosDam == "y" & EarlyMow == "n" & LateMow == "n" ~ "RD",
  RosDam == "y" & EarlyMow == "y" & LateMow == "n" ~ "RD+EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "n" ~ "EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "y" ~ "EM+LM",
  RosDam == "n" & EarlyMow == "n" & LateMow == "y" ~ "LM",
  RosDam == "y" & EarlyMow == "y" & LateMow == "y" ~ "RD+EM+LM",
  RosDam == "y" & EarlyMow == "n" & LateMow == "y" ~ "RD+LM",
))

#make a plot
Cap2024Plot<-ggplot(Cap2024,aes(y=response, Treatment))+
  geom_bar_pattern(stat = "identity", aes(fill=Treatment, pattern=Treatment), pattern_fill = "black",
                   pattern_density = 0.05, pattern_spacing = 0.05, pattern_angle = 45, pattern_key_scale_factor = 0.6)+
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(legend.position = "none", panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30))+
  labs(x="Treatment", y="No. Capitula")+
  geom_text(aes(label = .group, y = asymp.UCL + 3), #+ number specifies how high the cld appears
            position = position_dodge(width = 0.75), size=7)+
  ylim(0, 20)+
  ggtitle("Capitulum Production (2024)")+
  geom_errorbar(aes(x=Treatment, ymin=asymp.LCL, ymax=asymp.UCL), size=1)
Cap2024Plot

#ggsave("ELS Figure S22024.svg",
    #plot=Cap2024Plot,
    #width=dims[1],
    #height=dims[2],
    #units="px",
    #dpi=72)

#Height at Flowering####
##Combined####
#fit model and summarize results
fitFlHt_combined <- glmmTMB(HeightatFlowering ~ RosDam*EarlyMow*LateMow + LLLSpring+ (1|Year/Block), data = dat_combined, family = "nbinom1")
summary(fitFlHt_combined)

#check residuals
sim.fitFlHt_combined<-simulateResiduals(fitFlHt_combined)
plot(sim.fitFlHt_combined) #visual inspection: looks great

#calculate treatment means (estimated marginal means) and pairwise comparisons by treatment
emmFlHt_combined <- emmeans(fitFlHt_combined, specs=pairwise~RosDam*EarlyMow*LateMow, type="response")
emmFlHt_combined

#Put emmeans output into a data frame along with compact letter display for pairwise comparisons
FlHt_combined<-as.data.frame(cld(emmFlHt_combined, Letters=letters))

#Add Treatment Name to Data Frame
FlHt_combined<-mutate(FlHt_combined, Treatment=case_when(
  RosDam == "n" & EarlyMow == "n" & LateMow == "n" ~ "Control",
  RosDam == "y" & EarlyMow == "n" & LateMow == "n" ~ "RD",
  RosDam == "y" & EarlyMow == "y" & LateMow == "n" ~ "RD+EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "n" ~ "EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "y" ~ "EM+LM",
  RosDam == "n" & EarlyMow == "n" & LateMow == "y" ~ "LM",
  RosDam == "y" & EarlyMow == "y" & LateMow == "y" ~ "RD+EM+LM",
  RosDam == "y" & EarlyMow == "n" & LateMow == "y" ~ "RD+LM",
))

#make a plot
FlHt_combinedPlot<-ggplot(FlHt_combined,aes(y=response, Treatment))+
  geom_bar_pattern(stat = "identity", aes(fill=Treatment, pattern=Treatment), pattern_fill = "black",
                   pattern_density = 0.05,
                   pattern_spacing = 0.05,
                   pattern_angle = 45,
                   pattern_key_scale_factor = 0.6)+
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(legend.position = "none",panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30))+
  labs(x="Treatment", y="Height at Flowering (cm)")+
  geom_text(aes(label = .group, y = asymp.UCL + 10), #+ number specifies how high the cld appears
            position = position_dodge(width = 0.75), size=7)+
  ylim(0, 170)+
  ggtitle("Height at Flowering")+
  geom_errorbar(aes(x=Treatment, ymin=asymp.LCL, ymax=asymp.UCL), size=1)
FlHt_combinedPlot

#ggsave("ELS Figure 4.svg",
       #plot=FlHt_combinedPlot,
       #width=dims[1],
       #height=dims[2],
       #units="px",
       #dpi=72)

##2025####
#fit model and summarize results
fitFlHt2025 <- glmmTMB(HeightatFlowering ~ RosDam*EarlyMow*LateMow + LLLApr30+ (1|Block), data = dat_2025, family = "nbinom1")
summary(fitFlHt2025)

#check residuals
sim.fitFlHt2025<-simulateResiduals(fitFlHt2025)
plot(sim.fitFlHt2025) #visual inspection: looks great

#calculate treatment means (estimated marginal means) and pairwise comparisons by treatment
emmFlHt2025 <- emmeans(fitFlHt2025, specs=~RosDam*EarlyMow*LateMow, type="response")
emmFlHt2025

#Put emmeans output into a data frame along with compact letter display for pairwise comparisons
FlHt2025<-as.data.frame(cld(emmFlHt2025, Letters=letters))

#Add Treatment Name to Data Frame
FlHt2025<-mutate(FlHt2025, Treatment=case_when(
  RosDam == "n" & EarlyMow == "n" & LateMow == "n" ~ "Control",
  RosDam == "y" & EarlyMow == "n" & LateMow == "n" ~ "RD",
  RosDam == "y" & EarlyMow == "y" & LateMow == "n" ~ "RD+EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "n" ~ "EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "y" ~ "EM+LM",
  RosDam == "n" & EarlyMow == "n" & LateMow == "y" ~ "LM",
  RosDam == "y" & EarlyMow == "y" & LateMow == "y" ~ "RD+EM+LM",
  RosDam == "y" & EarlyMow == "n" & LateMow == "y" ~ "RD+LM",
))

FlHt2025Plot<-ggplot(FlHt2025,aes(y=response, Treatment))+
  geom_bar_pattern(stat = "identity", aes(fill=Treatment, pattern=Treatment), pattern_fill = "black",
                   pattern_density = 0.05,
                   pattern_spacing = 0.05,
                   pattern_angle = 45,
                   pattern_key_scale_factor = 0.6)+
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(legend.position = "none", panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30))+
  labs(x="Treatment", y="Height at Flowering (cm)")+
  geom_text(aes(label = .group, y = asymp.UCL + 10), #+ number specifies how high the cld appears
            position = position_dodge(width = 0.75), size=7)+
  ylim(0, 170)+
  ggtitle("Height at Flowering (2025)")+
  geom_errorbar(aes(x=Treatment, ymin=asymp.LCL, ymax=asymp.UCL), size=1)
FlHt2025Plot

#ggsave("ELS Figure S42025.svg",
       #plot=FlHt2025Plot,
       #width=dims[1],
       #height=dims[2],
       #units="px",
       #dpi=72)

##2024####
#fit model and summarize results
fitFlHt2024 <- glmmTMB(HeightatFlowering ~ RosDam*EarlyMow*LateMow + LLLMay2+ (1|Block), data = dat_2024, family = "nbinom1")
summary(fitFlHt2024)

#check residuals
sim.fitFlHt2024<-simulateResiduals(fitFlHt2024)
plot(sim.fitFlHt2024) #visual inspection: looks great

#calculate treatment means (estimated marginal means) and pairwise comparisons by treatment
emmFlHt2024 <- emmeans(fitFlHt2024, specs=~RosDam*EarlyMow*LateMow, type="response")
emmFlHt2024

#Put emmeans output into a data frame along with compact letter display for pairwise comparisons
FlHt2024<-as.data.frame(cld(emmFlHt2024, Letters=letters))

#Add Treatment Name to Data Frame
FlHt2024<-mutate(FlHt2024, Treatment=case_when(
  RosDam == "n" & EarlyMow == "n" & LateMow == "n" ~ "Control",
  RosDam == "y" & EarlyMow == "n" & LateMow == "n" ~ "RD",
  RosDam == "y" & EarlyMow == "y" & LateMow == "n" ~ "RD+EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "n" ~ "EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "y" ~ "EM+LM",
  RosDam == "n" & EarlyMow == "n" & LateMow == "y" ~ "LM",
  RosDam == "y" & EarlyMow == "y" & LateMow == "y" ~ "RD+EM+LM",
  RosDam == "y" & EarlyMow == "n" & LateMow == "y" ~ "RD+LM",
))

#make a plot
FlHt2024Plot<-ggplot(FlHt2024,aes(y=response, Treatment))+
  geom_bar_pattern(stat = "identity", aes(fill=Treatment, pattern=Treatment), pattern_fill = "black",
                   pattern_density = 0.05,
                   pattern_spacing = 0.05,
                   pattern_angle = 45,
                   pattern_key_scale_factor = 0.6)+
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(legend.position = "none", panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30))+
  labs(x="Treatment", y="Height at Flowering (cm)")+
  geom_text(aes(label = .group, y = asymp.UCL + 10), #+ number specifies how high the cld appears
            position = position_dodge(width = 0.75), size=7)+
  ylim(0, 150)+
  ggtitle("Height at Flowering (2024)")+
  geom_errorbar(aes(x=Treatment, ymin=asymp.LCL, ymax=asymp.UCL), size=1)
FlHt2024Plot

#ggsave("ELS Figure S42024.svg",
       #plot=FlHt2024Plot,
       #width=dims[1],
       #height=dims[2],
       #units="px",
       #dpi=72)

#Maximum Height####
#This is maximum post-treatment height (after all physical damages had been applied).

##Combined####
#fit model and summarize
fitMaxHt_combined<-glmmTMB(MaxHeight~RosDam*EarlyMow*LateMow+LLLSpring+(1|Year/Block), data=dat_combined, family="nbinom1")
summary(fitMaxHt_combined)

#check residuals
sim.fitMaxHt_combined<-simulateResiduals(fitMaxHt_combined)
plot(sim.fitMaxHt_combined) #looks okay

#calculate treatment means (estimated marginal means) and pairwise comparisons by treatment
emmMaxHt_combined <- emmeans(fitMaxHt_combined, specs=pairwise~RosDam*EarlyMow*LateMow, type="response")
emmMaxHt_combined

#Put emmeans output into a data frame along with compact letter display for pairwise comparisons
MaxHt_combined<-as.data.frame(cld(emmMaxHt_combined, Letters=letters))

#Add Treatment Name to Data Frame
MaxHt_combined<-mutate(MaxHt_combined, Treatment=case_when(
  RosDam == "n" & EarlyMow == "n" & LateMow == "n" ~ "Control",
  RosDam == "y" & EarlyMow == "n" & LateMow == "n" ~ "RD",
  RosDam == "y" & EarlyMow == "y" & LateMow == "n" ~ "RD+EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "n" ~ "EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "y" ~ "EM+LM",
  RosDam == "n" & EarlyMow == "n" & LateMow == "y" ~ "LM",
  RosDam == "y" & EarlyMow == "y" & LateMow == "y" ~ "RD+EM+LM",
  RosDam == "y" & EarlyMow == "n" & LateMow == "y" ~ "RD+LM",
))

#make a plot
MaxHt_combinedPlot<-ggplot(MaxHt_combined,aes(y=response, x=Treatment))+
  geom_bar_pattern(stat = "identity", aes(fill=Treatment, pattern=Treatment), pattern_fill = "black",
                   pattern_density = 0.05,
                   pattern_spacing = 0.05,
                   pattern_angle = 45,
                   pattern_key_scale_factor = 0.6)+
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(legend.position = "none", panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1),  axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30))+
  labs(x="Treatment", y="Maximum Height (cm)")+
  geom_text(aes(label = .group, y = asymp.UCL + 10), #+ number specifies how high the cld appears
            position = position_dodge(width = 0.75), size=7)+
  ylim(0,200)+
  ggtitle("Maximum Height")+
  geom_errorbar(aes(x=Treatment, ymin=asymp.LCL, ymax=asymp.UCL))
MaxHt_combinedPlot

#ggsave("ELS Figure S6combined.svg",
    #plot=MaxHt_combinedPlot,
    #width=dims[1],
    #height=dims[2],
    #units="px",
    #dpi=72)

##2025####
#fit model and summarize
fitMaxHt2025<-glmmTMB(MaxHeight~RosDam*EarlyMow*LateMow+LLLApr30+(1|Block), data=dat_2025, family="nbinom1")
summary(fitMaxHt2025)

#check residuals
sim.fitMaxHt2025<-simulateResiduals(fitMaxHt2025)
plot(sim.fitMaxHt2025) #looks okay

#calculate treatment means (estimated marginal means) and pairwise comparisons by treatment
emmMaxHt2025 <- emmeans(fitMaxHt2025, specs=~RosDam*EarlyMow*LateMow, type="response")
emmMaxHt2025

#Put emmeans output into a data frame along with compact letter display for pairwise comparisons
MaxHt2025<-as.data.frame(cld(emmMaxHt2025, Letters=letters))

#Add Treatment Name to Data Frame
MaxHt2025<-mutate(MaxHt2025, Treatment=case_when(
  RosDam == "n" & EarlyMow == "n" & LateMow == "n" ~ "Control",
  RosDam == "y" & EarlyMow == "n" & LateMow == "n" ~ "RD",
  RosDam == "y" & EarlyMow == "y" & LateMow == "n" ~ "RD+EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "n" ~ "EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "y" ~ "EM+LM",
  RosDam == "n" & EarlyMow == "n" & LateMow == "y" ~ "LM",
  RosDam == "y" & EarlyMow == "y" & LateMow == "y" ~ "RD+EM+LM",
  RosDam == "y" & EarlyMow == "n" & LateMow == "y" ~ "RD+LM",
))

#make a plot
MaxHt2025Plot<-ggplot(MaxHt2025,aes(y=response, x=Treatment))+
  geom_bar_pattern(stat = "identity", aes(fill=Treatment, pattern=Treatment), pattern_fill = "black",
                   pattern_density = 0.05,
                   pattern_spacing = 0.05,
                   pattern_angle = 45,
                   pattern_key_scale_factor = 0.6)+
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(legend.position = "none", panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1),  axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30))+
  labs(x="Treatment", y="Maximum Height (cm)")+
  geom_text(aes(label = .group, y = asymp.UCL + 10), #+ number specifies how high the cld appears
            position = position_dodge(width = 0.75), size=7)+
  ylim(0,200)+
  ggtitle("Maximum Height (2025)")+
  geom_errorbar(aes(x=Treatment, ymin=asymp.LCL, ymax=asymp.UCL))
MaxHt2025Plot

#ggsave("ELS Figure S62025.svg",
       #plot=MaxHt2025Plot,
       #width=dims[1],
       #height=dims[2],
       #units="px",
       #dpi=72)

##2024####
#fit model and summarize
fitMaxHt2024<-glmmTMB(MaxHeight~RosDam*EarlyMow*LateMow+LLLMay2+(1|Block), data=dat_2024, family="nbinom1")
summary(fitMaxHt2024)

#check residuals
sim.fitMaxHt2024<-simulateResiduals(fitMaxHt2024)
plot(sim.fitMaxHt2024) #looks great

#calculate treatment means (estimated marginal means) and pairwise comparisons by treatment
emmMaxHt2024 <- emmeans(fitMaxHt2024, specs=~RosDam*EarlyMow*LateMow, type="response")
emmMaxHt2024

#Put emmeans output into a data frame along with compact letter display for pairwise comparisons
MaxHt2024<-as.data.frame(cld(emmMaxHt2024, Letters=letters))

#Add Treatment Name to Data Frame
MaxHt2024<-mutate(MaxHt2024, Treatment=case_when(
  RosDam == "n" & EarlyMow == "n" & LateMow == "n" ~ "Control",
  RosDam == "y" & EarlyMow == "n" & LateMow == "n" ~ "RD",
  RosDam == "y" & EarlyMow == "y" & LateMow == "n" ~ "RD+EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "n" ~ "EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "y" ~ "EM+LM",
  RosDam == "n" & EarlyMow == "n" & LateMow == "y" ~ "LM",
  RosDam == "y" & EarlyMow == "y" & LateMow == "y" ~ "RD+EM+LM",
  RosDam == "y" & EarlyMow == "n" & LateMow == "y" ~ "RD+LM",
))

#make a plot
MaxHt2024Plot<-ggplot(MaxHt2024,aes(y=response, x=Treatment))+
  geom_bar_pattern(stat = "identity", aes(fill=Treatment, pattern=Treatment), pattern_fill = "black",
                   pattern_density = 0.05,
                   pattern_spacing = 0.05,
                   pattern_angle = 45,
                   pattern_key_scale_factor = 0.6)+
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(legend.position = "none", panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1),  axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30))+
  labs(x="Treatment", y="Maximum Height (cm)")+
  geom_text(aes(label = .group, y = asymp.UCL + 10), #+ number specifies how high the cld appears
            position = position_dodge(width = 0.75), size=7)+
  ylim(0,160)+
  ggtitle("Maximum Height (2024)")+
  geom_errorbar(aes(x=Treatment, ymin=asymp.LCL, ymax=asymp.UCL))
MaxHt2024Plot

#ggsave("ELS Figure S62024.svg",
    #plot=MaxHt2024Plot,
    #width=dims[1],
    #height=dims[2],
    #units="px",
    #dpi=72)

#Number of Stems####
##Combined####
#fit model and summarize
fitNS_combined<-glmmTMB(NoStemsFinal~RosDam*EarlyMow*LateMow+NoStemsSpring+LLLSpring+(1|Year/Block), data=dat_combined, family="poisson")
summary(fitNS_combined)

#check residuals
sim.fitNS_combined<-simulateResiduals(fitNS_combined)
plot(sim.fitNS_combined) #looks fine

#calculate treatment means (estimated marginal means) and pairwise comparisons by treatment
emmNS_combined <- emmeans(fitNS_combined, specs=pairwise~RosDam*EarlyMow*LateMow, type="response")
emmNS_combined

#Put emmeans output into a data frame along with compact letter display for pairwise comparisons
NoStems_combined<-as.data.frame(cld(emmNS_combined, Letters=letters))

#Add Treatment Name to Data Frame
NoStems_combined<-mutate(NoStems_combined, Treatment=case_when(
  RosDam == "n" & EarlyMow == "n" & LateMow == "n" ~ "Control",
  RosDam == "y" & EarlyMow == "n" & LateMow == "n" ~ "RD",
  RosDam == "y" & EarlyMow == "y" & LateMow == "n" ~ "RD+EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "n" ~ "EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "y" ~ "EM+LM",
  RosDam == "n" & EarlyMow == "n" & LateMow == "y" ~ "LM",
  RosDam == "y" & EarlyMow == "y" & LateMow == "y" ~ "RD+EM+LM",
  RosDam == "y" & EarlyMow == "n" & LateMow == "y" ~ "RD+LM",
))

#make a plot
NoStems_combinedPlot<-ggplot(NoStems_combined,aes(y=rate, x=Treatment))+
  geom_bar_pattern(stat = "identity", aes(fill=Treatment, pattern=Treatment), pattern_fill = "black",
                   pattern_density = 0.05,
                   pattern_spacing = 0.05,
                   pattern_angle = 45,
                   pattern_key_scale_factor = 0.6)+
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(legend.position = "none", panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30))+
  labs(x="Treatment", y="Number of Stems")+
  geom_text(aes(label = .group, y = asymp.UCL + .5), #+ number specifies how high the cld appears
            position = position_dodge(width = 0.75), size=7)+
  ylim(0,7)+
  ggtitle("Number of Stems at 10 cm, at Experiment Termination")+
  geom_errorbar(aes(x=Treatment, ymin=asymp.LCL, ymax=asymp.UCL))
NoStems_combinedPlot

#ggsave("ELS Figure S7combined.svg",
    #plot=NoStems_combinedPlot,
    #width=dims[1],
    #height=dims[2],
    #units="px",
    #dpi=72)

##2025####
#fit model and summarize
fitNS2025<-glmmTMB(NoStemsFinal~RosDam*EarlyMow*LateMow+NoStemsApr30+LLLApr30+(1|Block), data=dat_2025, family="poisson")
summary(fitNS2025)

#check residuals
sim.fitNS2025<-simulateResiduals(fitNS2025)
plot(sim.fitNS2025) #looks fine

#calculate treatment means (estimated marginal means) and pairwise comparisons by treatment
emmNS2025 <- emmeans(fitNS2025, specs=~RosDam*EarlyMow*LateMow, type="response")
emmNS2025

#Put emmeans output into a data frame along with compact letter display for pairwise comparisons
NoStems2025<-as.data.frame(cld(emmNS2025, Letters=letters))

#Add Treatment Name to Data Frame
NoStems2025<-mutate(NoStems2025, Treatment=case_when(
  RosDam == "n" & EarlyMow == "n" & LateMow == "n" ~ "Control",
  RosDam == "y" & EarlyMow == "n" & LateMow == "n" ~ "RD",
  RosDam == "y" & EarlyMow == "y" & LateMow == "n" ~ "RD+EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "n" ~ "EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "y" ~ "EM+LM",
  RosDam == "n" & EarlyMow == "n" & LateMow == "y" ~ "LM",
  RosDam == "y" & EarlyMow == "y" & LateMow == "y" ~ "RD+EM+LM",
  RosDam == "y" & EarlyMow == "n" & LateMow == "y" ~ "RD+LM",
))

#make a plot
NoStems2025Plot<-ggplot(NoStems2025,aes(y=rate, x=Treatment))+
  geom_bar_pattern(stat = "identity", aes(fill=Treatment, pattern=Treatment), pattern_fill = "black",
                   pattern_density = 0.05,
                   pattern_spacing = 0.05,
                   pattern_angle = 45,
                   pattern_key_scale_factor = 0.6)+
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(legend.position = "none", panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30))+
  labs(x="Treatment", y="Number of Stems")+
  geom_text(aes(label = .group, y = asymp.UCL + .5), #+ number specifies how high the cld appears
            position = position_dodge(width = 0.75), size=7)+
  ylim(0,8)+
  ggtitle("Number of Stems at 10 cm, at Experiment Termination (2025)")+
  geom_errorbar(aes(x=Treatment, ymin=asymp.LCL, ymax=asymp.UCL))
NoStems2025Plot

#ggsave("ELS Figure S72025.svg",
       #plot=NoStems2025Plot,
       #width=dims[1],
       #height=dims[2],
       #units="px",
       #dpi=72)

##2024####
#fit model and summarize
fitNS2024<-glmmTMB(NoStemsFinal~RosDam*EarlyMow*LateMow+NoStemsMay2+LLLMay2+(1|Block), data=dat_2024, family="poisson")
summary(fitNS2024)

#check residuals
sim.fitNS2024<-simulateResiduals(fitNS2024)
plot(sim.fitNS2024) #looks fine

#calculate treatment means (estimated marginal means) and pairwise comparisons by treatment
emmNS2024 <- emmeans(fitNS2024, specs=~RosDam*EarlyMow*LateMow, type="response")
emmNS2024

#Put emmeans output into a data frame
NoStems2024<-as.data.frame(cld(emmNS2024, Letters=letters))

#Add Treatment Name to Data Frame
NoStems2024<-mutate(NoStems2024, Treatment=case_when(
  RosDam == "n" & EarlyMow == "n" & LateMow == "n" ~ "Control",
  RosDam == "y" & EarlyMow == "n" & LateMow == "n" ~ "RD",
  RosDam == "y" & EarlyMow == "y" & LateMow == "n" ~ "RD+EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "n" ~ "EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "y" ~ "EM+LM",
  RosDam == "n" & EarlyMow == "n" & LateMow == "y" ~ "LM",
  RosDam == "y" & EarlyMow == "y" & LateMow == "y" ~ "RD+EM+LM",
  RosDam == "y" & EarlyMow == "n" & LateMow == "y" ~ "RD+LM",
))

#make a plot
NoStems2024Plot<-ggplot(NoStems2024,aes(y=rate, x=Treatment))+
  geom_bar_pattern(stat = "identity", aes(fill=Treatment, pattern=Treatment), pattern_fill = "black",
                   pattern_density = 0.05,
                   pattern_spacing = 0.05,
                   pattern_angle = 45,
                   pattern_key_scale_factor = 0.6)+
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(legend.position = "none", panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30))+
  labs(x="Treatment", y="Number of Stems")+
  geom_text(aes(label = .group, y = asymp.UCL + .5), #+ number specifies how high the cld appears
            position = position_dodge(width = 0.75), size=7)+
  ylim(0,5)+
  ggtitle("Number of Stems at 10 cm, at Experiment Termination (2024)")+
  geom_errorbar(aes(x=Treatment, ymin=asymp.LCL, ymax=asymp.UCL))
NoStems2024Plot

#ggsave("ELS Figure S72024.svg",
    #plot=NoStems2024Plot,
    #width=dims[1],
    #height=dims[2],
    #units="px",
    #dpi=72)

#Capitulum Size####
##Combined####
#load data
CapSizeDat_combined<-read.csv("Combined Mature Capitulum Size Data.csv")

#make factors
CapSizeDat_combined$Year<-as.factor(CapSizeDat_combined$Year)
CapSizeDat_combined$Block<-as.factor(CapSizeDat_combined$Block)
CapSizeDat_combined$RGP<-as.factor(CapSizeDat_combined$RGP)
CapSizeDat_combined$Capitulum<-as.factor(CapSizeDat_combined$Capitulum)
CapSizeDat_combined$Treatment<-as.factor(CapSizeDat_combined$Treatment)

#abbreviate treatment names
CapSizeDat_combined<-mutate(CapSizeDat_combined, Trt=case_when(
  Treatment == "Control" ~ "Control",
  Treatment == "Rosette Damage" ~ "RD",
  Treatment == "Rosette Damage/Early Mow" ~ "RD+EM",
  Treatment == "Early Mow" ~ "EM",
  Treatment == "Early Mow/Late Mow" ~ "EM+LM",
  Treatment == "Late Mow" ~ "LM",
  Treatment == "Rosette Damage/Early Mow/Late Mow" ~ "RD+EM+LM",
  Treatment == "Rosette Damage/Late Mow" ~ "RD+LM",
))

#fit model and summarize
fitCapSize_combined<-glmmTMB(AvgCapDia~Trt+(1|RGP), data=CapSizeDat_combined, family="gaussian")
summary(fitCapSize_combined)

#check residuals
sim.fitCapSize_combined<-simulateResiduals(fitCapSize_combined)
plot(sim.fitCapSize_combined) #looks fine

#calculate treatment means (estimated marginal means) and pairwise comparisons by treatment
emmCapSize_combined <- emmeans(fitCapSize_combined, specs=pairwise~Trt, type="response")
emmCapSize_combined

#put emmeans output in a data frame along with compact letter display for pairwise comparisons
CapSize_combined<-as.data.frame(cld(emmCapSize_combined, Letters=letters))

# make sure Treatment is a factor in the original dataframe
CapSizeDat_combined$Trt<-factor(CapSizeDat_combined$Trt)

#Make plot with size distribution by treatment
CapDist_combinedPlot<-
  ggplot(CapSizeDat_combined, aes(x = Trt, y = AvgCapDia, fill = Trt))+
  geom_boxplot_pattern(aes(fill=Trt, pattern=Trt), pattern_fill = "black",
                       pattern_density = 0.05,
                       pattern_spacing = 0.05,
                       pattern_angle = 45,
                       pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(
    legend.position="none",
    panel.background = element_blank(), 
    panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30)) +
  labs(title = "Capitulum Size Distribution", x = "Treatment", y = "Capitulum Size (cm)")+
  ylim(0,5) + 
  geom_text(
    data = CapSize_combined,
    aes(x = Trt, y = 4.7, label = .group), vjust = 0, size = 7)
CapDist_combinedPlot

#ggsave("ELS Figure 3.svg",
    #plot=CapDist_combinedPlot,
    #width=dims[1],
    #height=dims[2],
    #units="px",
    #dpi=72)

##2025####
#load data
CapSizeDat2025<-read.csv("2025 Mature Capitulum Size Data.csv")

#make factors
CapSizeDat2025$RGP<-as.factor(CapSizeDat2025$RGP)
CapSizeDat2025$Capitulum<-as.factor(CapSizeDat2025$Capitulum)
CapSizeDat2025$Treatment<-as.factor(CapSizeDat2025$Treatment)

#abbreviate treatment names
CapSizeDat2025<-mutate(CapSizeDat2025, Trt=case_when(
  Treatment == "Control" ~ "Control",
  Treatment == "Rosette Damage" ~ "RD",
  Treatment == "Rosette Damage/Early Mow" ~ "RD+EM",
  Treatment == "Early Mow" ~ "EM",
  Treatment == "Early Mow/Late Mow" ~ "EM+LM",
  Treatment == "Late Mow" ~ "LM",
  Treatment == "Rosette Damage/Early Mow/Late Mow" ~ "RD+EM+LM",
  Treatment == "Rosette Damage/Late Mow" ~ "RD+LM",
))
CapSizeDat2025$Trt<-as.factor(CapSizeDat2025$Trt)

#fit model and summarize
fitCapSize2025<-glmmTMB(AvgCapDia~Trt+(1|Block), data=CapSizeDat2025, family="gaussian")
summary(fitCapSize2025)

#check residuals
sim.fitCapSize2025<-simulateResiduals(fitCapSize2025)
plot(sim.fitCapSize2025) #looks fine on the left, variance not homogeneous

#variance is heterogeneous, so use non-parametric Kruskal-Wallis test instead
kruskal.test(AvgCapDia~Trt, data=CapSizeDat2025)
#This tells me that at least one of my groups is different

#I can do multiple comparisons with Dunn's Test
CapSizeDunn2025<-dunnTest(AvgCapDia~Trt, data=CapSizeDat2025, method="bonferroni")$res

#generate cld for Dunn's Test
cldDat2025<-cldList(P.adj~Comparison, data=CapSizeDunn2025)
cldDat2025$Trt<-as.factor(cldDat2025$Group)

#Make plot with size distribution by treatment
CapDist2025Plot<-
  ggplot(CapSizeDat2025, aes(x = Trt, y = AvgCapDia, fill = Trt))+
  geom_boxplot_pattern(aes(fill=Trt, pattern=Trt), pattern_fill = "black",
                       pattern_density = 0.05,
                       pattern_spacing = 0.05,
                       pattern_angle = 45,
                       pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(
    legend.position="none",
    panel.background = element_blank(), 
    panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30)
  ) +
  ylim(0, 4.3)+
  labs(title = "Capitulum Size Distribution (2025)", x = "Treatment", y = "Capitulum Size (cm)")
CapDist2025Plot<-CapDist2025Plot+
  geom_text(data=cldDat2025, aes(x=Trt, y=4.2, label = Letter), size = 7, color = "black")

#ggsave("ELS Figure S32025.svg",
       #plot=CapDist2025Plot,
       #width=dims[1],
       #height=dims[2],
       #units="px",
       #dpi=72)

##2024####
#load data
CapSizeDat2024<-read.csv("2024 Mature Capitulum Size Data.csv")

#make factors
CapSizeDat2024$RGP<-as.factor(CapSizeDat2024$RGP)
CapSizeDat2024$Capitulum<-as.factor(CapSizeDat2024$Capitulum)
CapSizeDat2024$Treatment<-as.factor(CapSizeDat2024$Treatment)

#add treatment abbreviations
CapSizeDat2024<-mutate(CapSizeDat2024, Trt=case_when(
  Treatment == "Control" ~ "Control",
  Treatment == "Rosette Damage" ~ "RD",
  Treatment == "Rosette Damage/Early Mow" ~ "RD+EM",
  Treatment == "Early Mow" ~ "EM",
  Treatment == "Early Mow/Late Mow" ~ "EM+LM",
  Treatment == "Late Mow" ~ "LM",
  Treatment == "Rosette Damage/Early Mow/Late Mow" ~ "RD+EM+LM",
  Treatment == "Rosette Damage/Late Mow" ~ "RD+LM",
))
CapSizeDat2024$Trt<-as.factor(CapSizeDat2024$Trt)

#fit model and summarize
fitCapSize2024<-glmmTMB(AvgCapDia~Trt+(1|Block), data=CapSizeDat2024, family="gaussian")
summary(fitCapSize2024)

#check residuals
sim.fitCapSize2024<-simulateResiduals(fitCapSize2024)
plot(sim.fitCapSize2024)

#variance is heterogeneous, so use non-parametric Kruskal-Wallis test instead
kruskal.test(AvgCapDia~Trt, data=CapSizeDat2024)
  #This tells me that at least one of my groups is different

#I can do multiple comparisons with Dunn's Test
CapSizeDunn2024<-dunnTest(AvgCapDia~Trt, data=CapSizeDat2024, method="bonferroni")$res

#generate cld for Dunn's Test
cldDat2024<-cldList(P.adj~Comparison, data=CapSizeDunn2024)
cldDat2024$Trt<-as.factor(cldDat2024$Group)

#Make plot with size distribution by treatment
CapDist2024Plot<-
  ggplot(CapSizeDat2024, aes(x = Trt, y = AvgCapDia, fill = Trt))+
  geom_boxplot_pattern(aes(fill=Trt, pattern=Trt), pattern_fill = "black",
                       pattern_density = 0.05,
                       pattern_spacing = 0.05,
                       pattern_angle = 45,
                       pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(
    legend.position="none",
    panel.background = element_blank(), 
    panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30)
  ) +
  ylim(0, 4.5)+
  labs(title = "Capitulum Size Distribution (2024)", x = "Treatment", y = "Capitulum Size (cm)")
#Show plot and add letters
CapDist2024Plot<-CapDist2024Plot+
  geom_text(data=cldDat2024, aes(x=Trt, y=4.2, label = Letter), size = 7, color = "black")

#ggsave("ELS Figure S32024.svg",
       #plot=CapDist2024Plot,
       #width=dims[1],
       #height=dims[2],
       #units="px",
       #dpi=72)
