#This script contains the survival analysis for rosette damage x mowing experiments 2024-25

#Load packages####
library(dplyr) 
library(survival)
library(ggplot2)
library(ggpattern)
library(emmeans)
library(multcomp)
library(ggsurvfit)
library(svglite)

#Load Data####

##Combined Data####
#data is be set up so that each plant has survival time in days (since the beginning of the year or start of experiment)
#censoring status: 0=censored (made it to the end), 1=dead
survdat_combined<-read.csv("SurvDataCombined.csv")
#Make treatment, etc. into factors (status does not need to be a factor for this analysis)
survdat_combined$Year<-as.factor(survdat_combined$Year)
survdat_combined$Block <- as.factor(survdat_combined$Block)
survdat_combined$Treatment <- as.factor(survdat_combined$Treatment)
#Add Treatment Abbreviations to Shorten Treatment Names
survdat_combined<-mutate(survdat_combined, TreatmentAbb=case_when(
  Treatment=="Control" ~ "Control",
  Treatment=="Rosette Damage" ~ "RD",
  Treatment=="Rosette Damage/Early Mow" ~ "RD+EM",
  Treatment=="Early Mow" ~ "EM",
  Treatment=="Early Mow/Late Mow" ~ "EM+LM",
  Treatment=="Late Mow" ~ "LM",
  Treatment=="Rosette Damage/Early Mow/Late Mow" ~ "RD+EM+LM",
  Treatment=="Rosette Damage/Late Mow" ~ "RD+LM",
))

##2025 Data####
survdat2025<-read.csv("SurvData2025.csv")
#Make treatment, etc. into factors 
survdat2025$Block <- as.factor(survdat2025$Block)
survdat2025$Treatment <- as.factor(survdat2025$Treatment)

#Add Treatment Abbreviations to Shorten Treatment Names
survdat2025<-mutate(survdat2025, TreatmentAbb=case_when(
  Treatment=="Control" ~ "Control",
  Treatment=="Rosette Damage" ~ "RD",
  Treatment=="Rosette Damage/Early Mow" ~ "RD+EM",
  Treatment=="Early Mow" ~ "EM",
  Treatment=="Early Mow/Late Mow" ~ "EM+LM",
  Treatment=="Late Mow" ~ "LM",
  Treatment=="Rosette Damage/Early Mow/Late Mow" ~ "RD+EM+LM",
  Treatment=="Rosette Damage/Late Mow" ~ "RD+LM",
))

##2024####
survdat2024<-read.csv("SurvData2024.csv")
#Make treatment, etc. into factors
survdat2024$Block <- as.factor(survdat2024$Block)
survdat2024$Treatment <- as.factor(survdat2024$Treatment)

#Add Treatment Abbreviations to Data for Exploratory Plots
survdat2024<-mutate(survdat2024, TreatmentAbb=case_when(
  Treatment=="Control" ~ "Control",
  Treatment=="Rosette Damage" ~ "RD",
  Treatment=="Rosette Damage/Early Mow" ~ "RD+EM",
  Treatment=="Early Mow" ~ "EM",
  Treatment=="Early Mow/Late Mow" ~ "EM+LM",
  Treatment=="Late Mow" ~ "LM",
  Treatment=="Rosette Damage/Early Mow/Late Mow" ~ "RD+EM+LM",
  Treatment=="Rosette Damage/Late Mow" ~ "RD+LM",
))

#get pixel dimensions I want to use for graphics export
dims<-dev.size("px") 

#Exploratory Plots####
##Combined Data####
hist(survdat_combined$TimeOfDeath_doe)
ToDPlot_combined<-survdat_combined %>%
  ggplot(aes(x=TreatmentAbb, y=TimeOfDeath_doe, fill=TreatmentAbb)) +
  geom_boxplot_pattern(aes(fill=TreatmentAbb, pattern=TreatmentAbb), pattern_fill = "black",
                       pattern_density = 0.05,
                       pattern_spacing = 0.05,
                       pattern_angle = 45,
                       pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(
    legend.position="none",panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30)) +
  ggtitle("Time of Death") +
  xlab("Treatment")+
  ylab("Time of Death (day of experiment)")+
  ylim(30,110)
ToDPlot_combined

##2025####
hist(survdat2025$TimeOfDeath_doe)
ToDPlot2025<-survdat2025 %>%
  ggplot(aes(x=TreatmentAbb, y=TimeOfDeath_doe, fill=TreatmentAbb)) +
  geom_boxplot_pattern(aes(fill=TreatmentAbb, pattern=TreatmentAbb), pattern_fill = "black",
                       pattern_density = 0.05,
                       pattern_spacing = 0.05,
                       pattern_angle = 45,
                       pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(
    legend.position="none",panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30)) +
  ggtitle("Time of Death (2025)") +
  xlab("Treatment")+
  ylab("Time of Death (day of experiment)")+
  ylim(35,NA)
ToDPlot2025

##2024####
hist(survdat2024$TimeOfDeath_doe)
ToDPlot2024<-survdat2024 %>%
  ggplot(aes(x=TreatmentAbb, y=TimeOfDeath_doe, fill=TreatmentAbb)) +
  geom_boxplot_pattern(aes(fill=TreatmentAbb, pattern=TreatmentAbb), pattern_fill = "black",
                       pattern_density = 0.05,
                       pattern_spacing = 0.05,
                       pattern_angle = 45,
                       pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(
    legend.position="none",panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30)) +
  ggtitle("Time of Death (2024)") +
  xlab("Treatment")+
  ylab("Time of Death (day of experiment)")+
  ylim(30, NA)
ToDPlot2024

#Survival Regressions and Curves####
##Combined Data####
#Fit a survival regression with treatment as a predictor
s_combined <- survfit(Surv(TimeOfDeath_doe, Status) ~ TreatmentAbb, data = survdat_combined)
summary(s_combined)

#plot s_combined
survfit2(Surv(TimeOfDeath_doe, Status) ~ TreatmentAbb, data = survdat_combined) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+
  scale_color_manual(values =  c("coral1", "darkgoldenrod2", "olivedrab3", "springgreen3", 
                                 "darkturquoise", "deepskyblue", "darkorchid1", "hotpink"))

##2025####
#Fit a survival regression with treatment as a predictor
s2025 <- survfit(Surv(TimeOfDeath_doe, Status) ~ TreatmentAbb, data = survdat2025)
summary(s2025)

#plot s_combined
survfit2(Surv(TimeOfDeath_doe, Status) ~ TreatmentAbb, data = survdat2025) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+
  scale_color_manual(values =  c("coral1", "darkgoldenrod2", "olivedrab3", "springgreen3", 
                                 "darkturquoise", "deepskyblue", "darkorchid1", "hotpink"))

##2024####
#Fit a survival regression with treatment as a predictor
s2024 <- survfit(Surv(TimeOfDeath_doe, Status) ~ TreatmentAbb, data = survdat2024)
summary(s2024)

#plot s_combined
survfit2(Surv(TimeOfDeath_doe, Status) ~ TreatmentAbb, data = survdat2024) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+
  scale_color_manual(values =  c("coral1", "darkgoldenrod2", "olivedrab3", "springgreen3", 
                                 "darkturquoise", "deepskyblue", "darkorchid1", "hotpink"))

#Parametric Survival Regressions####

##Combined####
#To figure out which treatments affected survival, we will fit parametric survival regressions with different hazard models and compare with AIC
#exponential
s1_combined<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat_combined, dist="exponential")

#Weibull (the default for survreg)
s2_combined<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat_combined)

#Gaussian
s3_combined<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat_combined, dist="gaussian")

#Logistic
s4_combined<-survreg(Surv(TimeOfDeath_doe, Status)~TreatmentAbb, data=survdat_combined, dist="logistic")

#Lognormal
s5_combined<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat_combined, dist="lognormal")

#Loglogistic
s6_combined<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat_combined, dist = "loglogistic")

#compare these options
AIC(s1_combined, s2_combined, s3_combined, s4_combined, s5_combined, s6_combined)
#s4 has the lowest AIC, so it looks like logistic hazard fits the best
summary(s4_combined)

#get pairwise comparisons for the best model
emmTOD_combined <- emmeans(s4_combined, specs=pairwise~TreatmentAbb, type="response")
emmTOD_combined

#get compact letter display for pairwise comparisons
cld_combined<-cld(emmTOD_combined, Letters=letters)
cld_combined

#Add these letters to the earlier ToDPlot
ToDPlot_combined<-ToDPlot_combined+
  geom_text(data = cld_combined, aes(x=TreatmentAbb, y=35, label = .group), size = 7, color = "black")

#save this figure
#ggsave("Figure 1.svg",
       #plot=ToDPlot_combined,
       #width=dims[1],
       #height=dims[2],
       #units="px",
       #dpi=72)

##2025####
#To figure out which treatments affected survival, we will fit parametric survival regressions with different hazard models and compare with AIC
#exponential
s1_2025<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat2025, dist="exponential")

#Weibull (the default for survreg)
s2_2025<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat2025)

#Gaussian
s3_2025<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat2025, dist="gaussian")

#Logistic
s4_2025<-survreg(Surv(TimeOfDeath_doe, Status)~TreatmentAbb, data=survdat2025, dist="logistic")

#Lognormal
s5_2025<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat2025, dist="lognormal")

#Loglogistic
s6_2025<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat2025, dist = "loglogistic")

#compare these options
AIC(s1_2025, s2_2025, s3_2025, s4_2025, s5_2025, s6_2025)
#s4 has the lowest AIC, so it looks like logistic hazard fits the best
summary(s4_2025)

#get pairwise comparisons for the best model
emmTOD_2025 <- emmeans(s4_2025, specs=pairwise~TreatmentAbb, type="response")
emmTOD_2025

#get compact letter display for pairwise comparisons
cld_2025<-cld(emmTOD_2025, Letters=letters)
cld_2025

#Add these letters to the earlier ToDPlot
ToDPlot2025<-ToDPlot2025+
  geom_text(data = cld_2025, aes(x=TreatmentAbb, y=40, label = .group), size = 7, color = "black")

#ggsave("Figure S12025.svg",
       #plot=ToDPlot2025,
       #width=dims[1],
       #height=dims[2],
       #units="px",
       #dpi=72)

##2024####
#To figure out which treatments affected survival, we will fit parametric survival regressions with different hazard models and compare with AIC
#exponential
s1_2024<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat2024, dist="exponential")

#Weibull (the default for survreg)
s2_2024<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat2024)
#does not converge

#Gaussian
s3_2024<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat2024, dist="gaussian")

#Logistic
s4_2024<-survreg(Surv(TimeOfDeath_doe, Status)~TreatmentAbb, data=survdat2024, dist="logistic")

#Lognormal
s5_2024<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat2024, dist="lognormal")

#Loglogistic
s6_2024<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat2024, dist = "loglogistic")

#compare these options (leave out s2, which didn't converge)
AIC(s1_2024, s3_2024, s4_2024, s5_2024, s6_2024)
#s4 has the lowest AIC, so it looks like logistic hazard fits the best
summary(s4_2024)

#get pairwise comparisons for the best model
emmTOD_2024 <- emmeans(s4_2024, specs=pairwise~TreatmentAbb, type="response")
emmTOD_2024

#get compact letter display for pairwise comparisons
cld_2024<-cld(emmTOD_2024, Letters=letters)
cld_2024

#Add these letters to the earlier ToDPlot
ToDPlot2024<-ToDPlot2024+
  geom_text(data = cld_2024, aes(x=TreatmentAbb, y=35, label = .group), size = 7, color = "black")

#ggsave("Figure S12024.svg",
       #plot=ToDPlot2024,
       #width=dims[1],
       #height=dims[2],
       #units="px",
       #dpi=72)

#Cox Proportional Hazards Model (non-parametric)####
#We compare our above results to a non-parametric approach

##Combined####
scox_combined<-coxph(Surv(TimeOfDeath_doe, Status)~TreatmentAbb, data = survdat_combined)
summary(scox_combined)
cld(emmeans(scox_combined, specs=pairwise~TreatmentAbb, type="response"), Letters=letters)
#Results are not qualitatively different

##2025####
scox_2025<-coxph(Surv(TimeOfDeath_doe, Status)~TreatmentAbb, data = survdat2025)
summary(scox_2025)
cld(emmeans(scox_2025, specs=pairwise~TreatmentAbb, type="response"), Letters=letters)
#Results are not qualitatively different

##2024####
scox_2024<-coxph(Surv(TimeOfDeath_doe, Status)~TreatmentAbb, data = survdat2024)
summary(scox_2024)
cld(emmeans(scox_2024, specs=pairwise~TreatmentAbb, type="response"), Letters=letters)
#Results are not qualitatively different