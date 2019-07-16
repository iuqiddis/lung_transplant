# library
library(tidyverse)
library(ggplot2)
library(plyr)
require(reshape2)
# library(psych)
library(afex)
library(emmeans)

#raw_lungs <- read_csv('/khazaddum/siddiqui/projects/transplant_lung/r_codes/transplant_data_R_all.csv') #rogue
#raw_lungs <- read_csv('/khazaddum/siddiqui/projects/transplant_lung/r_codes/transplant_data_R_balanced.csv') #rogue, balanced
#raw_lungs <- read_csv('/Users/iuqiddis/siddiqui/projects/transplant_lung/r_codes/transplant_data_R_all.csv') #sydney
raw_lungs <- read_csv('/khazaddum/alanine/sarmad_upenn/siddiqui/projects/transplant_lung/r_codes/transplant_data_R_all.csv') #lizzy
#raw_lungs <- read_csv('/khazaddum/siddiqui/projects/transplant_lung/r_codes/transplant_data_R_balanced.csv') #balanced, rogue

#------------------------------------------------------------

#lungs_no_col <- filter(raw_lungs, type != 'col')
#lungs_no_hrt <- filter(raw_lungs, lung != 'hrt')
lungs <- filter(raw_lungs, type != 'col', lung != 'hrt')
lungs$rat <- factor(lungs$rat)
lungs$day <- factor(lungs$day)


#lung summary statistics----

lungs_st <- ddply(lungs, c("type", "day", "lung"), summarize,
                  N = length(pl2h),
                  pmean = mean(pl2h, na.rm = TRUE),
                  psd = sd(pl2h, na.rm = TRUE),
                  pse = psd/sqrt(N),
                  lmean = mean(ll2h, na.rm = TRUE),
                  lsd = sd(ll2h, na.rm = TRUE),
                  lse = lsd/sqrt(N),
                  l2pmean = mean(l2p, na.rm = TRUE),
                  l2psd = sd(l2p, na.rm = TRUE),
                  l2pse = l2psd/sqrt(N),
                  volmean = mean(vol, na.rm = TRUE)/1000,
                  volsd = sd(vol, na.rm = TRUE)/1000,
                  volse = volsd/sqrt(N),
                  HUmean = mean(HU, na.rm = TRUE),
                  HUsd = sd(HU, na.rm = TRUE),
                  HUse = HUsd/sqrt(N),
                  HUSDmean = mean(HUSD, na.rm = TRUE),
                  HUSDsd = sd(HUSD, na.rm = TRUE),
                  HUSDse = HUSDsd/sqrt(N),
                  normHUmean = mean(normHU, na.rm = TRUE),
                  normHUsd = sd(normHU, na.rm = TRUE),
                  normHUse = normHUsd/sqrt(N)
)

#write_csv(lungs_st, "/Users/iuqiddis/siddiqui/projects/transplant_lung/lungs_st.csv")

#labels and proper names-----
lungs_st$lung <- factor(lungs_st$lung, levels=c("nat", "tx"), labels=c("Native", "Transplant"))
#lungs_st$type <- factor(lungs_st$type, levels=c("allo", "syn"), labels=c("Rejected", "Non-Rejected"))
lungs_st$type <- factor(lungs_st$type, levels=c("allo", "syn"), labels=c("Allogeneic", "Syngeneic"))

#These plots are faceted by cohort type (allo or syn)-----------

pl2h_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = pmean, color = lung, group = lung))
pl2h_plot <- pl2h_plot +geom_point(aes(shape = lung), size = 4) +scale_shape_manual(values=c(16,2)) +scale_color_manual(values=c("black", "black")) 
pl2h_plot <- pl2h_plot +geom_line(aes(linetype = lung)) +scale_linetype_manual(values=c("solid","dashed"))
pl2h_plot <- pl2h_plot +geom_errorbar(mapping = aes(ymin = pmean-pse, ymax = pmean+pse), width = 0.1, position = "dodge") 
pl2h_plot <- pl2h_plot +facet_wrap(~ type) +labs(x = 'Day', y = 'Pyruvate (Normalized to Heart Signal)', color = "Lung")
pl2h_plot <- pl2h_plot +theme_bw()+theme(strip.background = element_rect(fill = "white"))
pl2h_plot

#old theme
ll2h_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = lmean, color = lung, group = lung)) 
ll2h_plot <- ll2h_plot +geom_point()+geom_line() 
ll2h_plot <- ll2h_plot +geom_errorbar(mapping = aes(ymin = lmean-lse, ymax = lmean+lse), width = 0.1, position = "dodge") 
ll2h_plot <- ll2h_plot +facet_wrap(~ type) +labs(x = 'Day', y = 'Lactate (Normalized to Heart Signal)', color = "Lung")
ll2h_plot

#new theme
ll2h_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = lmean, color = lung, group = lung)) 
ll2h_plot <- ll2h_plot +geom_point(aes(shape = lung), size = 4) +scale_shape_manual(values=c(16,2)) +scale_color_manual(values=c("black", "black")) 
ll2h_plot <- ll2h_plot +geom_line(aes(linetype = lung)) +scale_linetype_manual(values=c("solid","dashed"))
ll2h_plot <- ll2h_plot +geom_errorbar(mapping = aes(ymin = lmean-lse, ymax = lmean+lse), width = 0.1, position = "dodge") 
ll2h_plot <- ll2h_plot +facet_wrap(~ type) +labs(x = 'Day', y = 'Lactate (Normalized to Heart Signal)', color = "Lung")
ll2h_plot <- ll2h_plot +theme_bw()+theme(strip.background = element_rect(fill = "white"))
ll2h_plot


l2p_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = l2pmean, color = lung, group = lung)) 
l2p_plot <- l2p_plot +geom_point()+geom_line() 
l2p_plot <- l2p_plot +geom_errorbar(mapping = aes(ymin = l2pmean-l2pse, ymax = l2pmean+l2pse), width = 0.1, position = "dodge") 
l2p_plot <- l2p_plot +facet_wrap(~ type) +labs(x = 'Day', y = 'HP Lactate-to-Pyruvate', color = "Lung")
l2p_plot

vol_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = volmean, color = lung, group = lung)) 
vol_plot <- vol_plot +geom_point()+geom_line() 
vol_plot <- vol_plot +geom_errorbar(mapping = aes(ymin = volmean-volse, ymax = volmean+volse), width = 0.1, position = "dodge") 
vol_plot <- vol_plot +facet_wrap(~ type) +labs(x = 'Day', y = 'Volume (mL)', color = "Lung")
vol_plot

HU_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = HUmean, color = lung, group = lung)) 
HU_plot <- HU_plot +geom_point()+geom_line() 
HU_plot <- HU_plot +geom_errorbar(mapping = aes(ymin = HUmean-HUse, ymax = HUmean+HUse), width = 0.1, position = "dodge") 
HU_plot <- HU_plot +facet_wrap(~ type) +labs(x = 'Day', y = 'Lung Density (HU)', color = "Lung")
HU_plot

normHU_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = normHUmean, color = lung, group = lung)) 
normHU_plot <- normHU_plot +geom_point()+geom_line() 
normHU_plot <- normHU_plot +geom_errorbar(mapping = aes(ymin = normHUmean-normHUse, ymax = normHUmean+normHUse), width = 0.1, position = "dodge") 
normHU_plot <- normHU_plot +facet_wrap(~ type) +labs(x = 'Day', y = 'Normalized Lung Density (L*HU)', color = "Lung")
normHU_plot



#These plots are faceted by lung type (nat or tx)------------

#old theme
vol_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = volmean, color = type, group = type)) 
vol_plot <- vol_plot +geom_point()+geom_line() 
vol_plot <- vol_plot +geom_errorbar(mapping = aes(ymin = volmean-volse, ymax = volmean+volse), width = 0.1, position = "dodge") 
vol_plot <- vol_plot +facet_wrap(~ lung) +labs(x = 'Day', y = 'Volume (mL)', color = "Cohort") +theme_bw()
vol_plot

#new theme
vol_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = volmean, color = type, group = type)) 
vol_plot <- vol_plot +geom_point(aes(shape = type), size = 4) +scale_shape_manual(values=c(2,16)) +scale_color_manual(values=c("black", "black"))
vol_plot <- vol_plot +geom_line(aes(linetype = type)) +scale_linetype_manual(values=c("dashed","solid"))
vol_plot <- vol_plot +geom_errorbar(mapping = aes(ymin = volmean-volse, ymax = volmean+volse), width = 0.1, position = "dodge") 
vol_plot <- vol_plot +facet_wrap(~ lung) +labs(x = 'Day', y = 'Volume (mL)', color = "Cohort")
vol_plot <- vol_plot +theme_bw()+theme(strip.background = element_rect(fill = "white"), strip.text = element_text(size=12))
vol_plot

#old theme
HU_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = HUmean, color = type, group = type)) 
HU_plot <- HU_plot +geom_point()+geom_line() 
HU_plot <- HU_plot +geom_errorbar(mapping = aes(ymin = HUmean-HUse, ymax = HUmean+HUse), width = 0.1, position = "dodge") 
HU_plot <- HU_plot +facet_wrap(~ lung) +labs(x = 'Day', y = 'Lung Density (HU)', color = "Cohort") +theme_bw()
HU_plot

#new theme
HU_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = HUmean, color = type, group = type)) 
HU_plot <- HU_plot +geom_point(aes(shape = type), size = 4) +scale_shape_manual(values=c(2,16)) +scale_color_manual(values=c("black", "black")) 
HU_plot <- HU_plot +geom_line(aes(linetype = type)) +scale_linetype_manual(values=c("dashed","solid"))
HU_plot <- HU_plot +geom_errorbar(mapping = aes(ymin = HUmean-HUse, ymax = HUmean+HUse), width = 0.1, position = "dodge") 
HU_plot <- HU_plot +facet_wrap(~ lung) +labs(x = 'Day', y = 'Lung Density (HU)', color = "Cohort")
HU_plot <- HU_plot +theme_bw()+theme(strip.background = element_rect(fill = "white"), strip.text = element_text(size=12))
HU_plot

#old theme
normHU_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = normHUmean, color = type, group = type)) 
normHU_plot <- normHU_plot +geom_point()+geom_line() 
normHU_plot <- normHU_plot +geom_errorbar(mapping = aes(ymin = normHUmean-normHUse, ymax = normHUmean+normHUse), width = 0.1, position = "dodge") 
normHU_plot <- normHU_plot +facet_wrap(~ lung) +labs(x = 'Day', y = 'Normalized Lung Density (L*HU)', color = "Cohort") +theme_bw()
normHU_plot

#new theme
normHU_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = normHUmean, color = type, group = type)) 
normHU_plot <- normHU_plot +geom_point(aes(shape = type), size = 4) +scale_shape_manual(values=c(2,16)) +scale_color_manual(values=c("black", "black")) 
normHU_plot <- normHU_plot +geom_line(aes(linetype = type)) +scale_linetype_manual(values=c("dashed","solid"))
normHU_plot <- normHU_plot +geom_errorbar(mapping = aes(ymin = normHUmean-normHUse, ymax = normHUmean+normHUse), width = 0.1, position = "dodge") 
normHU_plot <- normHU_plot +facet_wrap(~ lung) +labs(x = 'Day', y = 'Normalized Lung Density (L*HU)', color = "Cohort")
normHU_plot <- normHU_plot +theme_bw()+theme(strip.background = element_rect(fill = "white"), strip.text = element_text(size=12))
normHU_plot

#old theme
pl2h_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = pmean, color = type, group = type)) 
pl2h_plot <- pl2h_plot +geom_point()+geom_line() 
pl2h_plot <- pl2h_plot +geom_errorbar(mapping = aes(ymin = pmean-pse, ymax = pmean+pse), width = 0.1, position = "dodge") 
pl2h_plot <- pl2h_plot +facet_wrap(~ lung) +labs(x = 'Day', y = 'Pyruvate (Normalized to Heart Signal)', color = "Cohort") +theme_bw()
pl2h_plot

#new theme
pl2h_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = pmean, color = type, group = type)) 
pl2h_plot <- pl2h_plot +geom_point(aes(shape = type), size = 4) +scale_shape_manual(values=c(2,16)) +scale_color_manual(values=c("black", "black")) 
pl2h_plot <- pl2h_plot +geom_line(aes(linetype = type)) +scale_linetype_manual(values=c("dashed","solid"))
pl2h_plot <- pl2h_plot +geom_errorbar(mapping = aes(ymin = pmean-pse, ymax = pmean+pse), width = 0.1, position = "dodge") 
pl2h_plot <- pl2h_plot +facet_wrap(~ lung) +labs(x = 'Day', y = 'Pyruvate (Normalized to Heart Signal)', color = "Cohort")
pl2h_plot <- pl2h_plot +theme_bw()+theme(strip.background = element_rect(fill = "white"), strip.text = element_text(size=12))
pl2h_plot

#old theme
l2h_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = lmean, color = type, group = type)) 
ll2h_plot <- ll2h_plot +geom_point()+geom_line() 
ll2h_plot <- ll2h_plot +geom_errorbar(mapping = aes(ymin = lmean-lse, ymax = lmean+lse), width = 0.1, position = "dodge") 
ll2h_plot <- ll2h_plot +facet_wrap(~ lung) +labs(x = 'Day', y = 'Lactate (Normalized to Heart Signal)', color = "Cohort") +theme_bw()
ll2h_plot

#new theme
ll2h_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = lmean, color = type, group = type)) 
ll2h_plot <- ll2h_plot +geom_point(aes(shape = type), size = 4) +scale_shape_manual(values=c(2,16)) +scale_color_manual(values=c("black", "black")) 
ll2h_plot <- ll2h_plot +geom_line(aes(linetype = type)) +scale_linetype_manual(values=c("dashed","solid"))
ll2h_plot <- ll2h_plot +geom_errorbar(mapping = aes(ymin = lmean-lse, ymax = lmean+lse), width = 0.1, position = "dodge") 
ll2h_plot <- ll2h_plot +facet_wrap(~ lung) +labs(x = 'Day', y = 'Lactate (Normalized to Heart Signal)', color = "Cohort")
ll2h_plot <- ll2h_plot +theme_bw()+theme(strip.background = element_rect(fill = "white"), strip.text = element_text(size=12))
ll2h_plot

#old theme
l2p_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = l2pmean, color = type, group = type)) 
l2p_plot <- l2p_plot +geom_point()+geom_line() 
l2p_plot <- l2p_plot +geom_errorbar(mapping = aes(ymin = l2pmean-l2pse, ymax = l2pmean+l2pse), width = 0.1, position = "dodge") 
l2p_plot <- l2p_plot +facet_wrap(~ lung) +labs(x = 'Day', y = 'HP Lactate-to-Pyruvate', color = "Cohort") +theme_bw()
l2p_plot

#new theme
l2p_plot <- ggplot(data=lungs_st, mapping = aes(x = factor(day), y = l2pmean, color = type, group = type))
l2p_plot <- l2p_plot +geom_point(aes(shape = type), size = 4) +scale_shape_manual(values=c(2,16)) +scale_color_manual(values=c("black", "black")) 
l2p_plot <- l2p_plot +geom_line(aes(linetype = type)) +scale_linetype_manual(values=c("dashed","solid"))
l2p_plot <- l2p_plot +geom_errorbar(mapping = aes(ymin = l2pmean-l2pse, ymax = l2pmean+l2pse), width = 0.1, position = "dodge") 
l2p_plot <- l2p_plot +facet_wrap(~ lung) +labs(x = 'Day', y = 'HP Lactate-to-Pyruvate', color = "Cohort")
l2p_plot <- l2p_plot +theme_bw()+theme(strip.background = element_rect(fill = "white"), strip.text = element_text(size=12))
l2p_plot

#ANOVA test------



#IV between: lung, type
#IV within: day
#DV: l2p

#Two way mixed design
aov_lung_type_day <- aov(l2p ~ type*lung*day + Error(rat/day*lung) + type, data=lungs)

#Two Way
aov_lung_type_day <- aov(l2p ~ type*lung*day, data=lungs)
#TukeyHSD(aov_lung_type_day)
summary(aov_lung_type_day)

#Two Way
aov_ltd_ct_vol <- aov(vol ~ type*lung*day, data=lungs)
#TukeyHSD(aov_lung_ltd_ct_vol)
summary(aov_ltd_ct_vol)

aov_ltd_ct_hu <- aov(HU ~ type*lung*day, data=lungs)
#TukeyHSD(aov_lung_ltd_ct_hu)
summary(aov_ltd_ct_hu)

aov_ltd_normhu <- aov(normHU ~ type*lung*day, data=lungs)
#TukeyHSD(aov_ltd_normhu)
summary(aov_ltd_normhu)

aov_ltd_pyr <- aov(pyr ~ type*lung*day, data=lungs)
#TukeyHSD(aov_ltd_pyr)
summary(aov_ltd_pyr)

aov_ltd_lac <- aov(lac ~ type*lung*day, data=lungs)
#TukeyHSD(aov_ltd_lac)
summary(aov_ltd_lac)



#from afex package
#afex_ltd <- aov_car(l2p ~ type*lung*day + Error(rat/day*lung), data=lungs)
#afex_ltd <- aov_ez("rat", "l2p", between=c("type"), within=c("day", "lung"), data=lungs)
#

lm_ltd <- lmer(l2p~type*lung*day , data=lungs)


##------------------------

#changed lungs to tx

tx$type<-factor(tx$type,levels = c("allo","syn"), labels = c("Allogeneic","Syngeneic"))
tx$day<-factor(tx$day,levels =  c("3", "7", "14"), labels = c("Day 3", "Day 7", "Day 14"))
tx$lung<-factor(tx$lung, levels = c("nat","tx", "hrt"), labels = c("Native","Tx", "Heart"))
#tx$survived<-factor(tx$survived)
tx$reject<- factor(tx$reject, levels = c("1", "0"), labels = c("Rejected", "Successful"))
tx$vol <- as.numeric(tx$vol)



### Testing Day 3, all allo vs. all syn
allo_tx3 = subset(tx, tx$type == "Allogeneic" & tx$lung == "Tx" & tx$day == "Day 3")
syn_tx3 = subset(tx, tx$type == 'Syngeneic' & tx$lung == 'Tx' & tx$day == "Day 3")

allo_n3 = subset(tx, tx$type == 'Allogeneic' & tx$lung == 'Native' & tx$day == "Day 3")
syn_n3 = subset(tx, tx$type == 'Syngeneic' & tx$lung == 'Native' & tx$day == "Day 3")

allo_tx7 = subset(tx, tx$type == "Allogeneic" & tx$lung == "Tx" & tx$day == "Day 7")
syn_tx7 = subset(tx, tx$type == 'Syngeneic' & tx$lung == 'Tx' & tx$day == "Day 7")

allo_n7 = subset(tx, tx$type == 'Allogeneic' & tx$lung == 'Native' & tx$day == "Day 7")
syn_n7 = subset(tx, tx$type == 'Syngeneic' & tx$lung == 'Native' & tx$day == "Day 7")

allo_tx14 = subset(tx, tx$type == "Allogeneic" & tx$lung == "Tx" & tx$day == "Day 14")
syn_tx14 = subset(tx, tx$type == 'Syngeneic' & tx$lung == 'Tx' & tx$day == "Day 14")

allo_n14 = subset(tx, tx$type == 'Allogeneic' & tx$lung == 'Native' & tx$day == "Day 14")
syn_n14 = subset(tx, tx$type == 'Syngeneic' & tx$lung == 'Native' & tx$day == "Day 14")



t.test(allo_n7$HU,syn_n7$HU, paired = FALSE)
t.test(allo_tx7$HU,syn_tx7$HU, paired = FALSE)

#wilcox.test(allo_n3$l2p,syn_n3$l2p, paired=FALSE)
t.test(allo_n3$vol,syn_n3$vol, paired = FALSE)

t.test(allo_n$pyr,syn_n3$normHU, paired=FALSE)
t.test(allo_tx3$normHU,syn_tx3$normHU, paired=FALSE)

t.test(allo_n7$pyr,allo_tx7$pyr, paired=TRUE)
t.test(allo_n14$pyr,allo_tx14$pyr, paired=TRUE)


wilcox.test(allo_n3$normHU,syn_n3$normHU, paired=FALSE)

wilcox.test(allo_tx3$l2p,syn_tx3$l2p, paired=FALSE)
wilcox.test(allo_tx3$vol,syn_tx3$vol, paired=FALSE)
wilcox.test(allo_tx3$HU,syn_tx3$HU, paired=FALSE)
wilcox.test(allo_tx3$normHU,syn_tx3$normHU, paired=FALSE)

### Day 3, all successful vs. all rejected
suc_tx3 = subset(tx, tx$reject == "Successful" & tx$lung == "Tx" & tx$day == "Day 3")
rej_tx3 = subset(tx, tx$reject == "Rejected" & tx$lung == 'Tx' & tx$day == "Day 3")

suc_n3 = subset(tx, tx$reject == 'Successful' & tx$lung == 'Native' & tx$day == "Day 3")
rej_n3 = subset(tx, tx$reject == 'Rejected' & tx$lung == 'Native' & tx$day == "Day 3")

wilcox.test(suc_n3$l2p,rej_n3$l2p, paired=FALSE)
wilcox.test(suc_n3$vol,rej_n3$vol, paired=FALSE)
wilcox.test(suc_n3$HU,rej_n3$HU, paired=FALSE)
wilcox.test(suc_n3$normHU,rej_n3$normHU, paired=FALSE)

wilcox.test(suc_tx3$l2p,rej_tx3$l2p, paired=FALSE)
wilcox.test(suc_tx3$vol,rej_tx3$vol, paired=FALSE)
wilcox.test(suc_tx3$HU,rej_tx3$HU, paired=FALSE)
wilcox.test(suc_tx3$normHU,rej_tx3$normHU, paired=FALSE)

###  Day 7, all successful vs. all rejected
suc_tx7 = subset(tx, tx$reject == "Successful" & tx$lung == "Tx" & tx$day == "Day 7")
rej_tx7 = subset(tx, tx$reject == "Rejected" & tx$lung == 'Tx' & tx$day == "Day 7")

suc_n7 = subset(tx, tx$reject == 'Successful' & tx$lung == 'Native' & tx$day == "Day 7")
rej_n7 = subset(tx, tx$reject == 'Rejected' & tx$lung == 'Native' & tx$day == "Day 7")

wilcox.test(suc_n7$l2p,rej_n7$l2p, paired=FALSE)
wilcox.test(suc_n7$vol,rej_n7$vol, paired=FALSE)
wilcox.test(suc_n7$HU,rej_n7$HU, paired=FALSE)
wilcox.test(suc_n7$normHU,rej_n7$normHU, paired=FALSE)

                         