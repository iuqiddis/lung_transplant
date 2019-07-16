# library
library(ggplot2)
require(reshape2)
# library(psych)


rm(list = ls())
raw_lungs <- read_csv('/Users/iuqiddis/siddiqui/projects/transplant_lung/r_codes/transplant_data_R_all.csv') #sydney
tx <- filter(raw_lungs, type != 'col', lung != 'hrt')

tx$type<-factor(tx$type,levels = c("allo","syn"), labels = c("Allogeneic","Syngeneic"))
tx$day<-factor(tx$day,levels =  c("3", "7", "14"), labels = c("Day 3", "Day 7", "Day 14"))
tx$lung<-factor(tx$lung, levels = c("nat","tx", "hrt"), labels = c("Native","Tx", "Heart"))
#tx$survived<-factor(tx$survived)
tx$reject<- factor(tx$reject, levels = c("1", "0"), labels = c("Rejected", "Successful"))
tx$vol <- as.numeric(tx$vol)

summary(tx)

############# Observing Normality in l2p data  #################################### 
p <- ggplot(tx, aes(l2p, color=day)) + geom_histogram() + facet_grid(lung~type)
p <- p + geom_density(adjustment=3)
p

# plotting type
p <- ggplot(tx,aes(x = day, y = l2p, color = type))
p <- p + geom_boxplot(fill="white",outlier.colour = NA, 
                      position = position_dodge(width=0.9))
p <- p + geom_point(position=position_jitterdodge(dodge.width=0.9))
p

p <- p + facet_grid(.~lung)
p


# plotting rejection
p <- ggplot(tx,aes(x = day, y = l2p, color = reject))
p <- p + geom_boxplot(fill="white",outlier.colour = NA, 
                      position = position_dodge(width=0.9))
p <- p + geom_point(position=position_jitterdodge(dodge.width=0.9))
p

p <- p + facet_grid(.~lung)
p

# plotting spO2
nat_all = subset(tx, tx$lung == "Tx")

p <- ggplot(nat_all,aes(x = day, y = spO2, color = type))
p <- p + geom_boxplot(fill="white",outlier.colour = NA, 
                      position = position_dodge(width=0.9))
p <- p + geom_point(position=position_jitterdodge(dodge.width=0.9))
p

# plotting amss

p <- ggplot(nat_all,aes(x = day, y = mass, color = type))
p <- p + geom_boxplot(fill="white",outlier.colour = NA, 
                      position = position_dodge(width=0.9))
p <- p + geom_point(position=position_jitterdodge(dodge.width=0.9))
p



############# Observing Normality in HU data  #################################### 
p <- ggplot(tx,aes(x = day, y = normHU, color = type))
p <- p + geom_boxplot(fill="white",outlier.colour = NA, 
                      position = position_dodge(width=0.9))
p <- p + geom_point(position=position_jitterdodge(dodge.width=0.9))
p

p <- p + facet_grid(.~lung)
p

############# Observing Normality in HU data  #################################### 
p <- ggplot(tx,aes(x = day, y = normHU, color = reject))
p <- p + geom_boxplot(fill="white",outlier.colour = NA, 
                      position = position_dodge(width=0.9))
p <- p + geom_point(position=position_jitterdodge(dodge.width=0.9))
p

p <- p + facet_grid(.~lung)
p

############# Observing Normality in HU data  #################################### 
p <- ggplot(tx,aes(x = day, y = HU, color = type))
p <- p + geom_boxplot(fill="white",outlier.colour = NA, 
                      position = position_dodge(width=0.9))
p <- p + geom_point(position=position_jitterdodge(dodge.width=0.9))
p

p <- p + facet_grid(.~lung)
p

p <- ggplot(tx,aes(x = day, y = vol, color = type))
p <- p + geom_boxplot(fill="white",outlier.colour = NA, 
                      position = position_dodge(width=0.9))
p <- p + geom_point(position=position_jitterdodge(dodge.width=0.9))
p

p <- p + facet_grid(.~lung)
p



### Testing Day 3, all allo vs. all syn
allo_tx3 = subset(tx, tx$type == "Allogeneic" & tx$lung == "Tx" & tx$day == "Day 3")
syn_tx3 = subset(tx, tx$type == 'Syngeneic' & tx$lung == 'Tx' & tx$day == "Day 3")

allo_n3 = subset(tx, tx$type == 'Allogeneic' & tx$lung == 'Native' & tx$day == "Day 3")
syn_n3 = subset(tx, tx$type == 'Syngeneic' & tx$lung == 'Native' & tx$day == "Day 3")

wilcox.test(allo_n3$l2p,syn_n3$l2p, paired=FALSE)
wilcox.test(allo_n3$vol,syn_n3$vol, paired=FALSE)
wilcox.test(allo_n3$HU,syn_n3$HU, paired=FALSE)
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

####

library(rpart.plot)
model_based_l2p <- rpart(l2p ~ day+lung+type, data = tx,control=rpart.control(minsplit=2, minbucket=1, cp=0.1))
rpart.plot(model_based_l2p,  digits=3)

# Trees with various CP levels

model_based_HU <- rpart(HU ~ day+lung+type, data = tx,control=rpart.control(minsplit=2, minbucket=1, cp=0.005))
rpart.plot(model_based_HU,  digits=3)

model_based_l2p_HUlevel <- rpart(l2p ~ day+lung+type, data = tx,control=rpart.control(minsplit=2, minbucket=1, cp=0.005))
rpart.plot(model_based_l2p_HUlevel,  digits=3)

model_basel2p <- rpart(l2p ~ day+lung+type+HU, data = tx,control=rpart.control(minsplit=2, minbucket=1, cp=0.025))
rpart.plot(model_basel2p,  digits=3)


# The rest is categorical data analysis to find specific dpendent outcome
find_allog <- rpart(type ~ l2p+HU+day+lung, data = tx,control=rpart.control(minsplit=2, minbucket=1, cp=0.022))
prp(find_allog,  digits=3)

find_rejected <- rpart(reject ~ type+l2p+normHU+day, data = tx,control=rpart.control(minsplit=2, minbucket=1, cp=0.0001))
prp(find_rejected,  digits=3)

find_rejected <- rpart(reject ~ type+l2p+normHU+(day=="Day 3"), data = tx,control=rpart.control(minsplit=2, minbucket=1, cp=0.03))
rpart.plot(find_rejected,  digits=3)


#####

rm(list = ls())
tx <- read.csv('/home/sarmadsiddiqui/sarmad/siddiqui/projects/transplant_lung/r-codes/transplant_data_R-day3.csv',header=T)

tx$type<-factor(tx$type,levels = c("allo","syn"), labels = c("Allogeneic","Syngeneic"))
tx$day<-factor(tx$day,levels =  c("3"), labels = c("Day 3"))
tx$lung<-factor(tx$lung, levels = c("nat","tx"), labels = c("Native","Tx"))
#tx$survived<-factor(tx$survived)
tx$reject<- factor(tx$reject, levels = c("1", "0"), labels = c("Rejected", "Successful"))
tx$vol <- as.numeric(tx$vol)

summary(tx)

find_rejected <- rpart(reject ~ type+l2p+normHU+day, data = tx,control=rpart.control(minsplit=2, minbucket=1, cp=0.03))
rpart.plot(find_rejected,  digits=3)
