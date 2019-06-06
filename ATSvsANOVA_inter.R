library (nparLD)
library(ggplot2)
library(pROC)

# Initial setup
likert <- c(1,2,3,4,5)
simruns = 1000
set.seed(52*6*simruns*2)

# Between subjects probabilities
Female <-c(.25,.30,.25,.1,.1)
Male <-c(.1,.1,.25,.30,.25)

# Within subjects probabilities
Anthropomorphic <-c(.1,.25,.3,.25,.1)
Zoomorphic <-c(.1,.1,.25,.30,.25)
Machinelike <-c(.25,.30,.25,.1,.1)

# Simulation run
pvalues_ats <- matrix(nrow = simruns, ncol = 3)
pvalues_anova <- matrix(nrow = simruns, ncol = 3)


for (i in 1:simruns)
{
  
AF <- sample(likert, size=52, replace=T, prob=Anthropomorphic*Female)
ZF <- sample(likert, size=52, replace=T, 
               prob=Zoomorphic*(Female-.1))
MF <- sample(likert, size=52, replace=T, prob=Machinelike*Female)
AM <- sample(likert, size=52, replace=T, prob=Anthropomorphic*Male)
ZM <- sample(likert, size=52, replace=T, 
               prob=Zoomorphic*(Male-.1))
MM <- sample(likert, size=52, replace=T, prob=Machinelike*Male)

DAF <- data.frame(id=1:52,Group="Female",Condition="Anthropomorphic",response=AF)
DZF <- data.frame(id=1:52,Group="Female",Condition="Zoomorphic",response=ZF)
DMF <- data.frame(id=1:52,Group="Female",Condition="Machinelike",response=MF)
DAM <- data.frame(id=53:104,Group="Male",Condition="Anthropomorphic",response=AM)
DZM <- data.frame(id=53:104,Group="Male",Condition="Zoomorphic",response=ZM)
DMM <- data.frame(id=53:104,Group="Male",Condition="Machinelike",response=MM)

experiment <- rbind(DAF,DZF,DMF,DAM,DZM,DMM)

ats <- nparLD(response ~ Condition*Group, data = experiment, subject = 'id', description = TRUE)
anova <- aov(response ~ Group*Condition + Error(id/Condition), data=experiment)

p_ats <- ats$ANOVA.test[7:9] #p values of group,condition and group*condition
p_anova <- summary(anova)$"Error: Within"[[1]]$"Pr(>F)"[1:3]

pvalues_ats[i,1:3] <- p_ats
pvalues_anova[i,1:3] <- p_anova
}

simresults_ats <- data.frame(test="ATS",group=pvalues_ats[,1],condition=pvalues_ats[,2],interaction=pvalues_ats[,3])
simresults_anova <- data.frame(test="ANOVA",group=pvalues_anova[,1],condition=pvalues_anova[,2],interaction=pvalues_anova[,3])
simresults <- rbind(simresults_ats,simresults_anova)

# Plotting histograms
p7 <- ggplot(simresults, aes(x = interaction, fill=test)) + # change x to evaluate group,condition,interaction
  geom_histogram(aes(y=..count..),position="identity", alpha=0.6) +
  scale_x_continuous(name = "p values") +
  scale_y_continuous(name = "Count") +
  ggtitle("Distribution of p values - Sex") +
  theme_bw() +
  geom_vline(xintercept = 0.05, size = 1, colour = "#FF3721",
           linetype = "dashed") +
  scale_fill_brewer(palette="Accent")+
  labs(fill="Test")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size=18,hjust = 0.5,face="bold"),axis.title=element_text(size=14,face="bold"))

p7

plot(ats)
title(ylab = "RTE",cex.lab=1.2)

# Null simulation
null <- c(.2,.2,.2,.2,.2)

nullpvalues_ats <- matrix(nrow = simruns, ncol = 3)
nullpvalues_anova <- matrix(nrow = simruns, ncol = 3)

for (i in 1:simruns)
{
nA1 <- sample(likert, size=52, replace=T, prob=null*null)
nB1 <- sample(likert, size=52, replace=T, prob=null*null)
nC1 <- sample(likert, size=52, replace=T, prob=null*null)

nA2 <- sample(likert, size=52, replace=T, prob=null*null)
nB2 <- sample(likert, size=52, replace=T, prob=null*null)
nC2 <- sample(likert, size=52, replace=T, prob=null*null)

nDA1 <- data.frame(id=1:52,Group="Female",Condition="Anthropomorphic",response=nA1)
nDB1 <- data.frame(id=1:52,Group="Female",Condition="Zoomorphic",response=nB1)
nDC1 <- data.frame(id=1:52,Group="Female",Condition="Machinelike",response=nC1)
nDA2 <- data.frame(id=53:104,Group="Male",Condition="Anthropomorphic",response=nA2)
nDB2 <- data.frame(id=53:104,Group="Male",Condition="Zoomorphic",response=nB2)
nDC2 <- data.frame(id=53:104,Group="Male",Condition="Machinelike",response=nC2)

nullexperiment <- rbind(nDA1,nDB1,nDC1,nDA2,nDB2,nDC2)

nullats <- nparLD(response ~ Condition*Group, data = nullexperiment, subject = 'id', description = TRUE)
nullanova <- aov(response ~ Group*Condition + Error(id/Condition), data=nullexperiment)

nullp_ats <- nullats$ANOVA.test[7:9] #p values of group,condition and group*condition
nullp_anova <- summary(nullanova)$"Error: Within"[[1]]$"Pr(>F)"[1:3]

nullpvalues_ats[i,1:3] <- nullp_ats
nullpvalues_anova[i,1:3] <- nullp_anova

}

nullsimresults_ats <- data.frame(test="ATS",group=nullpvalues_ats[,1],condition=nullpvalues_ats[,2],interaction=nullpvalues_ats[,3])
nullsimresults_anova <- data.frame(test="ANOVA",group=nullpvalues_anova[,1],condition=nullpvalues_anova[,2],interaction=nullpvalues_anova[,3])
nullsimresults <- rbind(nullsimresults_ats,nullsimresults_anova)

# Plotting histograms from null simulation
p8 <- ggplot(nullsimresults, aes(x = interaction, fill=test)) + # change x to evaluate group,condition,interaction
  geom_histogram(aes(y=..count..),position="identity", alpha=0.6) +
  scale_x_continuous(name = "p values") +
  scale_y_continuous(name = "Count") +
  ggtitle("Frequency histogram of p values") +
  theme_bw() +
  geom_vline(xintercept = 0.05, size = 1, colour = "#FF3721",
             linetype = "dashed") +
  scale_fill_brewer(palette="Accent")+
  labs(fill="Test")

plot(nullats)
title(ylab = "RTE",cex.lab=1.2)

# Compute ROC
simresults$type=1
nullsimresults$type=0
dataROC <- rbind(simresults,nullsimresults)
dataROC$b=(dataROC$group<0.05)*1
dataROC$w=(dataROC$condition<0.05)*1
dataROC$i=(dataROC$interaction<0.05)*1

isats = dataROC$test == "ATS"
roc_atsbetween <- roc(dataROC[isats,]$type,dataROC[isats,]$b)
auc(roc_atsbetween)

roc_atswithin <- roc(dataROC[isats,]$type,dataROC[isats,]$w)
auc(roc_atswithin)

roc_atsinter <- roc(dataROC[isats,]$type,dataROC[isats,]$i)
auc(roc_atsinter)

isanova = dataROC$test == "ANOVA"
roc_anovabetween <- roc(dataROC[isanova,]$type,dataROC[isanova,]$b)
auc(roc_anovabetween)

roc_anovawithin <- roc(dataROC[isanova,]$type,dataROC[isanova,]$w)
auc(roc_anovawithin)

roc_anovainter <- roc(dataROC[isanova,]$type,dataROC[isanova,]$i)
auc(roc_anovainter)

# Plot ROC
plot(roc_atswithin,main="ROC - Condition",col="black")
plot(roc_anovawithin,add=TRUE,col="red")
legend(0.45, 0.5, legend=c("ATS", "ANOVA"),
       col=c("black", "red"), lty=1, cex=0.8)

plot(roc_atsbetween,main="ROC - Group",col="black")
plot(roc_anovabetween,add=TRUE,col="red")
legend(0.45, 0.5, legend=c("ATS", "ANOVA"),
       col=c("black", "red"), lty=1, cex=0.8)

plot(roc_atsinter,main="ROC - Interaction",col="black")
plot(roc_anovainter,add=TRUE,col="red")
legend(0.45, 0.5, legend=c("ATS", "ANOVA"),
       col=c("black", "red"), lty=1, cex=0.8)

