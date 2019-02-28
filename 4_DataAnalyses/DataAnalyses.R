#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Data analyses FlexibleSexualAppetite part 1
#	 Start : 11/8/2017
#	 last modif : 3/15/2018
#	 commit: separate data handling from data analyses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))

{# packages
library(lme4)
library(arm)
library(rptR)
library(pbapply)
library(ggplot2)
require(gridExtra)
require(grid)
library(here)
}

{# load data

MY_TABLE_BugTest <- read.csv(paste(here(),"3_ProcessedData/MY_TABLE_BugTest.csv", sep="/")) 
MY_TABLE_TermiteTest <- read.csv(paste(here(),"3_ProcessedData/MY_TABLE_TermiteTest.csv", sep="/")) 
MY_TABLE_MaleTest <- read.csv(paste(here(),"3_ProcessedData/MY_TABLE_MaleTest.csv", sep="/")) 
MY_TABLE_MID <- read.csv(paste(here(),"3_ProcessedData/MY_TABLE_MID.csv", sep="/")) 
MY_TABLE_Step <- read.csv(paste(here(),"3_ProcessedData/MY_TABLE_Step.csv", sep="/")) 

MY_TABLE_Videos <- read.csv(paste(here(),"3_ProcessedData/MY_TABLE_Videos.csv", sep="/")) 


}


## Trt Red Averse is the reference (intercept)


{# step 1

mod1 <- glm (AttackBugYN ~ Trt + Fcondition , "binomial", data = MY_TABLE_BugTest)

par(mfrow=c(2,2))
plot(mod1)

summary(mod1)


bugaverse <- invlogit(coef(summary(mod1))[1, 1]) # likelihood of eating the bug for red averse females

bugpref <- invlogit(coef(summary(mod1))[1, 1] + coef(summary(mod1))[2, 1]) # likelihood of eating the bug for red preference females

buglowerCIaverse <-  invlogit(coef(summary(mod1))[1, 1]-coef(summary(mod1))[1, 2]*1.96)
bugupperCIaverse <-invlogit(coef(summary(mod1))[1, 1]+coef(summary(mod1))[1, 2]*1.96)
  
  (invlogit(coef(summary(mod1))[1, 1]+coef(summary(mod1))[1, 2])
  -invlogit(coef(summary(mod1))[1, 1]-coef(summary(mod1))[1, 2]))/2	# 0.06510539 average SE for red averse

 (invlogit(coef(summary(mod1))[1, 1]+ coef(summary(mod1))[2, 1]+coef(summary(mod1))[2, 2])
  -invlogit(coef(summary(mod1))[1, 1]+ coef(summary(mod1))[2, 1]-coef(summary(mod1))[2, 2]))/2	# 0.08832562 average SE for red preference

bugupperCIpref <-invlogit(coef(summary(mod1))[1, 1]+ coef(summary(mod1))[2, 1]+coef(summary(mod1))[2, 2]*1.96)
buglowerCIpref <-invlogit(coef(summary(mod1))[1, 1]+ coef(summary(mod1))[2, 1]-coef(summary(mod1))[2, 2]*1.96)


invlogit(coef(summary(mod1))[3, 1]) # 0.0001578938 back trasnformed estimate for body condition
(invlogit(coef(summary(mod1))[3, 1]+coef(summary(mod1))[3, 2]) 
- invlogit(coef(summary(mod1))[3, 1]-coef(summary(mod1))[3, 2]) ) /2# 0.5 back trasnformed estimate for body condition SE

nBugYes <- nrow(MY_TABLE_BugTest[MY_TABLE_BugTest$AttackBugYN == 1,])

  ## to get one sided test p value
  mod1p <- coef(summary(mod1))[2, 4]/2
  
  
  shapiro.test(MY_TABLE_BugTest$Fcondition)
  hist(MY_TABLE_BugTest$Fcondition)
  wilcox.test(MY_TABLE_BugTest$Fcondition[MY_TABLE_BugTest$Trt == "RedPreference"],
        MY_TABLE_BugTest$Fcondition[MY_TABLE_BugTest$Trt == "RedAverse"])
  



# exploration: among those tat did attack, red averse took longer time ?
  shapiro.test(log(MY_TABLE_BugTest$LatencyAttack,10))
  hist(log(MY_TABLE_BugTest$LatencyAttack,10))
  wilcox.test(MY_TABLE_BugTest$LatencyAttack[MY_TABLE_BugTest$Trt == "RedPreference"],
         MY_TABLE_BugTest$LatencyAttack[MY_TABLE_BugTest$Trt == "RedAverse"], na.rm=TRUE)
 
  
  mean(MY_TABLE_BugTest$LatencyAttack[MY_TABLE_BugTest$Trt == "RedPreference"], na.rm=TRUE)
  mean(MY_TABLE_BugTest$LatencyAttack[MY_TABLE_BugTest$Trt == "RedAverse"], na.rm=TRUE)
  sd(MY_TABLE_BugTest$LatencyAttack[MY_TABLE_BugTest$Trt == "RedPreference"], na.rm=TRUE)/
    sqrt(length(MY_TABLE_BugTest$LatencyAttack[MY_TABLE_BugTest$Trt == "RedPreference" & !is.na(MY_TABLE_BugTest$LatencyAttack)]))
  sd(MY_TABLE_BugTest$LatencyAttack[MY_TABLE_BugTest$Trt == "RedAverse"], na.rm=TRUE)/
    sqrt(length(MY_TABLE_BugTest$LatencyAttack[MY_TABLE_BugTest$Trt == "RedAverse"& !is.na(MY_TABLE_BugTest$LatencyAttack)]))


}

{# step 2

mod2 <- glm (AttackNewRedYN ~ Trt, family = "binomial",  data = MY_TABLE_TermiteTest)

par(mfrow=c(2,2))
plot(mod2)

summary(mod2)


termiteaverse <- invlogit(coef(summary(mod2))[1, 1]) # likelihood of eating the red termite for red averse females

termitepref <- invlogit(coef(summary(mod2))[1, 1] + coef(summary(mod2))[2, 1]) # likelihood of eating the red termite for red preference females

(invlogit(coef(summary(mod2))[1, 1]+coef(summary(mod2))[1, 2])
  -invlogit(coef(summary(mod2))[1, 1]-coef(summary(mod2))[1, 2]))/2	# 0.06621911 average SE for red averse

termitelowerseaverse <- invlogit(coef(summary(mod2))[1, 1]-coef(summary(mod2))[1, 2]*1.96)
termiteupperseaverse <- invlogit(coef(summary(mod2))[1, 1]+coef(summary(mod2))[1, 2]*1.96)

(invlogit(coef(summary(mod2))[1, 1]+ coef(summary(mod2))[2, 1]+coef(summary(mod2))[2, 2])
  -invlogit(coef(summary(mod2))[1, 1]+ coef(summary(mod2))[2, 1]-coef(summary(mod2))[2, 2]))/2	# 0.09226776 average SE for red preference

termiteuppersepref <- invlogit(coef(summary(mod2))[1, 1]+ coef(summary(mod2))[2, 1]+coef(summary(mod2))[2, 2]*1.96)
termitelowersepref <- invlogit(coef(summary(mod2))[1, 1]+ coef(summary(mod2))[2, 1]-coef(summary(mod2))[2, 2]*1.96)


nTermiteYes <- nrow(MY_TABLE_TermiteTest[MY_TABLE_TermiteTest$AttackNewRedYN == 1,])


  ## to get one sided test p value
 mod2p <- coef(summary(mod2))[2, 4]/2


  ## to check equality of motivation to feed
  shapiro.test(MY_TABLE_TermiteTest$LatencyAttack)
  hist(MY_TABLE_TermiteTest$LatencyAttack)
  hist(log(MY_TABLE_TermiteTest$LatencyAttack))
  shapiro.test(log(MY_TABLE_TermiteTest$LatencyAttack))
  LogLatencyAttackTermiteRedPreference <- log(MY_TABLE_TermiteTest$LatencyAttack[MY_TABLE_TermiteTest$Trt == "RedPreference"] )
  LogLatencyAttackTermiteRedAverse <- log(MY_TABLE_TermiteTest$LatencyAttack[MY_TABLE_TermiteTest$Trt == "RedAverse"] ) 
                                                                                          
  t.test (LogLatencyAttackTermiteRedPreference, LogLatencyAttackTermiteRedAverse)


  median(MY_TABLE_TermiteTest$LatencyAttack[MY_TABLE_TermiteTest$Trt == "RedPreference"])
  median(MY_TABLE_TermiteTest$LatencyAttack[MY_TABLE_TermiteTest$Trt == "RedAverse"])
  summary(MY_TABLE_TermiteTest$LatencyAttack)
  
  
}

{# step 3

mod3 <- glm (CannibalizedRedYN ~ Trt+ DeltaMsize + DeltaMcondition, family = "binomial", data = MY_TABLE_MaleTest)

par(mfrow=c(2,2))
plot(mod3)

summary(mod3)

  ## to get one sided test p value
 mod3p <-  coef(summary(mod3))[2, 4]/2

 nMaleYes <- nrow(MY_TABLE_MaleTest[MY_TABLE_MaleTest$CannibalizedRedYN == 1,])
 
 
maleaverse <- invlogit(coef(summary(mod3))[1, 1]) # likelihood of eating the red male for red averse females
 
malepref <- invlogit(coef(summary(mod3))[1, 1] + coef(summary(mod3))[2, 1]) # likelihood of eating the red male for red preference females
 
(invlogit(coef(summary(mod3))[1, 1]+coef(summary(mod3))[1, 2])
   -invlogit(coef(summary(mod3))[1, 1]-coef(summary(mod3))[1, 2]))/2	# 0.06621911 average SE for red averse

maleupperseaverse <- invlogit(coef(summary(mod3))[1, 1]+coef(summary(mod3))[1, 2]*1.96)
malelowerseaverse <- invlogit(coef(summary(mod3))[1, 1]-coef(summary(mod3))[1, 2]*1.96)
 
(invlogit(coef(summary(mod3))[1, 1]+ coef(summary(mod3))[2, 1]+coef(summary(mod3))[2, 2])
   -invlogit(coef(summary(mod3))[1, 1]+ coef(summary(mod3))[2, 1]-coef(summary(mod3))[2, 2]))/2	# 0.09226776 average SE for red preference
 
malelowersepref <- invlogit(coef(summary(mod3))[1, 1]+ coef(summary(mod3))[2, 1]-coef(summary(mod3))[2, 2]*1.96)
maleuppersepref <- invlogit(coef(summary(mod3))[1, 1]+ coef(summary(mod3))[2, 1]+coef(summary(mod3))[2, 2]*1.96)
 



invlogit(coef(summary(mod3))[3, 1]) # 0.001618516 back transformed estimate for DeltaMsize
(invlogit(coef(summary(mod3))[3, 1]+coef(summary(mod3))[3, 2]) 
  - invlogit(coef(summary(mod3))[3, 1]-coef(summary(mod3))[3, 2]) ) /2# 0.1987484 back transformed estimate for DeltaMsize SE


invlogit(coef(summary(mod3))[4, 1]) # 3.141732e-12 back transformed estimate for DeltaMcondition
(invlogit(coef(summary(mod3))[4, 1]+coef(summary(mod3))[4, 2]) 
  - invlogit(coef(summary(mod3))[4, 1]-coef(summary(mod3))[4, 2]) ) /2# 0.5 back transformed estimate forDeltaMcondition SE




  ## to check equality of male motivation to court
  # shapiro.test(MY_TABLE_MID$Latency_to_court)
  # t.test (MY_TABLE_MID$Latency_to_court[MY_TABLE_MID$Mcolor == "Red"], 
  #         MY_TABLE_MID$Latency_to_court[MY_TABLE_MID$Mcolor == "Black"])


 # exploration: male test for test where male consumption happened within the first day
 MY_TABLE_MaleTest$TrialDate <-  as.POSIXct(MY_TABLE_MaleTest$TrialDate)
 MY_TABLE_MaleTest$TrialDateEnd <-  as.POSIXct(MY_TABLE_MaleTest$TrialDateEnd)
 
 mod3_firstDay <- glm (CannibalizedRedYN ~ Trt+ DeltaMsize + DeltaMcondition
                    , family = "binomial" 
                    , data = MY_TABLE_MaleTest[MY_TABLE_MaleTest$TrialDateEnd == MY_TABLE_MaleTest$TrialDate 
                                                    & !is.na(MY_TABLE_MaleTest$TrialDateEnd),])
 
 par(mfrow=c(2,2))
 plot(mod3_firstDay)
 
 summary(mod3_firstDay)
 
 
 invlogit(coef(summary(mod3_firstDay))[1, 1]) # likelihood of eating the red male for red averse females
 
 invlogit(coef(summary(mod3_firstDay))[1, 1] + coef(summary(mod3_firstDay))[2, 1]) # likelihood of eating the red male for red preference females
 
 (invlogit(coef(summary(mod3_firstDay))[1, 1]+coef(summary(mod3_firstDay))[1, 2])
   -invlogit(coef(summary(mod3_firstDay))[1, 1]-coef(summary(mod3_firstDay))[1, 2]))/2	# 0.06621911 average SE for red averse
 
 (invlogit(coef(summary(mod3_firstDay))[1, 1]+ coef(summary(mod3_firstDay))[2, 1]+coef(summary(mod3_firstDay))[2, 2])
   -invlogit(coef(summary(mod3_firstDay))[1, 1]+ coef(summary(mod3_firstDay))[2, 1]-coef(summary(mod3_firstDay))[2, 2]))/2	# 0.09226776 average SE for red preference
 
 
 
 # exploration: male tests where no male male competition to exclude it entirely as a possible confounding factor
 ## subset of valid trials (no spider died from other reason than cannibalism) and where no male male competition
 FID_NoMaleMaleFight <- 17000+ MY_TABLE_Videos$FID[MY_TABLE_Videos$NbMphysicalInter == 0 & MY_TABLE_Videos$ExcludeYN ==0]
 FID_NoMaleMaleAttack <- 17000+ MY_TABLE_Videos$FID[MY_TABLE_Videos$NbMAttacks == 0 & MY_TABLE_Videos$ExcludeYN ==0]
 MY_TABLE_MaleTest_NoMaleMaleFight <- MY_TABLE_MaleTest[MY_TABLE_MaleTest$FID %in% FID_NoMaleMaleFight,]
 MY_TABLE_MaleTest_NoMaleMaleAttack <- MY_TABLE_MaleTest[MY_TABLE_MaleTest$FID %in% FID_NoMaleMaleFight,]
 
 mod3_NoMaleMaleFight <- glm (CannibalizedRedYN ~ Trt+ DeltaMsize + DeltaMcondition
                              , family = "binomial"
                              , data = MY_TABLE_MaleTest_NoMaleMaleFight)
 
 par(mfrow=c(2,2))
 plot(mod3_NoMaleMaleFight)
 
 summary(mod3_NoMaleMaleFight)
 

 
 mod3_NoMaleMaleAttack <- glm (CannibalizedRedYN ~ Trt+ DeltaMsize + DeltaMcondition
                              , family = "binomial"
                              , data = MY_TABLE_MaleTest_NoMaleMaleAttack)
 
 par(mfrow=c(2,2))
 plot(mod3_NoMaleMaleAttack)
 
 summary(mod3_NoMaleMaleAttack)
 
}  
 
  
{# exploratory analyses: repeatability of female bias

mod4 <- glmer (AttackRedYN ~ Trt + (1|FID), family = "binomial", data=MY_TABLE_Step)
 
par(mfrow=c(2,2))
qqnorm(resid(mod4))
qqline(resid(mod4))
qqnorm(unlist(ranef(mod4)$FID))
qqline(unlist(ranef(mod4)$FID))
plot(fitted(mod4), resid(mod4))
abline(h=0)
plot(fitted(mod4),jitter(MY_TABLE_Step$AttackRedYN, 0.5))
abline(0,1)

mod4withrowID <- glm (AttackRedYN ~ Trt + (1|FID) + (1|rowID), family = "binomial", data=MY_TABLE_Step)
anova(mod4,mod4withrowID)

summary(mod4)

print(rpt(formula = AttackRedYN ~ Trt + (1|FID),
          grname = c("Fixed","FID"),
          data= MY_TABLE_Step,
          datatype = "Binary",
          nboot = 1000,
          npermut = 0,
          adjusted = FALSE))

# female response with termite predict response with male ?
MY_TABLE_TermiteMale <- merge(MY_TABLE_TermiteTest[,c('FID','AttackNewRedYN')],MY_TABLE_MaleTest[,c('FID','CannibalizedRedYN')],by='FID' )
nrow(MY_TABLE_TermiteMale)

cor.test(MY_TABLE_TermiteMale$CannibalizedRedYN,MY_TABLE_TermiteMale$AttackNewRedYN)
summary(glm (CannibalizedRedYN ~ AttackNewRedYN, family = "binomial", data=MY_TABLE_TermiteMale))



# Repeatability of female latency to attack

mod5 <- lmer(LatencyAttack ~ Trt + (1|FID), data=MY_TABLE_Step) # weird
summary(mod5)

}


{# to calculate odds ratios of eating the red prey for the Red Preference group relative to the red averse group

## step 1 
table(MY_TABLE_BugTest$Trt, MY_TABLE_BugTest$AttackBugYN)
oddsBug <- exp(cbind(OR=coef(mod1), confint(mod1)))[2,] 

## step 2
table(MY_TABLE_TermiteTest$Trt, MY_TABLE_TermiteTest$AttackNewRedYN)
oddsTermite <- exp(cbind(OR=coef(mod2), confint(mod2)))[2,]

## step 3: odds ratio ?
table(MY_TABLE_MaleTestValid$Trt, MY_TABLE_MaleTestValid$CannibalizedRedYN)
oddsMales <- exp(cbind(OR=coef(mod3_firstDay), confint(mod3_firstDay)))[2,]  
oddsMales1stDay <- exp(cbind(OR=coef(mod3_firstDay_firstDay), confint(mod3_firstDay_firstDay)))[2,]


odds <- data.frame(rbind(oddsBug,oddsTermite,oddsMales1stDay,oddsMales))
colnames(odds) <- c('OR','lower','upper')
odds$testname <- c('3_Bug','2_Termite', '1_Male1stday', '0_Males')

ggplot(odds, aes(y= OR, x = testname)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
    scale_y_log10(breaks=c(0.1,0.5,1,2,3,4), labels = c(0.1,0.5,1,2,3,4)) +
    scale_x_discrete(labels= c("Males","Males 1st day", "Termites", "Bug")) +
    geom_hline(yintercept = 1, linetype=2) +
    coord_flip() +
    labs(x = NULL, y = 'Odds Ratio') +
    theme_bw() +  theme(text = element_text(size=20))

}

{# Comparing each group to 50/50

## step 1
chisq.test(table(MY_TABLE_BugTest$AttackBugYN[MY_TABLE_BugTest$Trt == 'RedPreference']), p=c(0.5,0.5))
chisq.test(table(MY_TABLE_BugTest$AttackBugYN[MY_TABLE_BugTest$Trt == 'RedAverse']), p=c(0.5,0.5))

ggplot(MY_TABLE_BugTest,aes(x=AttackBugYN,group=Trt,fill=Trt))+
  geom_bar(position="dodge",aes(y = ..prop.., fill = Trt)) +
  scale_x_discrete(NULL, c(0,1), c("Did not consumed bug", "Consumed bug"), c(0,1)) +
 scale_y_continuous(NULL,labels=scales::percent) +
  theme_bw()+
  scale_fill_discrete(name = "Treatment")+
  theme(text = element_text(size=20))


##step 2
chisq.test(table(MY_TABLE_TermiteTest$AttackNewRedYN[MY_TABLE_TermiteTest$Trt == 'RedPreference']), p=c(0.5,0.5))
chisq.test(table(MY_TABLE_TermiteTest$AttackNewRedYN[MY_TABLE_TermiteTest$Trt == 'RedAverse']), p=c(0.5,0.5))

ggplot(MY_TABLE_TermiteTest,aes(x=AttackNewRedYN,group=Trt,fill=Trt))+
  geom_bar(position="dodge",aes(y = ..prop.., fill = Trt)) +
  scale_x_discrete(NULL, c(0,1), c("Consumed grey termite", "Consumed red termite"), c(0,1)) +
  scale_y_continuous(NULL,labels=scales::percent) + theme_bw()+
  scale_fill_discrete(name = "Treatment")+
  theme(text = element_text(size=20))

##step 3
chisq.test(table(MY_TABLE_MaleTest$CannibalizedRedYN[MY_TABLE_MaleTest$Trt == 'RedPreference']), p=c(0.5,0.5))
chisq.test(table(MY_TABLE_MaleTest$CannibalizedRedYN[MY_TABLE_MaleTest$Trt == 'RedAverse']), p=c(0.5,0.5))


ggplot(MY_TABLE_MaleTest,aes(x=CannibalizedRedYN,group=Trt,fill=Trt))+
  geom_bar(position="dodge",aes(y = ..prop.., fill = Trt)) +
  scale_x_discrete(NULL, c(0,1), c("Consumed black male", "Consumed red male"), c(0,1)) +
  theme_bw()+scale_y_continuous(NULL,labels=scales::percent) +
  scale_fill_discrete(name = "Treatment")+
  theme(text = element_text(size=20))


# juvenile females painted black or left white faces, predated by adult female (red preference females finished with the experiment)

contingencyTable <- rbind(table(MY_TABLE_MaleTest$Trt, MY_TABLE_MaleTest$CannibalizedRedYN), c(13,5))
rownames(contingencyTable)[3] <- 'Control'


chisq.test(contingencyTable)

}

{# Comparing all to 50/50
  
#step 1
chisq.test(table(MY_TABLE_BugTest$AttackBugYN), p=c(0.5,0.5))


#step 2
chisq.test(table(MY_TABLE_TermiteTest$AttackNewRedYN), p=c(0.5,0.5))


#step 3    
chisq.test(table(MY_TABLE_MaleTest$CannibalizedRedYN), p=c(0.5,0.5))
summary(glm ( MY_TABLE_MaleTest$CannibalizedRedYN~1, family = "binomial", data=MY_TABLE_Step))

# all 
mod4 <- glmer (AttackRedYN ~ Trt + (1|FID), family = "binomial", data=MY_TABLE_Step)
summary(mod4) 

# all 
mod4bis <- glmer (AttackRedYN ~ (1|FID), family = "binomial", data=MY_TABLE_Step)
summary(mod4bis)

}








# graphs 

{figdata <- data.frame(rbind(c('Bug', bugaverse,buglowerCIaverse, bugupperCIaverse, 'Red averse'), 
                            c('Bug',bugpref, buglowerCIpref, bugupperCIpref,'Red accustomed'),
                            c('Termite', termiteaverse, termitelowerseaverse, termiteupperseaverse, 'Red averse'),
                            c('Termite', termitepref, termitelowersepref, termiteuppersepref,'Red accustomed'),
                            c('Male', maleaverse, malelowerseaverse, maleupperseaverse, 'Red averse'),
                            c('Male', malepref, malelowersepref, maleuppersepref, 'Red accustomed')))

colnames(figdata) <- c('test', 'estimate', 'lowerCI', 'upperCI','Treatment')
figdata$estimate <- as.numeric(as.character(figdata$estimate))*100
figdata$lowerCI <- as.numeric(as.character(figdata$lowerCI))*100
figdata$upperCI <- as.numeric(as.character(figdata$upperCI))*100
}

Fig1 <- {ggplot(data=figdata[figdata$test == 'Bug',], aes(x=Treatment, y=estimate))+
    labs(x=NULL, y= "Percentage of spiders 
         consuming the red prey (95% CI)", title = "A. Bug test")+
    
   scale_y_continuous(breaks =seq(0,100, by = 10),limits = c(0,100)) +
  
    annotate("text", x="Red averse", y = 1, label = "n = 56", size=7) +
    annotate("text", x="Red accustomed", y = 1, label = "n = 69", size=7) +
    
    geom_errorbar(aes(ymin=lowerCI, ymax=upperCI), size = 2, width =1,na.rm=TRUE)+
    geom_point(size =6, stroke = 1) +   
      theme_classic()+
    theme(
      panel.border = element_rect(colour = "black", fill=NA), 
      axis.title.y=element_text(size=20,face="bold", margin=margin(l=5)),
      axis.text.y=element_text(size=15),
      axis.text.x=element_text(size=20, face="bold",margin=margin(t=5)),
      plot.title = element_text(size=25, face="bold", hjust= 0.5, vjust = 0.5),
      plot.margin = unit(c(0.2,0.2,0.3,0.3), "cm"))
}


Fig2 <- {ggplot(data=figdata[figdata$test == 'Termite',], aes(x=Treatment, y=estimate))+
    labs(x=NULL, y= NULL, title = "B. Termite test")+
    
    
    scale_y_continuous(breaks =seq(0,100, by = 10),limits = c(0,100)) +
    annotate("text", x="Red averse", y = 1, label = "n = 53", size=7) +
    annotate("text", x="Red accustomed", y = 1, label = "n = 64", size=7) +
    
    geom_errorbar(aes(ymin=lowerCI, ymax=upperCI), size = 2, width =1,na.rm=TRUE)+
    geom_point(size =6, stroke = 1) +   
    theme_classic()+
    theme(
      panel.border = element_rect(colour = "black", fill=NA), 
      axis.text.y = element_blank(), 
      axis.text.x=element_text(size=20, face="bold",margin=margin(t=5)),
      plot.title = element_text(size=25, face="bold", hjust= 0.5, vjust = 0.5),
      plot.margin = unit(c(0.2,0.2,0.3,0.3), "cm"))
}


Fig3 <- {ggplot(data=figdata[figdata$test == 'Male',], aes(x=Treatment, y=estimate))+
    labs(x=NULL, y= NULL, title = "C. Male test")+
    
       
    scale_y_continuous(breaks =seq(0,100, by = 10),limits = c(0,100)) +
    annotate("text", x="Red averse", y = 1, label = "n = 42", size=7) +
    annotate("text", x="Red accustomed", y = 1, label = "n = 37", size=7) +
    
    geom_errorbar(aes(ymin=lowerCI, ymax=upperCI), size = 2, width =1,na.rm=TRUE)+
    geom_point(size =6,  stroke = 1) +   
    theme_classic()+
    theme(
      panel.border = element_rect(colour = "black", fill=NA),
      axis.text.y = element_blank(),
      axis.text.x=element_text(size=20, face="bold",margin=margin(t=5)),
      plot.title = element_text(size=25, face="bold", hjust= 0.5, vjust = 0.5),
      plot.margin = unit(c(0.2,0.2,0.3,0.3), "cm"))
}

g1 <- ggplotGrob(Fig1)
g2 <- ggplotGrob(Fig2)
g3 <- ggplotGrob(Fig3)

grid.arrange(cbind(g1, g2, g3, size = "last"))




