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
library(rptR)
library(pbapply)
library(ggplot2)
}

{# load data

R_Data_folder <- "R_Data"

MY_TABLE_BugTest <- read.csv(paste(R_Data_folder,"MY_TABLE_BugTest.csv", sep="/")) 
MY_TABLE_TermiteTest <- read.csv(paste(R_Data_folder,"MY_TABLE_TermiteTest.csv", sep="/")) 
MY_TABLE_MaleTest <- read.csv(paste(R_Data_folder,"MY_TABLE_MaleTest.csv", sep="/")) 
MY_TABLE_MID <- read.csv(paste(R_Data_folder,"MY_TABLE_MID.csv", sep="/")) 
MY_TABLE_Step <- read.csv(paste(R_Data_folder,"MY_TABLE_Step.csv", sep="/")) 

}


## Trt Red Averse is the reference (intercept)


{# step 1

mod1 <- glm (AttackBugYN ~ Trt + Fcondition , "binomial", data = MY_TABLE_BugTest)

par(mfrow=c(2,2))
plot(mod1)

summary(mod1)

  ## to get one sided test p value
  mod1p <- coef(summary(mod1))[2, 4]/2
  
  
  shapiro.test(MY_TABLE_BugTest$Fcondition)
  hist(MY_TABLE_BugTest$Fcondition)
  t.test(MY_TABLE_BugTest$Fcondition[MY_TABLE_BugTest$Trt == "RedPreference"],
        MY_TABLE_BugTest$Fcondition[MY_TABLE_BugTest$Trt == "RedAverse"])

  
# exploration: among those tat did attack, red averse took longer time ?
  shapiro.test(log(MY_TABLE_BugTest$LatencyAttack,10))
  hist(log(MY_TABLE_BugTest$LatencyAttack,10))
  t.test(log(MY_TABLE_BugTest$LatencyAttack[MY_TABLE_BugTest$Trt == "RedPreference"]),
        log( MY_TABLE_BugTest$LatencyAttack[MY_TABLE_BugTest$Trt == "RedAverse"]), na.rm=TRUE)
  kruskal.test(MY_TABLE_BugTest$LatencyAttack~MY_TABLE_BugTest$Trt)
  
  
}

{# step 2

mod2 <- glm (AttackNewRedYN ~ Trt, family = "binomial",  data = MY_TABLE_TermiteTestValid)

par(mfrow=c(2,2))
plot(mod2)

summary(mod2)

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


}

{# step 3


mod3 <- glm (CannibalizedRedYN ~ Trt+ DeltaMsize + DeltaMcondition, family = "binomial", data = MY_TABLE_MaleTestValid)

par(mfrow=c(2,2))
plot(mod3)

summary(mod3)

  ## to get one sided test p value
 mod3p <-  coef(summary(mod3))[2, 4]/2




  ## to check equality of male motivation to court
  # shapiro.test(MY_TABLE_MID$Latency_to_court)
  # t.test (MY_TABLE_MID$Latency_to_court[MY_TABLE_MID$Mcolor == "Red"], 
  #         MY_TABLE_MID$Latency_to_court[MY_TABLE_MID$Mcolor == "Black"])


 # exploration: male test for test where male consumption happened within the first day
 
 mod3_firstDay <- glm (CannibalizedRedYN ~ Trt+ DeltaMsize + DeltaMcondition
                    , family = "binomial" 
                    , data = MY_TABLE_MaleTestValid[MY_TABLE_MaleTestValid$TrialDateEnd == MY_TABLE_MaleTestValid$TrialDate 
                                                    & !is.na(MY_TABLE_MaleTestValid$TrialDateEnd),])
 
 par(mfrow=c(2,2))
 plot(mod3_firstDay)
 
 summary(mod3_firstDay)
 
 
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
oddsMales <- exp(cbind(OR=coef(mod3), confint(mod3)))[2,]  
oddsMales1stDay <- exp(cbind(OR=coef(mod3_firstDay), confint(mod3_firstDay)))[2,]


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
chisq.test(table(MY_TABLE_TermiteTestValid$AttackNewRedYN[MY_TABLE_TermiteTestValid$Trt == 'RedPreference']), p=c(0.5,0.5))
chisq.test(table(MY_TABLE_TermiteTestValid$AttackNewRedYN[MY_TABLE_TermiteTestValid$Trt == 'RedAverse']), p=c(0.5,0.5))

ggplot(MY_TABLE_TermiteTestValid,aes(x=AttackNewRedYN,group=Trt,fill=Trt))+
  geom_bar(position="dodge",aes(y = ..prop.., fill = Trt)) +
  scale_x_discrete(NULL, c(0,1), c("Consumed grey termite", "Consumed red termite"), c(0,1)) +
  scale_y_continuous(NULL,labels=scales::percent) + theme_bw()+
  scale_fill_discrete(name = "Treatment")+
  theme(text = element_text(size=20))

##step 3
chisq.test(table(MY_TABLE_MaleTestValid$CannibalizedRedYN[MY_TABLE_MaleTestValid$Trt == 'RedPreference']), p=c(0.5,0.5))
chisq.test(table(MY_TABLE_MaleTestValid$CannibalizedRedYN[MY_TABLE_MaleTestValid$Trt == 'RedAverse']), p=c(0.5,0.5))


ggplot(MY_TABLE_MaleTestValid,aes(x=CannibalizedRedYN,group=Trt,fill=Trt))+
  geom_bar(position="dodge",aes(y = ..prop.., fill = Trt)) +
  scale_x_discrete(NULL, c(0,1), c("Consumed black male", "Consumed red male"), c(0,1)) +
  theme_bw()+scale_y_continuous(NULL,labels=scales::percent) +
  scale_fill_discrete(name = "Treatment")+
  theme(text = element_text(size=20))


# juvenile females painted black or left white faces, predated by adult female (red preference females finished with the experiment)

contingencyTable <- rbind(table(MY_TABLE_MaleTestValid$Trt, MY_TABLE_MaleTestValid$CannibalizedRedYN), c(13,5))
rownames(contingencyTable)[3] <- 'Control'


chisq.test(contingencyTable)

}
