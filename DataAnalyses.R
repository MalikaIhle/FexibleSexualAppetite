#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Data analyses FlexibleSexualAppetite part 1
#	 Start : 11/8/2017
#	 last modif : 11/9/2017
#	 commit: modify prereg code to call database  - update rptR package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{# Remarks
# for two females male test, males were replaces when one died:
# for FID 193: initially with 24 Black, 53 Red (Died) trial ID 405  ; later 63 Black, 37 Red trial, trial ID 179
# for FID 112: initially with 349 Black (Died), 362 (Red) trial ID 297 ; later 230 Black trial ID 317
# for FID 106: initially with 60 Black (Died), 221 (Red) trial ID 406 ; later 34 Black, 32 Red, trial ID 160
# first trial IDs were excluded for the test on female cannibalism 405 297 406
}


rm(list = ls(all = TRUE))

{# packages
library(lme4)
library(rptR)
library(pbapply)
library(RODBC)
library(ggplot2)
}

{# load data

conDB= odbcConnectAccess2007("C:\\Users\\malika.ihle\\Dropbox\\HabronatusPyrrithrix\\HabronatusPyrrithrix_DB.accdb")


MY_TABLE_BugTest <- sqlQuery(conDB,"
SELECT Behav_Female.FID, Basic_Trials.GroupName AS Trt, Behav_Female.AttackRedYN AS AttackBugYN, Behav_Female.LatencyAttack, Max(Morph_Measurements.CarapaceWidth) AS CarapaceWidth, Max(Morph_Measurements.Mass) AS Mass
FROM Morph_Measurements RIGHT JOIN (Basic_Trials INNER JOIN Behav_Female ON Basic_Trials.Ind_ID = Behav_Female.FID) ON Morph_Measurements.Ind_ID = Behav_Female.FID
GROUP BY Behav_Female.FID, Basic_Trials.GroupName, Basic_Trials.Sex, Basic_Trials.Experiment, Behav_Female.TestName, Behav_Female.AttackRedYN, Behav_Female.LatencyAttack
HAVING (((Basic_Trials.Sex)=0) AND ((Basic_Trials.Experiment)='MatedFemaleCannibalism') AND ((Behav_Female.TestName)='Bug'))
")


MY_TABLE_TermiteTest <- sqlQuery(conDB,"
SELECT Behav_Female.FID, Behav_Female.TrialDate, Basic_Trials.GroupName AS Trt, Behav_Female.AttackRedYN AS AttackNewRedYN, Behav_Female.AttackGreyBlackYN, Behav_Female.LatencyAttack, Behav_Female.DidNotAttackAnyYN, Behav_Female.Remarks
FROM Morph_Measurements RIGHT JOIN (Basic_Trials INNER JOIN Behav_Female ON Basic_Trials.Ind_ID = Behav_Female.FID) ON Morph_Measurements.Ind_ID = Behav_Female.FID
GROUP BY Behav_Female.FID, Behav_Female.TrialDate, Basic_Trials.GroupName, Basic_Trials.Sex, Basic_Trials.Experiment, Behav_Female.TestName, Behav_Female.AttackRedYN, Behav_Female.AttackGreyBlackYN, Behav_Female.LatencyAttack, Behav_Female.DidNotAttackAnyYN, Behav_Female.ExcludeYN, Behav_Female.Remarks
HAVING (((Basic_Trials.Sex)=0) AND ((Basic_Trials.Experiment)='MatedFemaleCannibalism') AND ((Behav_Female.TestName)='Termite') AND ((Behav_Female.ExcludeYN)=False))
ORDER BY Behav_Female.FID, Behav_Female.TrialDate
")


MY_TABLE_MaleTest <- sqlQuery(conDB,"
SELECT Behav_Female.TrialFID, Behav_Female.FID, Behav_Female.TrialDate, Behav_Female.TrialTime, Basic_Trials.GroupName AS Trt, Behav_Female.AttackRedYN AS CannibalizedRedYN, Behav_Female.AttackGreyBlackYN, Behav_Female.LatencyAttack,Behav_Female.DuringVideo, Behav_Female.TrialDateEnd, Behav_Female.TrialTimeEnd, Behav_Female.ExcludeYN, Behav_Female.ReasonExclusion, Behav_Female.Remarks
FROM Morph_Measurements RIGHT JOIN (Basic_Trials INNER JOIN Behav_Female ON Basic_Trials.Ind_ID = Behav_Female.FID) ON Morph_Measurements.Ind_ID = Behav_Female.FID
GROUP BY Behav_Female.TrialFID, Behav_Female.FID, Behav_Female.TrialDate, Behav_Female.TrialTime, Basic_Trials.GroupName, Basic_Trials.Sex, Basic_Trials.Experiment, Behav_Female.TestName, Behav_Female.AttackRedYN, Behav_Female.AttackGreyBlackYN, Behav_Female.LatencyAttack, Behav_Female.DuringVideo,Behav_Female.TrialDateEnd, Behav_Female.TrialTimeEnd, Behav_Female.ExcludeYN, Behav_Female.ReasonExclusion, Behav_Female.Remarks
HAVING (((Basic_Trials.Sex)=0) AND ((Basic_Trials.Experiment)='MatedFemaleCannibalism') AND ((Behav_Female.TestName)='Male'))
ORDER BY Behav_Female.FID, Behav_Female.TrialDate
")


MY_TABLE_MID <- sqlQuery(conDB,"
SELECT Basic_Trials.Ind_ID AS MID, Basic_Trials.GroupName AS Color, Basic_Trials.GroupNumber AS FID, Max(Morph_Measurements.Mass) AS Mass, Max(Morph_Measurements.CarapaceWidth) AS CarapaceWidth, Basic_Trials.Remarks
FROM Basic_Trials INNER JOIN Morph_Measurements ON Basic_Trials.Ind_ID = Morph_Measurements.Ind_ID
GROUP BY Basic_Trials.Ind_ID, Basic_Trials.Sex, Basic_Trials.Experiment, Basic_Trials.GroupName, Basic_Trials.GroupNumber, Basic_Trials.Remarks
HAVING (((Basic_Trials.Sex)=1) AND ((Basic_Trials.Experiment)='MatedFemaleCannibalism') AND ((Basic_Trials.GroupNumber) Is Not Null))
")

close(conDB)


# missing one value of mass for a female from the red averse group
MY_TABLE_BugTest$Mass[is.na(MY_TABLE_BugTest$Mass)] <- mean(MY_TABLE_BugTest$Mass[MY_TABLE_BugTest$Trt == 'RedAverse'], na.rm=TRUE)

# missing one value for male mass
MY_TABLE_MID$Mass[is.na(MY_TABLE_MID$Mass)] <- mean(MY_TABLE_MID$Mass, na.rm=TRUE)


# calculate body condition
MY_TABLE_BugTest$Fcondition <- residuals(lm(MY_TABLE_BugTest$Mass~ MY_TABLE_BugTest$CarapaceWidth))
MY_TABLE_MID$Mcondition <- residuals(lm(MY_TABLE_MID$Mass~ MY_TABLE_MID$CarapaceWidth))

# remove termite test where no prey was attacked
MY_TABLE_TermiteTestValid <- MY_TABLE_TermiteTest[MY_TABLE_TermiteTest$DidNotAttackAnyYN == 0,]

# exclude initialTrials IDs where replacement males were given
MY_TABLE_MaleTestValid <- MY_TABLE_MaleTest[#MY_TABLE_MaleTest$TrialFID != 405 
                                            #& MY_TABLE_MaleTest$TrialFID != 297 
                                            #& MY_TABLE_MaleTest$TrialFID != 406 &
                                             MY_TABLE_MaleTest$Exclude == FALSE,]  

# calculate Delta Male traits for males in valid female male tests

MY_TABLE_MIDValid <- MY_TABLE_MID[MY_TABLE_MID$MID != 24 
                                  & MY_TABLE_MID$MID != 53
                                  & MY_TABLE_MID$MID != 349 
                                  & MY_TABLE_MID$MID != 60 
                                  & MY_TABLE_MID$MID != 221 &
                                   MY_TABLE_MID$FID%in% MY_TABLE_MaleTestValid$FID, ]

MY_TABLE_MIDValid <- merge(
      MY_TABLE_MIDValid[MY_TABLE_MIDValid$Color == 'Red',c('FID','CarapaceWidth','Mcondition')],
      MY_TABLE_MIDValid[MY_TABLE_MIDValid$Color == 'Black',c('FID','CarapaceWidth','Mcondition')],by='FID')

MY_TABLE_MIDValid$DeltaMsize <- MY_TABLE_MIDValid$CarapaceWidth.x - MY_TABLE_MIDValid$CarapaceWidth.y
MY_TABLE_MIDValid$DeltaMcondition <- MY_TABLE_MIDValid$Mcondition.x - MY_TABLE_MIDValid$Mcondition.y

# merge Delta Male traits to  MY_TABLE_MaleTestValid

MY_TABLE_MaleTestValid <- merge(MY_TABLE_MaleTestValid, MY_TABLE_MIDValid[,c('FID','DeltaMsize','DeltaMcondition')], by = 'FID')
MY_TABLE_MaleTestValid$TrialDateEnd[MY_TABLE_MaleTestValid$DuringVideo == 1] <-  MY_TABLE_MaleTestValid$TrialDate[MY_TABLE_MaleTestValid$DuringVideo == 1]
MY_TABLE_MaleTestValid$LatencyAttackDay <- as.numeric(as.character(difftime(MY_TABLE_MaleTestValid$TrialDateEnd, MY_TABLE_MaleTestValid$TrialDate, unit ='days'))) 
MY_TABLE_MaleTestValid$LatencyAttack <- MY_TABLE_MaleTestValid$LatencyAttackDay*8*60+60

# remove Male tests not finished yet
MY_TABLE_MaleTestValid <- MY_TABLE_MaleTestValid[!is.na(MY_TABLE_MaleTestValid$LatencyAttack),]


# combine into MY_TABLE_Step

MY_TABLE_Step <-data.frame(mapply(c,MY_TABLE_BugTest[,c('FID','Trt','AttackBugYN','LatencyAttack')],
                       MY_TABLE_TermiteTest[,c('FID','Trt','AttackNewRedYN','LatencyAttack')],
                       MY_TABLE_MaleTestValid[,c('FID','Trt','CannibalizedRedYN','LatencyAttack')]))
colnames(MY_TABLE_Step) <- c('FID','Trt','AttackRedYN','LatencyAttack')
MY_TABLE_Step$Trt[MY_TABLE_Step$Trt == 1] <- 'RedAverse'
MY_TABLE_Step$Trt[MY_TABLE_Step$Trt == 2] <- 'RedPreference'
MY_TABLE_Step <- MY_TABLE_Step[ order(MY_TABLE_Step$FID), ]
MY_TABLE_Step$rowID <- 1:nrow(MY_TABLE_Step)


head(MY_TABLE_BugTest)
head(MY_TABLE_TermiteTest)
head(MY_TABLE_MaleTest)
head(MY_TABLE_MaleTestValid)
head(MY_TABLE_MID)
head(MY_TABLE_MIDValid)
head(MY_TABLE_Step)
}

{# Descriptive statistics

{# female death during training:

conDB= odbcConnectAccess2007("C:\\Users\\malika.ihle\\Dropbox\\HabronatusPyrrithrix\\HabronatusPyrrithrix_DB.accdb")
  
FemaleDeadDuringTraining <- sqlQuery(conDB,"
SELECT Basic_Individuals.Ind_ID AS FID, Basic_Trials.GroupName AS Trt, [Basic_Individuals]![DeathDate]-[Basic_Trials]![PeriodBeginDate] AS DaysInTrialWhenDied, Basic_Individuals.UnnaturalDeath, Basic_Individuals.Disappear
FROM (Basic_Individuals LEFT JOIN (SELECT Behav_Female.FID
FROM Behav_Female
WHERE (((Behav_Female.ReasonExclusion)='FemaleStarved')))  AS FemaleStarved ON Basic_Individuals.Ind_ID = FemaleStarved.FID) LEFT JOIN Basic_Trials ON Basic_Individuals.Ind_ID = Basic_Trials.Ind_ID
WHERE (((Basic_Individuals.Sex)=0) AND ((Basic_Individuals.DeathDate) Is Not Null And (Basic_Individuals.DeathDate)<=[Basic_Trials]![PeriodEndDate]) AND ((Basic_Trials.Experiment)='MatedFemaleCannibalism') AND ((FemaleStarved.FID) Is Null) AND ((Basic_Individuals.UnnaturalDeath)=False) AND ((Basic_Individuals.Disappear)=False))
ORDER BY Basic_Trials.GroupName;
") # remove dead from accident, escapees, starved during male tests


AllFemales <- sqlQuery(conDB,"
SELECT Basic_Individuals.Ind_ID AS FID, Basic_Trials.GroupName AS Trt
FROM Basic_Individuals LEFT JOIN Basic_Trials ON Basic_Individuals.Ind_ID = Basic_Trials.Ind_ID
WHERE (((Basic_Individuals.Sex)=0) AND ((Basic_Trials.Experiment)='MatedFemaleCannibalism'))
ORDER BY Basic_Trials.GroupName
")

FemaleStarve <- sqlQuery(conDB,"
SELECT Basic_Trials.Ind_ID, Basic_Trials.GroupName AS Trt, Behav_Female.ReasonExclusion, Behav_Female.TrialDate, Behav_Female.TrialDateEnd, [Behav_Female]![TrialDateEnd]-[Behav_Female]![TrialDate] AS DelayStarve
FROM Basic_Trials INNER JOIN Behav_Female ON Basic_Trials.Ind_ID = Behav_Female.FID
WHERE (((Basic_Trials.Experiment)='MatedFemaleCannibalism') AND ((Behav_Female.ReasonExclusion)='FemaleStarved'))
ORDER BY Basic_Trials.GroupName
")


close(conDB)

table(AllFemales$Trt)

AllFemalesMinusAccident <- AllFemales[AllFemales$FID != 138 # escaped
                                       & AllFemales$FID != 188 # disappeared
                                       & AllFemales$FID != 170 # killed by accident
                                       & AllFemales$FID != 294,] # killed by accident
  
DeadFemales <- table(FemaleDeadDuringTraining$Trt)  
AliveFemales <- table(AllFemalesMinusAccident$Trt) - DeadFemales

chisq.test(rbind(DeadFemales,AliveFemales))


# female starvation

summary(FemaleStarve$DelayStarve)



}
  
{# excluded male tests
  
nrow(MY_TABLE_MaleTest)
nrow(MY_TABLE_MaleTest[MY_TABLE_MaleTest$ExcludeYN == TRUE,]) # 24 out of 107
table(MY_TABLE_MaleTest$ReasonExclusion[MY_TABLE_MaleTest$ExcludeYN == TRUE]) 
# BlackMaleDied    FemaleDied FemaleStarved   RedMaleDied 
# 10             3             8             3   
# total tests where male didn't died during trial: 107-10=97 black; 107-3: 104 red

chisq.test(rbind(c(10,3),c(97,104)))

}

# duration to male consumption
  
head(MY_TABLE_MaleTestValid)
  
nrow(MY_TABLE_MaleTestValid) # 82
nrow(MY_TABLE_MaleTestValid[MY_TABLE_MaleTestValid$DuringVideo == 1,]) # 15

Within1stDay <- nrow(MY_TABLE_MaleTestValid[!(is.na(MY_TABLE_MaleTestValid$TrialDateEnd)) & MY_TABLE_MaleTestValid$TrialDate == MY_TABLE_MaleTestValid$TrialDateEnd,]) # 22
PercentageWithinFirstDay <- Within1stDay*  100/nrow(MY_TABLE_MaleTestValid) # 26.5
summary(MY_TABLE_MaleTestValid$LatencyAttackDay)

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
