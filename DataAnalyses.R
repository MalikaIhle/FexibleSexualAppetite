#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Data analyses FlexibleSexualAppetite part 1
#	 Start : 11/8/2017
#	 last modif : 11/8/2017
#	 commit: modify prereg code to call database 
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
SELECT Behav_Female.FID, Basic_Trials.GroupName AS Trt, Behav_Female.AttackRedYN AS AttackBugYN, Max(Morph_Measurements.CarapaceWidth) AS CarapaceWidth, Max(Morph_Measurements.Mass) AS Mass
FROM Morph_Measurements RIGHT JOIN (Basic_Trials INNER JOIN Behav_Female ON Basic_Trials.Ind_ID = Behav_Female.FID) ON Morph_Measurements.Ind_ID = Behav_Female.FID
GROUP BY Behav_Female.FID, Basic_Trials.GroupName, Basic_Trials.Sex, Basic_Trials.Experiment, Behav_Female.TestName, Behav_Female.AttackRedYN
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
SELECT Behav_Female.TrialFID, Behav_Female.FID, Behav_Female.TrialDate, Behav_Female.TrialTime, Basic_Trials.GroupName AS Trt, Behav_Female.AttackRedYN AS CannibalizedRedYN, Behav_Female.AttackGreyBlackYN, Behav_Female.LatencyAttack, Behav_Female.TrialDateEnd, Behav_Female.TrialTimeEnd, Behav_Female.ExcludeYN, Behav_Female.ReasonExclusion, Behav_Female.Remarks
FROM Morph_Measurements RIGHT JOIN (Basic_Trials INNER JOIN Behav_Female ON Basic_Trials.Ind_ID = Behav_Female.FID) ON Morph_Measurements.Ind_ID = Behav_Female.FID
GROUP BY Behav_Female.TrialFID, Behav_Female.FID, Behav_Female.TrialDate, Behav_Female.TrialTime, Basic_Trials.GroupName, Basic_Trials.Sex, Basic_Trials.Experiment, Behav_Female.TestName, Behav_Female.AttackRedYN, Behav_Female.AttackGreyBlackYN, Behav_Female.LatencyAttack, Behav_Female.TrialDateEnd, Behav_Female.TrialTimeEnd, Behav_Female.ExcludeYN, Behav_Female.ReasonExclusion, Behav_Female.Remarks
HAVING (((Basic_Trials.Sex)=0) AND ((Basic_Trials.Experiment)='MatedFemaleCannibalism') AND ((Behav_Female.TestName)='Male'))
ORDER BY Behav_Female.FID, Behav_Female.TrialDate
")


MY_TABLE_MID <- sqlQuery(conDB,"
SELECT Basic_Trials.Ind_ID AS MID, Basic_Trials.GroupName AS Color, Basic_Trials.GroupNumber AS FID, Max(Morph_Measurements.Mass) AS Mass, Max(Morph_Measurements.CarapaceWidth) AS CarapaceWidth, Basic_Trials.Remarks
FROM Basic_Trials INNER JOIN Morph_Measurements ON Basic_Trials.Ind_ID = Morph_Measurements.Ind_ID
GROUP BY Basic_Trials.Ind_ID, Basic_Trials.Sex, Basic_Trials.Experiment, Basic_Trials.GroupName, Basic_Trials.GroupNumber, Basic_Trials.Remarks
HAVING (((Basic_Trials.Sex)=1) AND ((Basic_Trials.Experiment)='MatedFemaleCannibalism') AND ((Basic_Trials.GroupNumber) Is Not Null))
")



# missing one value of mass for a female from the red averse group
MY_TABLE_BugTest$Mass[is.na(MY_TABLE_BugTest$Mass)] <- mean(MY_TABLE_BugTest$Mass[MY_TABLE_BugTest$Trt == 'RedAverse'], na.rm=TRUE)

# missing one value for male mass
MY_TABLE_MID$Mass[is.na(MY_TABLE_MID$Mass)] <- mean(MY_TABLE_MID$Mass, na.rm=TRUE)


# calculate body condition
MY_TABLE_BugTest$Fcondition <- residuals(lm(MY_TABLE_BugTest$Mass~ MY_TABLE_BugTest$CarapaceWidth))
MY_TABLE_MID$Mcondition <- residuals(lm(MY_TABLE_MID$Mass~ MY_TABLE_MID$CarapaceWidth))

# exclude initialTrials IDs where replacement males were given
MY_TABLE_MaleTestValid <- MY_TABLE_MaleTest[MY_TABLE_MaleTest$TrialFID != 405 
                                            & MY_TABLE_MaleTest$TrialFID != 297 
                                            & MY_TABLE_MaleTest$TrialFID != 406 
                                            & MY_TABLE_MaleTest$Exclude == FALSE,]  

# calculate Delta Male traits for males in valid female male tests

MY_TABLE_MIDValid <- MY_TABLE_MID[MY_TABLE_MID$MID != 24 
                                  & MY_TABLE_MID$MID != 53
                                  & MY_TABLE_MID$MID != 349 
                                  & MY_TABLE_MID$MID != 60 
                                  & MY_TABLE_MID$MID != 221
                                  & !MY_TABLE_MID$FID%in% MY_TABLE_MaleTest$FID[MY_TABLE_MaleTest$Exclude == TRUE]  , ]

MY_TABLE_MIDValid <- merge(
      MY_TABLE_MIDValid[MY_TABLE_MIDValid$Color == 'Red',c('FID','CarapaceWidth','Mcondition')],
      MY_TABLE_MIDValid[MY_TABLE_MIDValid$Color == 'Black',c('FID','CarapaceWidth','Mcondition')],by='FID')


MY_TABLE_MIDValid$DeltaMsize <- MY_TABLE_MIDValid$CarapaceWidth.x - MY_TABLE_MIDValid$CarapaceWidth.y
MY_TABLE_MIDValid$DeltaMcondition <- MY_TABLE_MIDValid$Mcondition.x - MY_TABLE_MIDValid$Mcondition.y

# merge Delta Male traits to  MY_TABLE_MaleTestValid

MY_TABLE_MaleTestValid <- merge(MY_TABLE_MaleTestValid, MY_TABLE_MIDValid[,c('FID','DeltaMsize','DeltaMcondition')], by = 'FID')
  

head(MY_TABLE_BugTest)
head(MY_TABLE_TermiteTest)
head(MY_TABLE_MaleTest)
head(MY_TABLE_MaleTestValid)
head(MY_TABLE_MID)
head(MY_TABLE_MIDValid)
}

{# Descriptive statistics

{# female death during training:
  
FemaleDeadDuringTraining <- sqlQuery(conDB,"
SELECT Basic_Individuals.Ind_ID AS FID, Basic_Trials.GroupName AS Trt, [Basic_Individuals]![DeathDate]-[Basic_Trials]![PeriodBeginDate] AS DaysInTrialWhenDied
FROM Basic_Individuals LEFT JOIN Basic_Trials ON Basic_Individuals.Ind_ID = Basic_Trials.Ind_ID
WHERE (((Basic_Individuals.Sex)=0) AND ((Basic_Individuals.DeathDate) Is Not Null And (Basic_Individuals.DeathDate)<=[Basic_Trials]![PeriodEndDate]) AND ((Basic_Trials.Experiment)='MatedFemaleCannibalism'))
ORDER BY Basic_Trials.GroupName
")


FemaleAliveUntilEndTraining <- sqlQuery(conDB,"
SELECT Basic_Individuals.Ind_ID, Basic_Trials.GroupName AS Trt, [Basic_Individuals]![DeathDate]-[Basic_Trials]![PeriodBeginDate] AS DaysInTrialWhenDied
FROM Basic_Individuals LEFT JOIN Basic_Trials ON Basic_Individuals.Ind_ID = Basic_Trials.Ind_ID
WHERE (((Basic_Individuals.Sex)=0) AND ((Basic_Individuals.DeathDate) Is Null Or (Basic_Individuals.DeathDate)>[Basic_Trials]![PeriodEndDate]) AND ((Basic_Trials.Experiment)='MatedFemaleCannibalism'))
")
  
  
DeadFemales <- table(FemaleDeadDuringTraining$Trt)  
AliveFemales <- table(FemaleAliveUntilEndTraining$Trt)  

chisq.test(c(12,45),c(23,48))
}
  
# male death after painting
  
  

# Female starvation

}



## Trt Red Averse is the reference (intercept)


{# step 1

mod1 <- glm (AttackBugYN ~ Trt + Fcondition, "binomial", data = MY_TABLE_BugTest)

par(mfrow=c(2,2))
plot(mod1)

summary(mod1)

  ## to get one sided test p value
  mod1p <- coef(summary(mod1))[2, 4]/2
  
}

{# step 2

mod2 <- glm (AttackNewRedYN ~ Trt, family = "binomial",  data = MY_TABLE_TermiteTest)

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
  
  t.test (log(MY_TABLE_TermiteTest$LatencyAttack[MY_TABLE_TermiteTest$Trt == "RedPreference"]), 
          log(MY_TABLE_TermiteTest$LatencyAttack[MY_TABLE_TermiteTest$Trt == "RedAverse"]))


}

{# step 3


mod3 <- glm (CannibalizedRedYN ~ Trt+ DeltaMsize + DeltaMcondition, family = "binomial", data = MY_TABLE_MaleTestValid)

par(mfrow=c(2,2))
plot(mod3)

summary(mod3)

  ## to get one sided test p value
 mod3p <-  coef(summary(mod3))[2, 4]/2




  ## to check equality of male motivation to court
  shapiro.test(MY_TABLE_MID$Latency_to_court)
  t.test (MY_TABLE_MID$Latency_to_court[MY_TABLE_MID$Mcolor == "Red"], 
          MY_TABLE_MID$Latency_to_court[MY_TABLE_MID$Mcolor == "Black"])


}  
 
  
{# exploratory analyses: repeatability of female bias

mod4 <- glmer (attackRedYN ~ Trt + (1|FID), family = "binomial", data=MY_TABLE_Step)
 
par(mfrow=c(2,2))
qqnorm(resid(mod4))
qqline(resid(mod4))
qqnorm(unlist(ranef(mod4)$FID))
qqline(unlist(ranef(mod4)$FID))
plot(fitted(mod4), resid(mod4))
abline(h=0)
plot(fitted(mod4),jitter(MY_TABLE_Step$attackRedYN, 0.5))
abline(0,1)

mod4withrowID <- glm (attackRedYN ~ Trt + (1|FID) + (1|rowID), family = "binomial", data=MY_TABLE_Step)
anova(mod4,mod4withrowID)

summary(mod4)

print(rpt(formula = attackRedYN ~ Trt + (1|FID),
          grname = c("Fixed","Overdispersion","FID"), 
          data= MY_TABLE_Step, 
          datatype = "Binary", 
          nboot = 1000, 
          npermut = 0, 
          adjusted = FALSE))
}


{# to calculate odds ratios of eating the red prey for the Red Preference group relative to the red averse group

## step 1 
table(MY_TABLE_BugTest$Trt, MY_TABLE_BugTest$AttackBugYN)
exp(cbind(OR=coef(mod1), confint(mod1)))[2,] 

## step 2
table(MY_TABLE_TermiteTest$Trt, MY_TABLE_TermiteTest$AttackNewRedYN)
exp(cbind(OR=coef(mod2), confint(mod2)))[2,]

## step 3: odds ratio ?
exp(cbind(OR=coef(mod3), confint(mod3)))[2,]  
}


{# Comparing each group to 50/50

##step 2
chisq.test(table(MY_TABLE_TermiteTest$AttackNewRedYN[MY_TABLE_TermiteTest$Trt == 'RedPreference']), p=c(0.5,0.5))
chisq.test(table(MY_TABLE_TermiteTest$AttackNewRedYN[MY_TABLE_TermiteTest$Trt == 'RedAverse']), p=c(0.5,0.5))

ggplot(MY_TABLE_TermiteTest,aes(x=AttackNewRedYN,group=Trt,fill=Trt))+
  geom_histogram(position="dodge",binwidth=0.25)+theme_bw()


##step 3
chisq.test(table(MY_TABLE_MaleTestValid$CannibalizedRedYN[MY_TABLE_MaleTestValid$Trt == 'RedPreference']), p=c(0.5,0.5))
chisq.test(table(MY_TABLE_MaleTestValid$CannibalizedRedYN[MY_TABLE_MaleTestValid$Trt == 'RedAverse']), p=c(0.5,0.5))


ggplot(MY_TABLE_MaleTestValid,aes(x=CannibalizedRedYN,group=Trt,fill=Trt))+
  geom_histogram(position="dodge",binwidth=0.25)+theme_bw()

}
