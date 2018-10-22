#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Data handling FlexibleSexualAppetite part 1
#	 Start : 11/8/2017
#	 last modif : 3/15/2018
#	 commit: separate data handling from data analyses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{# Remarks
  # for three female male tests, males were replaced when one died (within two days), and that males were already painted  (the day before)
  # in all three cases, the color of the male that died was the same as the color of the male which was subsequently cannibalised.
  # as this was not planed and is difficult to present succintly in a paper. those replacement tests were ignored (unfortunately reducing the sample size of valid male tests)
  # for FID 193: initially with 24 Black, 53 Red (Died) trial ID 297  ; later 63 Black, 37 Red trial, trial ID 317
  # for FID 112: initially with 349 Black (Died), 362 (Red) trial ID 405 ; later 230 Black trial ID 179
  # for FID 106: initially with 60 Black (Died), 221 (Red) trial ID 160 ; later 34 Black, 32 Red, trial ID 406
  # first trial IDs for the test on female 106 112 and 193 cannibalism are 160 405 297, are excluded as one spider dies during the test (as in other such cases, entered in DB)
  # second trials IDs to be excluded in this script for these females were: 406 179 317
}


rm(list = ls(all = TRUE))


{# packages
library(RODBC) # this require R AND ACCESS to run on 32 bits !
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
                                SELECT Behav_Female.TrialFID, Behav_Female.FID, Behav_Female.TrialDate, Behav_Female.TrialTime, Basic_Trials.GroupName AS Trt, Behav_Female.AttackRedYN AS CannibalizedRedYN, Behav_Female.AttackGreyBlackYN, Behav_Female.LatencyAttack,Behav_Female.EatDuringVideo, Behav_Female.TrialDateEnd, Behav_Female.TrialTimeEnd, Behav_Female.ExcludeYN, Behav_Female.ReasonExclusion, Behav_Female.Remarks
                                FROM Morph_Measurements RIGHT JOIN (Basic_Trials INNER JOIN Behav_Female ON Basic_Trials.Ind_ID = Behav_Female.FID) ON Morph_Measurements.Ind_ID = Behav_Female.FID
                                GROUP BY Behav_Female.TrialFID, Behav_Female.FID, Behav_Female.TrialDate, Behav_Female.TrialTime, Basic_Trials.GroupName, Basic_Trials.Sex, Basic_Trials.Experiment, Behav_Female.TestName, Behav_Female.AttackRedYN, Behav_Female.AttackGreyBlackYN, Behav_Female.LatencyAttack, Behav_Female.EatDuringVideo,Behav_Female.TrialDateEnd, Behav_Female.TrialTimeEnd, Behav_Female.ExcludeYN, Behav_Female.ReasonExclusion, Behav_Female.Remarks
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
  
  
  
  # exclude Trials IDs with replacement males 
  MY_TABLE_MaleTest <- MY_TABLE_MaleTest[MY_TABLE_MaleTest$TrialFID != 406 
                                         & MY_TABLE_MaleTest$TrialFID != 179 
                                         & MY_TABLE_MaleTest$TrialFID != 317,]
  
  MY_TABLE_MaleTestValid <-  MY_TABLE_MaleTest[MY_TABLE_MaleTest$Exclude == FALSE,]  
  
  # exclude males from replacement tests
  
  MY_TABLE_MIDValid <- MY_TABLE_MID[MY_TABLE_MID$MID != 63 
                                    & MY_TABLE_MID$MID != 37
                                    & MY_TABLE_MID$MID != 230 
                                    & MY_TABLE_MID$MID != 34 
                                    & MY_TABLE_MID$MID != 32 &
                                      MY_TABLE_MID$FID%in% MY_TABLE_MaleTestValid$FID, ]
  
  
  # missing one value of mass for a female from the red averse group
  MY_TABLE_BugTest$Mass[is.na(MY_TABLE_BugTest$Mass)] <- mean(MY_TABLE_BugTest$Mass[MY_TABLE_BugTest$Trt == 'RedAverse'], na.rm=TRUE)
  
  # missing one value for male mass
  MY_TABLE_MIDValid$Mass[is.na(MY_TABLE_MIDValid$Mass)] <- mean(MY_TABLE_MIDValid$Mass, na.rm=TRUE)
  
  
  # calculate body condition
  MY_TABLE_BugTest$Fcondition <- residuals(lm(MY_TABLE_BugTest$Mass~ MY_TABLE_BugTest$CarapaceWidth))
  MY_TABLE_MIDValid$Mcondition <- residuals(lm(MY_TABLE_MIDValid$Mass~ MY_TABLE_MIDValid$CarapaceWidth))
  
  # remove termite test where no prey was attacked
  MY_TABLE_TermiteTestValid <- MY_TABLE_TermiteTest[MY_TABLE_TermiteTest$DidNotAttackAnyYN == 0,]
  

  # calculate  delta male condition
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
  
  
  # combine into MY_TABLE_Step
  
  MY_TABLE_Step <-data.frame(mapply(c,MY_TABLE_BugTest[,c('FID','Trt','AttackBugYN','LatencyAttack')],
                                    MY_TABLE_TermiteTestValid[,c('FID','Trt','AttackNewRedYN','LatencyAttack')],
                                    MY_TABLE_MaleTestValid[,c('FID','Trt','CannibalizedRedYN','LatencyAttack')]))
  colnames(MY_TABLE_Step) <- c('FID','Trt','AttackRedYN','LatencyAttack')
  MY_TABLE_Step$Trt[MY_TABLE_Step$Trt == 1] <- 'RedAverse'
  MY_TABLE_Step$Trt[MY_TABLE_Step$Trt == 2] <- 'RedPreference'
  MY_TABLE_Step <- MY_TABLE_Step[ order(MY_TABLE_Step$FID), ]
  MY_TABLE_Step$rowID <- 1:nrow(MY_TABLE_Step)
  
  
  head(MY_TABLE_BugTest)
  head(MY_TABLE_TermiteTestValid)
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
    
    nrow(MY_TABLE_MaleTest) # 104
    nrow(MY_TABLE_MaleTest[MY_TABLE_MaleTest$ExcludeYN == TRUE,]) # 25 out of 107
    table(MY_TABLE_MaleTest$ReasonExclusion[MY_TABLE_MaleTest$ExcludeYN == TRUE]) 
    # BlackMaleDied    FemaleDied FemaleStarved   RedMaleDied 
    # 10             3             9             3   
    # total tests where male didn't died during trial: 104-10=94 black; 104-3: 101 red
    
    chisq.test(rbind(c(10,3),c(94,101)))
    
  }
  
  # duration to male consumption
  
  head(MY_TABLE_MaleTestValid)
  
  nrow(MY_TABLE_MaleTestValid) # 79
  nrow(MY_TABLE_MaleTestValid[MY_TABLE_MaleTestValid$Trt == 'RedAverse',]) # 42
  nrow(MY_TABLE_MaleTestValid[MY_TABLE_MaleTestValid$Trt == 'RedPreference',]) # 37
  nrow(MY_TABLE_MaleTest[MY_TABLE_MaleTest$Trt == 'RedAverse',]) # 52
  nrow(MY_TABLE_MaleTest[MY_TABLE_MaleTest$Trt == 'RedPreference',]) # 52
  nrow(MY_TABLE_MaleTestValid[MY_TABLE_MaleTestValid$DuringVideo == 1,]) # 15
  
  Within1stDay <- nrow(MY_TABLE_MaleTestValid[!(is.na(MY_TABLE_MaleTestValid$TrialDateEnd)) & MY_TABLE_MaleTestValid$TrialDate == MY_TABLE_MaleTestValid$TrialDateEnd,]) # 22
  PercentageWithinFirstDay <- Within1stDay*  100/nrow(MY_TABLE_MaleTestValid) # 27.8
  summary(MY_TABLE_MaleTestValid$LatencyAttackDay)
  

}

# write data


output_folder <- "R_Data"

### 20180315 
# write.csv(MY_TABLE_BugTest, file = paste(output_folder,"MY_TABLE_BugTest.csv", sep="/"), row.names = FALSE)
# write.csv(MY_TABLE_TermiteTest, file = paste(output_folder,"MY_TABLE_TermiteTest.csv", sep="/"), row.names = FALSE)
# write.csv(MY_TABLE_MaleTestValid, file = paste(output_folder,"MY_TABLE_MaleTest.csv", sep="/"), row.names = FALSE)
# write.csv(MY_TABLE_MIDValid, file = paste(output_folder,"MY_TABLE_MID.csv", sep="/"), row.names = FALSE)
# write.csv(MY_TABLE_Step, file = paste(output_folder,"MY_TABLE_Step.csv", sep="/"), row.names = FALSE)

# 20180328 (validtermitetest)
# write.csv(MY_TABLE_TermiteTestValid, file = paste(output_folder,"MY_TABLE_TermiteTest.csv", sep="/"), row.names = FALSE)
