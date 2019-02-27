#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Video analyses FlexibleSexualAppetite part 1
#	 Start : 12 Feb 2018
#	 last modif : 13/02/2018
#	 commit: combine raw data
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
  library(RODBC)
  library(stringr)
  library(dplyr)
  library(here)
}


#~~~ GET DATASET

{# load data
conDB= odbcConnectAccess2007(paste(here(),"VideoAnalyses_MaleTests_2017.accdb", sep="/"))
sqlTables(conDB)

Behav_Video_MaleTest <- sqlFetch(conDB, 'Behav_Video_MaleTest')
Behav_Female <- sqlFetch(conDB, 'Behav_Female')
Behav_Female_Attacks <- sqlFetch(conDB, 'Behav_Female_Attacks')
Behav_Male_Courtships <- sqlFetch(conDB, 'Behav_Male_Courtships')
Behav_MaleMale_Competition <- sqlFetch(conDB, 'Behav_Male-Male_Competition')

close(conDB)

}

{# functions

## read text column into time

ConvertToTime <- function(x){
  as.POSIXct(str_pad(x, 6, pad = "0"), format="%H%M%S") # this adds the date of today to every time... stupid but hard to get around and not important
}
  
ConvertTimeToSecs <- function(x){difftime(x, as.POSIXct(as.character('000000'), format="%H%M%S"), unit='secs')}

}

{# convert time and calculate delays, convert color

Behav_Video_MaleTest <- mutate_at(Behav_Video_MaleTest,c('VideoTimeStart',
                                                         'BlueTimeLeaveContainer',
                                                         'YellowTimeLeaveContainer',
                                                         'TimeStoppedWatching') ,ConvertToTime)


Behav_Female_Attacks <- mutate_at(Behav_Female_Attacks,'AttackTime',ConvertToTime)

Behav_Male_Courtships <- mutate_at(Behav_Male_Courtships,c('CourtshipStart',
                                                         'CourtshipEnd'),ConvertToTime)

Behav_MaleMale_Competition <- mutate_at(Behav_MaleMale_Competition,c('InteractionStart',
                                                                    'InteractionEnd'),ConvertToTime)

Behav_Video_MaleTest$BlackDelayLeaveDish <- as.numeric(difftime(Behav_Video_MaleTest$BlueTimeLeaveContainer, Behav_Video_MaleTest$VideoTimeStart, units='secs'))
Behav_Video_MaleTest$RedDelayLeaveDish <- as.numeric(difftime(Behav_Video_MaleTest$YellowTimeLeaveContainer, Behav_Video_MaleTest$VideoTimeStart, units='secs'))
Behav_Video_MaleTest$TotalWatch <- as.numeric(difftime(Behav_Video_MaleTest$TimeStoppedWatching, Behav_Video_MaleTest$VideoTimeStart, units='secs'))

Behav_Video_MaleTest$BlackDelayLeaveDish[Behav_Video_MaleTest$BlackDelayLeaveDish < 0] <- 0
Behav_Video_MaleTest$RedDelayLeaveDish[Behav_Video_MaleTest$RedDelayLeaveDish < 0] <- 0

Behav_Male_Courtships <- Behav_Male_Courtships[Behav_Male_Courtships$CourtMale == 0 & Behav_Male_Courtships$UnsureMaleID == 0,] # remove unsure courtships
Behav_Male_Courtships$CourtDuration <- as.numeric(difftime(Behav_Male_Courtships$CourtshipEnd,Behav_Male_Courtships$CourtshipStart, units = 'secs'))

Behav_MaleMale_Competition <- Behav_MaleMale_Competition[Behav_MaleMale_Competition$UnsureMaleIDYN == 0,] # remove interaction with unsure IDs
Behav_MaleMale_Competition$InterDur <- as.numeric(difftime(Behav_MaleMale_Competition$InteractionEnd,Behav_MaleMale_Competition$InteractionStart, units = 'secs'))

# standardize annotation (Access does not consider capital letters...)
Behav_Female_Attacks$Mcol[Behav_Female_Attacks$Mcol == 'yellow'] <- 'Yellow'
Behav_Female_Attacks$Mcol[Behav_Female_Attacks$Mcol == 'blue'] <- 'Blue'
Behav_Male_Courtships$Mcol[Behav_Male_Courtships$Mcol == 'yellow'] <- 'Yellow'
Behav_Male_Courtships$Mcol[Behav_Male_Courtships$Mcol == 'blue'] <- 'Blue'
Behav_MaleMale_Competition$McolStart[Behav_MaleMale_Competition$McolStart == 'yellow'] <- 'Yellow'
Behav_MaleMale_Competition$McolStart[Behav_MaleMale_Competition$McolStart == 'blue'] <- 'Blue'
Behav_MaleMale_Competition$McolWin[Behav_MaleMale_Competition$McolWin == 'yellow'] <- 'Yellow'
Behav_MaleMale_Competition$McolWin[Behav_MaleMale_Competition$McolWin == 'blue'] <- 'Blue'


}

head(Behav_Video_MaleTest)
head(Behav_Female)
head(Behav_Female_Attacks)
head(Behav_Male_Courtships)
head(Behav_MaleMale_Competition)

{# summarize type of interactions

{## female attacks 
### real attacks or consum
FAttacks <- Behav_Female_Attacks[Behav_Female_Attacks$AttackYN == 1 | Behav_Female_Attacks$ConsumYN == 1,] %>% 
  group_by(VideoID,Mcol) %>% 
  summarize(FirstFAttack = min(AttackTime),
            NbFAttacks = n())    
### all instances of female aggression (even missed attackes)
TotalIntendedFAttacks <- Behav_Female_Attacks %>% 
  group_by(VideoID,Mcol) %>% 
  summarize(NbIntendedFAttacks = n())  

TotalIntendedFAttacks$VideoIDMcol <- paste(TotalIntendedFAttacks$VideoID, TotalIntendedFAttacks$Mcol, sep='')

### consum male during video
Cannibalism <- Behav_Female_Attacks[Behav_Female_Attacks$ConsumYN == 1,c('VideoID','Mcol', 'AttackTime')]
colnames(Cannibalism) <- c('VideoID','McolConsumed','ConsumTime')

Cannibalism$VideoIDMcol <- paste(Cannibalism$VideoID, Cannibalism$Mcol, sep='')

}
  
{## remove courtships starting after one male is consumed
Behav_Male_Courtships <- merge(Behav_Male_Courtships,Cannibalism[,c('VideoID','ConsumTime')],by='VideoID', all.x=TRUE)
Behav_Male_Courtships$DiffConsumCourtTime <- as.numeric(difftime(Behav_Male_Courtships$ConsumTime,Behav_Male_Courtships$CourtshipStart,units='secs'))
  # Behav_Male_Courtships[Behav_Male_Courtships$DiffConsumCourtTime < 0 & !is.na(Behav_Male_Courtships$DiffConsumCourtTime),]
Behav_Male_Courtships <- Behav_Male_Courtships[Behav_Male_Courtships$DiffConsumCourtTime > 0 | is.na(Behav_Male_Courtships$DiffConsumCourtTime),]

### all courtships watched between time start and time consume or time stop watching video
AllCourts <- Behav_Male_Courtships %>%
  group_by(VideoID,Mcol) %>% 
  summarize(NBCourt = n(),
            TotalCourtDur = sum(CourtDuration),
            FirstCourt = min(CourtshipStart))

AllCourts$VideoIDMcol = paste (AllCourts$VideoID, AllCourts$Mcol, sep='')
}
  
{## male male interactions
### all interaction type (including displaying to each other)
AllMInter <- Behav_MaleMale_Competition %>%
  group_by(VideoID) %>% 
  summarize(NbMInter = n(),
            TotalMInterDur = sum(InterDur),
            FirstMInter = min(InteractionStart))

### check whether those who attack always win: we will consider that yes
Behav_MaleMale_Competition[Behav_MaleMale_Competition$NBAttacks > 0 & 
                             Behav_MaleMale_Competition$McolStart != Behav_MaleMale_Competition$McolWin & 
                             !is.na(Behav_MaleMale_Competition$McolWin),]

### all male aggression towards another (attacks or physical contact)
MAttacks <- Behav_MaleMale_Competition[Behav_MaleMale_Competition$NBAttacks > 0 | 
                                            Behav_MaleMale_Competition$PushPushYN == 1 | 
                                            Behav_MaleMale_Competition$RollOverYN == 1 ,] %>%
                        group_by(VideoID, McolStart) %>% 
                        summarize(NbMAttacks = sum(NBAttacks), 
                                  NbPushPush = sum(PushPushYN),
                                  NbRollOverYN = sum(RollOverYN), 
                                  FirstMAttack = min(InteractionStart))

### the opposite color is the one attacked by the male who start the malemale interaction
MAttacks$McolAttacked <- NA
MAttacks$McolAttacked[MAttacks$McolStart == 'Blue'] <- 'Red'
MAttacks$McolAttacked[MAttacks$McolStart == 'Yellow'] <- 'Blue'
MAttacks$McolAttacked[MAttacks$McolAttacked == 'Red'] <- 'Yellow'
}
  
{## keep only courtships before attacks from other male or from female
### Female attacks
FAttacks$VideoIDMcol <- paste(FAttacks$VideoID,FAttacks$Mcol,sep='')
Behav_Male_Courtships$VideoIDMcol <- paste(Behav_Male_Courtships$VideoID,Behav_Male_Courtships$Mcol,sep='')
Behav_Male_Courtships <- merge(x=Behav_Male_Courtships,y=as.data.frame(FAttacks[c('VideoIDMcol','FirstFAttack')]), by="VideoIDMcol", all.x=TRUE)
 
### Male attacks: 
MAttacks$VideoIDMcol <- paste(MAttacks$VideoID,MAttacks$McolAttacked,sep='')
Behav_Male_Courtships <- merge(x=Behav_Male_Courtships,y=as.data.frame(MAttacks[c('VideoIDMcol','FirstMAttack')]), by="VideoIDMcol", all.x=TRUE)

### Courtships done between time start video and time first attack (by other male or female)
    ### Courtships done after first attack
    AfterAttackCourtshipID <- Behav_Male_Courtships$CourtshipID[Behav_Male_Courtships$CourtshipStart > Behav_Male_Courtships$FirstFAttack & !is.na(Behav_Male_Courtships$FirstFAttack)
                           |Behav_Male_Courtships$CourtshipStart > Behav_Male_Courtships$FirstMAttack & !is.na(Behav_Male_Courtships$FirstMAttack )]
    # Behav_Male_Courtships[Behav_Male_Courtships$VideoID == 24,]
    # NaiveBehav_Male_Courtships[NaiveBehav_Male_Courtships$VideoID == 24,]
    
NaiveBehav_Male_Courtships <- Behav_Male_Courtships[! Behav_Male_Courtships$CourtshipID %in% AfterAttackCourtshipID,]    
    
NaiveCourts <- NaiveBehav_Male_Courtships %>%
  group_by(VideoID,Mcol) %>% 
  summarize(NaiveNBCourt = n(),
            NaiveTotalCourtDur = sum(CourtDuration),
            NaiveFirstCourt = min(CourtshipStart))

NaiveCourts$VideoIDMcol = paste (NaiveCourts$VideoID, NaiveCourts$Mcol, sep='')

}
  
}

FAttacks
TotalIntendedFAttacks
Cannibalism
AllCourts
AllMInter
MAttacks
NaiveCourts

{# Combine summaries per male

MY_TABLE_Videos_perMale <- rbind(data.frame(Behav_Video_MaleTest[,c('VideoID', 'Author', 'VideoDate','FID','TotalWatch','VideoTimeStart','Done')],
                                            Mcol = 'Blue', 
                                            MID=Behav_Video_MaleTest$MIDBlue, 
                                            DelayLeaveDish= Behav_Video_MaleTest$BlackDelayLeaveDish),
                                data.frame(Behav_Video_MaleTest[,c('VideoID', 'Author', 'VideoDate','FID', 'TotalWatch','VideoTimeStart','Done')], 
                                           Mcol = 'Yellow',
                                           MID=Behav_Video_MaleTest$MIDYellow,  
                                           DelayLeaveDish= Behav_Video_MaleTest$RedDelayLeaveDish))

MY_TABLE_Videos_perMale <- arrange(MY_TABLE_Videos_perMale,VideoID)
MY_TABLE_Videos_perMale$VideoIDMcol <- paste(MY_TABLE_Videos_perMale$VideoID,MY_TABLE_Videos_perMale$Mcol, sep='')

MY_TABLE_Videos_perMale <- merge (x=MY_TABLE_Videos_perMale, y= FAttacks[,c('FirstFAttack', 'NbFAttacks', 'VideoIDMcol')],by='VideoIDMcol', all.x=TRUE)
MY_TABLE_Videos_perMale <- merge (x=MY_TABLE_Videos_perMale, y= TotalIntendedFAttacks[,c('NbIntendedFAttacks', 'VideoIDMcol')],by='VideoIDMcol', all.x=TRUE)
MY_TABLE_Videos_perMale <- merge (x=MY_TABLE_Videos_perMale, y= Cannibalism[,c('ConsumTime', 'VideoIDMcol')],by='VideoIDMcol', all.x=TRUE)
MY_TABLE_Videos_perMale <- merge (x=MY_TABLE_Videos_perMale, y= MAttacks[,c('NbMAttacks', 'NbPushPush', 'NbRollOverYN','FirstMAttack', 'VideoIDMcol')],by='VideoIDMcol', all.x=TRUE)
MY_TABLE_Videos_perMale <- merge (x=MY_TABLE_Videos_perMale, y= AllCourts[,c('NBCourt','TotalCourtDur', 'FirstCourt','VideoIDMcol')],by='VideoIDMcol', all.x=TRUE)
MY_TABLE_Videos_perMale <- merge (x=MY_TABLE_Videos_perMale, y= NaiveCourts[,c('NaiveNBCourt','NaiveTotalCourtDur', 'NaiveFirstCourt','VideoIDMcol')],by='VideoIDMcol', all.x=TRUE)

MY_TABLE_Videos_perMale <- MY_TABLE_Videos_perMale %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

MY_TABLE_Videos_perMale <- merge (x = MY_TABLE_Videos_perMale, y=  Behav_Female[,c('FID', 'ExcludeYN', 'ReasonExclusion', 'Remarks')],
                          by = 'FID', all.x=TRUE)  

## Convert first times into delay from time start video, in seconds

MY_TABLE_Videos_perMale$DelayFirstFAttack <- as.numeric(difftime(MY_TABLE_Videos_perMale$FirstFAttack,MY_TABLE_Videos_perMale$VideoTimeStart, units = 'secs'))
MY_TABLE_Videos_perMale$DelayConsum <- as.numeric(difftime(MY_TABLE_Videos_perMale$ConsumTime,MY_TABLE_Videos_perMale$VideoTimeStart, units = 'secs'))
MY_TABLE_Videos_perMale$DelayFirstMAttack <- as.numeric(difftime(MY_TABLE_Videos_perMale$FirstMAttack,MY_TABLE_Videos_perMale$VideoTimeStart, units = 'secs'))
MY_TABLE_Videos_perMale$DelayFirstCourt <- as.numeric(difftime(MY_TABLE_Videos_perMale$FirstCourt,MY_TABLE_Videos_perMale$VideoTimeStart, units = 'secs'))
MY_TABLE_Videos_perMale$DelayNaiveFirstCourt <- as.numeric(difftime(MY_TABLE_Videos_perMale$NaiveFirstCourt,MY_TABLE_Videos_perMale$VideoTimeStart, units = 'secs'))

### remove times
MY_TABLE_Videos_perMale <- subset(MY_TABLE_Videos_perMale, select = - c(VideoTimeStart,FirstFAttack,ConsumTime,FirstMAttack,FirstCourt,NaiveFirstCourt))

## add if end up consum 
outcomeMCannibalized <- rbind(data.frame(ConsumYN= Behav_Female$AttackRedYN, FIDMcol = paste(Behav_Female$FID,'Yellow',sep='')), 
                              (data.frame( ConsumYN= Behav_Female$AttackGreyBlackYN, FIDMcol = paste(Behav_Female$FID,'Blue',sep=''))))
MY_TABLE_Videos_perMale$FIDMcol <- paste(MY_TABLE_Videos_perMale$FID,MY_TABLE_Videos_perMale$Mcol, sep='')

MY_TABLE_Videos_perMale <- merge (x = MY_TABLE_Videos_perMale, y= outcomeMCannibalized, by = 'FIDMcol', all.x=TRUE)  

MY_TABLE_Videos_perMale$Died <- 0
for (i in 1:nrow(MY_TABLE_Videos_perMale)){
if(!is.na(MY_TABLE_Videos_perMale$ReasonExclusion[i]) & MY_TABLE_Videos_perMale$ReasonExclusion[i] == 'BlackMaleDied' & MY_TABLE_Videos_perMale$Mcol[i] == 'Blue') 
{MY_TABLE_Videos_perMale$Died[i] <- 1}
  if(!is.na(MY_TABLE_Videos_perMale$ReasonExclusion[i]) &MY_TABLE_Videos_perMale$ReasonExclusion[i] == 'RedMaleDied' & MY_TABLE_Videos_perMale$Mcol[i] == 'Yellow') 
  {MY_TABLE_Videos_perMale$Died[i] <- 1}
}

summary(MY_TABLE_Videos_perMale)
}

head(MY_TABLE_Videos_perMale)

{# Combine summaries per video

MY_TABLE_Videos_perVideo <-  MY_TABLE_Videos_perMale %>% 
    group_by(VideoID) %>% 
    summarize(NbFAttacks = sum(NbFAttacks),
              NbIntendedFAttacks = sum(NbIntendedFAttacks),
              NbMAttacks = sum(NbMAttacks),
              NbMphysicalInter = sum(NbMAttacks,NbPushPush, NbRollOverYN), 
              NBCourt = sum(NBCourt),
              NaiveNBCourt = sum (NaiveNBCourt))
               
MY_TABLE_Videos <- Behav_Video_MaleTest[,c('VideoID', 'Author', 'VideoDate','FID','TotalWatch','VideoTimeStart','Done')]
MY_TABLE_Videos <- merge (x = MY_TABLE_Videos, y=  MY_TABLE_Videos_perVideo, by = 'VideoID', all.x=TRUE)                                           
MY_TABLE_Videos <- merge (x = MY_TABLE_Videos, y=  AllMInter, by = 'VideoID', all.x=TRUE)  
MY_TABLE_Videos <- merge (x = MY_TABLE_Videos, y=  Behav_Female[,c('FID', 'AttackRedYN', 'AttackGreyBlackYN', 'ExcludeYN', 'ReasonExclusion', 'Remarks')],
                          by = 'FID', all.x=TRUE)  

MY_TABLE_Videos <- MY_TABLE_Videos %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

## Convert first times into delay from time start video, in seconds
MY_TABLE_Videos$DelayFirstMInter <-  as.numeric(difftime(MY_TABLE_Videos$FirstMInter,MY_TABLE_Videos$VideoTimeStart, units = 'secs'))
MY_TABLE_Videos <- subset(MY_TABLE_Videos, select = - c(VideoTimeStart,FirstMInter))

summary(MY_TABLE_Videos)

}

head(MY_TABLE_Videos)



#~~~ ANALYSES

{## Preregistered
### comparison delay to court for both type of male in the valid tests (not excluded because one of the three spiders died for other reason than cannibalism)

modDelayCourtValidTest <- lmer(DelayFirstCourt ~ Mcol + (1|FID)
                               ,data = MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$ExcludeYN == 0,])
summary(modDelayCourtValidTest) # n=111 NS
}

{## Exploratory
### are black males behaving differently than red males, or are they receiving more attacks from the other male and the female ?

modDelayCourtAllVideo <- lmer(DelayFirstCourt ~ Mcol*Author + (1|FID)
                               ,data = MY_TABLE_Videos_perMale)
summary(modDelayCourtAllVideo)# n=150 NS


modDelayNaiveCourtAllVideo <- lmer(DelayNaiveFirstCourt ~ Mcol*Author + (1|FID)
                              ,data = MY_TABLE_Videos_perMale)
summary(modDelayNaiveCourtAllVideo)# n=129 NS


modDelayLeaveDish <- lmer(DelayLeaveDish~ Mcol*Author + (1|FID)
                          ,data = MY_TABLE_Videos_perMale)
summary(modDelayLeaveDish)# n=170 NS


modNbFAttacks <- lmer(NbFAttacks~ Mcol
                      *Author # I score signficantly more attacks, trends toward scoring less attacks towards yellow male
                      + (1|FID)
                          ,data = MY_TABLE_Videos_perMale)
summary(modNbFAttacks)# n=170 * significantly less attacks towards yellow male * > due to my bias

  ### Lauren was blind to the treatment of the male, I wasn't
  modNbFAttacksLG <- lmer(NbFAttacks~ Mcol
                        + (1|FID)
                        ,data = MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$Author == 'LG',])
  summary(modNbFAttacksLG)# n=72 NS !!
  
  modNbFAttacksMI <- lmer(NbFAttacks~ Mcol
                          + (1|FID)
                          ,data = MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$Author == 'MI',])
  summary(modNbFAttacksMI)# n=102 * significantly less attacks towards yellow *

  ### Did I score more of the videos where male end up dying (which could explain why I scored more attacks against blue male ?)
  table(MY_TABLE_Videos$Author, MY_TABLE_Videos$ReasonExclusion)
  
  ### I have done more of those videos of tests where the black male ended up dead
  modNbFAttacksValidTests <- lmer(NbFAttacks~ Mcol
                        *Author # I score signficantly more attacks, trends toward scoring less attacks towards yellow male
                        + (1|FID)
                        ,data = MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$ExcludeYN == 0,])
  summary(modNbFAttacksValidTests)
  
  modNbFAttacksValidTestsLG <- lmer(NbFAttacks~ Mcol
                                  + (1|FID)
                                  ,data = MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$ExcludeYN == 0 & MY_TABLE_Videos_perMale$Author == 'LG',])
  summary(modNbFAttacksValidTestsLG) 
  
  modNbFAttacksValidTestsMI <- lmer(NbFAttacks~ Mcol
                                    + (1|FID)
                                    ,data = MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$ExcludeYN == 0 & MY_TABLE_Videos_perMale$Author == 'MI',])
  summary(modNbFAttacksValidTestsMI) # n= 76 * significantly less attacks towards yellow *
  
  

modNbIntendedFAttacks <- lmer(NbIntendedFAttacks~ Mcol*Author + (1|FID)
                      ,data = MY_TABLE_Videos_perMale)
summary(modNbIntendedFAttacks)# n=170 . trend less attacks towards yellow male (my bias)


modNbMAttacks <- lmer(NbMAttacks~ Mcol*Author + (1|FID)
                              ,data = MY_TABLE_Videos_perMale)
summary(modNbMAttacks)# n=170 . trend less attacks towards yellow male


modTotalCourtDur <- lmer(TotalCourtDur~ Mcol*Author + (1|FID)
                      ,data = MY_TABLE_Videos_perMale)
summary(modTotalCourtDur)# n=170 NS


modNaiveTotalCourtDur <- lmer(NaiveTotalCourtDur~ Mcol*Author + (1|FID)
                         ,data = MY_TABLE_Videos_perMale)
summary(modNaiveTotalCourtDur)# n=170 NS

## Do F attacks predict futur consumption ?
modFconsum <- lmer(ConsumYN~ Mcol+NbFAttacks + (1|FID)
                      ,data = MY_TABLE_Videos_perMale)
summary(modFconsum) # ** Nb attack predicts furtur cinsumption, Yellow less consumed

modFconsumAttackRate <- lmer(ConsumYN~ Mcol+I(NbFAttacks/TotalWatch) + (1|FID)
                   ,data = MY_TABLE_Videos_perMale)
summary(modFconsumAttackRate)

## Do F and M attacks predict futur death of male for reason other than consumption ?
modMaleDied <- lmer(Died~ Mcol+ I((NbFAttacks+NbMAttacks)) + (1|FID)
                    ,data = MY_TABLE_Videos_perMale)
summary(modMaleDied) 

modMaleDiedrate <- lmer(Died~ Mcol+ I((NbFAttacks+NbMAttacks)/TotalWatch) + (1|FID)
                    ,data = MY_TABLE_Videos_perMale)
summary(modMaleDiedrate) 

}

{## Descriptive

{### Nb of videos with male male fight
#### All videos
summary(MY_TABLE_Videos$NbMAttacks)
length(MY_TABLE_Videos$NbMAttacks[MY_TABLE_Videos$NbMAttacks > 0])/
  length(MY_TABLE_Videos$NbMAttacks)*100 # 36%
#### valid tests
summary(MY_TABLE_Videos$NbMAttacks[MY_TABLE_Videos$ExcludeYN ==0])
length(MY_TABLE_Videos$NbMAttacks[MY_TABLE_Videos$NbMAttacks > 0 & MY_TABLE_Videos$ExcludeYN ==0])/
  length(MY_TABLE_Videos$NbMAttacks[MY_TABLE_Videos$ExcludeYN ==0])*100 # 38%
}

{### Nb of videos with male male physical interaction
#### All videos
summary(MY_TABLE_Videos$NbMphysicalInter)
length(MY_TABLE_Videos$NbMphysicalInter[MY_TABLE_Videos$NbMphysicalInter > 0])/
  length(MY_TABLE_Videos$NbMphysicalInter)*100 # 43%
#### valid tests
summary(MY_TABLE_Videos$NbMphysicalInter[MY_TABLE_Videos$ExcludeYN ==0])
length(MY_TABLE_Videos$NbMphysicalInter[MY_TABLE_Videos$NbMphysicalInter > 0 & MY_TABLE_Videos$ExcludeYN ==0])/
  length(MY_TABLE_Videos$NbMphysicalInter[MY_TABLE_Videos$ExcludeYN ==0])*100 # 46%
}

{### Nb of videos with male male any interaction (include display)
#### All videos
summary(MY_TABLE_Videos$NbMInter)
length(MY_TABLE_Videos$NbMInter[MY_TABLE_Videos$NbMInter > 0])/
  length(MY_TABLE_Videos$NbMInter)*100 # 68%
#### valid tests
summary(MY_TABLE_Videos$NbMInter[MY_TABLE_Videos$ExcludeYN ==0])
length(MY_TABLE_Videos$NbMInter[MY_TABLE_Videos$NbMInter > 0 & MY_TABLE_Videos$ExcludeYN ==0])/
  length(MY_TABLE_Videos$NbMInter[MY_TABLE_Videos$ExcludeYN ==0])*100 # 74%
}

{### Nb of males who didn't court
#### All videos
summary(MY_TABLE_Videos_perMale$NBCourt)
length(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$NBCourt == 0])/
  length(MY_TABLE_Videos_perMale$NBCourt)*100 # 11%
#### valid tests
summary(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$ExcludeYN ==0])
length(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$NBCourt == 0 & MY_TABLE_Videos_perMale$ExcludeYN ==0])/
  length(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$ExcludeYN ==0])*100 # 12%
}

{### Nb of males who didn't court BUT not because they were eaten (video Done = YES, ConsumYN = NO)
#### were the blue male less likely not to court: No
#### All videos
summary(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$Done == 1 & !MY_TABLE_Videos_perMale$VideoID%in%Cannibalism$VideoID])
length(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$NBCourt == 0 & MY_TABLE_Videos_perMale$Done == 1 & !MY_TABLE_Videos_perMale$VideoID%in%Cannibalism$VideoID])/
  length(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$Done == 1 & !MY_TABLE_Videos_perMale$VideoID%in%Cannibalism$VideoID])*100 # 25% (3/12)
MY_TABLE_Videos_perMale$Mcol[MY_TABLE_Videos_perMale$NBCourt == 0 & MY_TABLE_Videos_perMale$Done == 1 & !MY_TABLE_Videos_perMale$VideoID%in%Cannibalism$VideoID]
#### valid tests
summary(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$ExcludeYN ==0 & MY_TABLE_Videos_perMale$Done == 1 & !MY_TABLE_Videos_perMale$VideoID%in%Cannibalism$VideoID])
length(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$NBCourt == 0 & MY_TABLE_Videos_perMale$ExcludeYN ==0 & MY_TABLE_Videos_perMale$Done == 1 & !MY_TABLE_Videos_perMale$VideoID%in%Cannibalism$VideoID])/
  length(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$ExcludeYN ==0 & MY_TABLE_Videos_perMale$Done == 1 & !MY_TABLE_Videos_perMale$VideoID%in%Cannibalism$VideoID])*100 # 17% (1/6)
MY_TABLE_Videos_perMale$Mcol[MY_TABLE_Videos_perMale$ExcludeYN ==0 & MY_TABLE_Videos_perMale$NBCourt == 0 & MY_TABLE_Videos_perMale$Done == 1 & !MY_TABLE_Videos_perMale$VideoID%in%Cannibalism$VideoID]
}

{### Nb of videos where female attacked (or at least intended to)
#### All videos
summary(MY_TABLE_Videos$NbIntendedFAttacks)
length(MY_TABLE_Videos$NbIntendedFAttacks[MY_TABLE_Videos$NbIntendedFAttacks > 0])/
  length(MY_TABLE_Videos$NbIntendedFAttacks)*100 # 49%
#### valid tests
summary(MY_TABLE_Videos$NbIntendedFAttacks[MY_TABLE_Videos$ExcludeYN ==0])
length(MY_TABLE_Videos$NbIntendedFAttacks[MY_TABLE_Videos$NbIntendedFAttacks > 0 & MY_TABLE_Videos$ExcludeYN ==0])/
  length(MY_TABLE_Videos$NbIntendedFAttacks[MY_TABLE_Videos$ExcludeYN ==0])*100 # 59% 
} 

### average time watched
summary(MY_TABLE_Videos$TotalWatch)/60 # in min
summary(MY_TABLE_Videos$TotalWatch[MY_TABLE_Videos$ExcludeYN ==0])/60 # in min

### average delay to court
summary(MY_TABLE_Videos_perMale$DelayFirstCourt)/60 # in min

### average duration courting (out of duration watched)
summary(60*(MY_TABLE_Videos_perMale$TotalCourtDur/60)/(MY_TABLE_Videos_perMale$TotalWatch/60)) # in min of coursthip per hour

### average duration male male interaction (out of duration watched)
summary(60*(MY_TABLE_Videos$TotalMInterDur/60)/(MY_TABLE_Videos$TotalWatch/60)) # in min of interaction per hour

### average delay to male male interaction
summary(MY_TABLE_Videos$DelayFirstMInter)/60 # in min

### Nb of courtships that lead to female attack
summary(Behav_Male_Courtships$FemaleResponsiveness)
length(Behav_Male_Courtships$FemaleResponsiveness[Behav_Male_Courtships$FemaleResponsiveness == 'Attacks'])/nrow(Behav_Male_Courtships)*100 # 11%



}


head(MY_TABLE_Videos)
head(MY_TABLE_Videos_perMale)



