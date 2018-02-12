#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Video analyses FlexibleSexualAppetite part 1
#	 Start : 12 Feb 2018
#	 last modif : 12/02/2018
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
  library(rptR)
  library(pbapply)
  library(RODBC)
  library(ggplot2)
  library(stringr)
  library(dplyr)
}

{# load data
  
  #conDB= odbcConnectAccess2007("C:\\Users\\malika.ihle\\Dropbox\\HabronatusPyrrithrix\\VideoAnalyses_MaleTests.accdb")
conDB= odbcConnectAccess2007("C:\\Users\\malika.ihle\\Desktop\\COPY VideoAnalyses_MaleTests.accdb")
sqlTables(conDB)

Behav_Video_MaleTest <- sqlFetch(conDB, 'Behav_Video_MaleTest')
Behav_Female <- sqlFetch(conDB, 'Behav_Female')
Behav_Female_Attacks <- sqlFetch(conDB, 'Behav_Female_Attacks')
Behav_Male_Courtships <- sqlFetch(conDB, 'Behav_Male_Courtships')
Behav_MaleMale_Competition <- sqlFetch(conDB, 'Behav_Male-Male_Competition')
BlackMales <- sqlFetch(conDB, 'BlackMales')
RedMales <- sqlFetch(conDB, 'RedMales')
  
close(conDB)

}


{# functions

## read text column into time

ConvertToTime <- function(x){
  as.POSIXct(strptime(str_pad(x, 6, pad = "0"), format="%H%M%S"))
}

}


{## convert time and calculate delays, convert color

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

Behav_Male_Courtships <- Behav_Male_Courtships[Behav_Male_Courtships$CourtMale == 0 & Behav_Male_Courtships$UnsureMaleID == 0,]
Behav_Male_Courtships$CourtDuration <- as.numeric(difftime(Behav_Male_Courtships$CourtshipEnd,Behav_Male_Courtships$CourtshipStart, units = 'secs'))

Behav_MaleMale_Competition <- Behav_MaleMale_Competition[Behav_MaleMale_Competition$UnsureMaleIDYN == 0,]
Behav_MaleMale_Competition$InterDur <- as.numeric(difftime(Behav_MaleMale_Competition$InteractionEnd,Behav_MaleMale_Competition$InteractionStart, units = 'secs'))


Behav_Female_Attacks$Mcol[Behav_Female_Attacks$Mcol == 'yellow'] <- 'Yellow'
Behav_Female_Attacks$Mcol[Behav_Female_Attacks$Mcol == 'blue'] <- 'Blue'
Behav_Male_Courtships$Mcol[Behav_Male_Courtships$Mcol == 'yellow'] <- 'Yellow'
Behav_Male_Courtships$Mcol[Behav_Male_Courtships$Mcol == 'blue'] <- 'Blue'
Behav_MaleMale_Competition$McolStart[Behav_MaleMale_Competition$McolStart == 'yellow'] <- 'Yellow'
Behav_MaleMale_Competition$McolStart[Behav_MaleMale_Competition$McolStart == 'blue'] <- 'Blue'
Behav_MaleMale_Competition$McolWin[Behav_MaleMale_Competition$McolWin == 'yellow'] <- 'Yellow'
Behav_MaleMale_Competition$McolWin[Behav_MaleMale_Competition$McolWin == 'blue'] <- 'Blue'


}

## summarize type of interactions

### female attacks
FAttacks <- Behav_Female_Attacks[Behav_Female_Attacks$AttackYN == 1 | Behav_Female_Attacks$ConsumYN == 1,] %>% 
  group_by(VideoID,Mcol) %>% 
  summarize(FirstFAttack = min(AttackTime),
            NbAttacks = n())    

TotalIntendedFAttacks <- Behav_Female_Attacks %>% 
  group_by(VideoID,Mcol) %>% 
  summarize(NbIntendedAttacks = n())  


Cannibalism <- Behav_Female_Attacks[Behav_Female_Attacks$ConsumYN == 1,c('VideoID','Mcol', 'AttackTime')]
colnames(Cannibalism) <- c('VideoID','McolConsumed','ConsumTime')

### remove courtships starting after one male is consumed
Behav_Male_Courtships <- merge(Behav_Male_Courtships,Cannibalism[,c('VideoID','ConsumTime')],by='VideoID', all.x=TRUE)
Behav_Male_Courtships$DiffConsumCourtTime <- as.numeric(difftime(Behav_Male_Courtships$ConsumTime,Behav_Male_Courtships$CourtshipStart,units='secs'))
  # Behav_Male_Courtships[Behav_Male_Courtships$DiffConsumCourtTime < 0 & !is.na(Behav_Male_Courtships$DiffConsumCourtTime),]
Behav_Male_Courtships <- Behav_Male_Courtships[Behav_Male_Courtships$DiffConsumCourtTime > 0 | is.na(Behav_Male_Courtships$DiffConsumCourtTime),]


AllCourts <- Behav_Male_Courtships %>%
  group_by(VideoID,Mcol) %>% 
  summarize(NBCourt = n(),
            TotalCourtDur = sum(CourtDuration),
            FirstCourt = min(CourtshipStart))

### male male interactions
AllMInter <- Behav_MaleMale_Competition %>%
  group_by(VideoID) %>% 
  summarize(NbMaleInter = n(),
            TotalMaleInterDur = sum(InterDur),
            FirstMaleInter = min(InteractionStart))

MAttacks <- Behav_MaleMale_Competition[Behav_MaleMale_Competition$NBAttacks > 0 | 
                                            Behav_MaleMale_Competition$PushPushYN == 1 | 
                                            Behav_MaleMale_Competition$RollOverYN == 1 ,] %>%
                        group_by(VideoID, McolStart) %>% 
                        summarize(NbMAttacks = sum(NBAttacks), 
                                  NbPushPush = sum(PushPushYN),
                                  NbRollOverYN = sum(RollOverYN), 
                                  FirstMAttack = min(InteractionStart))

MAttacks$McolAttacked <- NA
MAttacks$McolAttacked[MAttacks$McolStart == 'Blue'] <- 'Red'
MAttacks$McolAttacked[MAttacks$McolStart == 'Yellow'] <- 'Blue'
MAttacks$McolAttacked[MAttacks$McolAttacked == 'Red'] <- 'Yellow'


#### check whether those who attack always win
Behav_MaleMale_Competition[Behav_MaleMale_Competition$NBAttacks > 0 & 
                             Behav_MaleMale_Competition$McolStart != Behav_MaleMale_Competition$McolWin & 
                             !is.na(Behav_MaleMale_Competition$McolWin),]


### keep only courtships before attacks from other male or from female
FAttacks$VideoIDMcol <- paste(FAttacks$VideoID,FAttacks$Mcol,sep='')
Behav_Male_Courtships$VideoIDMcol <- paste(Behav_Male_Courtships$VideoID,Behav_Male_Courtships$Mcol,sep='')
Behav_Male_Courtships <- merge(x=Behav_Male_Courtships,y=as.data.frame(FAttacks[c('VideoIDMcol','FirstFAttack')]), by="VideoIDMcol", all.x=TRUE)
 
  #### the opposite color is the one attacked by the male who start the malemale interaction
  MAttacks$VideoIDMcol <- paste(MAttacks$VideoID,MAttacks$McolAttacked,sep='')
  Behav_Male_Courtships <- merge(x=Behav_Male_Courtships,y=as.data.frame(MAttacks[c('VideoIDMcol','FirstMAttack')]), by="VideoIDMcol", all.x=TRUE)

NaiveBehav_Male_Courtships <- Behav_Male_Courtships[Behav_Male_Courtships$CourtshipStart < Behav_Male_Courtships$FirstFAttack | is.na(Behav_Male_Courtships$FirstFAttack)
                      |Behav_Male_Courtships$CourtshipStart < Behav_Male_Courtships$FirstMAttack | is.na(Behav_Male_Courtships$FirstMAttack ),]

# Behav_Male_Courtships[Behav_Male_Courtships$CourtshipStart > Behav_Male_Courtships$FirstFAttack & !is.na(Behav_Male_Courtships$FirstFAttack)
#                       |Behav_Male_Courtships$CourtshipStart > Behav_Male_Courtships$FirstMAttack & !is.na(Behav_Male_Courtships$FirstMAttack ),]


NaiveCourts <- NaiveBehav_Male_Courtships %>%
  group_by(VideoID,Mcol) %>% 
  summarize(NBCourt = n(),
            TotalCourtDur = sum(CourtDuration),
            FirstCourt = min(CourtshipStart))

MY_TABLE_Videos <- Behav_Video_MaleTest[,c('VideoID', 'Author', 'VideoDate','FID', 'MIDBlue', 'MIDYellow', 'BlackDelayLeaveDish','RedDelayLeaveDish','TotalWatch')]

head(Behav_Video_MaleTest)
head(Behav_Female)
head(Behav_Female_Attacks)
head(Behav_Male_Courtships)
head(Behav_MaleMale_Competition)
head(BlackMales)
head(RedMales)
head(MY_TABLE_Videos)

