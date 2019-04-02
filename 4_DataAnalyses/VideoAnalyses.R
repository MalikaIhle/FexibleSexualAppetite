#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Video analyses FlexibleSexualAppetite part 1
#	 Start : 12 Feb 2018
#	 last modif : 17 October 2018
#	 commit: corrected data prep
# 1. FID rename to what they are (remove fake ID that avoided duplicated during video analyses)
# 2. cut the time watch for both males after any of them receive an attack (to have duration of courtship comparable between the two, or courtship rate calculated on same duration)
# 3. added number of second watched for naive courtship (those prior to attack, removing delay to start video)
# modif: 28 February 2019
# commit: corrected NbAttacks to be the nb of attacks male received rather than gave (since it is sometimes combined with NbFattacks which obsviously is attack received)
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
  # videos were watched for trials 160 405 297 (video ID 29, 31, 35) (which end up being excluded trials), these are the first trial of females 193, 112 and 106 (their second tedt were not taped/watched)
  # video from the second test of female 193 (test 317), video ID 36, was watched but need to be excluded here.
  # video recording failed for female 210 and 254
  # video recording was shorter (due to technical difficulties) for females 224 and 207
  # male male interactions were initialy scored as attack / push push, rollover ect, all physical aggressive interactions are now pulled into the variable NbMphysicalInter
}

rm(list = ls(all = TRUE))

{# packages
  library(lme4)
  library(RODBC) # this require R AND ACCESS to run on 32 bits !
  library(stringr)
  library(dplyr)
  library(here)
  require(sjPlot) # for model automatic interaction plot
  library(gridExtra) # for function gridarrange
}

{# functions
  
  ## read text column into time
  
  ConvertToTime <- function(x){
    as.POSIXct(str_pad(x, 6, pad = "0"), format="%H%M%S") # this adds the date of today to every time... stupid but hard to get around and not important
  }
  
  ConvertTimeToSecs <- function(x){difftime(x, as.POSIXct(as.character('000000'), format="%H%M%S"), unit='secs')}
  
}


#~~~ EXTRACT DATASET

{# load data
conDB= odbcConnectAccess2007(paste(here(),"1_RawData/VideoAnalyses_MaleTests_2017.accdb", sep="/"))
  
sqlTables(conDB)

Behav_Video_MaleTest <- sqlFetch(conDB, 'Behav_Video_MaleTest')
Behav_Female <- sqlFetch(conDB, 'Behav_Female')
Behav_Female_Attacks <- sqlFetch(conDB, 'Behav_Female_Attacks')
Behav_Male_Courtships <- sqlFetch(conDB, 'Behav_Male_Courtships')
Behav_MaleMale_Competition <- sqlFetch(conDB, 'Behav_Male-Male_Competition')
FemaleTrt <- sqlFetch(conDB, 'Basic_Trials')

# exclude video IDs from test with replacement males (see remarks)
Behav_Video_MaleTest$FID[Behav_Video_MaleTest$FID >1000] # the zero at the end of the FID stands for the first trial, the 1 for the second trial

Behav_Video_MaleTest <- Behav_Video_MaleTest[Behav_Video_MaleTest$VideoID != 36,] 
Behav_Female_Attacks <- Behav_Female_Attacks[Behav_Female_Attacks$VideoID != 36,] 
Behav_Male_Courtships <- Behav_Male_Courtships[Behav_Male_Courtships$VideoID != 36,] 
Behav_MaleMale_Competition <- Behav_MaleMale_Competition[Behav_MaleMale_Competition$VideoID != 36,] 

# exclude 'fake FID' (used to watch two trials per female - but whose second trial is now excluded, see remarks)
Behav_Video_MaleTest$FID[Behav_Video_MaleTest$FID >1000] # the zero at the end of the FID stands for the first trial, the 1 for the second trial

Behav_Video_MaleTest$FID[Behav_Video_MaleTest$FID == 1120] <- 112
Behav_Video_MaleTest$FID[Behav_Video_MaleTest$FID == 1930] <- 193
Behav_Video_MaleTest$FID[Behav_Video_MaleTest$FID == 1060] <- 106


Behav_Female$FID[Behav_Female$FID == 1120] <- 112
Behav_Female$FID[Behav_Female$FID == 1930] <- 193
Behav_Female$FID[Behav_Female$FID == 1060] <- 106

Behav_Female <- Behav_Female[Behav_Female$FID <1000,]

close(conDB)

}


#~~~ PROCESS DATA

{

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

FAttacks$VideoIDMcol <- paste(FAttacks$VideoID, FAttacks$Mcol, sep='')

### all instances of female aggression (even missed attacks)
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

### check whether those who attack always win: we will consider that yes (very rare that it is the opposite)
Behav_MaleMale_Competition[Behav_MaleMale_Competition$NBAttacks > 0 & 
                             Behav_MaleMale_Competition$McolStart != Behav_MaleMale_Competition$McolWin & 
                             !is.na(Behav_MaleMale_Competition$McolWin),]

### all male aggression towards another (attacks or physical contact)
MAttacksGiven <- Behav_MaleMale_Competition[Behav_MaleMale_Competition$NBAttacks > 0 | 
                                            Behav_MaleMale_Competition$PushPushYN == 1 | 
                                            Behav_MaleMale_Competition$RollOverYN == 1 ,] %>%
                        group_by(VideoID, McolStart) %>% 
                        summarize(NbMAttacks = sum(NBAttacks), 
                                  NbPushPush = sum(PushPushYN),
                                  NbRollOverYN = sum(RollOverYN), 
                                  FirstMAttack = min(InteractionStart))

### the opposite color is the one attacked by the male who start the malemale interaction
MAttacksGiven$McolAttacked <- NA
MAttacksGiven$McolAttacked[MAttacksGiven$McolStart == 'Blue'] <- 'Red'
MAttacksGiven$McolAttacked[MAttacksGiven$McolStart == 'Yellow'] <- 'Blue'
MAttacksGiven$McolAttacked[MAttacksGiven$McolAttacked == 'Red'] <- 'Yellow'

MAttacksReceived <- MAttacksGiven
MAttacksReceived$VideoIDMcol <- paste(MAttacksReceived$VideoID, MAttacksReceived$McolAttacked, sep='')


}
  
{## keep only courtships before attacks from other male or from female, use the lowest number for both males to cut time for both equally
### Female attacks (on either male)
FFirstAttack <- FAttacks  %>% group_by(VideoID) %>% 
    summarize(FirstAttack  =  min(FirstFAttack))

### Male attacks (from either male against either male)
MFirstAttack <- MAttacksReceived  %>% group_by(VideoID) %>% 
  summarize(FirstAttack  =  min(FirstMAttack))

### First attack per video: max time for any male to court
FirstAttack <- rbind(FFirstAttack,MFirstAttack) %>% group_by(VideoID) %>% 
  summarize(FirstAttackInVideo  =  min(FirstAttack))

### Courtships done between time start video and time first attack (by other male or female)
    ### Courtships done after first attack on any male by either the other male or the female

Behav_Male_Courtships <- merge(x=Behav_Male_Courtships,y=as.data.frame(FirstAttack), by="VideoID", all.x=TRUE)

AfterAttackCourtshipID <- Behav_Male_Courtships$CourtshipID[Behav_Male_Courtships$CourtshipStart > Behav_Male_Courtships$FirstAttackInVideo & !is.na(Behav_Male_Courtships$FirstAttackInVideo)]

NaiveBehav_Male_Courtships <- Behav_Male_Courtships[! Behav_Male_Courtships$CourtshipID %in% AfterAttackCourtshipID,]    
    ### Behav_Male_Courtships[Behav_Male_Courtships$VideoID == 24,]
    ### NaiveBehav_Male_Courtships[NaiveBehav_Male_Courtships$VideoID == 24,] 

# duration watched for courtship (if we want to calculate fair courtship rates where both male within a video had equal time to court (before any attack happened))
DurationWatchedForNaiveCourtship <- rbind(Behav_Video_MaleTest[,c('VideoID','TimeStoppedWatching')], setNames(FirstAttack, c('VideoID','TimeStoppedWatching')))

DurationWatchedForNaiveCourtship <- DurationWatchedForNaiveCourtship %>% group_by(VideoID) %>% 
  summarize(TotalDurationForCourtship  =  min(TimeStoppedWatching))

DurationWatchedForNaiveCourtship <- merge(x=Behav_Video_MaleTest[,c('VideoID','VideoTimeStart')],y=as.data.frame(DurationWatchedForNaiveCourtship), by="VideoID", all.x=TRUE)
DurationWatchedForNaiveCourtship$TotalDurationWatchedForNaiveCourt <- as.numeric(difftime(DurationWatchedForNaiveCourtship$TotalDurationForCourtship, DurationWatchedForNaiveCourtship$VideoTimeStart, units='secs'))

NaiveBehav_Male_Courtships <- merge(x=NaiveBehav_Male_Courtships,y=as.data.frame(DurationWatchedForNaiveCourtship[,c('VideoID','TotalDurationWatchedForNaiveCourt')]), by="VideoID", all.x=TRUE)



NaiveCourts <- NaiveBehav_Male_Courtships %>%
  group_by(VideoID,Mcol) %>% 
  summarize(NaiveNBCourt = n(),
            NaiveTotalCourtDur = sum(CourtDuration),
            NaiveFirstCourt = min(CourtshipStart),
            TotalWatchNaiveCourt = min(TotalDurationWatchedForNaiveCourt)
            )

NaiveCourts$VideoIDMcol = paste (NaiveCourts$VideoID, NaiveCourts$Mcol, sep='')

}
  
}

FAttacks
TotalIntendedFAttacks
Cannibalism
AllCourts
AllMInter
MAttacksReceived
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
MY_TABLE_Videos_perMale <- merge (x=MY_TABLE_Videos_perMale, y= MAttacksReceived[,c('NbMAttacks', 'NbPushPush', 'NbRollOverYN','FirstMAttack', 'VideoIDMcol')],by='VideoIDMcol', all.x=TRUE)
MY_TABLE_Videos_perMale <- merge (x=MY_TABLE_Videos_perMale, y= AllCourts[,c('NBCourt','TotalCourtDur', 'FirstCourt','VideoIDMcol')],by='VideoIDMcol', all.x=TRUE)
MY_TABLE_Videos_perMale <- merge (x=MY_TABLE_Videos_perMale, y= NaiveCourts[,c('NaiveNBCourt','NaiveTotalCourtDur', 'NaiveFirstCourt','TotalWatchNaiveCourt','VideoIDMcol')],by='VideoIDMcol', all.x=TRUE)

MY_TABLE_Videos_perMale$NbMphysicalInter <- MY_TABLE_Videos_perMale$NbMAttacks+MY_TABLE_Videos_perMale$NbPushPush+ MY_TABLE_Videos_perMale$NbRollOverYN


summary(MY_TABLE_Videos_perMale)
DelayLeaveDish <- MY_TABLE_Videos_perMale$DelayLeaveDish

MY_TABLE_Videos_perMale <- MY_TABLE_Videos_perMale %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) # replace all NAs by 0

MY_TABLE_Videos_perMale$DelayLeaveDish <- DelayLeaveDish

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

# add Female Trt
MY_TABLE_Videos_perMale <- merge(MY_TABLE_Videos_perMale, FemaleTrt[,c('Ind_ID','GroupName')], by.x='FID', by.y='Ind_ID')

summary(MY_TABLE_Videos_perMale)


# Change Mcol Blue/Yellow to ZBlack and ARed to have red male as reference (intercept)
MY_TABLE_Videos_perMale$Mcol <- as.character(MY_TABLE_Videos_perMale$Mcol)
MY_TABLE_Videos_perMale$Mcol[MY_TABLE_Videos_perMale$Mcol == "Yellow"] <- 'ARed'
MY_TABLE_Videos_perMale$Mcol[MY_TABLE_Videos_perMale$Mcol == "Blue"] <- 'ZBlack'

}

head(MY_TABLE_Videos_perMale)
# nrow(MY_TABLE_Videos_perMale) # 204 (104 tests performed - 2 videos missing, see remarks, = 102*2 males))
# nrow(MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$ExcludeYN==0,]) # 154 (79 valid tests - 2 videos missing, see remarks, =76*2 = 154 males))


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

# add Female Trt
MY_TABLE_Videos <- merge(MY_TABLE_Videos, FemaleTrt[,c('Ind_ID','GroupName')], by.x='FID', by.y='Ind_ID')

summary(MY_TABLE_Videos)
}

head(MY_TABLE_Videos)

# nrow(MY_TABLE_Videos) # 102 (out of 104 tests performed (2 videos missing, see remarks))
# nrow(MY_TABLE_Videos[MY_TABLE_Videos$ExcludeYN==0,]) # 77 (out of 79 valid tests (2 videos missing, see remarks))

}

head(MY_TABLE_Videos_perMale)
head(MY_TABLE_Videos)


#~~~ DATA ANALYSES

## Preregistered
{### comparison delay to court for both type of male in the valid tests (i.e. not excluded because one of the three spiders died for other reason than cannibalism)

modDelayCourtAllVideo <- lmer(DelayFirstCourt ~ Mcol + (1|FID)
                              ,data = MY_TABLE_Videos_perMale, REML =FALSE)
summary(modDelayCourtAllVideo)# n=179 (delay not NA our of 204 total male-video (25 NA)), NS


modDelayCourtValidTest <- lmer(DelayFirstCourt ~ Mcol + (1|FID)
                               ,data = MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$ExcludeYN == 0,], REML =FALSE)
summary(modDelayCourtValidTest) # n=134 (delay not NA out of 154 valid male-videos (20 NA)), NS

}

## Exploratory
{ 
### are black males behaving differently than red males 
{ ##### (I included author as I knew which male was which color, not Lauren)
    ##### Delay to court
      ###### all courts, even if had been attacked prior to starting to court
modDelayCourtAllVideo <- lmer(DelayFirstCourt ~ Mcol
                              #*Author 
                              + (1|FID)
                               ,data = MY_TABLE_Videos_perMale, REML =FALSE)
summary(modDelayCourtAllVideo)# n=179 (delay not NA out of 204 total male-video (25 NA)), NS

modDelayCourtAllVideo0 <- lmer(DelayFirstCourt ~ 1+ (1|FID),data = MY_TABLE_Videos_perMale, REML =FALSE)

anova(modDelayCourtAllVideo,modDelayCourtAllVideo0) # p value for paper


modDelayCourtValidTests <- lmer(DelayFirstCourt ~ Mcol*Author + (1|FID)
                              ,data = MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$ExcludeYN == 0,], REML =FALSE)
summary(modDelayCourtValidTests)# n=134 (delay not NA out of 154 valid male-videos (20 NA)), NS


      ###### delay to court before any attack happened on any males within test
modDelayNaiveCourtAllVideo <- lmer(DelayNaiveFirstCourt ~ Mcol*Author + (1|FID)
                              ,data = MY_TABLE_Videos_perMale, REML =FALSE)
summary(modDelayNaiveCourtAllVideo)# n=121 NS

modDelayNaiveCourtValidTests <- lmer(DelayNaiveFirstCourt ~ Mcol*Author + (1|FID)
                                   ,data = MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$ExcludeYN == 0,], REML =FALSE)
summary(modDelayNaiveCourtValidTests)# n=89 NS



    ##### delay to leave dish
modDelayLeaveDishAllVideos <- lmer(DelayLeaveDish~ Mcol
                                   #*Author 
                                   + (1|FID)
                          ,data = MY_TABLE_Videos_perMale, REML =FALSE)
summary(modDelayLeaveDishAllVideos)# n=202 NS (2 were eaten in the vial)

modDelayLeaveDishAllVideos0 <- lmer(DelayLeaveDish~1+ (1|FID),data = MY_TABLE_Videos_perMale, REML =FALSE)

anova(modDelayLeaveDishAllVideos,modDelayLeaveDishAllVideos0) # p value for paper


modDelayLeaveDishValidTests <- lmer(DelayLeaveDish~ Mcol*Author + (1|FID)
                          ,data = MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$ExcludeYN == 0,], REML =FALSE)
summary(modDelayLeaveDishValidTests)# n=152 NS



  ##### Courtship effort 
      ###### even if had been attacked prior to starting to court
modTotalCourtDur <- lmer(TotalCourtDur~  Mcol
                         #*Author 
                         + (1|FID)
                         ,data = MY_TABLE_Videos_perMale, REML =FALSE)
summary(modTotalCourtDur)# n=204 NS

modTotalCourtDur0 <- lmer(TotalCourtDur~ 1+ (1|FID) ,data = MY_TABLE_Videos_perMale, REML =FALSE)
anova(modTotalCourtDur,modTotalCourtDur0)# p value for paper

modTotalCourtDurValidTest <- lmer(TotalCourtDur~ Mcol*Author 
                         + (1|FID)
                         ,data = MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$ExcludeYN == 0,], REML =FALSE)
summary(modTotalCourtDurValidTest)# n=154 NS

      ###### before being attacked
modNaiveTotalCourtDur <- lmer(NaiveTotalCourtDur~ Mcol
                              #*Author 
                              + (1|FID)
                              ,data = MY_TABLE_Videos_perMale, REML =FALSE)
summary(modNaiveTotalCourtDur)# n=204 NS (no NAs but more zeros for duration, hence same sample size as above)

modNaiveTotalCourtDur0 <- lmer(NaiveTotalCourtDur~ 1+ (1|FID) ,data = MY_TABLE_Videos_perMale, REML =FALSE)
anova(modNaiveTotalCourtDur,modNaiveTotalCourtDur0)# p value for paper


modNaiveTotalCourtDurNaiveTests <- lmer(NaiveTotalCourtDur~ Mcol*Author 
                              + (1|FID)
                              ,data = MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$ExcludeYN == 0,], REML =FALSE)
summary(modNaiveTotalCourtDurNaiveTests)# n=154 NS (no NAs but more zeros for duration, hence same sample size as above)

}

### are black males receiving more attacks from the female ?
{ 
#### exploration of the potential author bias in attack observed
{  ######## attacks are binary and less subject to observation bias but
  ######## I scored signficantly more attacks
  ######## with trends or significant effect toward scoring less attacks towards the red male, but 
 
modNbFAttacksLG <- glmer(NbFAttacks~ Mcol
                        + (1|FID)
                        ,data = MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$Author == 'LG',]
                        , family = 'poisson')
summary(modNbFAttacksLG)# n=104 NS 

modNbFAttacksMI <- glmer(NbFAttacks~ Mcol
                        + (1|FID)
                        ,data = MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$Author == 'MI',]
                        , family = 'poisson')
summary(modNbFAttacksMI)# n=100 * significantly less attacks towards yellow *

  ######## I did watch more of those videos where the black male ended up dead
table(MY_TABLE_Videos$Author, MY_TABLE_Videos$ReasonExclusion)

  ######## Although it is possible that difference are due to me being biased, differences may be due to the random subset of videos we watched
}

#### Female attacks toward male of specific colors, in function of their training
{
modNbFAttacks <- glmer(NbFAttacks ~  Mcol*GroupName + scale(TotalWatch) 
                      #*Author - Mcol:GroupName:Author - GroupName:Author 
                      + (1|FID)
                      , data = MY_TABLE_Videos_perMale
                      , family = 'poisson')
summary(modNbFAttacks)# n=204 * signi more attacks towards black male, independent of the female treatment
drop1(modNbFAttacks, test= "Chisq")


modNbFAttacks_effects <- data.frame(est = exp(summary(modNbFAttacks)$coeff[,1]),
SEhigh = exp(summary(modNbFAttacks)$coeff[,1] + summary(modNbFAttacks)$coeff[,2]),
SElow = exp(summary(modNbFAttacks)$coeff[,1] - summary(modNbFAttacks)$coeff[,2])
)
modNbFAttacks_effects$avSE <- (modNbFAttacks_effects$SEhigh-modNbFAttacks_effects$SElow)/2



modNbFAttacksinter0 <- glmer(NbFAttacks~ Mcol+ GroupName   + scale(TotalWatch) + (1|FID)
                             , data = MY_TABLE_Videos_perMale 
                             , family = 'poisson')
summary(modNbFAttacksinter0)
anova(modNbFAttacks,modNbFAttacksinter0)# p value inter for paper

modNbFAttacks00 <- glmer(NbFAttacks~  GroupName + scale(TotalWatch) + (1|FID)
                        , data = MY_TABLE_Videos_perMale
                        , family = 'poisson')
anova(modNbFAttacksinter0,modNbFAttacks00)# p value Mcol for paper


modNbFAttacks000 <- glmer(NbFAttacks~  Mcol + scale(TotalWatch) + (1|FID)
                         , data = MY_TABLE_Videos_perMale
                         , family = 'poisson')
anova(modNbFAttacksinter0,modNbFAttacks000)# p value GroupName for paper




modNbFAttacks_ValidTests <- glmer(NbFAttacks~ Mcol* GroupName  + scale(TotalWatch)
                      #*Author - Mcol:GroupName:Author - GroupName:Author 
                      + (1|FID)
                      , data = MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$ExcludeYN == 0,]
                      , family = 'poisson')
summary(modNbFAttacks_ValidTests)#154 ** signi more attacks towards black male, independent of the female treatment
}

#### Same as above, in subset of trials without male male competition
{# !!! this is assuming than attack is a proxy for cannibalism which may not be the case - see below
All_FID_NoMaleMaleFight <- MY_TABLE_Videos$FID[MY_TABLE_Videos$NbMphysicalInter == 0]
All_MY_TABLE_Videos_perMale_NoMaleMaleFight <- MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$FID %in% All_FID_NoMaleMaleFight,]
### this is used in DataAnalysis script to test mod3 (spill over) in this subset

modNbFAttacks_NoMaleMaleFight <- glmer(NbFAttacks~ Mcol* GroupName + scale(TotalWatch)
                                                  + (1|FID)
                                                  , data = All_MY_TABLE_Videos_perMale_NoMaleMaleFight
                                                  , family = 'poisson')
summary(modNbFAttacks_NoMaleMaleFight)#106 .trend toward less attack against yellow males, independent of the female treatment

modNbFAttacks_MoMaleMaleFight_effects <- data.frame(est = exp(summary(modNbFAttacks_NoMaleMaleFight)$coeff[,1]),
                                    SEhigh = exp(summary(modNbFAttacks_NoMaleMaleFight)$coeff[,1] + summary(modNbFAttacks_NoMaleMaleFight)$coeff[,2]),
                                    SElow = exp(summary(modNbFAttacks_NoMaleMaleFight)$coeff[,1] - summary(modNbFAttacks_NoMaleMaleFight)$coeff[,2])
)
modNbFAttacks_MoMaleMaleFight_effects$avSE <- (modNbFAttacks_MoMaleMaleFight_effects$SEhigh-modNbFAttacks_MoMaleMaleFight_effects$SElow)/2



modNbFAttacks_NoMaleMaleFight_noINteraction <- glmer(NbFAttacks~ Mcol+ GroupName  + scale(TotalWatch)
                                                                + (1|FID)
                                                                , data = All_MY_TABLE_Videos_perMale_NoMaleMaleFight
                                                                , family = "poisson")
anova(modNbFAttacks_NoMaleMaleFight,modNbFAttacks_NoMaleMaleFight_noINteraction)
drop1(modNbFAttacks_NoMaleMaleFight, test="Chisq")

#plot_model(modNbFAttacks_NoMaleMaleFight, type = "pred", terms = c("Mcol", "GroupName"))
}      
      
#### figures NbF attacks ~ Ftrt*Mcol in all vids and all vids without male male interactions 
{      
  modNbFAttacks_forplotting <- glmer(NbFAttacks~ -1+ paste(Mcol,GroupName, sep="") + scale(TotalWatch) 
                                     + (1|FID)
                                     , data = MY_TABLE_Videos_perMale
                                     , family = "poisson" )
  
summary(modNbFAttacks_forplotting)

table_effect_FAttack_all <- as.data.frame(cbind(est=exp(summary(modNbFAttacks_forplotting)$coeff[,1]),
                                              CIhigh=exp(summary(modNbFAttacks_forplotting)$coeff[,1]+summary(modNbFAttacks_forplotting)$coeff[,2]*1.96),
                                              CIlow=exp(summary(modNbFAttacks_forplotting)$coeff[,1]-summary(modNbFAttacks_forplotting)$coeff[,2]*1.96)))
table_effect_FAttack_all <- table_effect_FAttack_all[-nrow(table_effect_FAttack_all),]
table_effect_FAttack_all$Mcol <- c("Red","Red", "Black", 'Black')
table_effect_FAttack_all$FTrt <- c("Red averse","Red preference", "Red averse", 'Red preference')
rownames(table_effect_FAttack_all) <- NULL
table_effect_FAttack_all

Attack_all_Fig <-   ggplot(data=table_effect_FAttack_all, aes(x=Mcol, y=est,colour=FTrt, shape = FTrt)) + 
scale_y_continuous(name="Number of female attacks", limits=c(0,0.8))+
scale_x_discrete(name = "Male facial color",limits = c("Red","Black") ) +
  labs(title = "All recorded male tests (N = 204 males)") +
theme_classic() + # white backgroun, x and y axis (no box)
geom_errorbar(aes(ymin=CIlow, ymax=CIhigh, col=FTrt), width =0.4,na.rm=TRUE, position = position_dodge(width=0.5))+ # don't plot bor bars on x axis tick, but separate them (dodge)
geom_point(size =4, aes(shape=FTrt, col=FTrt), stroke = 1, position = position_dodge(width=0.5)) +
scale_colour_manual(name= "Female treatment group", values=c("Black","Grey")) +
scale_shape_manual(name= "Female treatment group", values=c(16,17))+ # duplicate title to combine legend
theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
      legend.position=c(0.3,0.85),
      legend.title = element_text(size=rel(0.8)),
      legend.text = element_text(size=rel(0.7)),
      legend.key.size = unit(0.8, 'lines'),
      axis.title.x=element_blank(),
      axis.title.y=element_text(size=10),
      plot.title = element_text(hjust = 0.5, size = 10)) +
guides(shape = guide_legend(override.aes = list(linetype = 0, size = 2))) # remove bar o top of symbol in legend




modNbFAttacks_NoMaleMaleFight_forplotting <- glmer(NbFAttacks~ -1+ paste(Mcol,GroupName, sep="")  + scale(TotalWatch)
                                                   + (1|FID)
                                                   , data = All_MY_TABLE_Videos_perMale_NoMaleMaleFight
                                                   , family = "poisson" )
summary(modNbFAttacks_NoMaleMaleFight_forplotting)

table_effect_Attack_inter_all <- as.data.frame(cbind(est=exp(summary(modNbFAttacks_NoMaleMaleFight_forplotting)$coeff[,1]),
                                               CIhigh=exp(summary(modNbFAttacks_NoMaleMaleFight_forplotting)$coeff[,1]+summary(modNbFAttacks_NoMaleMaleFight_forplotting)$coeff[,2]*1.96),
                                               CIlow=exp(summary(modNbFAttacks_NoMaleMaleFight_forplotting)$coeff[,1]-summary(modNbFAttacks_NoMaleMaleFight_forplotting)$coeff[,2]*1.96)))
table_effect_Attack_inter_all <- table_effect_Attack_inter_all[-nrow(table_effect_Attack_inter_all),]
table_effect_Attack_inter_all$Mcol <- c("Red","Red", "Black", 'Black')
table_effect_Attack_inter_all$FTrt <- c("Red averse","Red preference", "Red averse", 'Red preference')
rownames(table_effect_Attack_inter_all) <- NULL
table_effect_Attack_inter_all

Attack_inter_all_Fig <-   ggplot(data=table_effect_Attack_inter_all, aes(x=Mcol, y=est,colour=FTrt, shape = FTrt)) + 
scale_y_continuous(name="Number of female attacks", limits=c(0,0.8))+
scale_x_discrete(name = "Male facial color",limits = c("Red","Black") ) +
  labs(title = "Male tests without male-male competition (N = 53 males)") +
theme_classic() + # white backgroun, x and y axis (no box)
geom_errorbar(aes(ymin=CIlow, ymax=CIhigh, col=FTrt), width =0.4,na.rm=TRUE, position = position_dodge(width=0.5))+ # don't plot bor bars on x axis tick, but separate them (dodge)
geom_point(size =4, aes(shape=FTrt, col=FTrt), stroke = 1, position = position_dodge(width=0.5)) +
scale_colour_manual(name= "Female treatment group", values=c("Black","Grey")) +
scale_shape_manual(name= "Female treatment group", values=c(16,17))+ # duplicate title to combine legend
theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
      legend.position="none",
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 10)) +
guides(shape = guide_legend(override.aes = list(linetype = 0, size = 2))) # remove bar o top of symbol in legend


{blank <-ggplot()+
    scale_y_continuous(name="PPP",limits = c(0, 0.8))+
    scale_x_continuous(limits = c(0,10))+
    
    annotate("text", x = 5, y = 0.65, label = "Male facial coloration",  hjust = 0.5, angle=0)+
    theme_classic()+
    
    theme(
      panel.border = element_rect(colour = "white", fill=NA),
      axis.title.y=element_text(size=10, color = "white"),
      axis.title.x = element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_text(color = "white"),
      axis.ticks.x=element_blank(),
      axis.ticks.y=element_blank(),
      axis.line = element_line("white"),
      plot.margin = unit(c(0,0.2,0,0.2), "cm")
    )
}
plotblank <- ggplotGrob(blank)

#setEPS()
#pdf(paste(here(), "5_FiguresReport/SuppFig2b.pdf", sep="/"), height=5, width=5)
grid.arrange (grobs = list(cbind(ggplotGrob(Attack_all_Fig), 
                                 ggplotGrob(Attack_inter_all_Fig), 
                                 size="last"),plotblank), 
              nrow=2, heights=c(19,1))

#dev.off()
}

}
  
#### are black males receiving more attacks from the other male?
{#####Male attack toward other male >>> NbAttacks is attacks received

modNbMAttacks <- glmer(NbMphysicalInter~ Mcol
                      #*Author 
                      + (1|FID)
                      , data = MY_TABLE_Videos_perMale
                      , family = "poisson")
summary(modNbMAttacks)# n=204 signi more attacks toward black males


modNbMAttacks_noIntercept <- glmer(NbMphysicalInter~ -1+ Mcol
                       #*Author 
                       + (1|FID)
                       , data = MY_TABLE_Videos_perMale
                       , family = "poisson")
summary(modNbMAttacks_noIntercept)


table_effect_MAttack <- as.data.frame(cbind(est=exp(summary(modNbMAttacks)$coeff[,1]),
                                            CIhigh=exp(summary(modNbMAttacks)$coeff[,1]+summary(modNbMAttacks)$coeff[,2]*1.96),
                                            CIlow=exp(summary(modNbMAttacks)$coeff[,1]-summary(modNbMAttacks)$coeff[,2]*1.96),
                                            SEhigh = exp(summary(modNbMAttacks)$coeff[,1]+summary(modNbMAttacks)$coeff[,2]),
                                            SElow = exp(summary(modNbMAttacks)$coeff[,1]-summary(modNbMAttacks)$coeff[,2])
                                             ))

table_effect_MAttack$avSE <- (table_effect_MAttack$SEhigh-table_effect_MAttack$SElow)/2
                                                        
table_effect_MAttack$Mcol <- c("Red",'Black')
rownames(table_effect_MAttack) <- NULL
table_effect_MAttack


MAttack_received_Fig <-   ggplot(data=table_effect_MAttack, aes(x=Mcol, y=est)) + 
  scale_y_continuous(name="Number of male attacks received")+
  scale_x_discrete(name = "Male facial color",limits = c("Red","Black") ) +
  theme_classic() + # white backgroun, x and y axis (no box)
  geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), width =0.4,na.rm=TRUE, position = position_dodge(width=0.5))+ # don't plot bor bars on x axis tick, but separate them (dodge)
  geom_point(size =4, stroke = 1, position = position_dodge(width=0.5)) +
   theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
        legend.position='none',
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 10))
 
#setEPS()
#pdf(paste(here(), "5_FiguresReport/SuppFig1.pdf", sep="/"), height=5, width=3.3)
MAttack_received_Fig
#dev.off()



modNbMAttacksValidTests <- glmer(NbMphysicalInter~ Mcol
                      #*Author 
                      + (1|FID)
                      , data = MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$ExcludeYN == 0,]
                      , family = "poisson")
summary(modNbMAttacksValidTests)# n=154 NS

}

### Do F attacks predict futur consumption ? 
{# only tested in valid test, as non valid test, a spider died
head(MY_TABLE_Videos_perMale)
 # MY_TABLE_Videos_perMale$FID[MY_TABLE_Videos_perMale$ConsumYN == 1 & MY_TABLE_Videos_perMale$ExcludeYN == 0]
 # MY_TABLE_Videos_perMale$FID[MY_TABLE_Videos_perMale$ConsumYN == 0 & MY_TABLE_Videos_perMale$ExcludeYN == 0]
  
wilcox.test(
MY_TABLE_Videos_perMale$NbFAttacks[MY_TABLE_Videos_perMale$ConsumYN == 1 & MY_TABLE_Videos_perMale$ExcludeYN == 0],
MY_TABLE_Videos_perMale$NbFAttacks[MY_TABLE_Videos_perMale$ConsumYN == 0 & MY_TABLE_Videos_perMale$ExcludeYN == 0],
paired = TRUE)

mean(MY_TABLE_Videos_perMale$NbFAttacks[MY_TABLE_Videos_perMale$ConsumYN == 1 & MY_TABLE_Videos_perMale$ExcludeYN == 0])
mean(MY_TABLE_Videos_perMale$NbFAttacks[MY_TABLE_Videos_perMale$ConsumYN == 0 & MY_TABLE_Videos_perMale$ExcludeYN == 0])

}

### Do F and M attacks predict futur death of male for reason other than consumption ?
{ ##### both NbF or MAttacks are attacks received

### in subset of trials where male died, did the one that ended up dead received more aggression from the female?
  ### these are poisson distributed data so t.test not quite appropriate....

subsetTrialwhereMaleDied <- MY_TABLE_Videos_perMale[MY_TABLE_Videos_perMale$FID %in% MY_TABLE_Videos_perMale$FID[MY_TABLE_Videos_perMale$Died == 1],]
subsetTrialwhereMaleDied$NbFMAttacks <- subsetTrialwhereMaleDied$NbFAttacks+ subsetTrialwhereMaleDied$NbMphysicalInter


wilcox.test(subsetTrialwhereMaleDied$NbFAttacks[subsetTrialwhereMaleDied$Died == 1],
            subsetTrialwhereMaleDied$NbFAttacks[subsetTrialwhereMaleDied$Died == 0],
            paired = TRUE)

wilcox.test(subsetTrialwhereMaleDied$NbFMAttacks[subsetTrialwhereMaleDied$Died == 1],
            subsetTrialwhereMaleDied$NbFMAttacks[subsetTrialwhereMaleDied$Died == 0],
            paired = TRUE)



}
  
}

## Descriptive
{

{### Nb of videos with male male physical interaction
#### All videos
summary(MY_TABLE_Videos$NbMphysicalInter)
length(MY_TABLE_Videos$NbMphysicalInter[MY_TABLE_Videos$NbMphysicalInter > 0])/
  length(MY_TABLE_Videos$NbMphysicalInter)*100 # 48%
#### valid tests
summary(MY_TABLE_Videos$NbMphysicalInter[MY_TABLE_Videos$ExcludeYN ==0])
length(MY_TABLE_Videos$NbMphysicalInter[MY_TABLE_Videos$NbMphysicalInter > 0 & MY_TABLE_Videos$ExcludeYN ==0])/
  length(MY_TABLE_Videos$NbMphysicalInter[MY_TABLE_Videos$ExcludeYN ==0])*100 # 50.6%
}

{### Nb of males who didn't court
#### All videos
summary(MY_TABLE_Videos_perMale$NBCourt)
length(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$NBCourt == 0])/
  length(MY_TABLE_Videos_perMale$NBCourt)*100 # 12.25%
#### valid tests
summary(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$ExcludeYN ==0])
length(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$NBCourt == 0 & MY_TABLE_Videos_perMale$ExcludeYN ==0])/
  length(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$ExcludeYN ==0])*100 # 13%
}

{### Nb of males who didn't court BUT not because they were eaten (video Done = YES, ConsumYN = NO)
#### were the blue male less likely not to court: No
#### All videos
summary(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$Done == 1 & !MY_TABLE_Videos_perMale$VideoID%in%Cannibalism$VideoID])
length(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$NBCourt == 0 & MY_TABLE_Videos_perMale$Done == 1 & !MY_TABLE_Videos_perMale$VideoID%in%Cannibalism$VideoID])/
  length(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$Done == 1 & !MY_TABLE_Videos_perMale$VideoID%in%Cannibalism$VideoID])*100 # 28.6% (4/14)
MY_TABLE_Videos_perMale$Mcol[MY_TABLE_Videos_perMale$NBCourt == 0 & MY_TABLE_Videos_perMale$Done == 1 & !MY_TABLE_Videos_perMale$VideoID%in%Cannibalism$VideoID] # 2 blues, 2 yellows
#### valid tests
summary(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$ExcludeYN ==0 & MY_TABLE_Videos_perMale$Done == 1 & !MY_TABLE_Videos_perMale$VideoID%in%Cannibalism$VideoID])
length(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$NBCourt == 0 & MY_TABLE_Videos_perMale$ExcludeYN ==0 & MY_TABLE_Videos_perMale$Done == 1 & !MY_TABLE_Videos_perMale$VideoID%in%Cannibalism$VideoID])/
  length(MY_TABLE_Videos_perMale$NBCourt[MY_TABLE_Videos_perMale$ExcludeYN ==0 & MY_TABLE_Videos_perMale$Done == 1 & !MY_TABLE_Videos_perMale$VideoID%in%Cannibalism$VideoID])*100 # 25% (2/8)
MY_TABLE_Videos_perMale$Mcol[MY_TABLE_Videos_perMale$ExcludeYN ==0 & MY_TABLE_Videos_perMale$NBCourt == 0 & MY_TABLE_Videos_perMale$Done == 1 & !MY_TABLE_Videos_perMale$VideoID%in%Cannibalism$VideoID] # 2 yellows
}

{### Nb of videos where female attacked (or at least intended to)
#### All videos
summary(MY_TABLE_Videos$NbIntendedFAttacks)
length(MY_TABLE_Videos$NbIntendedFAttacks[MY_TABLE_Videos$NbIntendedFAttacks > 0])/
  length(MY_TABLE_Videos$NbIntendedFAttacks)*100 # 48.0%
#### valid tests
summary(MY_TABLE_Videos$NbIntendedFAttacks[MY_TABLE_Videos$ExcludeYN ==0])
length(MY_TABLE_Videos$NbIntendedFAttacks[MY_TABLE_Videos$NbIntendedFAttacks > 0 & MY_TABLE_Videos$ExcludeYN ==0])/
  length(MY_TABLE_Videos$NbIntendedFAttacks[MY_TABLE_Videos$ExcludeYN ==0])*100 # 57.1% 
} 

### average time watched
summary(MY_TABLE_Videos$TotalWatch)/60 # in min
summary(MY_TABLE_Videos$TotalWatch[MY_TABLE_Videos$ExcludeYN ==0])/60 # in min
summary(MY_TABLE_Videos$TotalWatch[MY_TABLE_Videos$GroupName == 'RedAverse'])/60
summary(MY_TABLE_Videos$TotalWatch[MY_TABLE_Videos$GroupName == 'RedPreference'])/60
t.test(MY_TABLE_Videos$TotalWatch[MY_TABLE_Videos$GroupName == 'RedAverse'],
       MY_TABLE_Videos$TotalWatch[MY_TABLE_Videos$GroupName == 'RedPreference'])
sd(MY_TABLE_Videos$TotalWatch[MY_TABLE_Videos$GroupName == 'RedAverse']/60)/length(MY_TABLE_Videos$TotalWatch[MY_TABLE_Videos$GroupName == 'RedAverse'])
sd(MY_TABLE_Videos$TotalWatch[MY_TABLE_Videos$GroupName == 'RedPreference']/60)/length(MY_TABLE_Videos$TotalWatch[MY_TABLE_Videos$GroupName == 'RedPreference'])
sum(MY_TABLE_Videos$TotalWatch[MY_TABLE_Videos$GroupName == 'RedAverse'])/60/60
sum(MY_TABLE_Videos$TotalWatch[MY_TABLE_Videos$GroupName == 'RedPreference'])/60/60



### average delay to court
summary(MY_TABLE_Videos_perMale$DelayFirstCourt)/60 # in min

### average duration courting (out of duration watched)
summary(60*(MY_TABLE_Videos_perMale$TotalCourtDur)/(MY_TABLE_Videos_perMale$TotalWatch)) # in min of coursthip per hour

### average duration male male interaction (out of duration watched)
summary(60*(MY_TABLE_Videos$TotalMInterDur)/(MY_TABLE_Videos$TotalWatch)) # in min of interaction per hour

### average delay to male male interaction
summary(MY_TABLE_Videos$DelayFirstMInter)/60 # in min

### Nb of courtships that lead to female attack
summary(Behav_Male_Courtships$FemaleResponsiveness)
length(Behav_Male_Courtships$FemaleResponsiveness[Behav_Male_Courtships$FemaleResponsiveness == 'Attacks'])/nrow(Behav_Male_Courtships)*100 # 10.7%

### Nb of attack
summary(MY_TABLE_Videos$NbFAttacks)
length(MY_TABLE_Videos$NbFAttacks[MY_TABLE_Videos$NbFAttacks>0])/102*100
length(MY_TABLE_Videos$NbMphysicalInter[MY_TABLE_Videos$NbMphysicalInter>0])/102*100


}


head(MY_TABLE_Videos)
head(MY_TABLE_Videos_perMale)



### 20190227
# write.csv(MY_TABLE_Videos, file = paste(here(),"3_ProcessedData/MY_TABLE_Videos.csv", sep="/"), row.names = FALSE)
# write.csv(MY_TABLE_Videos_perMale, file = paste(here(),"3_ProcessedData/MY_TABLE_Videos_perMale.csv", sep="/"), row.names = FALSE)


