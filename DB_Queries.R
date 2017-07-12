#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Qury for scheduling experiment
#	 Start : 21/06/2017
#	 last modif : 23/06/2017
#	 commit: Daily summary
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))

library(tidyr)
library(dplyr) 
library(RODBC)
library(xlsx)

#conDB= odbcConnectAccess2007("C:\\Users\\Malika\\Dropbox\\HabronatusPyrrithrix\\HabronatusPyrrithrix_DB.accdb")
conDB= odbcConnectAccess2007("C:\\Users\\malika.ihle\\Dropbox\\HabronatusPyrrithrix\\HabronatusPyrrithrix_DB.accdb")

ScheduleDiet <- sqlQuery(conDB, "
SELECT Basic_Trials.Ind_ID, 
Basic_Trials.GroupName, 
Basic_Trials.PeriodBeginDate AS Start, 
DateAdd('d',2,[Basic_Trials].[PeriodBeginDate]) AS Feed2, 
DateAdd('d',4,[Basic_Trials].[PeriodBeginDate]) AS Feed3, 
DateAdd('d',6,[Basic_Trials].[PeriodBeginDate]) AS Feed4, 
DateAdd('d',8,[Basic_Trials].[PeriodBeginDate]) AS Feed5, 
DateAdd('d',10,[Basic_Trials].[PeriodBeginDate]) AS Feed6, 
DateAdd('d',11,[Basic_Trials].[PeriodBeginDate]) AS rmvPrey, 
DateAdd('d',12,[Basic_Trials].[PeriodBeginDate]) AS BugTest, 
DateAdd('d',14,[Basic_Trials].[PeriodBeginDate]) AS Feed8, 
DateAdd('d',16,[Basic_Trials].[PeriodBeginDate]) AS Feed9, 
DateAdd('d',18,[Basic_Trials].[PeriodBeginDate]) AS Feed10, 
DateAdd('d',20,[Basic_Trials].[PeriodBeginDate]) AS Feed11, 
DateAdd('d',22,[Basic_Trials].[PeriodBeginDate]) AS Feed12, 
DateAdd('d',24,[Basic_Trials].[PeriodBeginDate]) AS rmvPreyPaintTermite, 
DateAdd('d',25,[Basic_Trials].[PeriodBeginDate]) AS TermiteTest, 
DateAdd('d',26,[Basic_Trials].[PeriodBeginDate]) AS Feed14, 
DateAdd('d',28,[Basic_Trials].[PeriodBeginDate]) AS Feed15, 
DateAdd('d',30,[Basic_Trials].[PeriodBeginDate]) AS Feed16, 
DateAdd('d',32,[Basic_Trials].[PeriodBeginDate]) AS Feed17, 
DateAdd('d',34,[Basic_Trials].[PeriodBeginDate]) AS Feed18, 
DateAdd('d',35,[Basic_Trials].[PeriodBeginDate]) AS rmvPreyPaintMales, 
DateAdd('d',36,[Basic_Trials].[PeriodBeginDate]) AS MaleTest
FROM Basic_Trials 
INNER JOIN Basic_Individuals 
ON Basic_Trials.Ind_ID = Basic_Individuals.Ind_ID
")

DailySchedule <- gather(ScheduleDiet, "Event", "EventDate", 3:24 )
DailySchedule <- arrange(DailySchedule,as.POSIXct(EventDate),GroupName, Ind_ID)

for (i in 1:nrow(DailySchedule)){
  if (DailySchedule$Event[i] == "Start" | 
      grepl("Feed",DailySchedule$Event[i]) == TRUE)
        {DailySchedule$GrossEvent[i]<-"Feed"}
  if (grepl("rmv",DailySchedule$Event[i]) == TRUE)  
    {DailySchedule$GrossEvent[i]<-"PrepareTest"}
  if (grepl("Test",DailySchedule$Event[i]) == TRUE)  
  {DailySchedule$GrossEvent[i]<-DailySchedule$Event[i]}
}

DailySchedule$EventDate <- as.character(DailySchedule$EventDate)


write.xlsx(DailySchedule,"C:\\Users\\malika.ihle\\Dropbox\\HabronatusPyrrithrix\\R_DailySchedule.xlsx", row.names = FALSE) 


bb <- DailySchedule[,c("EventDate","GrossEvent")]

DailyScheduleSummary <- as.data.frame(summarise(group_by(bb, EventDate= as.character(EventDate)),
                                        Feed = sum(GrossEvent == "Feed"),
                                        Prepare = sum(GrossEvent == "PrepareTest"),
                                        BugTest = sum(GrossEvent == "BugTest"),
                                        TermiteTest = sum(GrossEvent == "TermiteTest"),
                                        MaleTest = sum(GrossEvent == "MaleTest")))

write.xlsx(DailyScheduleSummary,"C:\\Users\\malika.ihle\\Dropbox\\HabronatusPyrrithrix\\R_DailyScheduleSummary.xlsx", row.names = FALSE) 

