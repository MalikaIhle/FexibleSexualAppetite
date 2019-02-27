#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 check body condition
#	 Start : 24/07/2017
#	 last modif : 24/07/2017
#	 commit: first commit
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list = ls(all = TRUE))

library(RODBC)


#conDB= odbcConnectAccess2007("C:\\Users\\Malika\\Dropbox\\HabronatusPyrrithrix\\HabronatusPyrrithrix_DB.accdb")
conDB= odbcConnectAccess2007("C:\\Users\\malika.ihle\\Dropbox\\HabronatusPyrrithrix\\HabronatusPyrrithrix_DB.accdb")

Female_Measurements <- sqlQuery(conDB, "
SELECT Basic_Trials.Ind_ID, 
Basic_Trials.Sex, 
Basic_Trials.GroupName,
Morph_Measurements.Occasion, 
Morph_Measurements.Date, 
Morph_Measurements.Time, 
Morph_Measurements.Mass, 
Morph_Measurements.CarapaceWidth, 
Morph_Measurements.Remarks
FROM Basic_Trials 
INNER JOIN 
Morph_Measurements 
ON Basic_Trials.Ind_ID = Morph_Measurements.Ind_ID
WHERE (((Basic_Trials.Sex)=0));
")


bodycondition <- merge(
subset(Female_Measurements[!is.na(Female_Measurements$CarapaceWidth),], select = c("Ind_ID","CarapaceWidth","GroupName")),
subset(Female_Measurements[!is.na(Female_Measurements$Mass),], select = c("Ind_ID","Mass","GroupName")))

bodycondition$res <- residuals(lm(bodycondition$Mass~bodycondition$CarapaceWidth))


t.test(bodycondition$res[bodycondition$GroupName == "RedAverse"],bodycondition$res[bodycondition$GroupName == "RedPreference"])



