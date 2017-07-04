#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Preregistration FlexibleSexualAppetite part 1
#  simulation of data to see whether planned analyses code works
#	 Start : 21/04/2017
#	 last modif : 7/4/2017
#	 commit: add exploratory analyses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list = ls(all = TRUE))

{# packages
library(lme4)
library(rptR)
library(pbapply)
}

                  #Function to check number of significant result by chance
                  #Simulate_and_analyse <-function(){

{# simulation data

nF <- 100

FID <- 1:nF
Trt <- rep(c("RedPref","RedAverse"),nF/2)
AttackBugYN <- sample(c(0,1),nF, replace=TRUE) # step 1
AttackNewRedYN <- sample(c(0,1),nF, replace=TRUE) # step 2
Time_to_attack <- rnorm(nF, mean = 15, sd = 7) # step 2 find value previous experiment!!
Fsize <- sort(rnorm(nF,mean = 4, sd=1))# find real values!!
Fweight <- rnorm(nF,mean = 4, sd=1)# find real values!!
Fcondition <- resid(lm(Fweight~Fsize))
MY_TABLE_FID <- data.frame(FID, Trt, AttackBugYN,AttackNewRedYN,Time_to_attack,Fcondition)


MID <- 1:(nF*2)
Msize <- sort(rnorm(nF*2,mean = 4, sd=1))# find real values!!
Mweight <- rnorm(nF*2,mean = 4, sd=1)# find real values!!
Mcondition <- resid(lm(Mweight~Msize))

MY_TABLE_MID <- data.frame(MID, Msize, Mcondition)

is.even <- function(x) x %% 2 == 0 

MY_TABLE_Mpair <- data.frame(MY_TABLE_MID[is.even(MY_TABLE_MID$MID),],MY_TABLE_MID[!is.even(MY_TABLE_MID$MID),])
colnames(MY_TABLE_Mpair) <- c("MID1","Msize1","Mcondition1","MID2","Msize2", "Mcondition2")
MY_TABLE_Mpair$Mcolor1 <- sample(c("Red","Black"),nF, replace = TRUE)

for (i in 1:nrow(MY_TABLE_Mpair)){
  if(MY_TABLE_Mpair$Mcolor1[i] == "Red"){
    MY_TABLE_Mpair$Mcolor2[i] <- "Black"
    MY_TABLE_Mpair$DeltaMsize[i] <- MY_TABLE_Mpair$Msize1[i]-MY_TABLE_Mpair$Msize2[i]
    MY_TABLE_Mpair$DeltaMcondition[i] <- MY_TABLE_Mpair$Mcondition1[i]-MY_TABLE_Mpair$Mcondition2[i]
  }
  if(MY_TABLE_Mpair$Mcolor1[i] == "Black"){
    MY_TABLE_Mpair$Mcolor2[i] <- "Red"
    MY_TABLE_Mpair$DeltaMsize[i] <- MY_TABLE_Mpair$Msize2[i]-MY_TABLE_Mpair$Msize1[i]
    MY_TABLE_Mpair$DeltaMcondition[i] <- MY_TABLE_Mpair$Mcondition2[i]-MY_TABLE_Mpair$Mcondition1[i]}
}


CannibalizedRedYN <- sample(c(0,1), nF,replace=TRUE) # step 3

MY_TABLE_FID <- data.frame(MY_TABLE_FID,MY_TABLE_Mpair,CannibalizedRedYN)


Males1 <- MY_TABLE_Mpair[,c("MID1","Mcolor1")]
colnames(Males1) <- c("MID","Mcolor")
Males2 <- MY_TABLE_Mpair[,c("MID2","Mcolor2")]
colnames(Males2) <- c("MID","Mcolor")
  
MY_TABLE_MID <- merge(MY_TABLE_MID, rbind(Males1,Males2),all.x=TRUE)
MY_TABLE_MID$Latency_to_court <- rnorm(nF*2, mean = 15, sd = 7)

Females_and_Trt <- data.frame(FID,Trt)
Triple_Females_and_Trt <- data.frame(rbind(Females_and_Trt,Females_and_Trt,Females_and_Trt))
Step <- sort(rep (c("step1", "step2", "step3"),nF))
attackRedYN <- c(AttackBugYN, AttackNewRedYN, CannibalizedRedYN)

MY_TABLE_Step <- data.frame(Step,Triple_Females_and_Trt,attackRedYN)
MY_TABLE_Step <- MY_TABLE_Step[order(MY_TABLE_Step$FID),]
MY_TABLE_Step$rowID <- 1:nrow(MY_TABLE_Step)
}

head(MY_TABLE_FID)
head(MY_TABLE_MID)
head(MY_TABLE_Step)


## Trt Red Averse is the reference (intercept)


# step 1

mod1 <- glm (AttackBugYN ~ Trt + Fcondition, "binomial", data = MY_TABLE_FID)

#par(mfrow=c(2,2))
#plot(mod1)

summary(mod1)

  ## to get one sided test p value
  mod1p <- coef(summary(mod1))[2, 4]/2


# step 2

mod2 <- glm (AttackNewRedYN ~ Trt, family = "binomial",  data = MY_TABLE_FID)

#par(mfrow=c(2,2))
#plot(mod2)

summary(mod2)

  ## to get one sided test p value
 mod2p <- coef(summary(mod2))[2, 4]/2


  ## to check equality of motivation to feed
  shapiro.test(MY_TABLE_FID$Time_to_attack)
  t.test (Time_to_attack[Trt == "RedPref"], 
          Time_to_attack[Trt == "RedAverse"], 
          data = MY_TABLE_FID)




# step 3

mod3 <- glm (CannibalizedRedYN ~ Trt+ DeltaMsize + DeltaMcondition, family = "binomial", data = MY_TABLE_FID)

#par(mfrow=c(2,2))
#plot(mod3)

summary(mod3)

  ## to get one sided test p value
 mod3p <-  coef(summary(mod3))[2, 4]/2




  ## to check equality of male motivation to court
  shapiro.test(MY_TABLE_MID$Latency_to_court)
  t.test (MY_TABLE_MID$Latency_to_court[MY_TABLE_MID$Mcolor == "Red"], 
          MY_TABLE_MID$Latency_to_court[MY_TABLE_MID$Mcolor == "Black"])


  
                # return(list(mod1p, mod2p,mod3p))  
                #}  
                
                #OutputSimulation <- pbreplicate(1000,Simulate_and_analyse())
                #OutputSimulation <- OutputSimulation<0.05
                #rowSums(OutputSimulation)/1000
 
  
# exploratory analyses: repeatability of female bias

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



# to calculate odds ratios of eating the red prey for the Red Preference group relative to the red averse group

## step 1 
table(MY_TABLE_FID$Trt, MY_TABLE_FID$AttackBugYN)
exp(cbind(OR=coef(mod1), confint(mod1)))[2,] 

## step 2
table(MY_TABLE_FID$Trt, MY_TABLE_FID$AttackNewRedYN)
exp(cbind(OR=coef(mod2), confint(mod2)))[2,]

## step 3: odds ratio ?
exp(cbind(OR=coef(mod3), confint(mod3)))[2,]  



# Testing each group to 50/50

##step 2
chisq.test(table(MY_TABLE_FID$AttackNewRedYN[MY_TABLE_FID$Trt == 'RedPref']), p=c(0.5,0.5))
chisq.test(table(MY_TABLE_FID$AttackNewRedYN[MY_TABLE_FID$Trt == 'RedAverse']), p=c(0.5,0.5))

##step 3
chisq.test(table(MY_TABLE_FID$CannibalizedRedYN[MY_TABLE_FID$Trt == 'RedPref']), p=c(0.5,0.5))
chisq.test(table(MY_TABLE_FID$CannibalizedRedYN[MY_TABLE_FID$Trt == 'RedAverse']), p=c(0.5,0.5))




