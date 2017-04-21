#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Preregistration FlexibleSexualAppetite part 1
#	 Start : 21/04/2017
#	 last modif : 21/04/2017
#	 commit: simulation of data to see whether planned analyses code works
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))

{# packages
library(pbapply)
library(lme4)
library(dplyr)
}

# simulation data

nF <- 100

FID <- 1:nF
Trt <- rep(c("RedPref","RedAverse"),nF/2)
Females_and_Trt <- data.frame(FID,Trt)
Double_Females_and_Trt <- data.frame(rbind(Females_and_Trt,Females_and_Trt))
Double_Females_and_Trt <- Double_Females_and_Trt[order(Double_Females_and_Trt$FID),]
Triple_Females_and_Trt <- data.frame(rbind(Females_and_Trt,Females_and_Trt,Females_and_Trt))
Triple_Females_and_Trt <- Triple_Females_and_Trt[order(Triple_Females_and_Trt$FID),]

AttackBugYN <- sample(c(0,1),nF, replace=TRUE) # step 1
AttackNewRedYN <- sample(c(0,1),nF, replace=TRUE) # step 2
Time_to_attack <- rnorm(nF, mean = 15, sd = 7) # step 2 find value previous experiment!!

Fcondition <-  rnorm(nF*2,mean = 3, sd=1) # find real values !!
before_after <- rep(c("before","after"),nF)

MID <- 1:(nF*2)
Mcolor <- rep(c("Red","Black"),nF)
CannibalizedYN <- unlist(replicate(nF,sample(c(0,1), replace=FALSE),simplify = FALSE)) # step 3
Msize <- rnorm(nF*2,mean = 3, sd=1) # find real values!!
Mcondition <- rnorm(nF*2,mean = 3, sd=1) # find real values!!

Step <- rep (c("step1", "step2", "step3"),nF*3)

  
MY_TABLE_FID <- data.frame(FID, Trt, AttackBugYN,AttackNewRedYN,Time_to_attack)

NY_TABLE_FCondition <- data.frame(Fcondition,before_after,Double_Females_and_Trt)

MY_TABLE_MID <- data.frame( Double_Females_and_Trt,MID, Mcolor, Msize, Mcondition, CannibalizedYN)

MY_TABLE_Step <- data.frame(step = Step,Triple_Females_and_Trt,
                            attackRedYN = c(AttackBugYN, AttackNewRedYN, MY_TABLE_MID$CannibalizedYN[MY_TABLE_MID$Mcolor == "Red"]))


head(MY_TABLE_FID)
head(NY_TABLE_FCondition)
head(MY_TABLE_MID)
head(MY_TABLE_Step)

  
# step 1
mod1 <- glm (AttackBugYN ~ Trt, "binomial", data = MY_TABLE_FID)
summary(mod)


summary(lmer (Fcondition ~ before_after * Trt + (1|FID), data = NY_TABLE_FCondition))




# step 2

mod2 <- glm (AttackNewRedYN ~ Trt, family = "binomial",  data = MY_TABLE_FID)
summary(mod2)


t.test (Time_to_attack[Trt == "RedPref"], Time_to_attack[Trt == "RedAverse"], data = MY_TABLE_FID)




# step 3

mod3 <- glmer (CannibalizedYN ~ Trt + Mcolor+ Msize + Mcondition + (1|FID), family = "binomial", data = MY_TABLE_MID)
summary(mod3)



# exploratory analyses: repeatability of female bias, and step "strenght of effect"

mod4 <- glmer (attackRedYN ~ -1+step*Trt + (1|FID), family = "binomial", data=MY_TABLE_Step)
summary(mod4)




# to calculate odds ratio
## p is the probability of attacking the red prey
## Aversion is the treatment group trained to be red averse
## Preference, the treatment group trained to prefer red preys

pAversion <-  plogis(coef(mod)[1])
pPreference <- plogis(coef(mod)[1]+coef(mod)[2])
odds <- pP/(1-pA) / pA/(1-pP) # how much more the red preference group will attack the red bugs compared to the red averse group.



