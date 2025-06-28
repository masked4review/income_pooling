#Syntax File 01
  #Combine NLSY79 and HRS data (stack)
  #Perform simple imputations to recover missing data
  #Prepare income volatility exposure
  #Examine strength of univariable association (via R2) between matching variables and income volatility exposures
  #Descriptive Table 1 (distribution of matching variables in each cohort pre-matching)
  #Prepare and save final complete case stacked dataset for matching

############################################################################################################
#Open libraries
library(tidyverse)
library(ggplot2)
library(haven)
library(tidyr)
library(nnet)
library(nlme)
library(lme4)
library(optimx)
library(ggeffects)
library(cowplot)
library(table1)
library(boot)
library(nptest)
library(simpleboot)
library(dplyr)
library(hrbrthemes)
############################################################################################################

###1.Create combined dataset
#1a. upload datasets from Step 1 (cleaning)
#1b. combine based on harmonized variables then merge in variables unique to each dataset 
#1c. restrict combined samples based on cognition in 2010
#1d. examine/compare distributions of matching variables in both samples 

HRS.import<-read.csv("/Users/xxxxx/code/For publication/Data/HRS_short_cogfxn2010_6200.csv")[,-1] #memscore variables upload funny (x1996, x1998) but can fix that later
NLSY.import<-read.csv("/Users/xxxxx/code/For publication/Data/NLSY_data_final.csv")[,-1]

HRS<-HRS.import
NLSY<-NLSY.import

#examine age/birth year overlap - makes sense it will be narrower range because of how midlife cognitive assessments conducted in respective cohorts (assessed once at the survey between 2010 and 2016 at which participant turned 48+ in NLSY vs age 50+ in 2010 in HRS (or age 50+ at earlier/later survey if missing in 2010))
#not too worried about not having exact age overlap - close enough/members of similar birth cohort
table(NLSY$birthyear, NLSY$iwstatus_2010)
table(NLSY$age_2010, NLSY$iwstatus_2010)

#create combined ID variable
HRS$id<-HRS$hhidpn
NLSY$id<-NLSY$NLSY_CASE_ID

#list of harmonized variables - these are named/coded the same in HRS and NLSY79
match.vars<-c("id","female","race","eth","usbirth","pedu_cat","born.child.south","age1stmarried.cat",
              "age_2010","birthyear","adj10.hh.inc.10","poverty_2010","currentregion_2010","wealth_2010","LBRF_2010",
              "occuskill_2010","epinsurance_2010","weight_2010","height_2010","BMI_2010","marriage_2010","immedrecall","delrecall",
              "avg.memory","sum.memory","selfratemem","edu_yrs","eversmoke_2010","currsmoke_2010","drinksalc_2010",
              "everpsyche","cesd","everdiab","everhibp","everCVD","countCVD","srh","vigex.2010","lmodex.2010")

#restrict each dataset to the matching variables first
HRS.match<-HRS[,match.vars]
NLSY.match<-NLSY[,match.vars]
  
  #combine datasets - stack them
  match.vars.data<-rbind(NLSY.match,HRS.match)

#merge in exposure/outcome variables unique to each cohort (i.e., variables not in the list below) - if did not do prior step things would get wonky in combining process; now when merge in to combined dataset variables that do not exist in one dataset simply get "NA"
match.vars.noid<-c("female","race","eth","usbirth","pedu_cat","born.child.south","age1stmarried.cat",
              "age_2010","birthyear","adj10.hh.inc.10","poverty_2010","currentregion_2010","wealth_2010","LBRF_2010",
              "occuskill_2010","epinsurance_2010","weight_2010","height_2010","BMI_2010","marriage_2010","immedrecall","delrecall",
              "avg.memory","sum.memory","selfratemem","edu_yrs","eversmoke_2010","currsmoke_2010","drinksalc_2010",
              "everpsyche","cesd","everdiab","everhibp","everCVD","countCVD","srh","vigex.2010","lmodex.2010")

HRS.unique=HRS %>% select_if(!names(.) %in% match.vars.noid) #HRS dataset that contains variables unique to HRS - those not listed above
NLSY.unique=NLSY %>% select_if(!names(.) %in% match.vars.noid) #NLSY79 dataset that contains variables unique to NLSY79 - those not listed above

match.vars.data<-left_join(match.vars.data,NLSY.unique,by="id")#merge in unique NLSY variables (HRS will get NA)
match.vars.data<-left_join(match.vars.data,HRS.unique,by="id") #merge in unique HRS variables (NLSY participants will get NA)

#now combined dataset has matching variable information for both datasets at matching time point of 2010 plus additional unique exposure/outcome variables in each cohort
match.vars.data$cohort<-ifelse(!is.na(match.vars.data$hhidpn),"HRS","NLSY")
  table(match.vars.data$cohort)

#----------------------------------------------------------------------------------------------------------------------------------
#2. Restrict to those in the sample at the matching time point (with a 2020 cognitive outcome)
#restrict to cognition in 2010-2016 in NLSY79

table(match.vars.data$CogTestYear, useNA="ifany") #who did cog exam in 2010 or later
  #4410 did cog 2010-2016 (2112+1834+353+111)
table(match.vars.data$cohort,match.vars.data$r10iwstat,exclude=NULL) 
  #all HRS participants (makes sense) - 6200
table(match.vars.data$cohort,match.vars.data$iwstatus_2010,exclude=NULL) 
  #only some NLSY79 participants (also makes sense) - 7565

match.vars.data$instudy2010<-ifelse(match.vars.data$r10iwstat==1|match.vars.data$iwstatus_2010==1,1,0)
  table(match.vars.data$instudy2010,exclude=NULL)
  table(match.vars.data$birthyear, match.vars.data$cohort)

match.vars.data$incog2010<-ifelse(match.vars.data$CogTestYear>2008|!is.na(match.vars.data$hhidpn),1,NA)
  table(match.vars.data$incog2010, match.vars.data$cohort, exclude=NULL)
  table(match.vars.data$birthyear, match.vars.data$cohort)

table(match.vars.data$instudy2010, match.vars.data$incog2010, match.vars.data$cohort, useNA="ifany") 

#restrict to NLSY participants still in the study in 2010 & did cog test in 2010 (just restrict on CogTestYear then)
sample10<-match.vars.data[!is.na(match.vars.data$incog2010),] 
  table(sample10$cohort,exclude=NULL)
  table(sample10$birthyear, sample10$cohort, exclude=NULL)

#10610 participants (6200 HRS, 4410 NLSY)
 
#----------------------------------------------------------------------------------------------------------------------------------
#3. Clean variables in combined dataset, where applicable (labels, factoring, levels, etc.) for Table 1

#also look at overlap in combined race/ethnicity variable
sample10$raceth<-ifelse(sample10$race=="Black"&sample10$eth=="Non-hispanic",1, #Non-Hispanic, Black
                        ifelse(sample10$race=="White"&sample10$eth=="Non-hispanic",0, #Non-Hispanic, non-Black
                               ifelse(sample10$race=="Other"&sample10$eth=="Non-hispanic",0, #Non-Hispanic, non-Black
                                      ifelse(sample10$eth=="Hispanic",2,0))))#2=Hispanic, 
table(sample10$raceth, sample10$cohort, exclude=NULL)
  table(sample10$race, sample10$cohort, exclude=NULL)
  table(sample10$eth, sample10$cohort, exclude=NULL)
sample10$eth[sample10$eth=="Non-hispanic"]<-"Non-Hispanic"

sample10 <- sample10 %>% 
  mutate(
    raceth = factor(raceth, levels = c(0,1,2), labels = c("Non-Hispanic, Non-Black", "Non-Hispanic, Black", "Hispanic")),
    born.child.south = factor(born.child.south, levels = c(0, 1), labels = c("No", "Yes")),
    poverty_2010 = factor(poverty_2010, levels = c(0, 1), labels = c("No", "Yes")),
    epinsurance_2010 = factor(epinsurance_2010, levels = c(0, 1), labels = c("No", "Yes")),
    eversmoke_2010 = factor(eversmoke_2010, levels = c(0, 1), labels = c("No", "Yes")),
    currsmoke_2010 = factor(currsmoke_2010, levels = c(0, 1), labels = c("No", "Yes")),
    drinksalc_2010 = factor(drinksalc_2010, levels = c(0, 1), labels = c("No", "Yes")),
    everpsyche = factor(everpsyche, levels = c(0, 1), labels = c("No", "Yes")),
    everdiab = factor(everdiab, levels = c(0, 1), labels = c("No", "Yes")),
    everhibp = factor(everhibp, levels = c(0, 1), labels = c("No", "Yes")),
    everCVD = factor(everCVD, levels = c(0, 1), labels = c("No", "Yes")),
    vigex.2010 = factor(vigex.2010, levels = c(1,2,3,4,5), labels = c(">= everyday", "> 1 per week","1 per week","1-3 per month","unable to do/never")),
    lmodex.2010 = factor(lmodex.2010, levels = c(1,2,3,4,5), labels = c(">= everyday", "> 1 per week","1 per week","1-3 per month","unable to do/never")),
    currentregion_2010 = factor(currentregion_2010, levels = c(1,2,3,4), labels = c("Northeast", "Midwest", "South", "West")))

#look at overlap for continuous variables
sample10$cohort10<-ifelse(sample10$cohort=="HRS",1,0)
sample10 %>%
  ggplot(aes(x=as.numeric(BMI_2010), fill=cohort)) +
         geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
         scale_fill_manual(values=c("#69b3a2", "#404080")) +
         theme_ipsum() +
         labs(fill="")

summary(sample10$age_2010[sample10$cohort=="HRS"])
sample10$age50<-ifelse(sample10$age_2010<52,0,1)
  table(sample10$age50,sample10$cohort,exclude=NULL)
  
#----------------------------------------------------------------------------------------------------------------------------------
###4.Dealing with missing data for cognition (a key matching variable - carry forward/backward where possible in HRS)
  
#import mbb dataset created in syntax file 00 to pull outcome variables (or other variables) from prior time points if needed (in HRS)
  mbb<-read.csv("/Users/xxxxx/code/For publication/Data/mbb_cohort.csv")
#import raw test scores (vs. composite) on immediate and delayed word recall from HRS biennial CORE fat files (available: https://hrsdata.isr.umich.edu/data-products/rand)
  fat.cog<-read.csv("/Users/xxxxx/code/For publication/Data/fat.cog_data.csv")[-1] #excel file already combining tests across surveys
  
table(sample10$immedrecall, sample10$cohort, exclude=NULL)
table(sample10$delrecall, sample10$cohort, exclude=NULL)
table(sample10$avg.memory, sample10$cohort, exclude=NULL)
table(sample10$sum.memory, sample10$cohort, exclude=NULL) #has more missing data because for average, can contribute with just one of the two assessments, but for sum you need both

#first, for HRS carry forward from 2006 or 2008 time points
mbb$avg.memory.06<-rowMeans(mbb[,c("r8imrc","r8dlrc")], na.rm=TRUE)
mbb$avg.memory.08<-rowMeans(mbb[,c("r9imrc","r9dlrc")], na.rm=TRUE)
mbb$sum.memory.06<-rowSums(mbb[,c("r8imrc","r8dlrc")])
mbb$sum.memory.08<-rowSums(mbb[,c("r9imrc","r9dlrc")])

  summary(mbb$avg.memory.06)
  summary(mbb$avg.memory.08)
  summary(mbb$sum.memory.06)
  summary(mbb$sum.memory.08)
  
sample10$immedrecall.tm.i<-ifelse(is.na(sample10$immedrecall)&sample10$cohort=="HRS",mbb$r9imrc,
                                   ifelse(is.na(sample10$immedrecall)&sample10$cohort=="HRS",mbb$r8imrc,sample10$immedrecall))
  table(sample10$immedrecall.tm.i, sample10$cohort, exclude=NULL)

sample10$delrecall.tm.i<-ifelse(is.na(sample10$delrecall)&sample10$cohort=="HRS",mbb$r9dlrc,
                                  ifelse(is.na(sample10$delrecall)&sample10$cohort=="HRS",mbb$r8dlrc,sample10$delrecall))
  table(sample10$delrecall.tm.i, sample10$cohort, exclude=NULL)
  
sample10$avg.memory.tm.i<-ifelse(is.na(sample10$avg.memory)&sample10$cohort=="HRS",mbb$avg.memory.08,
                                   ifelse(is.na(sample10$avg.memory)&sample10$cohort=="HRS",mbb$avg.memory.06,sample10$avg.memory))
  table(sample10$avg.memory.tm.i, sample10$cohort, exclude=NULL)

sample10$sum.memory.tm.i<-ifelse(is.na(sample10$sum.memory)&sample10$cohort=="HRS",mbb$sum.memory.08,
                                   ifelse(is.na(sample10$sum.memory)&sample10$cohort=="HRS",mbb$sum.memory.06,sample10$sum.memory))
  table(sample10$sum.memory.tm.i, sample10$cohort, exclude=NULL)
  
#for consistency with prior anlaysis, just remove anyone without immediate AND delayed recall
  sample10.cog<-sample10[!is.na(sample10$immedrecall.tm.i)&!is.na(sample10$delrecall.tm.i),]  
  #N=10411 (199 removed)

table(sample10$cohort, sample10$sum.memory.tm.i, exclude=NULL)
table(sample10.cog$immedrecall.tm.i, sample10.cog$cohort, exclude=NULL)  
table(sample10.cog$delrecall.tm.i, sample10.cog$cohort, exclude=NULL)  
table(sample10.cog$avg.memory.tm.i, sample10.cog$cohort, exclude=NULL)  
table(sample10.cog$sum.memory.tm.i, sample10.cog$cohort, exclude=NULL)

#merge in the memory variables from the fat file
sample10.cog<-left_join(sample10.cog, fat.cog, by="hhidpn")

sample10.cog$sum.rawmem10<-rowSums(sample10.cog[,c("ir10","dr10")])
sample10.cog$sum.rawmem12<-rowSums(sample10.cog[,c("ir12","dr12")])
sample10.cog$sum.rawmem14<-rowSums(sample10.cog[,c("ir14","dr14")])
sample10.cog$sum.rawmem16<-rowSums(sample10.cog[,c("ir16","dr16")])
sample10.cog$sum.rawmem18<-rowSums(sample10.cog[,c("ir18","dr18")])
sample10.cog$sum.rawmem20<-rowSums(sample10.cog[,c("ir20","dr20")])

summary(sample10.cog$sum.rawmem10[sample10.cog$cohort=="HRS"]) #from CORE files
summary(sample10.cog$sum.memory.tm.i[sample10.cog$cohort=="HRS"]) #from RAND file

#now remove anyone missing a composite memory score outcome between 2010 and 2020 (will be slightly lower than those with 2010 memory because 2010 memory score comes from longitudinal file which includes imputations)
sample10.cog$atleast1cog<-ifelse(is.na(sample10.cog$memimp10)&is.na(sample10.cog$memimp12)&is.na(sample10.cog$memimp14)&is.na(sample10.cog$memimp16)&is.na(sample10.cog$memimp18)&is.na(sample10.cog$memimp20),0,1)
  table(sample10.cog$atleast1cog, exclude=NULL)
sample10.cog<-sample10.cog[!(sample10.cog$cohort=="HRS"&sample10.cog$atleast1cog==0),]
  table(sample10.cog$cohort, exclude=NULL)

#9272 observations

#----------------------------------------------------------------------------------------------------------------------------------
###5. prepare exposure (income volatility) prior to matching 

#5a. do this prior to matching so that in creating trajectories don't mess up having same # matches
#5b. do this prior to cleaning because this process will carry income forward, so then don't have to do that in later cleaning/missing data step

sample10.cog.inc<-sample10.cog
incomes<-grep("adj90.hh.inc", names(sample10.cog.inc), value = TRUE)

#a) recode invalid/missing/non-interview values to NA
sample10.cog.inc[,incomes] <- lapply(sample10.cog.inc[,incomes], function(x) ifelse(x<0, NA, x))
summary(sample10.cog.inc$adj90.hh.inc.90)
summary(sample10.cog.inc$adj90.hh.inc.10)

#b) restrict to people with 3 income time points
incomes
incomeind<-c("i.inc79","i.inc80","i.inc81","i.inc82","i.inc83",
             "i.inc84","i.inc85","i.inc86","i.inc87","i.inc88",
             "i.inc89","i.inc90","i.inc91","i.inc92","i.inc93",
             "i.inc94","i.inc96","i.inc98","i.inc00","i.inc02",
             "i.inc04","i.inc06","i.inc08","i.inc10","i.inc12",
             "i.inc14","i.inc16","i.inc18")

sample10.cog.inc[,incomeind] <- lapply(sample10.cog.inc[,incomes], function(x) ifelse(x>=0, 1, NA))
sample10.cog.inc[,incomeind] <- lapply(sample10.cog.inc[,incomeind], function(x) ifelse(is.na(x), 0, x))
summary(sample10.cog.inc$i.inc90)
summary(sample10.cog.inc$adj90.hh.inc.90)

inc90to10<-incomeind[12:24]
incomes90to10<-incomes[12:24]

#sum number of time points with 1 (non-missing income information)
sample10.cog.inc$sumincomes<-rowSums(sample10.cog.inc[,incomeind], na.rm=TRUE) #all time points
sample10.cog.inc$suminc90to10<-rowSums(sample10.cog.inc[,inc90to10], na.rm=TRUE) #restricted to 1990-2010 (exposure time frame for project)

table(sample10.cog.inc$sumincomes, exclude=NULL)
table(sample10.cog.inc$suminc90to10, exclude=NULL)

sample10.cog.inc$sumincomes[sample10.cog.inc$cohort=="HRS"]<-NA
sample10.cog.inc$suminc90to10[sample10.cog.inc$cohort=="HRS"]<-NA

#now restrict to people who have at least 3 time points with income information from 1990-2010 (as in NLSY79 replication paper)
sample10.cog.inc$inc3times<-ifelse(sample10.cog.inc$suminc90to10>=3,1,0)
  table(sample10.cog.inc$suminc90to10, exclude=NULL)
  table(sample10.cog.inc$inc3times, exclude=NULL) 

#5c) restrict to people with non-missing baseline (1990) income

#create random value/indicator to later delete so HRS people are not dropped here
sample10.cog.inc$adj90.hh.inc.90<-ifelse(sample10.cog.inc$cohort=="HRS",0123456789,sample10.cog.inc$adj90.hh.inc.90)

#now restrict to people with non-missing baseline (1990) income
sample10.cog.Binc<-sample10.cog.inc[!is.na(sample10.cog.inc$adj90.hh.inc.90),] 
  #8344 participants

#again, create indicator to later delete so HRS people are not dropped here
sample10.cog.Binc$inc3times<-ifelse(sample10.cog.Binc$cohort=="HRS",0123456789,sample10.cog.Binc$inc3times)

#then restrict to people with 3+ time points over exposure period
sample10.cog.3inc<-sample10.cog.Binc[sample10.cog.Binc$inc3times==1|sample10.cog.Binc$inc3times==0123456789,] 
  #8326 participants
  table(sample10.cog.3inc$cohort, exclude=NULL)

#now recode the indicator values back to NA for HRS people
sample10.cog.3inc$inc3times[sample10.cog.3inc$inc3times==0123456789]<-NA
  table(sample10.cog.3inc$inc3times,exclude=NULL)
sample10.cog.3inc$adj90.hh.inc.90[sample10.cog.3inc$adj90.hh.inc.90==0123456789]<-NA
  summary(sample10.cog.3inc$adj90.hh.inc.90,exclude=NULL)

#5d) now create dataset for income volatility continuous measures (one approach will just carry forward income from prior time points, other approach will just ignore time interval between reported income values)

#(i.)first carry forward last observation to fill in missing adjusted/deflated income data (just to avoid NAs)
#write.csv(sample10.cog.3inc[,c("hhidpn","NLSY_CASE_ID",incomes90to10)],"/Users/xxxxx/code/For publication/Data/missing_inc90to10_patterns.csv")
#save file with above code
#open in excel and then: 1) delete first column, 2) change "NA" in the INCOME rows (not in the hhidpn or NLSY_CASE_ID columns) to blank cells
#select all and then click "find & select" (top right corner of toolbar) and then "Go to Special" and then "Blanks" and then select "okay"
#now right click to delete the blank cells and when prompted allow excel to "shift cells left"
#now all of the individual exposure trajectories are sequential with no missing time points in between
#this means a standard deviation of % change variable can be created for each person without having to do imputation over time (e.g., if carry forward, can make the sd income volatility variable look conservative/less volatile than in truth because it adds time points of no change in between actual time points of observed income)
#number of income drops over time will be the same regardless of approach
#rename column headings to time1-time13 and use these when making income volatility variables using this approach  

#import cleaned income file back into R
fixmissinc90to10<-read.csv("/Users/xxxxx/code/For publication/Data/missing_inc90to10_patterns_fixed.csv")  

#merge these fixed income variables back into the original dataset (they are now renamed time1-time13 so won't mess up just merge by both cohort ID variables)
sample10.cog.3inc<-left_join(sample10.cog.3inc,fixmissinc90to10,by=c("hhidpn","NLSY_CASE_ID"))

#also create variables that do carry forward imputation (as alternative approach to the approach above, like in NLSY paper)
incimpute<-sample10.cog.3inc %>%
  mutate(
    adj90.hh.inc.91i = ifelse(is.na(adj90.hh.inc.91), adj90.hh.inc.90, adj90.hh.inc.91),
    adj90.hh.inc.92i = ifelse(is.na(adj90.hh.inc.92), adj90.hh.inc.91i, adj90.hh.inc.92),
    adj90.hh.inc.93i = ifelse(is.na(adj90.hh.inc.93), adj90.hh.inc.92i, adj90.hh.inc.93),
    adj90.hh.inc.94i = ifelse(is.na(adj90.hh.inc.94), adj90.hh.inc.93i, adj90.hh.inc.94),
    adj90.hh.inc.96i = ifelse(is.na(adj90.hh.inc.96), adj90.hh.inc.94i, adj90.hh.inc.96),
    adj90.hh.inc.98i = ifelse(is.na(adj90.hh.inc.98), adj90.hh.inc.96i, adj90.hh.inc.98),
    adj90.hh.inc.00i = ifelse(is.na(adj90.hh.inc.00), adj90.hh.inc.98i, adj90.hh.inc.00),
    adj90.hh.inc.02i = ifelse(is.na(adj90.hh.inc.02), adj90.hh.inc.00i, adj90.hh.inc.02),
    adj90.hh.inc.04i = ifelse(is.na(adj90.hh.inc.04), adj90.hh.inc.02i, adj90.hh.inc.04),
    adj90.hh.inc.06i = ifelse(is.na(adj90.hh.inc.06), adj90.hh.inc.04i, adj90.hh.inc.06),
    adj90.hh.inc.08i = ifelse(is.na(adj90.hh.inc.08), adj90.hh.inc.06i, adj90.hh.inc.08),
    adj90.hh.inc.10i = ifelse(is.na(adj90.hh.inc.10), adj90.hh.inc.08i, adj90.hh.inc.10)
  )

summary(sample10.cog.3inc$adj90.hh.inc.10)
summary(incimpute$adj90.hh.inc.10)
summary(incimpute$adj90.hh.inc.10i) #the missing variables now are just for the 5750 HRS participants 

#zeros are true zeros...so, how many zeros? 
table(sample10.cog.3inc$adj90.hh.inc.90[sample10.cog.3inc$adj90.hh.inc.90==0]) #only 55 in 1990 have income of 0

#do x+1 conversion for all income variables? run into issues when people have consecutive 0s (denominator becomes undefined in formula) - very few have this issue...probably okay to add a really small constant to all income variables to get around this
#for variables where missing time points were removed and everything was shifted left
incTime<-grep("time", names(incimpute), value = TRUE)[-1] 
incimpute[,incTime] <- lapply(incimpute[,incTime], function(x) x=x+0.0001) 
table(sample10.cog.3inc$adj90.hh.inc.90[sample10.cog.3inc$adj90.hh.inc.90==0]) #55
table(incimpute$time1[incimpute$time1==0]) #no more zeros

#for variables where missing time points were imputed via carry forward approach
eqnewinc1990i<-c("adj90.hh.inc.90","adj90.hh.inc.91i", "adj90.hh.inc.92i", "adj90.hh.inc.93i", "adj90.hh.inc.94i", 
                 "adj90.hh.inc.96i", "adj90.hh.inc.98i", "adj90.hh.inc.00i", "adj90.hh.inc.02i", "adj90.hh.inc.04i",
                 "adj90.hh.inc.06i", "adj90.hh.inc.08i", "adj90.hh.inc.10i")
incimpute[,eqnewinc1990i] <- lapply(incimpute[,eqnewinc1990i], function(x) x=x+0.0001)
head(sample10.cog.3inc$adj90.hh.inc.10)
head(incimpute$adj90.hh.inc.10)
table(sample10.cog.3inc$adj90.hh.inc.90[sample10.cog.3inc$adj90.hh.inc.90==0]) #55, as above
table(incimpute$adj90.hh.inc.90[incimpute$adj90.hh.inc.90==0]) #no more zeros

#(ii). Create income volatility measure #1: SD of percent change in income (adjusted/deflated income)

#Step 1: calculate the percentage change in deflated income between 2 consecutive surveys
#((Y_t2-Y_t1)/(0.5*(Y_t1+Y_t2)))*100

#no imputation but time points squeezed together (adjust for # time points provided in analyses)
incimpute$inc.pct9190 <- ((incimpute$time2-incimpute$time1)/(0.5*(incimpute$time2+incimpute$time1)))*100 
incimpute$inc.pct9291 <- ((incimpute$time3-incimpute$time2)/(0.5*(incimpute$time3+incimpute$time2)))*100
incimpute$inc.pct9392 <- ((incimpute$time4-incimpute$time3)/(0.5*(incimpute$time4+incimpute$time3)))*100
incimpute$inc.pct9493 <- ((incimpute$time5-incimpute$time4)/(0.5*(incimpute$time5+incimpute$time4)))*100
incimpute$inc.pct9694 <- ((incimpute$time6-incimpute$time5)/(0.5*(incimpute$time6+incimpute$time5)))*100
incimpute$inc.pct9896 <- ((incimpute$time7-incimpute$time6)/(0.5*(incimpute$time7+incimpute$time6)))*100
incimpute$inc.pct0098 <- ((incimpute$time8-incimpute$time7)/(0.5*(incimpute$time8+incimpute$time7)))*100
incimpute$inc.pct0200 <- ((incimpute$time9-incimpute$time8)/(0.5*(incimpute$time9+incimpute$time8)))*100
incimpute$inc.pct0402 <- ((incimpute$time10-incimpute$time9)/(0.5*(incimpute$time10+incimpute$time9)))*100
incimpute$inc.pct0604 <- ((incimpute$time11-incimpute$time10)/(0.5*(incimpute$time11+incimpute$time10)))*100
incimpute$inc.pct0806 <- ((incimpute$time12-incimpute$time11)/(0.5*(incimpute$time12+incimpute$time11)))*100
incimpute$inc.pct1008 <- ((incimpute$time13-incimpute$time12)/(0.5*(incimpute$time13+incimpute$time12)))*100
summary(incimpute$inc.pct9190) 
summary(incimpute$inc.pct1008) 

#imputation (carry forward)
incimpute$inc.pct9190i <- ((incimpute$adj90.hh.inc.91i-incimpute$adj90.hh.inc.90)/(0.5*(incimpute$adj90.hh.inc.91i+incimpute$adj90.hh.inc.90)))*100 
incimpute$inc.pct9291i <- ((incimpute$adj90.hh.inc.92i-incimpute$adj90.hh.inc.91i)/(0.5*(incimpute$adj90.hh.inc.92i+incimpute$adj90.hh.inc.91i)))*100
incimpute$inc.pct9392i <- ((incimpute$adj90.hh.inc.93i-incimpute$adj90.hh.inc.92i)/(0.5*(incimpute$adj90.hh.inc.93i+incimpute$adj90.hh.inc.92i)))*100
incimpute$inc.pct9493i <- ((incimpute$adj90.hh.inc.94i-incimpute$adj90.hh.inc.93i)/(0.5*(incimpute$adj90.hh.inc.94i+incimpute$adj90.hh.inc.93i)))*100
incimpute$inc.pct9694i <- ((incimpute$adj90.hh.inc.96i-incimpute$adj90.hh.inc.94i)/(0.5*(incimpute$adj90.hh.inc.96i+incimpute$adj90.hh.inc.94i)))*100
incimpute$inc.pct9896i <- ((incimpute$adj90.hh.inc.98i-incimpute$adj90.hh.inc.96i)/(0.5*(incimpute$adj90.hh.inc.98i+incimpute$adj90.hh.inc.96i)))*100
incimpute$inc.pct0098i <- ((incimpute$adj90.hh.inc.00i-incimpute$adj90.hh.inc.98i)/(0.5*(incimpute$adj90.hh.inc.00i+incimpute$adj90.hh.inc.98i)))*100
incimpute$inc.pct0200i <- ((incimpute$adj90.hh.inc.02i-incimpute$adj90.hh.inc.00i)/(0.5*(incimpute$adj90.hh.inc.02i+incimpute$adj90.hh.inc.00i)))*100
incimpute$inc.pct0402i <- ((incimpute$adj90.hh.inc.04i-incimpute$adj90.hh.inc.02i)/(0.5*(incimpute$adj90.hh.inc.04i+incimpute$adj90.hh.inc.02i)))*100
incimpute$inc.pct0604i <- ((incimpute$adj90.hh.inc.06i-incimpute$adj90.hh.inc.04i)/(0.5*(incimpute$adj90.hh.inc.06i+incimpute$adj90.hh.inc.04i)))*100
incimpute$inc.pct0806i <- ((incimpute$adj90.hh.inc.08i-incimpute$adj90.hh.inc.06i)/(0.5*(incimpute$adj90.hh.inc.08i+incimpute$adj90.hh.inc.06i)))*100
incimpute$inc.pct1008i <- ((incimpute$adj90.hh.inc.10i-incimpute$adj90.hh.inc.08i)/(0.5*(incimpute$adj90.hh.inc.10i+incimpute$adj90.hh.inc.08i)))*100
summary(incimpute$inc.pct9190i) ###(??): #no NAs because did x+0.0001 conversion for the 0 income values - ask AZAH/PL if appropriate
summary(incimpute$inc.pct1008i) ###(??): #no NAs because did x+0.0001 conversion for the 0 income values - ask AZAH/PL if appropriate


#Step 2. Compute SD of these percent changes
#without imputation
pctchgvars<-grep("inc.pct", names(incimpute), value = TRUE)[1:12]
incimpute$sd_pct <- apply(incimpute[, pctchgvars], 1, function(x) sd(x, na.rm = T))
summary(incimpute$sd_pct)
hist(incimpute$sd_pct)

#In Grasset looked at 1 SD of income volatility, which in their analysis was 34.5 SD of percent change
sd(incimpute$sd_pct,na.rm=TRUE) # here it is 37.79199 SD of percent change 
incimpute$sd_incvol<-incimpute$sd_pct/sd(incimpute$sd_pct, na.rm=TRUE) #this exposure variable gives results per 1 SD increase in income volatility
  ##this is main exposure variable!!

#with imputation
pctchgvars.i<-grep("inc.pct", names(incimpute), value = TRUE)[13:24]
incimpute$sd_pct.i <- apply(incimpute[, pctchgvars.i], 1, function(x) sd(x, na.rm = T))
summary(incimpute$sd_pct.i)
hist(incimpute$sd_pct.i)

#In Grasset looked at 1 SD of income volatility, which in their analysis was 34.5 SD of percent change
sd(incimpute$sd_pct.i,na.rm=TRUE) # here it is 30.81594 SD of percent change 
incimpute$sd_incvol.i<-incimpute$sd_pct.i/sd(incimpute$sd_pct.i,na.rm=TRUE) #this exposure variable (only for sensitivity analyses) gives results per 1 SD increase in income volatility

summary(incimpute$sd_incvol)

#(iii).Income volatility measure #2: number of income drops

#use unadjusted income (see Grasset et al. p.e1892)
#here imputation or not does not matter because outcome variable is the number of income drops...so if carry forward in between those will not count as drops until you hit the next time point in which a >25% occurs.
#so either dataset/approach will produce same # drops per person
#also need to adjust unequivalized incomes for HH size (we already did this for equivalized/adj income in prior cleaning dataset)

#Step 1: create mean income over time
unadjincomes<-grep("TOT_INC", names(incimpute), value = TRUE)[12:24]
equnadjincomes<-c("eqnetfamincome.1990","eqnetfamincome.1991","eqnetfamincome.1992","eqnetfamincome.1993","eqnetfamincome.1994",
                  "eqnetfamincome.1996","eqnetfamincome.1998","eqnetfamincome.2000","eqnetfamincome.2002","eqnetfamincome.2004",
                  "eqnetfamincome.2006","eqnetfamincome.2008","eqnetfamincome.2010")

hhsize<-grep("HH", names(incimpute), value = TRUE)[12:24]
incimpute[,equnadjincomes]<-incimpute[,unadjincomes]/sqrt(incimpute[,hhsize])

incimpute$mean_inc <- apply(incimpute[, equnadjincomes], 1, function(x) mean(x, na.rm = T)) 
summary(incimpute$mean_inc)#No NAs because na.rm=TRUE; also then don't have carry forward values impacting mean calculation here

#carry forward as above (easier than exporting to excel and reimporting cleaned file since it would result in same thing as carry forward)
incimpute2<-incimpute %>%
  mutate(
    unadjincome.1990 = eqnetfamincome.1990,
    unadjincome.1991 = ifelse(is.na(eqnetfamincome.1991), eqnetfamincome.1990, eqnetfamincome.1991),
    unadjincome.1992 = ifelse(is.na(eqnetfamincome.1992), unadjincome.1991, eqnetfamincome.1992),
    unadjincome.1993 = ifelse(is.na(eqnetfamincome.1993), unadjincome.1992, eqnetfamincome.1993),
    unadjincome.1994 = ifelse(is.na(eqnetfamincome.1994), unadjincome.1993, eqnetfamincome.1994),
    unadjincome.1996 = ifelse(is.na(eqnetfamincome.1996), unadjincome.1994, eqnetfamincome.1996),
    unadjincome.1998 = ifelse(is.na(eqnetfamincome.1998), unadjincome.1996, eqnetfamincome.1998),
    unadjincome.2000 = ifelse(is.na(eqnetfamincome.2000), unadjincome.1998, eqnetfamincome.2000),
    unadjincome.2002 = ifelse(is.na(eqnetfamincome.2002), unadjincome.2000, eqnetfamincome.2002),
    unadjincome.2004 = ifelse(is.na(eqnetfamincome.2004), unadjincome.2002, eqnetfamincome.2004),
    unadjincome.2006 = ifelse(is.na(eqnetfamincome.2006), unadjincome.2004, eqnetfamincome.2006),
    unadjincome.2008 = ifelse(is.na(eqnetfamincome.2008), unadjincome.2006, eqnetfamincome.2008),
    unadjincome.2010 = ifelse(is.na(eqnetfamincome.2010), unadjincome.2008, eqnetfamincome.2010)
  )

summary(incimpute$eqnetfamincome.1992)
summary(incimpute2$unadjincome.1992)
summary(incimpute$eqnetfamincome.2010)
summary(incimpute2$unadjincome.2010)

#Step 2: create if income drop at each follow-up waves
#definition: a *decrease* of ≥25% in income compared with the income at the previous study visit
#and less than the participant’s average income from all study visits

#again, need to do x+1 conversion for all unadjusted income variables or else have issues in calculations when zero is denominator (i.e., when no change between time points and income is zero?)
unadjincs<-grep("unadjinc", names(incimpute2), value = TRUE)
incimpute2[,unadjincs] <- lapply(incimpute2[,unadjincs], function(x) x=x+0.0001)

incimpute2$incdrop.9190 <- ifelse(((incimpute2$unadjincome.1991-incimpute2$unadjincome.1990)/
                                     (0.5*(incimpute2$unadjincome.1991+incimpute2$unadjincome.1990)) <= -0.25) & 
                                    incimpute2$unadjincome.1991 < incimpute2$mean_inc, 1, 0)
table(incimpute2$incdrop.9190, exclude = NULL)
head(incimpute2[, c("unadjincome.1991","unadjincome.1990","incdrop.9190")],100)

incimpute2$incdrop.9291 <- ifelse(((incimpute2$unadjincome.1992-incimpute2$unadjincome.1991)/
                                     (0.5*(incimpute2$unadjincome.1992+incimpute2$unadjincome.1991)) <= -0.25) & 
                                    incimpute2$unadjincome.1992 < incimpute2$mean_inc, 1, 0)
table(incimpute2$incdrop.9291, exclude = NULL)
head(incimpute2[, c("unadjincome.1992","unadjincome.1991","incdrop.9291")],100)

incimpute2$incdrop.9392 <- ifelse(((incimpute2$unadjincome.1993-incimpute2$unadjincome.1992)/
                                     (0.5*(incimpute2$unadjincome.1993+incimpute2$unadjincome.1992)) <= -0.25) & 
                                    incimpute2$unadjincome.1993 < incimpute2$mean_inc, 1, 0)
table(incimpute2$incdrop.9392, exclude = NULL)
head(incimpute2[, c("unadjincome.1993","unadjincome.1992","incdrop.9392")],100)

incimpute2$incdrop.9493 <- ifelse(((incimpute2$unadjincome.1994-incimpute2$unadjincome.1993)/
                                     (0.5*(incimpute2$unadjincome.1994+incimpute2$unadjincome.1993)) <= -0.25) & 
                                    incimpute2$unadjincome.1994 < incimpute2$mean_inc, 1, 0)
table(incimpute2$incdrop.9493, exclude = NULL)
head(incimpute2[, c("unadjincome.1994","unadjincome.1993","incdrop.9493")],100)

incimpute2$incdrop.9694 <- ifelse(((incimpute2$unadjincome.1996-incimpute2$unadjincome.1994)/
                                     (0.5*(incimpute2$unadjincome.1996+incimpute2$unadjincome.1994)) <= -0.25) & 
                                    incimpute2$unadjincome.1996 < incimpute2$mean_inc, 1, 0)
table(incimpute2$incdrop.9694, exclude = NULL)
head(incimpute2[, c("unadjincome.1996","unadjincome.1994","incdrop.9694")],100)

incimpute2$incdrop.9896 <- ifelse(((incimpute2$unadjincome.1998-incimpute2$unadjincome.1996)/
                                     (0.5*(incimpute2$unadjincome.1998+incimpute2$unadjincome.1996)) <= -0.25) & 
                                    incimpute2$unadjincome.1998 < incimpute2$mean_inc, 1, 0)
table(incimpute2$incdrop.9896, exclude = NULL)
head(incimpute2[, c("unadjincome.1998","unadjincome.1996","incdrop.9896")],100)

incimpute2$incdrop.0098 <- ifelse(((incimpute2$unadjincome.2000-incimpute2$unadjincome.1998)/
                                     (0.5*(incimpute2$unadjincome.2000+incimpute2$unadjincome.1998)) <= -0.25) & 
                                    incimpute2$unadjincome.2000 < incimpute2$mean_inc, 1, 0)
table(incimpute2$incdrop.0098, exclude = NULL)
head(incimpute2[, c("unadjincome.2000","unadjincome.1998","incdrop.0098")],100)

incimpute2$incdrop.0200 <- ifelse(((incimpute2$unadjincome.2002-incimpute2$unadjincome.2000)/
                                     (0.5*(incimpute2$unadjincome.2002+incimpute2$unadjincome.2000)) <= -0.25) & 
                                    incimpute2$unadjincome.2002 < incimpute2$mean_inc, 1, 0)
table(incimpute2$incdrop.0200, exclude = NULL)
head(incimpute2[, c("unadjincome.2002","unadjincome.2000","incdrop.0200")],100)


incimpute2$incdrop.0402 <- ifelse(((incimpute2$unadjincome.2004-incimpute2$unadjincome.2002)/
                                     (0.5*(incimpute2$unadjincome.2004+incimpute2$unadjincome.2002)) <= -0.25) & 
                                    incimpute2$unadjincome.2004 < incimpute2$mean_inc, 1, 0)
table(incimpute2$incdrop.0402, exclude = NULL)
head(incimpute2[, c("unadjincome.2004","unadjincome.2002","incdrop.0402")],100)

incimpute2$incdrop.0604 <- ifelse(((incimpute2$unadjincome.2006-incimpute2$unadjincome.2004)/
                                     (0.5*(incimpute2$unadjincome.2006+incimpute2$unadjincome.2004)) <= -0.25) & 
                                    incimpute2$unadjincome.2006 < incimpute2$mean_inc, 1, 0)
table(incimpute2$incdrop.0604, exclude = NULL)
head(incimpute2[, c("unadjincome.2006","unadjincome.2004","incdrop.0604")],100)

incimpute2$incdrop.0806 <- ifelse(((incimpute2$unadjincome.2008-incimpute2$unadjincome.2006)/
                                     (0.5*(incimpute2$unadjincome.2008+incimpute2$unadjincome.2006)) <= -0.25) & 
                                    incimpute2$unadjincome.2008 < incimpute2$mean_inc, 1, 0)
table(incimpute2$incdrop.0806, exclude = NULL)
head(incimpute2[, c("unadjincome.2008","unadjincome.2006","incdrop.0806")],100)

incimpute2$incdrop.1008 <- ifelse(((incimpute2$unadjincome.2010-incimpute2$unadjincome.2008)/
                                     (0.5*(incimpute2$unadjincome.2010+incimpute2$unadjincome.2008)) <= -0.25) & 
                                    incimpute2$unadjincome.2010 < incimpute2$mean_inc, 1, 0)
table(incimpute2$incdrop.1008, exclude = NULL)
head(incimpute2[, c("unadjincome.2010","unadjincome.2008","incdrop.1008")],100)

#Step 3: count the number of income drop
colnames(incimpute2)
incdrops<-grep("incdrop", names(incimpute2), value = TRUE)

incimpute2$num.drops <- rowSums(incimpute2[, incdrops], na.rm = T)
table(incimpute2$num.drops, exclude = NULL)#No NAs - need to convert HRS to NA
incimpute2$num.drops[incimpute2$cohort=="HRS"&incimpute2$num.drops==0]<-NA
table(incimpute2$num.drops, exclude = NULL)#4951 NAs
summary(incimpute2$sd_incvol, exclude = NULL)#4951 NAs

#categorize it: 0, 1, and >=2
incimpute2$num.drops3 <- ifelse(incimpute2$num.drops==0, "0",
                                ifelse(incimpute2$num.drops==1, "1",
                                       ifelse(incimpute2$num.drops>=2, ">=2", NA)))
incimpute2$num.drops3 <- factor(incimpute2$num.drops3, levels = c("0", "1",  ">=2"))
incimpute2$num.drops4 <- ifelse(incimpute2$num.drops==0, "0",
                                ifelse(incimpute2$num.drops==1, "1",
                                       ifelse(incimpute2$num.drops==2, "2", 
                                              ifelse(incimpute2$num.drops>=3, ">=3", NA))))
incimpute2$num.drops4 <- factor(incimpute2$num.drops4, levels = c("0", "1", "2", ">=3"))

table(incimpute2$num.drops3, exclude=NULL)
table(incimpute2$num.drops4, exclude=NULL)

#----------------------------------------------------------------------------------------------------------------------------------
###6. Create outcomes specific to each cohort in merged dataset (so can explore how relationships change pre-post matching)
table(incimpute2$immedrecall.tm.i, incimpute2$cohort, exclude=NULL)  
table(incimpute2$delrecall.tm.i, incimpute2$cohort, exclude=NULL)  
table(incimpute2$avg.memory.tm.i, incimpute2$cohort, exclude=NULL)  
table(incimpute2$sum.memory.tm.i, incimpute2$cohort, exclude=NULL)

incimpute2$immedrecall.tm.i.HRS<-ifelse(incimpute2$cohort=="HRS",incimpute2$immedrecall.tm.i,NA)
incimpute2$immedrecall.tm.i.NLSY<-ifelse(incimpute2$cohort=="NLSY",incimpute2$immedrecall.tm.i,NA)

incimpute2$delrecall.tm.i.HRS<-ifelse(incimpute2$cohort=="HRS",incimpute2$delrecall.tm.i,NA)
incimpute2$delrecall.tm.i.NLSY<-ifelse(incimpute2$cohort=="NLSY",incimpute2$delrecall.tm.i,NA)

incimpute2$avg.imp.mem.HRS<-ifelse(incimpute2$cohort=="HRS",incimpute2$avg.memory.tm.i,NA)
incimpute2$avg.imp.mem.NLSY<-ifelse(incimpute2$cohort=="NLSY",incimpute2$avg.memory.tm.i,NA)

incimpute2$sum.imp.mem.HRS<-ifelse(incimpute2$cohort=="HRS",incimpute2$sum.memory.tm.i,NA)
incimpute2$sum.imp.mem.NLSY<-ifelse(incimpute2$cohort=="NLSY",incimpute2$sum.memory.tm.i,NA)
  
  table(incimpute2$avg.memory.tm.i, incimpute2$cohort, exclude=NULL)
  hist(incimpute2$avg.memory.tm.i, exclude=NULL)
  table(incimpute2$avg.imp.mem.HRS, incimpute2$cohort, exclude=NULL) #good, coding worked
  table(incimpute2$avg.imp.mem.NLSY, incimpute2$cohort, exclude=NULL) #good, coding worked

combine.w.exp<-incimpute2 #n=8326
  table(combine.w.exp$cohort) 
  
#----------------------------------------------------------------------------------------------------------------------------------
###7. Table 1: Create descriptive summary/comparison table - help in choosing matching variables (if lots missing on a variable, deprioritize for matching)
#https://www.rdocumentation.org/packages/table1/versions/1.4.3
#clean variabes in combined dataset, where applicable (labels, factoring, levels, etc.) for Table 1

table(combine.w.exp$age1stmarried.cat, combine.w.exp$marriage_2010, combine.w.exp$cohort)
table(combine.w.exp$age1stmarried.cat, combine.w.exp$marriage_2010, combine.w.exp$cohort)
table(combine.w.exp$age1stmarried.cat, combine.w.exp$marriage_2010, combine.w.exp$cohort, exclude=NULL)
combine.w.exp$marriage_2010[is.na(combine.w.exp$marriage_2010)&combine.w.exp$age1stmarried.cat=="never married"]<-"never married"

#Create a Descriptive Table 1
label(combine.w.exp$age_2010)  <- "Age (years)"
label(combine.w.exp$birthyear)  <- "Birth year"
label(combine.w.exp$female) <- "Sex"
label(combine.w.exp$race) <- "Racial group membership"
label(combine.w.exp$eth) <- "Ethnicity"
label(combine.w.exp$raceth) <- "Race and ethnicity (combined)"
label(combine.w.exp$usbirth) <- "Nativity status"
label(combine.w.exp$pedu_cat) <- "Parental years of education"
label(combine.w.exp$edu_yrs) <- "Years of education"
label(combine.w.exp$born.child.south) <- "Born or childhood in the South"
label(combine.w.exp$age1stmarried.cat) <- "Age first married"
label(combine.w.exp$marriage_2010) <- "Marital status"
label(combine.w.exp$weight_2010) <- "Weight (kg)"
label(combine.w.exp$height_2010) <- "Height (m)"
label(combine.w.exp$BMI_2010) <- "Body mass index"
label(combine.w.exp$everhibp) <- "Doctor ever diagnosed with high blood pressure"
label(combine.w.exp$adj10.hh.inc.10) <- "Net household income"
label(combine.w.exp$poverty_2010) <- "Living in poverty"
label(combine.w.exp$currentregion_2010) <- "US region currently live in"
label(combine.w.exp$wealth_2010) <- "Net household wealth"
label(combine.w.exp$LBRF_2010) <- "Employment status"
label(combine.w.exp$occuskill_2010) <- "Occupational skill (ISCO)"
label(combine.w.exp$epinsurance_2010) <- "Has employer provided health insurance"
label(combine.w.exp$everdiab) <- "Doctor ever diagnosed with diabetes"
label(combine.w.exp$immedrecall) <- "Immediate 10-word recall score (0-10)"
label(combine.w.exp$delrecall) <- "Delayed 10-word recall score (0-10)"
label(combine.w.exp$avg.memory) <- "Average 10-word recall score (0-10)"
label(combine.w.exp$sum.memory) <- "Summed 10-word recall score (0-20)"
label(combine.w.exp$selfratemem) <- "Self-rated memory"
label(combine.w.exp$srh) <- "Self-rated health"
label(combine.w.exp$eversmoke_2010) <- "Ever smoked cigarettes"
label(combine.w.exp$currsmoke_2010) <- "Currently smokes cigarettes"
label(combine.w.exp$drinksalc_2010) <- "Currently drinks alcohol"
label(combine.w.exp$everpsyche) <- "Doctor ever diagnosed with depression or any other psychiatric illness"
label(combine.w.exp$everCVD) <- "Doctor ever diagnosed with cardiovascular disease"
label(combine.w.exp$countCVD) <- "# of doctor diagnosed cardiovascular diseases"
label(combine.w.exp$cesd) <- "CESD score"
label(combine.w.exp$vigex.2010) <- "Participates in vigorous exercise"
label(combine.w.exp$lmodex.2010) <- "Participates in light/moderate exercise"

cohort.compare<-table1(~ female+race+eth+raceth+usbirth+pedu_cat+born.child.south+age1stmarried.cat+
                         age_2010+birthyear+adj10.hh.inc.10+poverty_2010+currentregion_2010+wealth_2010+LBRF_2010+
                         occuskill_2010+epinsurance_2010+weight_2010+height_2010+BMI_2010+marriage_2010+immedrecall+delrecall+
                         avg.memory+sum.memory+selfratemem+edu_yrs+eversmoke_2010+currsmoke_2010+drinksalc_2010+
                         everpsyche+cesd+everdiab+everhibp+everCVD+countCVD+srh+vigex.2010+lmodex.2010 | cohort, data=combine.w.exp,
                       overall=c(left="Total")) #caption=caption, footnote=footnote
cohort.compare

#write.csv(cohort.compare,"/Users/xxxxx/code/For publication/Results/Table1_N_8326_beforeCC.csv")

#----------------------------------------------------------------------------------------------------------------------------------
###8. Now that created the analytic sample and exposures for NLSY, examine R2 in just the NLSY cohort (as did with HRS)

lm.vars<-c("female","race","eth","raceth","usbirth","pedu_cat","born.child.south","age1stmarried.cat",
           "age_2010","birthyear","adj10.hh.inc.10","poverty_2010","currentregion_2010","wealth_2010","LBRF_2010",
           "occuskill_2010","epinsurance_2010","weight_2010","height_2010","BMI_2010","marriage_2010","immedrecall","delrecall",
           "avg.memory","sum.memory","selfratemem","edu_yrs","eversmoke_2010","currsmoke_2010","drinksalc_2010",
           "everpsyche","cesd","everdiab","everhibp","everCVD","countCVD","srh","vigex.2010","lmodex.2010")

###Create data frame to save results - that has missing data in each time point and also R2 of interest
combine.w.exp$age1stmarried.cat
NLSY.corrs<-as.data.frame(as.matrix(lm.vars,nrow=39,ncol=1))
names(NLSY.corrs)<-"variable"

library(naniar)
md_NLSY_2010 <- miss_var_summary(combine.w.exp[which(combine.w.exp$iwstatus_2010==1&combine.w.exp$cohort=="NLSY"),lm.vars], order = F)
md_NLSY_2012 <- miss_var_summary(combine.w.exp[which(combine.w.exp$iwstatus_2012==1&combine.w.exp$cohort=="NLSY"),lm.vars], order = F)
md_NLSY_2014 <- miss_var_summary(combine.w.exp[which(combine.w.exp$iwstatus_2014==1&combine.w.exp$cohort=="NLSY"),lm.vars], order = F)
md_NLSY_2016 <- miss_var_summary(combine.w.exp[which(combine.w.exp$iwstatus_2016==1&combine.w.exp$cohort=="NLSY"),lm.vars], order = F)
md_NLSY_2018 <- miss_var_summary(combine.w.exp[which(combine.w.exp$iwstatus_2018==1&combine.w.exp$cohort=="NLSY"),lm.vars], order = F)

NLSY.corrs<-cbind(NLSY.corrs,md_NLSY_2010,md_NLSY_2012,md_NLSY_2014,md_NLSY_2016,md_NLSY_2018)
names(NLSY.corrs)
NLSY.corrs<-NLSY.corrs[,-c(2,5,8,11,14)]
names(NLSY.corrs)<-c("variable","n_miss_2010","pct_miss_2010","n_miss_2012","pct_miss_2012","n_miss_2014","pct_miss_2014","n_miss_2016","pct_miss_2016","n_miss_2018","pct_miss_2018")

###correlations between each variable and income volatility at matching time points

#income volatility
out.incvol <- vector('list', length(lm.vars))
for (i in seq_along(lm.vars)){
  out.incvol[[i]] <- summary(lm(paste("sd_incvol",  '~', lm.vars[i]),
                                  data = combine.w.exp[combine.w.exp$cohort=="NLSY",]))$r.squared
}
r.sq.incvol<-as.data.frame(as.matrix(out.incvol))
names(r.sq.incvol)[1]<-"R2_INCVOL"

#number of drops ("continuous")
out.numdrops <- vector('list', length(lm.vars))
for (i in seq_along(lm.vars)){
  out.numdrops[[i]] <- summary(lm(paste("num.drops",  '~', lm.vars[i]),
                                  data = combine.w.exp[combine.w.exp$cohort=="NLSY",]))$r.squared
}
r.sq.numdrops<-as.data.frame(as.matrix(out.numdrops))
names(r.sq.numdrops)[1]<-"R2_NUMDROPS"

#number of drops (analytic variable - 0, 1, 2, 3+)
table(combine.w.exp$num.drops4.n)
combine.w.exp$num.drops4.n<-ifelse(combine.w.exp$num.drops4=="0",0,
                                   ifelse(combine.w.exp$num.drops4=="1",1,
                                          ifelse(combine.w.exp$num.drops4=="2",2,
                                                 ifelse(combine.w.exp$num.drops4==">=3",3,NA))))

out.numdrops3 <- vector('list', length(lm.vars))
for (i in seq_along(lm.vars)){
  out.numdrops3[[i]] <- summary(lm(paste("num.drops4.n",  '~', lm.vars[i]),
                                  data = combine.w.exp[combine.w.exp$cohort=="NLSY",]))$r.squared
}
r.sq.numdrops3<-as.data.frame(as.matrix(out.numdrops3))
names(r.sq.numdrops3)[1]<-"R2_NUMDROPS3"

NLSY.corrs<-cbind(NLSY.corrs,r.sq.incvol,r.sq.numdrops, r.sq.numdrops3)

NLSY.corrs[,c(2:14)]<-lapply(NLSY.corrs[,c(2:14)],as.numeric)
NLSY.corrs.expt <- apply(NLSY.corrs,2,as.character)

#write.csv(NLSY.corrs.expt,"/Users/xxxxx/code/For publication/Results/NLSY_R2_results.csv")s

#----------------------------------------------------------------------------------------------------------------------------------
###9. Fill in (as much as possible) missing covariate data at matching time point

#a). missing income at Tm
combine.w.exp$income.tm.i<-combine.w.exp$adj10.hh.inc.10
  summary(combine.w.exp$income.tm.i) #597 missing
  summary(combine.w.exp$income.tm.i[combine.w.exp$cohort=="HRS"]) #0 missing
  summary(combine.w.exp$income.tm.i[combine.w.exp$cohort=="NLSY"]) #597 missing

#first use income (adjusted to 2010 dollars) from prior or immediately after 2010 time points as value for matching (2006, 2008, 2012, 2014) - as close to 2010 as possible)
combine.w.exp$income.tm.i[is.na(combine.w.exp$income.tm.i)]<-combine.w.exp$adj10.hh.inc.08[is.na(combine.w.exp$income.tm.i)]
  summary(combine.w.exp$income.tm.i) #252 remain missing
combine.w.exp$income.tm.i[is.na(combine.w.exp$income.tm.i)]<-combine.w.exp$adj10.hh.inc.06[is.na(combine.w.exp$income.tm.i)]
  summary(combine.w.exp$income.tm.i) #175 remain missing
combine.w.exp$income.tm.i[is.na(combine.w.exp$income.tm.i)]<-combine.w.exp$adj10.hh.inc.12[is.na(combine.w.exp$income.tm.i)]
  summary(combine.w.exp$income.tm.i) #110 remain missing
combine.w.exp$income.tm.i[is.na(combine.w.exp$income.tm.i)]<-combine.w.exp$adj10.hh.inc.14[is.na(combine.w.exp$income.tm.i)]
  summary(combine.w.exp$income.tm.i) #70 remain missing
  
  #after carrying fwd/bck, 70 (2% of sample) still missing income at matching time point 
  #figure out what predictors HRS rand uses to impute income and use that to fill in remaining income values so similar between studies
 
#do same for wealth so that use it in imputation (if needed - may not end up matching on wealth)
combine.w.exp$wealth.tm.i<-combine.w.exp$wealth_2010 
  summary(combine.w.exp$wealth.tm.i[combine.w.exp$cohort=="HRS"]) # no missing in HRS bc already imputed
  summary(combine.w.exp$wealth.tm.i[combine.w.exp$cohort=="NLSY"]) # 453 missing

combine.w.exp$wealth.tm.i[is.na(combine.w.exp$wealth.tm.i)]<-combine.w.exp$wealth10.2008[is.na(combine.w.exp$wealth.tm.i)]
combine.w.exp$wealth.tm.i[is.na(combine.w.exp$wealth.tm.i)]<-combine.w.exp$wealth10.2012[is.na(combine.w.exp$wealth.tm.i)]
combine.w.exp$wealth.tm.i[is.na(combine.w.exp$wealth.tm.i)]<-combine.w.exp$wealth10.2016[is.na(combine.w.exp$wealth.tm.i)]
  summary(combine.w.exp$wealth.tm.i[combine.w.exp$cohort=="NLSY"]) # 281 missing after carry forward/back

combine.w.exp$wealth.cat<-ifelse(combine.w.exp$wealth.tm.i<0,0,
                                ifelse(combine.w.exp$wealth.tm.i>=0&combine.w.exp$wealth.tm.i<294484,1,2))

table(combine.w.exp$wealth.cat, exclude=NULL)

#for remaining 70 with missing income, impute via median income within strata of SRH, years of education, employment status, race (non white v white), marital status (things used for income/wealth imputations in HRS) 

#HRS uses "nonwhite" indicator in imputation
table(combine.w.exp$race, combine.w.exp$eth, combine.w.exp$cohort, exclude=NULL)
combine.w.exp$nonwhite<-ifelse(combine.w.exp$race!="White",1,0)
combine.w.exp$nonwhite[is.na(combine.w.exp$race)&combine.w.exp$eth=="Hispanic"]<-1
  table(combine.w.exp$nonwhite, exclude=NULL)

imp.inc = combine.w.exp %>% 
  group_by(wealth.cat,srh, edu_yrs, female,LBRF_2010, marriage_2010, nonwhite) %>% 
  summarize(
    imp_income = median(income.tm.i, na.rm = T))

combine.w.exp = left_join(combine.w.exp, imp.inc, by = c("wealth.cat","srh", "edu_yrs", "female", "LBRF_2010", "marriage_2010","nonwhite")) %>% 
  mutate(income.tm.i = case_when(is.na(income.tm.i) ~ imp_income,
                                 TRUE ~ income.tm.i))

summary(combine.w.exp$income.tm.i) #42 still missing 

#adding wealth and female sex to model brought down # of obs that could get an imputed value)
#redo above approach, removing these are strata to recover as much remaining missing data as possible
imp.inc2 = combine.w.exp %>% 
  group_by(srh, edu_yrs, LBRF_2010, marriage_2010, nonwhite) %>% 
  summarize(
    imp_income2 = median(income.tm.i, na.rm = T))

combine.w.exp = left_join(combine.w.exp, imp.inc2, by = c("srh", "edu_yrs", "LBRF_2010", "marriage_2010","nonwhite")) %>% 
  mutate(income.tm.i = case_when(is.na(income.tm.i) ~ imp_income2,
                                 TRUE ~ income.tm.i))

summary(combine.w.exp$income.tm.i) #14 still missing 

#finally, remove SRH for some and see if can recover any more missingness
imp.inc3 = combine.w.exp %>% 
  group_by(edu_yrs, LBRF_2010, marriage_2010, nonwhite) %>% 
  summarize(
    imp_income3 = median(income.tm.i, na.rm = T))

combine.w.exp = left_join(combine.w.exp, imp.inc3, by = c("edu_yrs", "LBRF_2010", "marriage_2010","nonwhite")) %>% 
  mutate(income.tm.i = case_when(is.na(income.tm.i) ~ imp_income3,
                                 TRUE ~ income.tm.i))

summary(combine.w.exp$income.tm.i) #3 still missing - probably just need to delete these 2


#b). missing wealth at Tm (in HRS used same variables to impute missing income and wealth)
#try to do the same to reduce missing data on wealth

combine.w.exp$income.i.cat<-ifelse(combine.w.exp$income.tm.i==0,0,
                                  ifelse(combine.w.exp$income.tm.i<37500,1,2))

table(combine.w.exp$income.i.cat, exclude=NULL)  

imp.wlth = combine.w.exp %>% 
  group_by(income.i.cat, srh, edu_yrs, female, LBRF_2010, marriage_2010, nonwhite) %>% 
  summarize(
    imp_wtlh = median(wealth.tm.i, na.rm = T))

combine.w.exp = left_join(combine.w.exp, imp.wlth, by = c("income.i.cat","srh", "edu_yrs", "female", "LBRF_2010", "marriage_2010","nonwhite")) %>% 
  mutate(wealth.tm.i = case_when(is.na(wealth.tm.i) ~ imp_wtlh,
                                 TRUE ~ wealth.tm.i))

summary(combine.w.exp$wealth.tm.i) #75 still missing 

#adding income and female sex to model brought down # of obs that could get an imputed value)
#redo above approach, removing these are strata to recover as much remaining missing data as possible
imp.wlth2 = combine.w.exp %>% 
  group_by(srh, edu_yrs, LBRF_2010, marriage_2010, nonwhite) %>% 
  summarize(
    imp_wtlh2 = median(wealth.tm.i, na.rm = T))

combine.w.exp = left_join(combine.w.exp, imp.wlth2, by = c("srh", "edu_yrs", "LBRF_2010", "marriage_2010","nonwhite")) %>% 
  mutate(wealth.tm.i = case_when(is.na(wealth.tm.i) ~ imp_wtlh2,
                                 TRUE ~ wealth.tm.i))

summary(combine.w.exp$wealth.tm.i) #29 still missing - probably just need to delete these?

#finally, remove SRH for some and see if can recover any more missingness
imp.wlth3 = combine.w.exp %>% 
  group_by(edu_yrs, LBRF_2010, marriage_2010, nonwhite) %>% 
  summarize(
    imp_wlth3 = median(wealth.tm.i, na.rm = T))

combine.w.exp = left_join(combine.w.exp, imp.wlth3, by = c("edu_yrs", "LBRF_2010", "marriage_2010","nonwhite")) %>% 
  mutate(wealth.tm.i = case_when(is.na(wealth.tm.i) ~ imp_wlth3,
                                 TRUE ~ wealth.tm.i))

summary(combine.w.exp$wealth.tm.i) #3 missing wealth now!

#c).missing demographic data at matching time point - from prior analyses, the following covariates have strongest associations and X and/or Y, making them prioritized matching variables:
#Race
#Ethnicity
#Sex/gender
#Parental highest year of education
#Age first married
#Age (in 2010)
#Net household income at tm
#Net household wealth at tm
#Marital status at tm
#Occupational skill at tm
#Has employer provided health insurance at tm
#Years of own education
#Immediate 10-word recall score
#Delayed 10-word recall score
#Sum/average memory score
#Self—rated health 

table(combine.w.exp$female, exclude=NULL) #0 missing
table(combine.w.exp$race, exclude=NULL) #33 missing (in HRS)
table(combine.w.exp$eth, exclude=NULL) #22 missing (in HRS)
table(combine.w.exp$race, combine.w.exp$eth, exclude=NULL)  
table(combine.w.exp$raceth,exclude=NULL, combine.w.exp$cohort) #49 missing - but only 2 Hispanic in HRS - because of outcome; probably should restrict sample to non-Hispanic if using memory composite score

combine.w.exp[is.na(combine.w.exp$raceth),c("race","eth","raceth")]
#can't do much about the missing data from here - use  "black vs. nonblack" instead of raceth

combine.w.exp$blackrace<-ifelse(combine.w.exp$race=="Black",1,0)
  table(combine.w.exp$blackrace, exclude=NULL)
combine.w.exp$blackrace = factor(combine.w.exp$blackrace, levels = c(0,1), labels = c("Not Black", "Black"))
  
table(combine.w.exp$pedu_cat,exclude=NULL) #missing data is a category
table(combine.w.exp$age1stmarried.cat,exclude=NULL) #missing data is a category
table(combine.w.exp$occuskill_2010, exclude=NULL) #missing data is a category
table(combine.w.exp$epinsurance_2010,exclude=NULL) #333 missing
  table(combine.w.exp$epinsurance_2010, combine.w.exp$occuskill_2010, exclude=NULL)
  table(combine.w.exp$epinsurance_2010, combine.w.exp$LBRF_2010, exclude=NULL) 
  
table(combine.w.exp$edu_yrs,exclude=NULL) #1 missing (maybe need to exclude)
table(combine.w.exp$srh,exclude=NULL) #107 missing
table(combine.w.exp$epinsurance_2010, combine.w.exp$cohort, exclude=NULL)  
table(combine.w.exp$occuskill_2010, combine.w.exp$cohort, exclude=NULL) 
table(combine.w.exp$LBRF_2010, combine.w.exp$cohort, exclude=NULL) #200 missing  
table(combine.w.exp$srh, combine.w.exp$cohort, exclude=NULL) #1 in HRS and 106 in NLSY - so need to impute
table(combine.w.exp$poverty_2010, exclude=NULL) #599 
table(combine.w.exp$poverty_2010, combine.w.exp$cohort, exclude=NULL) #597 in NLSY, 2 in HRS

#can use imputed income to place people into poverty based on both cohort's guidelines
combine.w.exp$poverty_2010.i<-combine.w.exp$poverty_2010
combine.w.exp$poverty_2010.i[is.na(combine.w.exp$poverty_2010)&combine.w.exp$income.tm.i<22050]<-"Yes"
combine.w.exp$poverty_2010.i[is.na(combine.w.exp$poverty_2010)&combine.w.exp$income.tm.i>=22050]<-"No"
table(combine.w.exp$poverty_2010.i, exclude=NULL) #3 still missing so just kick them out

#create missing categories if possible for NULL#create missing categories if possible for ephealth insurance of these variables
combine.w.exp = combine.w.exp %>% 
  mutate(epinsurance_2010 = case_when(epinsurance_2010=="Yes" ~ "Yes",
                                      epinsurance_2010=="No" ~ "No",
                                      TRUE ~ "Missing"),
         LBRF_2010 = case_when(LBRF_2010=="Work FT" ~ "Work FT",
                               LBRF_2010=="Work PT" ~ "Work PT",
                               LBRF_2010=="Unemployed" ~ "Unemployed",
                               LBRF_2010=="Retired" ~ "Retired",
                               LBRF_2010=="Disabled" ~ "Disabled",
                               LBRF_2010=="Not in LbrF" ~ "Not in LbrF",
                               TRUE ~ "Missing"))
table(combine.w.exp$epinsurance_2010, exclude=NULL)
table(combine.w.exp$LBRF_2010, exclude=NULL)

table(combine.w.exp$epinsurance_2010, combine.w.exp$LBRF_2010, combine.w.exp$cohort, exclude=NULL) #now missing is separate category
table(combine.w.exp$epinsurance_2010, combine.w.exp$cohort, exclude=NULL) #now missing is separate category

combine.w.exp[is.na(combine.w.exp$LBRF_2010)]<-"Missing"
  table(combine.w.exp$LBRF_2010, exclude=NULL)

srh.imp = combine.w.exp %>% 
  group_by(raceth, female, income.i.cat, edu_yrs, countCVD, eversmoke_2010) %>% 
  summarize(
    imp_srh = median(srh, na.rm = T))

combine.w.exp = left_join(combine.w.exp, srh.imp, by = c("raceth", "female","income.i.cat","edu_yrs", "countCVD" ,"eversmoke_2010")) %>% 
  mutate(srh.i = ifelse(is.na(srh), imp_srh, srh))

summary(combine.w.exp$srh.i) #14 still missing (exclude)

#restrict to the final set of matching variables with non-missing data
  #blackrace
  #pedu_cat
  #age_2010
  #female
  #income.tm.i
  #sum.memory.tm.i
  #edu_yrs
  #marriage_2010
  #srh.i
  #epinsurance_2010
  #occuskill_2010
  #age1stmarried.cat
  #poverty_2010.i
  #countCVD

matchingtest<-combine.w.exp[!is.na(combine.w.exp$blackrace)&!is.na(combine.w.exp$pedu_cat)&!is.na(combine.w.exp$age_2010)&!is.na(combine.w.exp$female)&!is.na(combine.w.exp$income.tm.i)&!is.na(combine.w.exp$avg.memory.tm.i)&!is.na(combine.w.exp$edu_yrs)&!is.na(combine.w.exp$marriage_2010)&!is.na(combine.w.exp$srh.i)&!is.na(combine.w.exp$epinsurance_2010)&!is.na(combine.w.exp$occuskill_2010)&!is.na(combine.w.exp$age1stmarried.cat)&!is.na(combine.w.exp$poverty_2010.i)&!is.na(combine.w.exp$countCVD),]
#8079
(8326-8079)/8326
#drop 2.97% of sample with missing covariates for matching
table(matchingtest$cohort)
#to align cohorts better (because using composite memory score) restrict to non-Hispanic individuals
matchingtest<-matchingtest[matchingtest$eth!="Hispanic"&!is.na(matchingtest$eth),]
#7529 individuals in restricted complete case sample

#----------------------------------------------------------------------------------------------------------------------------------
#9. Create final variables of interest for matchinv

matchingtest$studyID<-matchingtest$id
  names(matchingtest)
matchingtest<-matchingtest[,-1]

#cohort specific z-scores for sum or average memory
matchingtest$sum.memory.tm.i.z.nlsy[matchingtest$cohort=="NLSY"]<-(matchingtest$sum.memory.tm.i[matchingtest$cohort=="NLSY"]-mean(matchingtest$sum.memory.tm.i[matchingtest$cohort=="NLSY"]))/sd(matchingtest$sum.memory.tm.i[matchingtest$cohort=="NLSY"])
matchingtest$avg.memory.tm.i.z.nlsy[matchingtest$cohort=="NLSY"]<-(matchingtest$avg.memory.tm.i[matchingtest$cohort=="NLSY"]-mean(matchingtest$avg.memory.tm.i[matchingtest$cohort=="NLSY"]))/sd(matchingtest$avg.memory.tm.i[matchingtest$cohort=="NLSY"])
  summary(matchingtest$avg.memory.tm.i.z.nlsy)
  summary(matchingtest$sum.memory.tm.i.z.nlsy)
  
matchingtest$sum.memory.tm.i.z.hrs[matchingtest$cohort=="HRS"]<-(matchingtest$sum.memory.tm.i[matchingtest$cohort=="HRS"]-mean(matchingtest$sum.memory.tm.i[matchingtest$cohort=="HRS"]))/sd(matchingtest$sum.memory.tm.i[matchingtest$cohort=="HRS"])
matchingtest$avg.memory.tm.i.z.hrs[matchingtest$cohort=="HRS"]<-(matchingtest$avg.memory.tm.i[matchingtest$cohort=="HRS"]-mean(matchingtest$avg.memory.tm.i[matchingtest$cohort=="HRS"]))/sd(matchingtest$avg.memory.tm.i[matchingtest$cohort=="HRS"])
  summary(matchingtest$avg.memory.tm.i.z.hrs)
  summary(matchingtest$sum.memory.tm.i.z.hrs)

matchingtest$avg.memory.tm.i.z<-ifelse(matchingtest$cohort=="NLSY", matchingtest$avg.memory.tm.i.z.nlsy, matchingtest$avg.memory.tm.i.z.hrs)
matchingtest$sum.memory.tm.i.z<-ifelse(matchingtest$cohort=="NLSY", matchingtest$sum.memory.tm.i.z.nlsy, matchingtest$sum.memory.tm.i.z.hrs)
  summary(matchingtest$avg.memory.tm.i.z)
  summary(matchingtest$sum.memory.tm.i.z)
  
aggregate(matchingtest$avg.memory.tm.i.z, by=list(matchingtest$cohort), FUN="summary")

#save if we want to use just test scores not composite score (problem there is attrition over time - composite score better handles differential attrition due to cognitive impairment)
matchingtest$sum.rawmem10z<-(matchingtest$sum.rawmem10-mean(matchingtest$sum.rawmem10, na.rm=TRUE))/sd(matchingtest$sum.rawmem10, na.rm=TRUE)
matchingtest$sum.rawmem12z<-(matchingtest$sum.rawmem12-mean(matchingtest$sum.rawmem10, na.rm=TRUE))/sd(matchingtest$sum.rawmem10, na.rm=TRUE)
matchingtest$sum.rawmem14z<-(matchingtest$sum.rawmem14-mean(matchingtest$sum.rawmem10, na.rm=TRUE))/sd(matchingtest$sum.rawmem10, na.rm=TRUE)
matchingtest$sum.rawmem16z<-(matchingtest$sum.rawmem16-mean(matchingtest$sum.rawmem10, na.rm=TRUE))/sd(matchingtest$sum.rawmem10, na.rm=TRUE)
matchingtest$sum.rawmem18z<-(matchingtest$sum.rawmem18-mean(matchingtest$sum.rawmem10, na.rm=TRUE))/sd(matchingtest$sum.rawmem10, na.rm=TRUE)
matchingtest$sum.rawmem20z<-(matchingtest$sum.rawmem20-mean(matchingtest$sum.rawmem10, na.rm=TRUE))/sd(matchingtest$sum.rawmem10, na.rm=TRUE)

summary(matchingtest$sum.rawmem10z)
summary(matchingtest$sum.rawmem12z)
summary(matchingtest$sum.rawmem14z)
summary(matchingtest$sum.rawmem16z)
summary(matchingtest$sum.rawmem18z)
summary(matchingtest$sum.rawmem20z)

#write.csv(matchingtest,"/Users/xxxxx/code/For publication/Data/ccdata_formatching.csv")

#----------------------------------------------------------------------------------------------------------------------------------
###10. Table 1: Final complete case table

#Create a Descriptive Table 1
label(matchingtest$age_2010)  <- "Age (years)"
label(matchingtest$female) <- "Sex"
label(matchingtest$blackrace) <- "Race"
label(matchingtest$pedu_cat) <- "Parental years of education"
label(matchingtest$edu_yrs) <- "Years of education"
label(matchingtest$age1stmarried.cat) <- "Age first married"
label(matchingtest$marriage_2010) <- "Marital status"
label(matchingtest$income.tm.i) <- "Net household income"
label(matchingtest$poverty_2010.i) <- "Living in poverty"
label(matchingtest$occuskill_2010) <- "Occupational skill (ISCO)"
label(matchingtest$epinsurance_2010) <- "Has employer provided health insurance"
label(matchingtest$avg.memory.tm.i) <- "Average 10-word recall score"
label(matchingtest$srh.i) <- "Self-rated health"  
label(matchingtest$countCVD) <- "# of doctor diagnosed cardiovascular diseases"

cohort.compare<-table1(~ income.tm.i + sum.memory.tm.i + age_2010 + female + blackrace + pedu_cat + edu_yrs + epinsurance_2010 + 
                         age1stmarried.cat + marriage_2010 + poverty_2010.i + occuskill_2010 + srh.i + countCVD | cohort, data=matchingtest,
                       overall=c(left="Total")) #caption=caption, footnote=footnote
cohort.compare
  
#write.csv(cohort.compare,"/Users/xxxxx/code/For publication/Results/Table1_cc.csv")
  