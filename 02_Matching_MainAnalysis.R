library(MatchIt)
library(tidyr)
library(haven)
library(cobalt)
library(mitools)
library(dplyr)
library(lme4)
library(ggplot2)
library(lmerTest)
library(performance) 
library(partR2) 

packageVersion("MatchIt") #version 4.5.3
packageVersion("lme4") #version 1.1.33

################################################################################################################################################################################################################################################################
#PART I. DATA PREPARATION

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#A. LOAD AND PREPARE COMPLETE CASE/STACKED DATA FOR MATCHING

#load data
matchingtest<-read.csv("/Users/xxxxx/code/For publication/Data/ccdata_formatching.csv")[-1] 
#mbb<-read.csv("/Users/xxxxx/code/For publication/Data/Final/mbb_cohort.csv") #if need merge in any variables later

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#B. RUN VALIDATION ANALYSES IN NLSY COHORT PRE-MATCHING

#Get benchmark estimates in sample pre-matching (examine raw and z-scores)
matchingtest$num.drops4 <- factor(matchingtest$num.drops4, 
                                  levels = c("0","1","2",">=3"),
                                  labels = c("0","1","2",">=3"))

#create dataframe to store results
truth.results.table<-as.data.frame(matrix(nrow=16,ncol=4))
colnames(truth.results.table)<-c("Analysis", "Beta", "LCL", "UCL")
truth.results.table[1]<-c("sd_invcol.m_mem_raw","numdrops1_mem_raw","numdrops2_mem_raw","numdrops3+_mem_raw",
                          "sd_invcol.m_mem_z","numdrops1_mem_z","numdrops2_mem_z","numdrops3+_mem_z",
                          "sd_invcol.m_mem_raw_AFQT","numdrops1_mem_raw_AFQT","numdrops2_mem_raw_AFQT","numdrops3+_mem_raw_AFQT",
                          "sd_invcol.m_mem_raw_AFQT","numdrops1_mem_raw_AFQT","numdrops2_mem_raw_AFQT","numdrops3+_mem_raw_AFQT")
sd(matchingtest$sum.imp.mem.NLSY, na.rm=TRUE)

#Compare with synthetic cohort primary analysis results (raw scores)
truth.results.table[1,2:4]<-cbind(summary(lm(sum.imp.mem.NLSY ~ sd_incvol + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat, data = matchingtest))$coefficients[2],
                                  confint(lm(sum.imp.mem.NLSY ~ sd_incvol + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat, data = matchingtest))[2,1],
                                  confint(lm(sum.imp.mem.NLSY ~ sd_incvol + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat, data = matchingtest))[2,2])

truth.results.table[2:4,2:4]<-cbind(summary(lm(sum.imp.mem.NLSY ~ num.drops4 + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat, data = matchingtest))$coefficients[2:4],
                                    confint(lm(sum.imp.mem.NLSY ~ num.drops4 + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat,data = matchingtest))[2:4,1],
                                    confint(lm(sum.imp.mem.NLSY ~ num.drops4 + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat,data = matchingtest))[2:4,2])

#Compare with synthetic cohort primary analysis results (z scores)
truth.results.table[5,2:4]<-cbind(summary(lm(sum.memory.tm.i.z.nlsy ~ sd_incvol + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat, data = matchingtest))$coefficients[2],
                                  confint(lm(sum.memory.tm.i.z.nlsy ~ sd_incvol + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat, data = matchingtest))[2,1],
                                  confint(lm(sum.memory.tm.i.z.nlsy ~ sd_incvol + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat, data = matchingtest))[2,2])

truth.results.table[6:8,2:4]<-cbind(summary(lm(sum.memory.tm.i.z.nlsy ~ num.drops4 + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat, data = matchingtest))$coefficients[2:4],
                                    confint(lm(sum.memory.tm.i.z.nlsy ~ num.drops4 + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat,data = matchingtest))[2:4,1],
                                    confint(lm(sum.memory.tm.i.z.nlsy ~ num.drops4 + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat,data = matchingtest))[2:4,2])

#Compare with synthetic cohort sensitivity analysis results adjusting for AFQT (raw scores)
truth.results.table[9,2:4]<-cbind(summary(lm(sum.imp.mem.NLSY ~ sd_incvol + suminc90to10 + AFQTpctlscore06rev.81.r + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat, data = matchingtest))$coefficients[2],
                                  confint(lm(sum.imp.mem.NLSY ~ sd_incvol + suminc90to10 + AFQTpctlscore06rev.81.r + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat, data = matchingtest))[2,1],
                                  confint(lm(sum.imp.mem.NLSY ~ sd_incvol + suminc90to10 + AFQTpctlscore06rev.81.r +  age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat, data = matchingtest))[2,2])

truth.results.table[10:12,2:4]<-cbind(summary(lm(sum.imp.mem.NLSY ~ num.drops4 + suminc90to10 + AFQTpctlscore06rev.81.r +  age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat, data = matchingtest))$coefficients[2:4],
                                      confint(lm(sum.imp.mem.NLSY ~ num.drops4 + suminc90to10 + AFQTpctlscore06rev.81.r +  age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat,data = matchingtest))[2:4,1],
                                      confint(lm(sum.imp.mem.NLSY ~ num.drops4 + suminc90to10 + AFQTpctlscore06rev.81.r +  age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat,data = matchingtest))[2:4,2])

#Compare with synthetic cohort sensitivity analysis results adjusting for AFQT (z scores)
truth.results.table[13,2:4]<-cbind(summary(lm(sum.memory.tm.i.z.nlsy ~ sd_incvol + suminc90to10 + AFQTpctlscore06rev.81.r + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat, data = matchingtest))$coefficients[2],
                                   confint(lm(sum.memory.tm.i.z.nlsy ~ sd_incvol + suminc90to10 + AFQTpctlscore06rev.81.r + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat, data = matchingtest))[2,1],
                                   confint(lm(sum.memory.tm.i.z.nlsy ~ sd_incvol + suminc90to10 + AFQTpctlscore06rev.81.r +  age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat, data = matchingtest))[2,2])

truth.results.table[14:16,2:4]<-cbind(summary(lm(sum.memory.tm.i.z.nlsy ~ num.drops4 + suminc90to10 + AFQTpctlscore06rev.81.r +  age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat, data = matchingtest))$coefficients[2:4],
                                      confint(lm(sum.memory.tm.i.z.nlsy ~ num.drops4 + suminc90to10 + AFQTpctlscore06rev.81.r +  age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat,data = matchingtest))[2:4,1],
                                      confint(lm(sum.memory.tm.i.z.nlsy ~ num.drops4 + suminc90to10 + AFQTpctlscore06rev.81.r +  age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat,data = matchingtest))[2:4,2])

#write.csv(truth.results.table,"/Users/xxxxx/code/For publication/Results/truth_NLSY_prematching_7529.csv")

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#C. PREPARE MATCHING DATASET FOR MATCHING

#Create variables for exact matching/explore different options (quartiles, quintiles, etc.)
#income, cognition, education likely key variables to exact match on (in addition to distance matching)
#explore cut points to maximize # of categories where matches are still available/don't lose #s from matched sample (maintain HRS sample size)
#match on quartiles of key variables

#match on memory scores
summary(matchingtest$sum.memory.tm.i)
aggregate(matchingtest$sum.memory.tm.i, by=list(matchingtest$cohort), FUN="summary")
matchingtest$cog.i.quart<-ifelse(matchingtest$sum.memory.tm.i<9,0,
                                 ifelse(matchingtest$sum.memory.tm.i>=9&matchingtest$sum.memory.tm.i<11,1,
                                        ifelse(matchingtest$sum.memory.tm.i>=11&matchingtest$sum.memory.tm.i<13,2,3)))
  table(matchingtest$cog.i.quart,matchingtest$cohort)
  summary(matchingtest$sum.memory.tm.i)

#create cohort specific versions of key variables, for data exploration as needed
matchingtest$income.tm.i.hrs<-ifelse(matchingtest$cohort=="HRS",matchingtest$income.tm.i,NA)
matchingtest$income.tm.i.nlsy<-ifelse(matchingtest$cohort=="NLSY",matchingtest$income.tm.i,NA)
  summary(matchingtest$income.tm.i.hrs)
  summary(matchingtest$income.tm.i.nlsy)

#matching on income quartiles 
summary(matchingtest$income.tm.i, useNA="ifany")
matchingtest$inc.i.quart<-ifelse(matchingtest$income.tm.i<17888,0,
                                 ifelse(matchingtest$income.tm.i>=17888&matchingtest$income.tm.i<37901,1,
                                        ifelse(matchingtest$income.tm.i>=37901&matchingtest$income.tm.i<66000,2,3)))
  table(matchingtest$inc.i.quart,matchingtest$cohort, useNA="ifany")

#cohort indicator
matchingtest$cohort10<-as.factor(matchingtest$cohort10)
  class(matchingtest$cohort10)

aggregate(matchingtest$income.tm.i, by=list(matchingtest$cohort10), FUN="summary")

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#D. IDENTIFY IN DATASET COHORT-SPECIFIC MATCHING VARIABLES TO COMPARE MATCHED VALUES WITH ORIGINAL VALUES IN HRS AND NLSY (INTERNAL/SENSITIVTY ANALYSES/DATA EXPLORATION)

#restrict HRS and NSLY data to matching variables 
match.vars<-c("income.tm.i","sum.memory.tm.i","age_2010","female","blackrace","pedu_cat","edu_yrs","epinsurance_2010","age1stmarried.cat","marriage_2010","poverty_2010.i","occuskill_2010","srh.i","countCVD")
NLSY.covs<-matchingtest[matchingtest$cohort=="NLSY",c("NLSY_CASE_ID","AFQTpctlscore06rev.81.r",match.vars)]
HRS.covs<-matchingtest[matchingtest$cohort=="HRS",c("hhidpn",match.vars)]

#rename variables so easy to identify original values in component cohorts
names(NLSY.covs)<-c("NLSY_CASE_ID","AFQTpctlscore06rev.81.r.NLSY","income.tm.i.NLSY","sum.memory.tm.i.NLSY","age_2010.NLSY","female.NLSY","blackrace.NLSY","pedu_cat.NLSY","edu_yrs.NLSY","epinsurance_2010.NLSY","age1stmarried.cat.NLSY","marriage_2010.NLSY","poverty_2010.i.NLSY","occuskill_2010.NLSY","srh.i.NLSY","countCVD.NLSY")
names(HRS.covs)<-c("hhidpn","income.tm.i.HRS","sum.memory.tm.i.HRS","age_2010.HRS","female.HRS","blackrace.HRS","pedu_cat.HRS","edu_yrs.HRS","epinsurance_2010.HRS","age1stmarried.cat.HRS","marriage_2010.HRS","poverty_2010.i.HRS","occuskill_2010.HRS","srh.i.HRS","countCVD.HRS")

#merge original data on matching covariates into matched dataset (which includes imputations for some covariates to maximize N in the complete case matching analysis)
matchingtest<-left_join(matchingtest,NLSY.covs,by="NLSY_CASE_ID")
matchingtest<-left_join(matchingtest,HRS.covs,by="hhidpn")

#data check
summary(NLSY.covs$sum.memory.tm.i.NLSY)
summary(matchingtest$sum.memory.tm.i.NLSY)
summary(HRS.covs$sum.memory.tm.i.HRS)
summary(matchingtest$sum.memory.tm.i.HRS)
  #matches - good

table(matchingtest$cohort10)

################################################################################################################################################################################################################################################################
#PART II. PERFORM MATCHING (USE MATCHIT PACKAGE TO FACILITATE)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#A. IMPLEMENT MATCHING

#Through matching, we identify closest NLSY-HRS parts whose data we will link
#Essentially functions as a hot deck imputation; we treat HRS as "treated" and NLSY79 as our "control"; eventually we manipulate the output matched dataset so that each HRS participant is the recipient of 20 NLSY79 donor income volatility exposures

#Matching using MatchIt - make "cohort" the "treatment"
set.seed(12345)
m.out <- matchit(cohort10 ~ income.tm.i + sum.memory.tm.i + age_2010 + female + blackrace + pedu_cat + edu_yrs + epinsurance_2010 + age1stmarried.cat + marriage_2010 + poverty_2010.i + occuskill_2010 + srh.i + countCVD,
                 data = matchingtest,
                 method = "nearest",
                 #caliper = 0.05,
                 distance = "scaled_euclidean",
                 #link="linear",
                 estimand = "ATT",
                 replace = TRUE,
                 reuse.max = 64, #minimum value of reuse.max that maximizes # of participants with 20 matches
                 ratio = 20,
                 #min.controls=20, #for full matching
                 drop.unmatched = TRUE,
                 exact = c("cog.i.quart","inc.i.quart")) #coarsened the memory and income variables so could better match on them at age ~50
summary(m.out, un=FALSE)

#64=number of reuse.max required (minimum number for all 20 datasets to have HRS sample size)

#For best approach (exact match with reuse.max=64 for "cog.median.i","inc.i.quart")
#Sample Sizes:
#  Control Treated
#All            2607.     4922
#Matched (ESS)  1926.65   4922
#Matched        2587.     4922
#Unmatched        20.        0
#Discarded         0.        0

#other matching approaches lead to unmatched HRS participants, smaller effective sample sizes, etc. 
#go head with quartiles of cognition, quartiles of income - the key midlife matching variables 

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#B. EVALUATE BALANCE PRE/POST-MATCHING WITH LOVE PLOT

#label love plot with value labels instead of variable category labels
new.names <- c(income.tm.i = "Income",
               sum.memory.tm.i = "Midlife episodic memory function raw score",
               age_2010  = "Age",
               female_Male  = "Male",
               "poverty_2010.i_Yes" = "Living in poverty",
               "poverty_2010.i_No" = "Not living in poverty",
               "occuskill_2010_not working" = "Occupation: not working",
               "occuskill_2010_higher skill" = "Occupation: higher skill",
               "occuskill_2010_lower skill" = "Occupation: lower skill",
               "occuskill_2010_missing" = "Occupation: missing",
               "blackrace_Not Black" = "Not Black",
               "blackrace_Black" = "Black",
               "pedu_cat_<8 years"="Parent education: <8 years",
               "pedu_cat_>12 years"="Parent education: >12 years",
               "pedu_cat_12 years"="Parent education: 12 years",
               "pedu_cat_8-<12 years"="Parent education: 8-<12 years",
               "pedu_cat_Missing"="Parent education: Missing",
               edu_yrs = "Participant years of education",
               epinsurance_2010_Missing="Health insurance: Missing",
               epinsurance_2010_No="Health insurance: Not employer-provided",
               epinsurance_2010_Yes="Health insurance: Employer-provided",
               "age1stmarried.cat_<20" = "Age at 1st marriage: <20",
               "age1stmarried.cat_20-29" = "Age at 1st marriage: 20-29",
               "age1stmarried.cat_30-39" = "Age at 1st marriage: 30-39",
               "age1stmarried.cat_40+" = "Age at 1st marriage: 40+",
               "age1stmarried.cat_missing" = "Age at 1st marriage: missing",
               "age1stmarried.cat_never married" = "Age at 1st marriage: never married",
               "marriage_2010_married/partnered" = "Marital status 2010: married/partnered",
               "marriage_2010_never married" = "Marital status 2010: never married/partnered",
               "marriage_2010_separated/divorced/widowed" = "Marital status 2010: separated/divorced/widowed",
               srh.i = "Self-rated health",
               countCVD = "Number of doctor diagnosed CVDs",
               cog.i.quart = "Average memory score (quartiles)",
               inc.i.quart = "Average income (quartiles)")

#make love plot publication worthy
pdf(file = "/Users/xxxxx/code/For publication/Results/Appendix_Figure1.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 7)

set.cobalt.options(binary = "std")
love.plot(bal.tab(m.out), 
          #stats = "mean.diffs", 
          drop.distance = TRUE, 
          binary = "std",
          stars = "std",
          var.names = new.names,
          var.order = "unadjusted",
          sample.names = c("Pre-matching", "Matched/Synthetic cohort"),
          colors = c("darkgoldenrod", "cornflowerblue"),
          #shapes = c("circle filled","circle"),
          position = "right",
          thresholds = c(m = .1))

dev.off()

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#C. CONVERT MATCHIT RESULTS TO DATA FRAME IDENTIFYING MATCHED PAIRS AND ASSIGN RESAMPLE ID TO ALL NLSY-HRS PAIRS (I.E., SYNTHETIC INDIVIDUALS WITH LIFE COURSE DATA)

matched.data<-as.data.frame(get_matches(m.out))
names(m.out) 

#how many times each control used (because of replacement)
sum(table(unique(matched.data$NLSY_CASE_ID))) #of unique NLSY "controls" used - matches what we saw above (N=2587)
table(matched.data$NLSY_CASE_ID) #with reuse.max option, control can only be used up to 64 times before can't be used again
  table(matched.data$subclass) #21 in a subclass = the HRS participant and their 20 NLSY matches  

test<-matched.data[,c("subclass","weights","NLSY_CASE_ID","income90.adj90","hhidpn","r10iwstat")]
  sum(table(unique(test$NLSY_CASE_ID))) #n is as expected (2587 unique NLSY79 IDs used; makes sense, 2587(used) + 20(unused) = 2607 = NLSY sample size)
  sum(table(unique(test$hhidpn))) #n is size of HRS (4922)
  sum(table(unique(test$subclass[(table(test$subclass)==21)==TRUE]))) #all HRS participants were assigned 20 matches

#store names of all variables we want to fill up or down a column to complete the outcomes/covariates for each synthetic individual
outcomes.covs<-c("hhidpn","income.tm.i.hrs","suminc90to10","delrecall.tm.i.NLSY","delrecall.tm.i.HRS","immedrecall.tm.i.NLSY","immedrecall.tm.i.HRS","avg.imp.mem.NLSY","sum.imp.mem.NLSY","avg.imp.mem.HRS","sum.imp.mem.HRS","sum.memory.tm.i.z.nlsy","sum.memory.tm.i.z.hrs","avg.memory.tm.i.z.nlsy","avg.memory.tm.i.z.hrs",
                 "memimp95","memimp96","memimp98","memimp00","memimp02","memimp04","memimp06","memimp08","memimp10","memimp12","memimp14","memimp16","memimp18","memimp20",
                 "AFQTpctlscore06rev.81.r.NLSY","income.tm.i.NLSY","sum.memory.tm.i.NLSY","age_2010.NLSY","female.NLSY","blackrace.NLSY","pedu_cat.NLSY","edu_yrs.NLSY","epinsurance_2010.NLSY","age1stmarried.cat.NLSY","marriage_2010.NLSY","poverty_2010.i.NLSY","occuskill_2010.NLSY","srh.i.NLSY","countCVD.NLSY",
                 "income.tm.i.HRS","sum.memory.tm.i.HRS","age_2010.HRS","female.HRS","blackrace.HRS","pedu_cat.HRS","edu_yrs.HRS","epinsurance_2010.HRS","age1stmarried.cat.HRS","marriage_2010.HRS","poverty_2010.i.HRS","occuskill_2010.HRS","srh.i.HRS","countCVD.HRS")

#want to fill down outcomes/covariates of interest in matched dataset (especially for HRS participants) - enables us to compare
test2 = matched.data %>% 
  group_by(subclass) %>% #fills up/down as appropriate within a subclass (e.g., all NSLY79 participants within a subclass given the HRS outcome value of their HRS pair; think of it as imputing exposure for HRS participants 20 times)
  fill(outcomes.covs, .direction = 'updown') %>%
  ungroup()
#open test2 and skim last page of columns...should all be filled now (up or down, the program knows; no NAs now)
#Why are we doing this? Because we want to make sure HRS outcomes (e.g., "sum.imp.mem.HRS") are used when doing validation outcome analyses in the synthetic cohort

#check data structure is maintained after manipulation
test3<-test2[!is.na(test2$NLSY_CASE_ID),] #now everyone is matched and no excess rows
  sum(table(unique(test3$NLSY_CASE_ID))) #2587 still - good!
  sum(table(unique(test3$hhidpn))) #4922 still - good!

#Resample each participant from a NLSY(20):HRS(1) subclass into 1 of 20 new datasets - here randomly assign people a value of 1-20 (these are the dataset ids...like you would get from an imputation dataset)
set.seed(12345)
test4 = test3 %>% 
  group_by(subclass) %>%
  mutate(ticker=1) %>%
  mutate(sampid = cumsum(as.numeric(ticker))) %>%
  mutate(maxmatch=max(sampid)) %>%
  mutate(resampleid = sample(row_number())) %>% 
  #summarise(sampid = sample(1:20,1,replace=FALSE)) %>%
  #add_column(sampid = sample(1:nrow(.),nrow(.),replace=FALSE)) %>%
  ungroup()

table(test4$resampleid) #good, code worked; for a given HRS participant, each NLSY-HRS pairing was resampled into 1 of 20 datasets (i.e., assigned a random discrete number 1 to 20)

#Rename new stacked dataframe 
match20<-test4

#save dataset so don't have to repeat steps above later on
#write.csv(match20,"/Users/xxxxx/code/For publication/Data/match20_Feb2025_7529.csv")

################################################################################################################################################################################################################################################################
#PART II. EXIT MATCHING FRAMEWORK AND MOVE INTO IMPUTATION FRAMEWORK

#Conduct study analyses in synthetic cohort (the 20 cohorts created via resampling of the matched pairs)
#Combine estimates across datasets using Rubin's Rules

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#A. RECREATE SD UNIT OF EXPOSURE (SD_PCT) IN MATCHED SAMPLE (EXPOSURE = SD_INCVOL)

#the existing sd_incvol exposure variable represents a 1 SD increase in the sd_pct variable for analyses...using the SD from income volatility prior to matching
#should be really close regardless, but if we want interpretation to truly be per 1 SD increase we should redo it in the matched sample

#distribution of exposures in NLSY before matching (to report in Results)
  summary(matchingtest$sd_incvol)
  summary(matchingtest$sd_pct, na.rm=TRUE) 
  sd(matchingtest$sd_pct, na.rm=TRUE) 
  table(matchingtest$num.drops4)
  prop.table(table(matchingtest$num.drops4))

summary(match20$sd_incvol)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.02363 0.68549 1.15112 1.44713 2.03631 5.76782   

#distributions of exposures in synthetic cohort (to report in Results)
  summary(match20$sd_pct)
  sd(match20$sd_pct) 
  summary(match20$sd_pct/sd(match20$sd_pct))
  prop.table(table(match20$num.drops4))

#recreate sd unit increase in matched sample (will/should be super similar though)
match20$sd_incvol.m<-match20$sd_pct/sd(match20$sd_pct)
summary(match20$sd_incvol.m)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.02436 0.70654 1.18645 1.49155 2.09882 5.94488

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#B. USING RESAMPLEID CREATE 20 DATASETS FOR ANALYSIS 

#Again, we're doing this to account for the uncertainty involved in identifying NLSY-HRS pairs that represent the full life course of a synthetic individual
#We are essentially imputing exposure through this procedure, so like we would with multiple imputation (to account for uncertainty) we do that here with a 1:20 matching ratio
#Because we assigned the pairs a resample id within a subclass (i.e., each of the 20 NLSY pairings for a given HRS participant) we can redistribute the pairs into 20 datasets (i.e., 20 synthetic cohorts), perform analyses within each, and then combine estimates across them

#create the 20 resampled dataframes
data1<-match20[match20$resampleid==1,]
data2<-match20[match20$resampleid==2,]
data3<-match20[match20$resampleid==3,]
data4<-match20[match20$resampleid==4,]
data5<-match20[match20$resampleid==5,]
data6<-match20[match20$resampleid==6,]
data7<-match20[match20$resampleid==7,]
data8<-match20[match20$resampleid==8,]
data9<-match20[match20$resampleid==9,]
data10<-match20[match20$resampleid==10,]
data11<-match20[match20$resampleid==11,]
data12<-match20[match20$resampleid==12,]
data13<-match20[match20$resampleid==13,]
data14<-match20[match20$resampleid==14,]
data15<-match20[match20$resampleid==15,]
data16<-match20[match20$resampleid==16,]
data17<-match20[match20$resampleid==17,]
data18<-match20[match20$resampleid==18,]
data19<-match20[match20$resampleid==19,]
data20<-match20[match20$resampleid==20,]
  #because of how we set up/specified matching, each should be the sample size of the HRS cohort - check environment window - yes!

#are there people in the new datasets with repeated measures?  
sum(table(unique(data10$hhidpn))) #every unique HRS participant - makes sense
sum(table(unique(data10$NLSY_CASE_ID))) #only 1940 unique NLSY participants in dataset 10 
sum(table(unique(data2$NLSY_CASE_ID))) #only 1935 unique NLSY participants in dataset 2 
  #This aligns with "effective sample size" above in matchit summary
  #Effective sample size decreases when multiple of the same "controls" aka NLSY exposure trajectories are used
  #NLSY individuals provide exposure information
  #This should not pose an issue for results (repeated use of a given NLSY participants *exposure*)
  #That is, just as in a multiple imputation model multiple people could be assigned the same value of a covariate based on having similar values of other covariates...that is essentially what is happening here

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#C. PERFORM MULTIPLE IMPUTATION ANALYSES ON VALIDATION OUTCOME (RAW AND Z-SCORES FOR SUM.MEMORY IN 2010)

lm.datasets<-list(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,
                  data11,data12,data13,data14,data15,data16,data17,data18,data19,data20)

#use the sum as the outcome variable of interest here and make sure for synthetic cohort analyses, the .HRS outcome is used
summary(data10$sum.imp.mem.HRS) 
summary(data10$sum.imp.mem.NLSY)

#distribution is quite similar between the two cohorts which is good

#Here, the .NLSY variable is the true midlife cognition value for the exposure trajectories and .HRS is the midlife outcome of the HRS person/the synthetic person
#To produce synthetic cohort results, we use/care about .HRS outcome(s)
#For this analyses, the ultimate outcome of interest is memory decline (using imputed memory scores) from 2010-2010

#We can do two validation/data checks here
  #1) Compare synthetic cohort results for the association between income volatility and sum.imp.mem.HRS and sum.imp.mem.z.HRS with those in the NLSY cohort pre-matching
      #this tells us if matching "worked" or if the synthetic cohort association is far off from the NLSY "truth" (i.e., the association in NLSY before matching)
  #2) Compare synthetic cohort results for the association between income volatility and sum.imp.mem.NLSY and sum.imp.mem.z.NLSY with those in the NLSY cohort pre-matching
      #this tells us if the way we matched really altered findings for the NLSY cohort (comparing how the association between income volatility and 2010 memory different within NLSY before and after matching)

#...............................................................................................................................................................................................................................................................
#C.1 Validation outcome in synthetic cohort (volatility->memory(hrs))

#First try hard-coding Rubin's rules and then compare with build in mi functions

#testing what to pull out and store as results/how to index to set up code for entire list, below
#in analyses, adjust for # contributed income variables indicator, age, sex/gender, race, years of education, parental early-life education, and age at first marriage (the confounders we are able to match on)
names(summary(lm(sum.imp.mem.HRS ~ sd_incvol.m + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat,
                 data = data1)))
fit.test<-summary(lm(sum.imp.mem.HRS ~ sd_incvol.m + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat,
                     data = data1))$coefficient

#set up object to store results
out.coef <- vector('list', 20)
out.std <- vector('list', 20)

#Testing for raw scores on memory (sum) as outcome; store results
for (i in seq_along(lm.datasets)){
  out.coef[[i]] <- summary(lm(sum.imp.mem.HRS ~ sd_incvol.m + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat,
                              data = lm.datasets[[i]]))$coefficients[2,1]
  out.std[[i]] <- summary(lm(sum.imp.mem.HRS ~ sd_incvol.m + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat,
                             data = lm.datasets[[i]]))$coefficients[2,2]}

#https://bookdown.org/mwheymans/bookmi/rubins-rules.html

#C.1.a raw score outcome for sd_invol exposure 
#combine estimates for sd_incvol across 20 dataseets first using rubin's rules (hand compute)
  m<-20
  inc.betas<-unlist(out.coef)
  Qmean.inc<-mean(inc.betas)
  vars<-(unlist(out.std))^2
  WVmean.inc<-mean(vars)
  BV.inc<-(1/(m-1))*(sum((inc.betas-Qmean.inc)^2))
  TV.inc<-WVmean.inc+((1+(1/m))*BV.inc)
  
  ucl<-Qmean.inc+1.96*(sqrt(TV.inc))
  lcl<-Qmean.inc-1.96*(sqrt(TV.inc))
  cbind(Qmean.inc,lcl,ucl)

#Compare with function MIcombine
  imputed.data<-imputationList(lm.datasets)
  models <- with(imputed.data, lm(sum.imp.mem.HRS ~ sd_incvol.m + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat))
  MIcombine(models,conf.int = TRUE, conf.level = 0.95)
  confint(MIcombine(models))
    cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])

#Get the same result with much less code - use this going forward for synthetic cohort analyses

#Set up an empty data.frame to store results in
  synthcohort.results.table<-as.data.frame(matrix(nrow=26,ncol=4))
  colnames(synthcohort.results.table)<-c("Analysis", "Beta", "LCL", "UCL")
  synthcohort.results.table[1]<-c("sd_invcol.m_mem_raw_synthcohort","numdrops1_mem_raw_synthcohort","numdrops2_mem_raw_synthcohort","numdrops3+_mem_raw_synthcohort",
                                  "sd_invcol.m_mem_z_synthcohort","numdrops1_mem_z_synthcohort","numdrops2_mem_z_synthcohort","numdrops3+_mem_z_synthcohort",
                                  "sd_invcol_mem_raw_NLSYpostmatch","numdrops1_mem_raw_NLSYpostmatch","numdrops2_mem_raw_NLSYpostmatch","numdrops3+_mem_raw_NLSYpostmatch",
                                  "sd_invcol_mem_z_NLSYpostmatch","numdrops1_mem_z_NLSYpostmatch","numdrops2_mem_z_NLSYpostmatch","numdrops3+_mem_z_NLSYpostmatch",
                                  "sd_invcol.m_memdecline_synthcohort","time(sdincvol)_memdecline_synthcohort","sd_invcol.m*time_memdecline_synthcohort",
                                  "numdrops1_memdecline_synthcohort","numdrops2_memdecline_synthcohort","numdrops3+_memdecline_synthcohort","time(numdrops)_memdecline_synthcohort","numdrops1*time_memdecline_synthcohort","numdrops2*time_memdecline_synthcohort","numdrops3+*time_memdecline_synthcohort"
                                  )
  
#store results from C.1.a
synthcohort.results.table[1,2:4]<-cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])

#C.1.b raw score outcome for num.drops4 exposure 
  imputed.data<-imputationList(lm.datasets)
  models <- with(imputed.data, lm(sum.imp.mem.HRS ~ num.drops4 + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat))
  MIcombine(models,conf.int = TRUE, conf.level = 0.95)
  confint(MIcombine(models))
    cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])
    cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[3],confint(MIcombine(models))[3,1],confint(MIcombine(models))[3,2])
    cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[4],confint(MIcombine(models))[4,1],confint(MIcombine(models))[4,2])

  synthcohort.results.table[2:4,2:4]<-rbind(cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2]),
                                      cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[3],confint(MIcombine(models))[3,1],confint(MIcombine(models))[3,2]),
                                      cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[4],confint(MIcombine(models))[4,1],confint(MIcombine(models))[4,2])
  )

#similar estimates -  confidence intervals in synthetic cohort estimate cover the truth

#C.1.c z-score outcome for sd_incvol exposure
  imputed.data<-imputationList(lm.datasets)
  models <- with(imputed.data, lm(sum.memory.tm.i.z.hrs ~ sd_incvol.m + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat))
  MIcombine(models,conf.int = TRUE, conf.level = 0.95)
  confint(MIcombine(models))
    cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])
  
  synthcohort.results.table[5,2:4]<-cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])

#C.1.d z-score outcome for num.drops4 exposure
  imputed.data<-imputationList(lm.datasets)
  models <- with(imputed.data, lm(sum.memory.tm.i.z.hrs ~ num.drops4 + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat))
  MIcombine(models,conf.int = TRUE, conf.level = 0.95)
  confint(MIcombine(models))
    cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])
    cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[3],confint(MIcombine(models))[3,1],confint(MIcombine(models))[3,2])
    cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[4],confint(MIcombine(models))[4,1],confint(MIcombine(models))[4,2])
  
  synthcohort.results.table[6:8,2:4]<-rbind(cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2]),
                                  cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[3],confint(MIcombine(models))[3,1],confint(MIcombine(models))[3,2]),
                                  cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[4],confint(MIcombine(models))[4,1],confint(MIcombine(models))[4,2])
  )

#really looks like we're doing a pretty good job with these validation analyses!
#the confidence intervals in synthetic cohort estimate cover the truth in all cases and we wouldn't arrive at different conclusions using synthetic cohort vs. benchmark NLSY sample for memory function in 2010 analyses
  
#...............................................................................................................................................................................................................................................................
#C.2 Compare results using NLSY outcome AFTER matching to pre-matching NLSY results - for validation we want to compare with NLSY outcome pre-matched; comparing pre- and post-matched NLSY estimates helps us get a sense of how much exposure sample composition shifts after matching

#C.2.a raw score outcome for sd_invol.m exposure 
  imputed.data<-imputationList(lm.datasets)
  models <- with(imputed.data, lm(sum.imp.mem.NLSY ~ sd_incvol.m + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat))
  MIcombine(models,conf.int = TRUE, conf.level = 0.95)
  confint(MIcombine(models))
    cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])

  synthcohort.results.table[9,2:4]<-cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])

#C.2.b raw score outcome for num.drops4 exposure 
  imputed.data<-imputationList(lm.datasets)
  models <- with(imputed.data, lm(sum.imp.mem.NLSY ~ num.drops4 + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat))
  MIcombine(models,conf.int = TRUE, conf.level = 0.95)
  confint(MIcombine(models))
    cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])
    cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[3],confint(MIcombine(models))[3,1],confint(MIcombine(models))[3,2])
    cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[4],confint(MIcombine(models))[4,1],confint(MIcombine(models))[4,2])
  
  synthcohort.results.table[10:12,2:4]<-rbind(cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2]),
                                  cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[3],confint(MIcombine(models))[3,1],confint(MIcombine(models))[3,2]),
                                  cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[4],confint(MIcombine(models))[4,1],confint(MIcombine(models))[4,2])
  )
  
#the sd_invol.m associations are pretty similar in the NLSY cohort before and after matching (a little higher after) - matching has not altered much!
#the num.drops results are a little less similar, especially at the highest num.drops category (an underestimate) but probably okay...more conservatives estimates produced in synthetic cohort (and all confidence intervals cover the pre-matched truth)
  
##C.2.c z-score outcome for sd_incvol exposure
  imputed.data<-imputationList(lm.datasets)
  models <- with(imputed.data, lm(sum.memory.tm.i.z.nlsy ~ sd_incvol.m + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat))
  MIcombine(models,conf.int = TRUE, conf.level = 0.95)
  confint(MIcombine(models))
    cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])
  
  synthcohort.results.table[13,2:4]<-cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])
  
##C.2.d z-score outcome for num.drops4 exposure
  imputed.data<-imputationList(lm.datasets)
  models <- with(imputed.data, lm(sum.memory.tm.i.z.nlsy ~ num.drops4 + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat))
  MIcombine(models,conf.int = TRUE, conf.level = 0.95)
  confint(MIcombine(models))
    cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])
    cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[3],confint(MIcombine(models))[3,1],confint(MIcombine(models))[3,2])
    cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[4],confint(MIcombine(models))[4,1],confint(MIcombine(models))[4,2])
  
  synthcohort.results.table[14:16,2:4]<-rbind(cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2]),
                                  cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[3],confint(MIcombine(models))[3,1],confint(MIcombine(models))[3,2]),
                                  cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[4],confint(MIcombine(models))[4,1],confint(MIcombine(models))[4,2])
  )
  

#this all looks pretty good - move ahead with memory decline analyses in the synthetic cohort

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#D. PERFORM MULTIPLE IMPUTATION ANALYSES ON MEMORY DECLINE OUTCOME (USING IMPUTED MEMORY SCORES UP TO 2020)

#a) Create practice effects variable
    #typically, the variable is coded as "1" for the first time a participant takes the test and "0" for subsequent test administrations. 
#b) Standardize memory scores to baseline of the analytic sample (2010)
#c) Data check: make sure again here that everyone has at least one cog

for (i in seq_along(lm.datasets)){
  lm.datasets[[i]]$memimp10z<-(lm.datasets[[i]]$memimp10-mean(lm.datasets[[i]]$memimp10, na.rm=TRUE))/sd(lm.datasets[[i]]$memimp10, na.rm=TRUE)
  lm.datasets[[i]]$memimp12z<-(lm.datasets[[i]]$memimp12-mean(lm.datasets[[i]]$memimp10, na.rm=TRUE))/sd(lm.datasets[[i]]$memimp10, na.rm=TRUE)
  lm.datasets[[i]]$memimp14z<-(lm.datasets[[i]]$memimp14-mean(lm.datasets[[i]]$memimp10, na.rm=TRUE))/sd(lm.datasets[[i]]$memimp10, na.rm=TRUE)
  lm.datasets[[i]]$memimp16z<-(lm.datasets[[i]]$memimp16-mean(lm.datasets[[i]]$memimp10, na.rm=TRUE))/sd(lm.datasets[[i]]$memimp10, na.rm=TRUE)
  lm.datasets[[i]]$memimp18z<-(lm.datasets[[i]]$memimp18-mean(lm.datasets[[i]]$memimp10, na.rm=TRUE))/sd(lm.datasets[[i]]$memimp10, na.rm=TRUE)
  lm.datasets[[i]]$memimp20z<-(lm.datasets[[i]]$memimp20-mean(lm.datasets[[i]]$memimp10, na.rm=TRUE))/sd(lm.datasets[[i]]$memimp10, na.rm=TRUE)
  
  #indicator for how many people will contribute to analyses 
  lm.datasets[[i]]$atleast1cog<-ifelse(is.na(lm.datasets[[i]]$memimp10)&is.na(lm.datasets[[i]]$memimp12)&is.na(lm.datasets[[i]]$memimp14)&is.na(lm.datasets[[i]]$memimp16)&is.na(lm.datasets[[i]]$memimp18)&is.na(lm.datasets[[i]]$memimp20),0,1)
}
  
summary(lm.datasets[[i]]$memimp10z)
summary(lm.datasets[[i]]$memimp12z)
summary(lm.datasets[[i]]$memimp14z)
summary(lm.datasets[[i]]$memimp16z)
summary(lm.datasets[[i]]$memimp18z)
summary(lm.datasets[[i]]$memimp20z)

table(lm.datasets[[i]]$atleast1cog) #should be everyone (already performed this exclusion) - good!

#convert data1-data20 into long dataframes 
memcol<-c(1995, seq(1996,2020,2)) #converting all but really only using 2010-2020 - but fine, delete in later step
colnames(lm.datasets[[i]])#just go ahead and rename the col names to the corresponding waveyrs so that I don't have to recode that later
colnames(lm.datasets[[i]])[c(801:808,1045:1050)] #identify the memory outcomes variables over time; okay if pre 2010 is not standardized, we remove that later
data.long <- lm.datasets

#create time variable
for (i in seq_along(data.long)){
  colnames(data.long[[i]])[c(801:808,1045:1050)]<-memcol
  data.long[[i]] = data.long[[i]] %>% 
  pivot_longer(
    cols = c(801:808,1045:1050), 
    names_to = "WAVEID_N",
    values_to = "memory_imp")
  data.long[[i]]$time<-ifelse(data.long[[i]]$WAVEID_N=="2010",0,
                            ifelse(data.long[[i]]$WAVEID_N=="2012",2,
                                   ifelse(data.long[[i]]$WAVEID_N=="2014",4,
                                          ifelse(data.long[[i]]$WAVEID_N=="2016",6,
                                                 ifelse(data.long[[i]]$WAVEID_N=="2018",8,
                                                        ifelse(data.long[[i]]$WAVEID_N=="2020",10,NA))))))
}

for (i in seq_along(data.long)){
  data.long[[i]] = data.long[[i]] %>%
  filter(!is.na(memory_imp)) %>% 
  group_by(id) %>% 
  mutate(n = row_number(),
         first_cog = ifelse(n == 1, 1, 0)) %>% 
  select(-n) %>% 
  ungroup()
}

for (i in seq_along(data.long)){
  data.long[[i]]<-data.long[[i]][!is.na(data.long[[i]]$time),] #don't have to do this, lmer would ignore the NAs for time - but cleaner this way maybe
}

table(data.long[[i]]$first_cog) #enough # to adjust for practice effects

#look at summary of follow-up time
  #average contributed follow-up time for each person (first ask for the maximum time value - since we eliminated time where memory was missing, will only return the max time value = last time person contributed a memory score between 2010-2020)
  summary(as.vector(aggregate(data.long[[i]][,c("time")], list(data.long[[i]]$id), max)[2]))
    #on average, participants 9.1 years of follow-up between 2010-2020

#...............................................................................................................................................................................................................................................................
#D.1 Memory decline analyses in synthetic cohort (sd_incvol.m exposure)

#First, create variables/relevel variables we need for LMM analyses
for (i in seq_along(data.long)){
  data.long[[i]]$num.drops4<-factor(data.long[[i]]$num.drops4, levels = c("0", "1", "2", ">=3"))
  data.long[[i]]$age_2010_cent50<-data.long[[i]]$age_2010-50
  data.long[[i]]$edu_yrs_cent12<-data.long[[i]]$edu_yrs-12
  data.long[[i]]$suminc90to10_cent<-data.long[[i]]$suminc90to10-13
  data.long[[i]]$AFQT_cent<-data.long[[i]]$AFQTpctlscore06rev.81.r-50
  data.long[[i]]$blackrace<-relevel(as.factor(data.long[[i]]$blackrace), ref="Not Black")
  data.long[[i]]$female<-relevel(as.factor(data.long[[i]]$female), ref="Female")
  data.long[[i]]$pedu_cat<-relevel(as.factor(data.long[[i]]$pedu_cat), ref="12 years")
  data.long[[i]]$age1stmarried.cat<-relevel(as.factor(data.long[[i]]$age1stmarried.cat), ref="20-29")
}

#check re-leveling etc.
table(data.long[[1]]$num.drops4)  
summary(data.long[[i]]$age_2010_cent50)
summary(data.long[[i]]$edu_yrs_cent12)
summary(data.long[[i]]$suminc90to10_cent)
levels(data.long[[i]]$blackrace)  
levels(data.long[[i]]$female)  
levels(data.long[[i]]$pedu_cat)  
levels(data.long[[i]]$pedu_cat)  

#test for one dataset
  summary(lmer(memory_imp ~ sd_incvol.m*time + first_cog + suminc90to10_cent + age_2010_cent50 + as.factor(female)*time + edu_yrs_cent12*time + pedu_cat + age1stmarried.cat + (1|hhidpn),
               data = data.long[[1]]))
  summary(lmer(memory_imp ~ sd_incvol.m*time + first_cog + suminc90to10_cent + age_2010_cent50 + as.factor(female)*time + blackrace*time + edu_yrs_cent12*time + pedu_cat + age1stmarried.cat + (1|hhidpn),
               data = data.long[[1]]))$coefficients
  fit.lmer<-lmer(memory_imp ~ sd_incvol.m*time + first_cog + suminc90to10_cent + age_2010_cent50 + as.factor(female)*time + blackrace*time + edu_yrs_cent12*time + pedu_cat + age1stmarried.cat + (1|hhidpn),
                 data = data.long[[1]])
  sigma(fit.lmer)

#convergence issues with random slope ; results are nearly identical with/without random slope...remove random slope

#run imputation model
  imputed.data.long<-imputationList(data.long)
  models <- with(imputed.data.long, lmer(memory_imp ~ sd_incvol.m*time + first_cog + suminc90to10_cent + age_2010_cent50 + blackrace*time + female*time + edu_yrs_cent12*time + pedu_cat + age1stmarried.cat + (1|hhidpn)))

#extract just fixed effects betas and variance components: https://stat.ethz.ch/pipermail/r-sig-mixed-models/2011q1/015765.html
  betas <- MIextract(models, fun=fixef)
  vars <- MIextract(models, fun=vcov)
  vars2 <- list()
  for ( i in 1:20) { vars2[[i]] <- as.matrix(vars[[i]])  }
  MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)
  confint(MIcombine(betas,vars2))

#use mi moving forward
synthcohort.results.table[17:19,2:4]<-rbind(cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(betas,vars2))[2,1],confint(MIcombine(betas,vars2))[2,2]),
                                cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[3],confint(MIcombine(betas,vars2))[3,1],confint(MIcombine(betas,vars2))[3,2]),
                                cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[19],confint(MIcombine(betas,vars2))[19,1],confint(MIcombine(betas,vars2))[19,2])
)

#...............................................................................................................................................................................................................................................................
#D.2 Memory decline analyses in synthetic cohort (num.drops4 exposure)

imputed.data.long<-imputationList(data.long)
models.drops <- with(imputed.data.long, lmer(memory_imp ~ as.factor(num.drops4)*time + first_cog + suminc90to10_cent + age_2010_cent50 + female*time + blackrace*time + edu_yrs_cent12*time + pedu_cat + age1stmarried.cat + (1|hhidpn)))

betas <- MIextract(models.drops, fun=fixef)
vars <- MIextract(models.drops, fun=vcov)
vars2 <- list()
for ( i in 1:20) { vars2[[i]] <- as.matrix(vars[[i]])  }
MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)
confint(MIcombine(betas,vars2))

synthcohort.results.table[20:26,2:4]<-rbind(cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(betas,vars2))[2,1],confint(MIcombine(betas,vars2))[2,2]),
                                 cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[3],confint(MIcombine(betas,vars2))[3,1],confint(MIcombine(betas,vars2))[3,2]),
                                 cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[4],confint(MIcombine(betas,vars2))[4,1],confint(MIcombine(betas,vars2))[4,2]),
                                 cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[5],confint(MIcombine(betas,vars2))[5,1],confint(MIcombine(betas,vars2))[5,2]),
                                 cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[21],confint(MIcombine(betas,vars2))[21,1],confint(MIcombine(betas,vars2))[21,2]),
                                 cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[22],confint(MIcombine(betas,vars2))[22,1],confint(MIcombine(betas,vars2))[22,2]),
                                 cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[23],confint(MIcombine(betas,vars2))[23,1],confint(MIcombine(betas,vars2))[23,2])
)

#write.csv(synthcohort.results.table,"/Users/xxxxx/code/For publication/Results/synthcohort_results.csv")

###################################################################################################################################################################################################################################################
#PART IV. SENSITIVITY ANALYSES

#A. Adjusting for AFQT

#Set up an empty data.frame to store sensitivity analysis results in
sensanalysis.AFQT.results.table<-as.data.frame(matrix(nrow=26,ncol=4))
colnames(sensanalysis.AFQT.results.table)<-c("Analysis", "Beta", "LCL", "UCL")
sensanalysis.AFQT.results.table[1]<-c("sd_invcol.m_mem_raw_synthcohort_AFQT","numdrops1_mem_raw_synthcohort_AFQT","numdrops2_mem_raw_synthcohort_AFQT","numdrops3+_mem_raw_synthcohort_AFQT",
                                "sd_invcol.m_mem_z_synthcohort_AFQT","numdrops1_mem_z_synthcohort_AFQT","numdrops2_mem_z_synthcohort_AFQT","numdrops3+_mem_z_synthcohort_AFQT",
                                "sd_invcol_mem_raw_NLSYpostmatch_AFQT","numdrops1_mem_raw_NLSYpostmatch_AFQT","numdrops2_mem_raw_NLSYpostmatch_AFQT","numdrops3+_mem_raw_NLSYpostmatch_AFQT",
                                "sd_invcol_mem_z_NLSYpostmatch_AFQT","numdrops1_mem_z_NLSYpostmatch_AFQT","numdrops2_mem_z_NLSYpostmatch_AFQT","numdrops3+_mem_z_NLSYpostmatch_AFQT",
                                "sd_invcol.m_memdecline_synthcohort_AFQT","time(sdincvol)_memdecline_synthcohort_AFQT","sd_invcol.m*time_memdecline_synthcohort_AFQT",
                                "numdrops1_memdecline_synthcohort_AFQT","numdrops2_memdecline_synthcohort_AFQT","numdrops3+_memdecline_synthcohort_AFQT","time(numdrops)_memdecline_synthcohort_AFQT","numdrops1*time_memdecline_synthcohort_AFQT","numdrops2*time_memdecline_synthcohort_AFQT","numdrops3+*time_memdecline_synthcohort_AFQT"
)
 
#A.1 raw score outcome for sd_incvol.m exposure 
imputed.data<-imputationList(lm.datasets)
models <- with(imputed.data, lm(sum.imp.mem.HRS ~ sd_incvol.m + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat + AFQTpctlscore06rev.81.r))
MIcombine(models,conf.int = TRUE, conf.level = 0.95)
confint(MIcombine(models))
cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])
sensanalysis.AFQT.results.table[1,2:4]<-cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])

#A.2 raw score outcome for num.drops4 exposure 
imputed.data<-imputationList(lm.datasets)
models <- with(imputed.data, lm(sum.imp.mem.HRS ~ num.drops4 + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat + AFQTpctlscore06rev.81.r))
MIcombine(models,conf.int = TRUE, conf.level = 0.95)
confint(MIcombine(models))
sensanalysis.AFQT.results.table[2:4,2:4]<-rbind(cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2]),
                                          cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[3],confint(MIcombine(models))[3,1],confint(MIcombine(models))[3,2]),
                                          cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[4],confint(MIcombine(models))[4,1],confint(MIcombine(models))[4,2])
)

#A.3 z-score outcome for sd_incvol exposure
imputed.data<-imputationList(lm.datasets)
models <- with(imputed.data, lm(sum.memory.tm.i.z.hrs ~ sd_incvol.m + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat  + AFQTpctlscore06rev.81.r))
MIcombine(models,conf.int = TRUE, conf.level = 0.95)
confint(MIcombine(models))
sensanalysis.AFQT.results.table[5,2:4]<-cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])

#A.4 z-score outcome for num.drops4 exposure
imputed.data<-imputationList(lm.datasets)
models <- with(imputed.data, lm(sum.memory.tm.i.z.hrs ~ num.drops4 + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat + AFQTpctlscore06rev.81.r))
MIcombine(models,conf.int = TRUE, conf.level = 0.95)
confint(MIcombine(models))
sensanalysis.AFQT.results.table[6:8,2:4]<-rbind(cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2]),
                                          cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[3],confint(MIcombine(models))[3,1],confint(MIcombine(models))[3,2]),
                                          cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[4],confint(MIcombine(models))[4,1],confint(MIcombine(models))[4,2])
)

#...............................................................................................................................................................................................................................................................
#Compare results using NLSY outcome AFTER matching to pre-matching NLSY results - for validation we want to compare with NLSY outcome pre-matched; comparing pre- and post-matched NLSY estimates helps us get a sense of how much exposure sample composition shifts after matching

#A.5 Raw score outcome for sd_invol.m exposure 
imputed.data<-imputationList(lm.datasets)
models <- with(imputed.data, lm(sum.imp.mem.NLSY ~ sd_incvol.m + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat + AFQTpctlscore06rev.81.r))
MIcombine(models,conf.int = TRUE, conf.level = 0.95)
confint(MIcombine(models))
cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])
sensanalysis.AFQT.results.table[9,2:4]<-cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])

#A.6 raw score outcome for num.drops4 exposure 
imputed.data<-imputationList(lm.datasets)
models <- with(imputed.data, lm(sum.imp.mem.NLSY ~ num.drops4 + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat + AFQTpctlscore06rev.81.r))
MIcombine(models,conf.int = TRUE, conf.level = 0.95)
confint(MIcombine(models))
sensanalysis.AFQT.results.table[10:12,2:4]<-rbind(cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2]),
                                            cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[3],confint(MIcombine(models))[3,1],confint(MIcombine(models))[3,2]),
                                            cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[4],confint(MIcombine(models))[4,1],confint(MIcombine(models))[4,2])
)

#A.7 z-score outcome for sd_incvol exposure
imputed.data<-imputationList(lm.datasets)
models <- with(imputed.data, lm(sum.memory.tm.i.z.nlsy ~ sd_incvol.m + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat + AFQTpctlscore06rev.81.r))
MIcombine(models,conf.int = TRUE, conf.level = 0.95)
confint(MIcombine(models))
cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])
sensanalysis.AFQT.results.table[13,2:4]<-cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2])

#A.8 z-score outcome for num.drops4 exposure
imputed.data<-imputationList(lm.datasets)
models <- with(imputed.data, lm(sum.memory.tm.i.z.nlsy ~ num.drops4 + suminc90to10 + age_2010 + female + blackrace + edu_yrs + pedu_cat + age1stmarried.cat + AFQTpctlscore06rev.81.r))
MIcombine(models,conf.int = TRUE, conf.level = 0.95)
confint(MIcombine(models))
sensanalysis.AFQT.results.table[14:16,2:4]<-rbind(cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(models))[2,1],confint(MIcombine(models))[2,2]),
                                            cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[3],confint(MIcombine(models))[3,1],confint(MIcombine(models))[3,2]),
                                            cbind(MIcombine(models,conf.int = TRUE, conf.level = 0.95)$coefficients[4],confint(MIcombine(models))[4,1],confint(MIcombine(models))[4,2])
)

#...............................................................................................................................................................................................................................................................
#Memory score analyses

#A.9 Memory decline analyses in synthetic cohort (sd_incvol.m exposure)

imputed.data.long<-imputationList(data.long)
models <- with(imputed.data.long, lmer(memory_imp ~ sd_incvol.m*time + first_cog + suminc90to10_cent + age_2010_cent50 + female*time + blackrace*time + edu_yrs_cent12*time + pedu_cat + age1stmarried.cat + AFQT_cent + (1|hhidpn)))

#how to extract just fixed effects betas and variance components: https://stat.ethz.ch/pipermail/r-sig-mixed-models/2011q1/015765.html
betas <- MIextract(models, fun=fixef)
vars <- MIextract(models, fun=vcov)
vars2 <- list()
for ( i in 1:20) { vars2[[i]] <- as.matrix(vars[[i]])  }
MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)
confint(MIcombine(betas,vars2))
sensanalysis.AFQT.results.table[17:19,2:4]<-rbind(cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(betas,vars2))[2,1],confint(MIcombine(betas,vars2))[2,2]),
                                            cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[3],confint(MIcombine(betas,vars2))[3,1],confint(MIcombine(betas,vars2))[3,2]),
                                            cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[20],confint(MIcombine(betas,vars2))[20,1],confint(MIcombine(betas,vars2))[20,2])
)

#A.10 Memory decline analyses in synthetic cohort (num.drops4 exposure)

imputed.data.long<-imputationList(data.long)
models.drops <- with(imputed.data.long, lmer(memory_imp ~ as.factor(num.drops4)*time + first_cog + suminc90to10_cent + age_2010_cent50 + female*time + blackrace*time + edu_yrs_cent12*time + pedu_cat + age1stmarried.cat + AFQT_cent + (1|hhidpn)))

betas <- MIextract(models.drops, fun=fixef)
vars <- MIextract(models.drops, fun=vcov)
vars2 <- list()
for ( i in 1:20) { vars2[[i]] <- as.matrix(vars[[i]])  }
MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)
confint(MIcombine(betas,vars2))
sensanalysis.AFQT.results.table[20:26,2:4]<-rbind(cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[2],confint(MIcombine(betas,vars2))[2,1],confint(MIcombine(betas,vars2))[2,2]),
                                            cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[3],confint(MIcombine(betas,vars2))[3,1],confint(MIcombine(betas,vars2))[3,2]),
                                            cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[4],confint(MIcombine(betas,vars2))[4,1],confint(MIcombine(betas,vars2))[4,2]),
                                            cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[5],confint(MIcombine(betas,vars2))[5,1],confint(MIcombine(betas,vars2))[5,2]),
                                            cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[22],confint(MIcombine(betas,vars2))[22,1],confint(MIcombine(betas,vars2))[22,2]),
                                            cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[23],confint(MIcombine(betas,vars2))[23,1],confint(MIcombine(betas,vars2))[23,2]),
                                            cbind(MIcombine(betas, vars2, conf.int = TRUE, conf.level = 0.95)$coefficients[24],confint(MIcombine(betas,vars2))[24,1],confint(MIcombine(betas,vars2))[24,2])
)

#write.csv(sensanalysis.AFQT.results.table,"/Users/xxxxx/code/For publication/Results/sensanalysis_AFQT_results_7529.csv")

