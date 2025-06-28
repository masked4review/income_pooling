#Syntax File 00_HRS
  #Import relevant HRS data (publicly available from https://hrs.isr.umich.edu/data-products)
  #Clean HRS variables relevant for research project
  #Recode/prepare variables as needed for harmonization with NLSY79 (see Appendix Table 1 for harmonization decisions) 
  #Rename variables to prepare for stacking with NLSY79
  #Examine strength of univariable association (via R2) between matching variables and memory decline in HRS and output table
  
############################################################################################################
#load libraries
library(tidyverse)
library(dplyr)
library(haven)
library(tidyr)
library(nnet)
library(nlme)
library(lme4)
library(optimx)
library(ggeffects)
library(cowplot)

#import rand dataset and identify participants with ages/birth years of interest ()
longrand<-read_sas("/Users/xxxxx/code/For publication/Data/randhrs1992_2022v1_SAS/randhrs1992_2022v1.sas7bdat") 
  names(longrand) <- tolower(names(longrand))
  longrand$hhidpn <- as.numeric(paste(as.numeric(longrand$hhid), longrand$pn, sep = ""))

############################################################################################################
#1. Create HRS Cohort with 2010 age range matching NLSY (+ small margin)
  mbb<-longrand[longrand$r10agey_e>=48&longrand$r10agey_e<=57,] #need to restrict to those with non-missing id's
  mbb<-mbb[!is.na(mbb$hhidpn),] #6200 obs 

  #write.csv(mbb,"/Users/xxxxx/code/For publication/Data/mbb_cohort.csv")

############################################################################################################
#2. Clean/create matching variables in HRS (matching variables that are also potential confounders)

#Basic demographic variables at baseline (not time-varying; confounders) 
  prop.table(table(mbb$rafeduc, useNA="ifany"))*100 # father's education
  prop.table(table(mbb$rameduc, useNA="ifany"))*100 # mother's education
  prop.table(table(mbb$raedyrs, useNA="ifany"))*100 # ptcpt years education
  prop.table(table(mbb$raedegrm, useNA="ifany"))*100 #ptcpt ed credentials/grades

#Recode and relabel candidate variables (potential confounders) for matching/harmonization
  mbb1 <- mbb %>% 
    rename(race=raracem,eth=rahispan,birthyear=rabyear, birthmonth=rabmonth, age_2010=r10agey_e, agem=r10agem_e, edu_yrs=raedyrs)%>%
    mutate(
      female = case_when(ragender==1 ~ 0,
                         ragender==2 ~ 1),
      female = factor(female, levels = c(0, 1), labels = c("Male", "Female")),
      race = factor(race, levels = c(1,2,3), labels = c("White", "Black", "Other")),
      eth = factor(eth, levels = c(0,1), labels = c("Non-hispanic", "Hispanic")),
      usbirth = ifelse(rabplace==11,0,1),
      usbirth = factor(usbirth, levels = c(0,1), labels = c("non-US birth", "US birth")),
      edu_cat = case_when(raeduc==1 ~ 1,
                           raeduc==2|raeduc==3 ~ 2,
                           raeduc==4 ~ 3,
                           raeduc==5 ~ 4),
      edu_cat = factor(edu_cat, levels = c(1,2,3,4), labels = c("<HS", "HS/GED","Some college","College+")),
      hpeduc1 = pmax(rafeduc,rameduc, na.rm=TRUE), #highest of either parents education, then categorize
      pedu_cat = case_when(hpeduc1<8 ~ 1,
                         hpeduc1>=8&hpeduc1<12 ~ 2,
                         hpeduc1==12 ~ 3,
                         hpeduc1>12 ~ 4,
                         TRUE ~ 5),
      pedu_cat = factor(pedu_cat, levels = c(1,2,3,4,5), labels = c("<8 years", "8-<12 years","12 years",">12 years","Missing"))
  )
  
  table(mbb1$edu_yrs, useNA="ifany")
  table(mbb1$edu_cat, useNA="ifany")
  table(mbb1$race, useNA="ifany")
  table(mbb1$eth, useNA="ifany")

#try to recover some missing edu_yrs using edu_cat  
  mbb1$edu_yrs<-ifelse(is.na(mbb1$edu_yrs)&mbb1$edu_cat=="<HS",9,
                            ifelse(is.na(mbb1$edu_yrs)&mbb1$edu_cat=="HS/GED",12,
                                   ifelse(is.na(mbb1$edu_yrs)&mbb1$edu_cat=="Some college",14,
                                          ifelse(is.na(mbb1$edu_yrs)&mbb1$edu_cat=="College+",16,mbb1$edu_yrs))))
  table(mbb1$edu_yrs, useNA="ifany")

##Region variable (birth, age 10)
  #geography: if born in South, if lived in South at age of 10
  #Cross-Wave Census Region/Division and Mobility File (https://hrsdata.isr.umich.edu/data-products/cross-wave-census-regiondivision-and-mobility-file)
  #codebook: https://hrsdata.isr.umich.edu/sites/default/files/documentation/codebooks/1629916913/hrsxregion18.txt 
    region <- read_sas("/Users/xxxxx/code/For publication/Data/HRSXRegion18v82/built/sas/hrsxregion18.sas7bdat")
    region <- as.data.frame(region)
    
    geography <- subset(region, select = c(HHID, PN,
                                           REGIONB, #CENSUS REGION-DIVISION OF BIRTH
                                           REGLIV10))#CENSUS REGION-DIVISION WHERE LIVE WHEN IN SCHOOL
    
  #rename
    names(geography)[3:4] <- c("BORN_SOUTH_HRS", "LIVE_SOUTH_CHILD_HRS")
    geography$hhidpn <- as.numeric(paste(as.numeric(geography$HHID), geography$PN, sep = ""))
    
    table(geography$BORN_SOUTH_HRS)#5,6,7 are South region; 10 is US but without Census region
    
    mbb2<-left_join(mbb1,geography,by="hhidpn")
      table(mbb2$rabplace, useNA="ifany")
      table(mbb2$BORN_SOUTH_HRS, useNA="ifany") #same as rabplace
      table(mbb2$LIVE_SOUTH_CHILD_HRS, useNA="ifany")
    
  #Combine into single variable (born or childhood in US South)
    mbb3 <- mbb2 %>% 
        mutate(
          bornsouth = case_when(rabplace %in% c(5,6,7) ~ 1,
                                rabplace %in% c(1,2,3,4,8,9) ~ 0,
                                rabplace == 10 ~ 2,
                                rabplace == 11 ~ 3),
          childsouth = case_when(LIVE_SOUTH_CHILD_HRS %in% c(5,6,7) ~ 1,
                                LIVE_SOUTH_CHILD_HRS %in% c(1,2,3,4,8,9) ~ 0,
                                #LIVE_SOUTH_CHILD_HRS == 10 ~ 2, #only 2 in US and missing region so add to missing category
                                LIVE_SOUTH_CHILD_HRS == 11 ~ 2,
                                TRUE ~ 3),
          bornsouth = factor(bornsouth, levels = c(0,1,2,3), labels = c("No", "Yes", "US, missing region", "Outside US")),
          childsouth = factor(childsouth, levels = c(0,1,2,3), labels = c("No", "Yes", "Outside US", "Missing")))
    
    #check distributions
      table(mbb3$bornsouth, mbb3$usbirth, useNA="ifany") #everyone born outside US coded correctly
      prop.table(table(mbb3$bornsouth, useNA="ifany"))*100
      prop.table(table(mbb3$childsouth, useNA="ifany"))*100
      table(mbb3$bornsouth, mbb3$childsouth, useNA="ifany")
  
  #create a combined variable from born and childhood in the South
    mbb3$born.child.south<-ifelse(mbb3$bornsouth=="Yes"|mbb3$childsouth=="Yes",1,0)
      prop.table(table(mbb3$born.child.south,useNA="ifany"))*100
  
##Age at first marriage
  #https://hrsdata.isr.umich.edu/data-products/cross-wave-marital-history-aggregated-data
  marhist <- read_sas("/Users/xxxxx/code/For publication/Data/AggMarHist2018/AGGMARHIST2018A_R.sas7bdat")
  marhist <- as.data.frame(marhist)
    
  Age1stMarriage <- subset(marhist, select = c(hhid, pn,
                                               MARHIST, #MARITAL HISTORY SUMMARY
                                               M1STARTYR))# FIRST MARRIAGE START YEAR
  #rename
    names(Age1stMarriage) <- c("HHID", "PN", "MARHIST","YEAR_1STMARRIAGE_HRS")
    table(Age1stMarriage$MARHIST) #2233 were  1.  NEVER MARRIED
    
    Age1stMarriage$hhidpn <- as.numeric(paste(as.numeric(Age1stMarriage$HHID), Age1stMarriage$PN, sep = ""))
    #1.  NEVER MARRIED
    #2.  MARRIED DATES KNOWN
    #7.  MARRIED DATES UNKNOWN
  
  #merge samplecohort and Age1stMarriage
    mbb4 <- left_join(mbb3, Age1stMarriage, by = "hhidpn")
      table(mbb4$MARHIST, mbb4$r10mstat, useNA="ifany")
      table(mbb4$r10mstat)
      table(mbb4$birthyear)
    
  #compute age at 1st marriage
    mbb4$age1stmarried <- mbb4$YEAR_1STMARRIAGE_HRS - mbb4$birthyear
    
  #check distribution
    table(mbb4$age1stmarried) #recode <0 to missing; then create categories (decade long categories probably fine in terms of trying to capture portion of working years you may have spent in dual income household (obv, based on assumptions about who is working and length of marriage etc.))
    
  #recode negative value into NA
    mbb4$age1stmarriage <- ifelse(mbb4$age1stmarried<=0, NA, mbb4$age1stmarried)
    hist(mbb4$age1stmarriage)
    summary(mbb4$age1stmarriage)# NAs
    table(mbb4$MARHIST, mbb4$r10mstat) #recode <0 to missing; then create categories (decade long categories probably fine in terms of trying to capture portion of working years you may have spent in dual income household (obv, based on assumptions about who is working and length of marriage etc.))
    
  #recode into categorical
    #never married (using marital status in 2010, r10mstat), <20, 20-29, 30-39, 40+, missing
    mbb4$age1stmarried.cat <- ifelse(mbb4$MARHIST==1|mbb4$r10mstat==8, "never married",
                                                 ifelse(mbb4$age1stmarriage<20, "<20",
                                                        ifelse(mbb4$age1stmarriage>=20&mbb4$age1stmarriage<30, "20-29",
                                                               ifelse(mbb4$age1stmarriage>=30&mbb4$age1stmarriage<40, "30-39",
                                                                      ifelse(mbb4$age1stmarriage>=40, "40+", NA)))))
    mbb4$age1stmarried.cat[is.na(mbb4$age1stmarried.cat)] <- "missing"
    mbb4$age1stmarried.cat <- factor(mbb4$age1stmarried.cat, levels = c("never married", "<20", "20-29", "30-39", "40+", "missing"))
    table(mbb4$age1stmarried.cat)
    round(prop.table(table(mbb4$age1stmarried.cat))*100, 2)
    
##Parental early-life working status 
  #can skip this part if you want because we do not use any of these variables for matching
  #https://hrsdata.isr.umich.edu/data-products/cross-wave-childhood-health-and-family-aggregated-data
  library(readr)
  source("/Users/xxxxx/code/For publication/Data/ChildhoodHealthAndFamily/earlylife_read_da.R") #write syntax to read in dataset
    data.file <- "/Users/xxxxx/code/For publication/Data/ChildhoodHealthAndFamily/AGGCHLDFH2016A_R.da" #files from HRS
    dict.file <- "/Users/xxxxx/code/For publication/Data/ChildhoodHealthAndFamily/AGGCHLDFH2016A_R.dct" #files from HRS
    earlylife = read_da(data = data.file, dict = dict.file) 
    
    parent <- subset(earlylife, select = c(HHID, PN, 
                                         FAEDUC, MOEDUC,#parents' years of education
                                         FAUNEM, #FATHER UNEMPLOYED DURING CHILDHOOD
                                         FJOB, #FATHER USUAL OCC WHEN R AGE 16 - MASKED
                                         MOWORK,#MOTHER WORK DURING CHILDHOOD
                                         FAMFIN)) #RATE FAMILY FINANCIAL SITUATION GROWING UP
  
    parent$hhidpn <- as.numeric(paste(as.numeric(parent$HHID), parent$PN, sep = ""))
    
    #create new measure: highest parent edu
    table(parent$MOEDUC, parent$FAEDUC)
    #recode NA
    #97.  OTH MISSING; 98.  DK (Don't Know); 99.  RF (Refused)
    parent$MOEDUC <- ifelse(parent$MOEDUC>90, NA, parent$MOEDUC)
    parent$FAEDUC <- ifelse(parent$FAEDUC>90, NA, parent$FAEDUC)
    
    parent$PARENT_EDU_HRS <- ifelse(parent$MOEDUC<parent$FAEDUC, parent$FAEDUC, 
                                    parent$MOEDUC)
    table(parent$PARENT_EDU_HRS, useNA="ifany") #file in RAND is WAY better / WAY less missing data
    
    #recode if mom work
    #1.  ALL OF THE TIME; 3.  SOME OF THE TIME; 5.  NOT AT ALL; 
    #7.  NEVER LIVED WITH MOTHER/MOTHER WAS NOT ALIVE (VOL); 8.  DK (Don't Know); 9.  RF
    table(parent$MOWORK)
    parent$MOWORK <- ifelse(parent$MOWORK==1,2,
                            ifelse(parent$MOWORK==3, 1, 
                                  ifelse(parent$MOWORK==5, 0, NA)))
    table(parent$MOWORK, exclude = NULL)
    #2 = mom always worked
    #1 = mom sometimes worked
    #0 = mom never worked    
    #Never lived with mother/mother not alive, don't know, refused
    
    #recode if dad work
    #the original question ask if FATHER UNEMPLOYED DURING CHILDHOOD. Reverse-coded
    #1.  YES;  5.  NO; 6.  FATHER NEVER WORKED/ALWAYS DISABLED (VOL); 
    #7.  NEVER LIVED WITH FATHER/FATHER WAS NOT ALIVE (VOL); 8.  DK (Don't Know); 9.  RF
    table(parent$FJOB, useNA="ifany")
    
    parent$FAWORK <- ifelse(parent$FAUNEM==1,1,
                            ifelse(parent$FAUNEM==6, 0,
                                  ifelse(parent$FAUNEM==5, 2, NA)))
    table(parent$FAWORK, exclude = NULL)
      #2 = dad always worked
      #1 = dad had unemployment spell/dad sometimes worked
      #0 = dad never worked
      #Never lived with father/father not alive, don't know, refused
    
    table(parent$FAWORK, parent$FJOB, useNA="ifany")
    
    table(parent$FAMFIN) #1=pretty well off, 3=about average, 5=poor, 6=it varied, 8=DK, 9=Refused
    
    #drop the variable we don't need - not sure we will use any of these variables as matching variables but go ahead and merge in for now
    colnames(parent)
    parent <- parent[, -6]
    
    mbb5<-left_join(mbb4, parent, by="hhidpn")
    
    prop.table(table(mbb5$MOWORK, useNA="ifany"))*100  
    prop.table(table(mbb5$FAWORK, useNA="ifany"))*100
    
    mbb5$bothparwork<-ifelse(mbb5$MOWORK==2&mbb5$FAWORK==2,1,0)
      prop.table(table(mbb5$bothparwork, useNA="ifany"))*100
      table(mbb5$hpeduc1, useNA="ifany") #less missing data
  
  #Will just use parental education because the parent work variables are not exactly the same in both datasets (e.g., dissimilar time frame). 

####################################################################################################################################################################################################################################################################################################################################
#3. Clean/create "mediator" matching variables in HRS

#Census region variables (at matching time (2010), at birth, etc.)  
  table(mbb5$r10cenreg) #census region in 2010
  #1=northeast
  #2=midwest
  #3=south
  #4=west
  #5=other
  mbb5$currentregion_2010<-ifelse(mbb5$r10cenreg==5,NA,mbb5$r10cenreg)
  table(mbb5$currentregion_2010, exclude=NULL) #census region in 2010

#rename other variables of interest  
mbb6 <- mbb5 %>% 
  rename(income=h10itot, poverty_2010=h10inpov, povertyratio=h10inpovr, hhsize=h10hhres, wealth_2010=h10atotb,
         occustat=r10lbrf, occuclass=r10jcoccb, epinsurance_resp=r10covr, epinsurance_sp=r10covs, selfratemem=r10slfmem, immedrecall=r10imrc, delrecall=r10dlrc, marriage_2010=r10mstat, height_2010=r10height, weight_2010=r10weight, BMI_2010=r10bmi,
         currhibp=r10hibp, everhibp=r10hibpe, everdiab=r10diabe, everheartprob=r10hearte, eversmoke_2010=r10smokev, currsmoke_2010=r10smoken, drinksalc_2010=r10drink,
         everpsyche=r10psyche, everstroke=r10stroke, vigex.2010=r10vgactx, modex.2010=r10mdactx, ltex.2010=r10ltactx,srh=r10shlt)
    
  prop.table(table(mbb6$r10cesd, useNA="ifany"))
    table(mbb6$r10depres, useNA="ifany")
    table(mbb6$r10effort, useNA="ifany")
    table(mbb6$r10sleepr, useNA="ifany")
    table(mbb6$r10fsad, useNA="ifany")
    table(mbb6$r10going, useNA="ifany")
    table(mbb6$r10flone, useNA="ifany")

#create despressive symptoms variable summing items in common between HRS and NLSY  
  mbb6$cesd<-rowSums(mbb6[,c("r10depres","r10effort","r10sleepr","r10fsad","r10going","r10flone")])
    prop.table(table(mbb6$cesd, useNA="ifany"))*100
    table(mbb6$r10cesd)

#examine distributions/missing data on other matching variables  
  summary(mbb6$income, na.rm=TRUE)  #use RAND file so get imputed $ values (then impute NLSY missing income/wealth based on same predictors)
  summary(mbb6$wealth_2010, na.rm=TRUE) #use RAND file so get imputed $ values (then impute NLSY missing income/wealth based on same predictors)
  summary(mbb6$povertyratio, na.rm=TRUE)
  summary(mbb6$hhsize, na.rm=TRUE)
  summary(mbb6$height_2010, na.rm=TRUE)
  summary(mbb6$weight_2010, na.rm=TRUE)
  prop.table(table(mbb6$selfratemem, useNA='ifany'))
  prop.table(table(mbb6$immedrecall, useNA='ifany'))
  prop.table(table(mbb6$delrecall, useNA='ifany'))
  prop.table(table(mbb6$marriage_2010, useNA='ifany'))
  prop.table(table(mbb6$epinsurance_resp, useNA='ifany'))
  prop.table(table(mbb6$epinsurance_sp, useNA='ifany'))
  prop.table(table(mbb6$eversmoke_2010, useNA='ifany'))
  prop.table(table(mbb6$currsmoke_2010, useNA='ifany'))
  prop.table(table(mbb6$drinksalc_2010, useNA='ifany'))
  prop.table(table(mbb6$everpsyche, useNA='ifany'))
  prop.table(table(mbb6$everheartprob, useNA='ifany'))
  prop.table(table(mbb6$everpsyche, useNA='ifany'))
  prop.table(table(mbb6$everdiab, useNA='ifany'))
  prop.table(table(mbb6$everhibp, useNA='ifany'))
  prop.table(table(mbb6$everstroke, useNA='ifany'))
  summary(mbb6$r10cesd)

#create chronic conditions variables  
  mbb6$countCVD <- rowSums(mbb6[, c("everhibp","everheartprob","everstroke")], na.rm=TRUE) 
  mbb6$everCVD <- ifelse(mbb6$countCVD>0,1,0)
    prop.table(table(mbb6$countCVD, useNA='ifany'))
    prop.table(table(mbb6$everCVD, useNA='ifany'))

#create cog variables for matching (but not analysis)
  mbb6$avg.memory<-rowMeans(mbb6[,c("immedrecall","delrecall")], na.rm=TRUE)
  mbb6$sum.memory<-rowSums(mbb6[,c("immedrecall","delrecall")])
    summary(mbb6$avg.memory)
    summary(mbb6$sum.memory)

#create employer-provided health insurance variable (on EP insurance via self or spouse)   
  mbb6$epinsurance_2010 <- ifelse(mbb6$epinsurance_resp==1|mbb6$epinsurance_sp==1,1,0)
    table(mbb6$epinsurance_resp, mbb6$epinsurance_sp, exclude=NULL)    
    table(mbb6$epinsurance_2010, exclude=NULL)    
  
#create exercise variables
  table(mbb6$vigex.2010, exclude=NULL)  
  table(mbb6$modex.2010, mbb6$ltex.2010,exclude=NULL)
  mbb6$lmodex.2010<-ifelse(mbb6$modex.2010==1|mbb6$ltex.2010==1,1,
                           ifelse(mbb6$modex.2010==2|mbb6$ltex.2010==2,2,
                                  ifelse(mbb6$modex.2010==3|mbb6$ltex.2010==3,3,
                                         ifelse(mbb6$modex.2010==4|mbb6$ltex.2010==4,4,
                                                ifelse(mbb6$modex.2010==5&mbb6$ltex.2010==5,5,NA)))))
  
  table(mbb6$vigex.2010)
  table(mbb6$lmodex.2010)
  
#Import composite memory score dataset created via fitted regression model from Wu et al. 
  #Wu et al. 2013; https://pubmed.ncbi.nlm.nih.gov/22992720/
  #Table 1 contains coefficients for composite memory score prediction model
  #HRS CORE and RAND files contain all predictor variables for Table 1 model to perform predictions in each survey year: https://hrsdata.isr.umich.edu/data-products/public-survey-data
  
memscore<-read_sas("/Users/xxxxx/code/For publication/Data/dpmemimp_jan2024.sas7bdat") 
  memscore$hhidpn <- paste(as.numeric(memscore$HHID), memscore$PN, sep = "")
  colnames(memscore)
  memscore<-memscore[,c(137:150,165)]
  mbb6.vars<-mbb6[,c("hhidpn","r10iwstat","r11iwstat","r12iwstat","r13iwstat","racohbyr","race","eth","birthyear","birthmonth","age_2010","edu_yrs","female","usbirth",
                   "pedu_cat","born.child.south","age1stmarriage","age1stmarried.cat","FAUNEM","MOWORK","FAWORK","FAMFIN","bothparwork","srh",
                   "income","poverty_2010","povertyratio","hhsize","currentregion_2010","wealth_2010","occuclass","occustat",
                   "epinsurance_2010","selfratemem","immedrecall","delrecall","avg.memory","sum.memory","marriage_2010","height_2010","weight_2010","BMI_2010","currhibp","everhibp",
                   "everheartprob","everdiab","countCVD","everCVD","eversmoke_2010","currsmoke_2010","drinksalc_2010","everpsyche","everstroke","cesd","vigex.2010","lmodex.2010")]

#merge memory score outcome (for analysis in synthetic cohort) into HRS covariates dataset
  memscore$hhidpn<-as.numeric(memscore$hhidpn)
  mbb6.vars$hhidpn<-as.numeric(mbb6.vars$hhidpn)
  mbb.mem<-left_join(mbb6.vars,memscore,by="hhidpn")  

#clean remaining matching variables not yet cleaned (income, marital status, employment status, occupational skill level)
mbb.mem1 <- mbb.mem %>%
  mutate(
    adj10.hh.inc.10 = income/sqrt(hhsize), #use 2010 'current' dollars in both cohorts (just for matching) because that is how participants would be answering this (non-inflation adjusted) and equivalize for household size
    marriage_2010 = case_when(marriage_2010 == 8 | age1stmarried.cat=="never married" ~ 0, #never married
                         marriage_2010 %in% c(1,2,3) ~ 1, #married w/ spouse present, partnered, married but spouse absent
                         marriage_2010 %in% c(4,5,6,7) ~ 2), #separated, divorced, separated/divorced, widowed
    marriage_2010 = factor(marriage_2010, levels = c(0,1,2),     labels = c("never married","married/partnered", "separated/divorced/widowed")),
    LBRF_2010 = case_when(occustat == 1 ~ 1, #work ft
                     occustat == 2 ~ 2, #work pt
                     occustat == 3 ~ 3, #unemployed
                     occustat %in% c(4,5) ~ 4, #retired (full or partial)
                     occustat == 6 ~ 5, #disabled
                     occustat == 7 ~ 6), #not in lf
    #use the occup/2000 census variables (bc that is what was used in NLSY)
    occuskill_2010 = case_when(occuclass %in% c(1,2,3,4,5,6,7,8,9,10,11,12) ~ 2, # higher skill
                          occuclass %in% c(13,14,15,16,17,18,19,20,21,22,23,24,25) ~ 1, #lower skill 
                          LBRF_2010 %in% c(3,4,5,6) ~ 0,
                          TRUE ~ 3),
occuskill_2010 = factor(occuskill_2010, levels = c(0,1,2,3), labels = c("not working","lower skill","higher skill", "missing")),
LBRF_2010 = factor(LBRF_2010, levels = c(1,2,3,4,5,6), labels = c("Work FT", "Work PT", "Unemployed", "Retired","Disabled", "Not in LbrF")))
  
#FINAL CLEANING/CATEGORIZATION SCHEME WILL MATCH DETAILED DESCRIPTIONS IN APPENDIX TABLE 1
#save dataset
#write.csv(mbb.mem1,"/Users/xxxxx/code/For publication/Data/HRS_short_cogfxn2010_6200.csv")

############################################################################################################
#4. Examining univariable associations (via R2) between matching variables and outcome in HRS (memory composite score in 2010 and 10-year change in score)

###Create data frame to save results - that has missing data in each time point and also R2 of interest
  HRS.corrs<-as.data.frame(as.matrix(lm.vars,nrow=38,ncol=1))
  names(HRS.corrs)<-"variable"

#store matching variables' names  
  lm.vars<-c("female","race","eth","usbirth","pedu_cat","born.child.south","age1stmarried.cat",
           "age_2010","birthyear","adj10.hh.inc.10","poverty_2010","currentregion_2010","wealth_2010","LBRF_2010",
           "occuskill_2010","epinsurance_2010","weight_2010","height_2010","BMI_2010","marriage_2010","immedrecall","delrecall",
           "avg.memory","sum.memory","selfratemem","edu_yrs","eversmoke_2010","currsmoke_2010","drinksalc_2010",
           "everpsyche","cesd","everdiab","everhibp","everCVD","countCVD","srh","vigex.2010","lmodex.2010")

library(naniar)
#quantify missing data on matching variables (deprioritize for matching variables with weak associations with X and Y and a lot of missing data)
  md_HRS_2010 <- miss_var_summary(mbb.mem1[,lm.vars], order = F)
  md_HRS_2010 <- md_HRS_2010[,-1]
  HRS.corrs<-cbind(HRS.corrs,md_HRS_2010)

out.inc <- vector('list', length(lm.vars))

#R^2 between each variable and composite memory score at one time point between 2010 and 2020
out.mem.2010 <- vector('list', length(lm.vars))
  for (i in seq_along(lm.vars)){
    out.mem.2010[[i]] <- summary(lm(paste("memimp10",  '~', lm.vars[i]),
                           data = mbb.mem1))$r.squared
  }
    r.sq.mem10<-as.data.frame(as.matrix(out.mem.2010))
    names(r.sq.mem10)[1]<-"R2_MEM2010"

out.mem.2012 <- vector('list', length(lm.vars))
  for (i in seq_along(lm.vars)){
    out.mem.2012[[i]] <- summary(lm(paste("memimp12",  '~', lm.vars[i]),
                                data = mbb.mem1))$r.squared
  }
    r.sq.mem12<-as.data.frame(as.matrix(out.mem.2012))
    names(r.sq.mem12)[1]<-"R2_MEM2012"

out.mem.2014 <- vector('list', length(lm.vars))
  for (i in seq_along(lm.vars)){
    out.mem.2014[[i]] <- summary(lm(paste("memimp14",  '~', lm.vars[i]),
                                data = mbb.mem1))$r.squared
  }
    r.sq.mem14<-as.data.frame(as.matrix(out.mem.2014))
    names(r.sq.mem14)[1]<-"R2_MEM2014"
    
out.mem.2016 <- vector('list', length(lm.vars))
  for (i in seq_along(lm.vars)){
    out.mem.2016[[i]] <- summary(lm(paste("memimp16",  '~', lm.vars[i]),
                              data = mbb.mem1))$r.squared
  }
    r.sq.mem16<-as.data.frame(as.matrix(out.mem.2016))
    names(r.sq.mem16)[1]<-"R2_MEM2016"
    
out.mem.2018 <- vector('list', length(lm.vars))
  for (i in seq_along(lm.vars)){
    out.mem.2018[[i]] <- summary(lm(paste("memimp18",  '~', lm.vars[i]),
                              data = mbb.mem1))$r.squared
  }
    r.sq.mem18<-as.data.frame(as.matrix(out.mem.2018))
    names(r.sq.mem18)[1]<-"R2_MEM2018"

out.mem.2020 <- vector('list', length(lm.vars))
  for (i in seq_along(lm.vars)){
    out.mem.2020[[i]] <- summary(lm(paste("memimp20",  '~', lm.vars[i]),
                              data = mbb.mem1))$r.squared
  }
    r.sq.mem20<-as.data.frame(as.matrix(out.mem.2020))
    names(r.sq.mem20)[1]<-"R2_MEM2020"
    
#bind these R^2 to results table with missing data
  HRS.corrs<-cbind(HRS.corrs,r.sq.mem10,r.sq.mem12,r.sq.mem14,r.sq.mem16,r.sq.mem18,r.sq.mem20)

#create a long data for pseudo-R^2s with 10-year memory decline
memcol<-c(1995, seq(1996,2020,2)) #just go ahead and rename the col names to the corresponding waveyrs so that I don't have to recode that later
colnames(mbb.mem1)
colnames(mbb.mem1)[57:70]<-memcol

mbb.long = mbb.mem1 %>% 
  pivot_longer(
    cols = 57:70, 
    names_to = "WAVEID_N",
    values_to = "memory")

library(sjPlot)
#https://stackoverflow.com/questions/45327217/r-squared-of-lmer-model-fit
#lets you create a html table with R2 (tab_model) but cannot extract it from table

library(lmerTest) # for model fitting
library(performance) # for Nakagawa conditional/marginal R2
library(partR2) # for part R2 values
#Nakagawa conditional/marginal R2 are same values as in table using tab_model so can extract these

mbb.long$time<-ifelse(mbb.long$WAVEID_N=="2010",0,
                      ifelse(mbb.long$WAVEID_N=="2012",2,
                             ifelse(mbb.long$WAVEID_N=="2014",4,
                                    ifelse(mbb.long$WAVEID_N=="2016",6,
                                           ifelse(mbb.long$WAVEID_N=="2018",8,
                                                  ifelse(mbb.long$WAVEID_N=="2020",10,NA))))))

mbb.long.s<-mbb.long[!is.na(mbb.long$time),]

out.mem.decl10 <- vector('list', length(lm.vars))
  for (i in seq_along(lm.vars)){
    out.mem.decl10[[i]] <- r2_nakagawa(lmer(paste("memory",  '~', lm.vars[i], "+", "time", "+", "(1|hhidpn)"),
                                    data = mbb.long.s))[2][[1]][[1]]
  }
    r.sq.mem.decl10<-as.data.frame(as.matrix(out.mem.decl10))
    names(r.sq.mem.decl10)[1]<-"R2_MEM_DECL10yr"

HRS.corrs<-cbind(HRS.corrs,r.sq.mem.decl10)    
HRS.corrs[,c(2:10)]<-lapply(HRS.corrs[,c(2:10)],as.numeric)

#write.csv(HRS.corrs,"/Users/xxxxx/code/For publication/Results/HRS_R2_results.csv")

#save final long dataset
#write.csv(mbb.long.s,"/Users/xxxxx/code/For publication/Data/HRS_long_memdecl10to20_6200.csv")
