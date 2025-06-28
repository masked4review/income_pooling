#Syntax File 00_NLSY
  #Import relevant NLSY79 data (publicly available from https://www.nlsinfo.org/investigator/pages/home)
  #Clean NLSY79 variables relevant for research project
  #Recode/prepare variables as needed for harmonization with HRS (see Appendix Table 1 for harmonization decisions) 
  #Rename variables to prepare for stacking with HRS
  #Examine strength of univariable association (via R2) between matching variables and memory decline in HRS and output table

############################################################################################################
library(tidyverse)
library(haven)
library(tidyr)
library(dplyr)
library(nnet)
library(nlme)
library(lme4)
library(optimx)
library(ggeffects)
library(cowplot)

setwd("/Users/xxxxx/code/For publication/Data/NLSY")

#####################################################################################
##interview status in 2010-2018
iwstatus <- read.csv("iwstatus.csv")
names(iwstatus) <- c("NLSY_CASE_ID", 
                     "iwstatus_2010", "iwstatus_2012", 
                     "iwstatus_2014", "iwstatus_2016", "iwstatus_2018") #double checked and labels refer to correct variables

#61 through 75 are reasons not interviewed

#recode into 1=yes (interviewed), 0=no (non-interview)
table(iwstatus$iwstatus_2010)
table(iwstatus$iwstatus_2012)
table(iwstatus$iwstatus_2014)
table(iwstatus$iwstatus_2016)
table(iwstatus$iwstatus_2018)

#VALID SKIP(-4), meaning these are respondent
iwstatus[, 2:6] <- lapply(iwstatus[, 2:6], function(x) ifelse(x==-4, 1, 0))

colnames(iwstatus)

#####################################################################################
#EXPOSURES

#-------------------------------------------------------------------------------
#TOTAL NET FAMILY INCOME

#These variables provide a composite income figure from a number of income values for household members related to the respondent by blood or marriage.
income <- read.csv("income.csv")
colnames(income)

#rename
names(income) <- c("NLSY_CASE_ID", "NLSY_SAMPLE_ID", "NLSY_RACE", "NLSY_SEX",
                   "TOT_INCOME_NLSY_1979", "TOT_INCOME_NLSY_1980", 
                   "TOT_INCOME_NLSY_1981", "TOT_INCOME_NLSY_1982", 
                   "TOT_INCOME_NLSY_1983", "TOT_INCOME_NLSY_1984", 
                   "TOT_INCOME_NLSY_1985", "TOT_INCOME_NLSY_1986", 
                   "TOT_INCOME_NLSY_1987", "TOT_INCOME_NLSY_1988", 
                   "TOT_INCOME_NLSY_1989", "TOT_INCOME_NLSY_1990", 
                   "TOT_INCOME_NLSY_1991", "TOT_INCOME_NLSY_1992", 
                   "TOT_INCOME_NLSY_1993", "TOT_INCOME_NLSY_1994", #starting 1994, biennial survey
                   "TOT_INCOME_NLSY_1996", "TOT_INCOME_NLSY_1998", 
                   "TOT_INCOME_NLSY_2000", "TOT_INCOME_NLSY_2002", 
                   "TOT_INCOME_NLSY_2004", "TOT_INCOME_NLSY_2006", 
                   "TOT_INCOME_NLSY_2008", "TOT_INCOME_NLSY_2010", 
                   "TOT_INCOME_NLSY_2012", "TOT_INCOME_NLSY_2014", 
                   "TOT_INCOME_NLSY_2016", "TOT_INCOME_NLSY_2018"
                   )

#recode negative value into NA
#Refusal(-1), Don't Know(-2), Invalid Skip(-3), VALID SKIP(-4), NON-INTERVIEW(-5)
hist(income$TOT_INCOME_NLSY_1979)
income[, 5:32] <- lapply(income[, 5:32], function(x) ifelse(x<0, NA, x))
summary(income$TOT_INCOME_NLSY_1979)

#create inflation adjusted income measures (exposure of interest in synthetic cohort provided by NLSY79) 
  #Store different inflation-adjustment strategies for later potential analysis of interest using this NLSY79 cohort
  #**Exposure for synthetic cohorT will use income adjusted to 1990 dollars across time points
  #https://www.usinflationcalculator.com/
  #recode to 2010 dollars (time of matching)
  cpi2010<-c(replicate(n=12686, 3), replicate(n=12686, 2.65), replicate(n=12686, 2.40), replicate(n=12686, 2.26), replicate(n=12686,2.19), replicate(n=12686, 2.10), replicate(n=12686, 2.03), replicate(n=12686, 1.99), replicate(n=12686, 1.92), replicate(n=12686, 1.84), replicate(n=12686, 1.76), replicate(n=12686, 1.67), replicate(n=12686, 1.60), replicate(n=12686, 1.55), replicate(n=12686, 1.51), replicate(n=12686, 1.47), replicate(n=12686, 1.39), replicate(n=12686, 1.34), replicate(n=12686, 1.27), replicate(n=12686, 1.21), replicate(n=12686, 1.15), replicate(n=12686, 1.08), replicate(n=12686, 1.01), replicate(n=12686, 1.00), replicate(n=12686, 0.95), replicate(n=12686, 0.92), replicate(n=12686, 0.91), replicate(n=12686, 0.87))
  #recode to 1979 dollars (baseline)
  cpi1979<-c(replicate(n=12686, 1.00), replicate(n=12686, 0.88), replicate(n=12686, 0.80), replicate(n=12686, 0.75), replicate(n=12686, 0.73), replicate(n=12686, 0.70), replicate(n=12686, 0.67), replicate(n=12686, 0.66), replicate(n=12686, 0.64), replicate(n=12686, 0.61), replicate(n=12686, 0.59), replicate(n=12686, 0.56), replicate(n=12686, 0.53), replicate(n=12686, 0.52), replicate(n=12686, 0.50), replicate(n=12686, 0.49), replicate(n=12686, 0.46), replicate(n=12686, 0.45), replicate(n=12686,  0.42), replicate(n=12686, 0.40), replicate(n=12686, 0.38), replicate(n=12686, 0.36), replicate(n=12686, 0.34), replicate(n=12686, 0.33), replicate(n=12686, 0.32), replicate(n=12686, 0.31), replicate(n=12686, 0.30), replicate(n=12686, 0.29))
  #recode to 1986 dollars (in case we want to use it as baseline for NLSY analyses)
  cpi1986<-c(replicate(n=12686, 1.51), replicate(n=12686, 1.33), replicate(n=12686, 1.21), replicate(n=12686, 1.14), replicate(n=12686, 1.10), replicate(n=12686, 1.05), replicate(n=12686, 1.02), replicate(n=12686, 1.00), replicate(n=12686, 0.96), replicate(n=12686, 0.93), replicate(n=12686, 0.88), replicate(n=12686, 0.84), replicate(n=12686, 0.80), replicate(n=12686, 0.78), replicate(n=12686, 0.76), replicate(n=12686, 0.74), replicate(n=12686, 0.70), replicate(n=12686, 0.67), replicate(n=12686,  0.64), replicate(n=12686, 0.61), replicate(n=12686, 0.58), replicate(n=12686, 0.54), replicate(n=12686, 0.51), replicate(n=12686, 0.50), replicate(n=12686, 0.48), replicate(n=12686, 0.46), replicate(n=12686, 0.46), replicate(n=12686, 0.44))
#recode to 1990 dollars (in case we want to use it as baseline for NLSY analyses & because income was in 1990 dollars in CARDIA paper)
cpi1990<-c(replicate(n=12686, 1.80), replicate(n=12686, 1.59), replicate(n=12686, 1.44), replicate(n=12686, 1.35), replicate(n=12686, 1.31), replicate(n=12686, 1.26), replicate(n=12686, 1.21), replicate(n=12686, 1.19), replicate(n=12686, 1.15), replicate(n=12686, 1.10), replicate(n=12686, 1.05), replicate(n=12686, 1.00), replicate(n=12686, 0.96), replicate(n=12686, 0.93), replicate(n=12686, 0.90), replicate(n=12686, 0.88), replicate(n=12686, 0.83), replicate(n=12686, 0.80), replicate(n=12686,  0.76), replicate(n=12686, 0.73), replicate(n=12686, 0.69), replicate(n=12686, 0.65), replicate(n=12686, 0.61), replicate(n=12686, 0.60), replicate(n=12686, 0.57), replicate(n=12686, 0.55), replicate(n=12686, 0.54), replicate(n=12686, 0.52))
  
#inflation adjust and add new variables to data frame (naming convention specifies year of income and adj=inflation adjusted and ending number means what real dollar value (10=2010, 79=1979...))
income2010=income[,5:32]*cpi2010
names(income2010)<- c("income79.adj10", "income80.adj10", "income81.adj10", "income82.adj10", "income83.adj10",
                  "income84.adj10", "income85.adj10", "income86.adj10", "income87.adj10", "income88.adj10",
                  "income89.adj10", "income90.adj10", "income91.adj10", "income92.adj10", "income93.adj10",
                  "income94.adj10", "income96.adj10", "income98.adj10", "income00.adj10", "income02.adj10",
                  "income04.adj10", "income06.adj10", "income08.adj10", "income10.adj10", "income12.adj10",
                  "income14.adj10", "income16.adj10", "income18.adj10")
income1979=income[,5:32]*cpi1979
names(income1979) <- c("income79.adj79", "income80.adj79", "income81.adj79", "income82.adj79", "income83.adj79",
                       "income84.adj79", "income85.adj79", "income86.adj79", "income87.adj79", "income88.adj79",
                       "income89.adj79", "income90.adj79", "income91.adj79", "income92.adj79", "income93.adj79",
                       "income94.adj79", "income96.adj79", "income98.adj79", "income00.adj79", "income02.adj79",
                       "income04.adj79", "income06.adj79", "income08.adj79", "income10.adj79", "income12.adj79",
                       "income14.adj79", "income16.adj79", "income18.adj79")
income1986=income[,5:32]*cpi1986
names(income1986)<- c("income79.adj86", "income80.adj86", "income81.adj86", "income82.adj86", "income83.adj86",
                      "income84.adj86", "income85.adj86", "income86.adj86", "income87.adj86", "income88.adj86",
                      "income89.adj86", "income90.adj86", "income91.adj86", "income92.adj86", "income93.adj86",
                      "income94.adj86", "income96.adj86", "income98.adj86", "income00.adj86", "income02.adj86",
                      "income04.adj86", "income06.adj86", "income08.adj86", "income10.adj86", "income12.adj86",
                      "income14.adj86", "income16.adj86", "income18.adj86")
income1990=income[,5:32]*cpi1990
names(income1990)<- c("income79.adj90", "income80.adj90", "income81.adj90", "income82.adj90", "income83.adj90",
                      "income84.adj90", "income85.adj90", "income86.adj90", "income87.adj90", "income88.adj90",
                      "income89.adj90", "income90.adj90", "income91.adj90", "income92.adj90", "income93.adj90",
                      "income94.adj90", "income96.adj90", "income98.adj90", "income00.adj90", "income02.adj90",
                      "income04.adj90", "income06.adj90", "income08.adj90", "income10.adj90", "income12.adj90",
                      "income14.adj90", "income16.adj90", "income18.adj90")

income.all<-cbind(income,income1979,income2010,income1986,income1990)

#-------------------------------------------------------------------------------
#Import household size to equivalize all income measures 
hhsize <- read.csv("hhsize.csv")

#rename
names(hhsize) <- c("NLSY_CASE_ID", "NLSY_SAMPLE_ID", 
                   "HH_SIZE_NLSY_1979",  "HH_SIZE_NLSY_1980",
                   "HH_SIZE_NLSY_1981", "HH_SIZE_NLSY_1982",
                   "HH_SIZE_NLSY_1983", "HH_SIZE_NLSY_1984",
                   "HH_SIZE_NLSY_1985", "HH_SIZE_NLSY_1986",
                   "HH_SIZE_NLSY_1987", "HH_SIZE_NLSY_1988",
                   "HH_SIZE_NLSY_1989", "HH_SIZE_NLSY_1990",
                   "HH_SIZE_NLSY_1991", "HH_SIZE_NLSY_1992",
                   "HH_SIZE_NLSY_1993", "HH_SIZE_NLSY_1994",#starting 1994, biennial survey
                   "HH_SIZE_NLSY_1996", "HH_SIZE_NLSY_1998",
                   "HH_SIZE_NLSY_2000", "HH_SIZE_NLSY_2002",
                   "HH_SIZE_NLSY_2004", "HH_SIZE_NLSY_2006",
                   "HH_SIZE_NLSY_2008", "HH_SIZE_NLSY_2010",
                   "HH_SIZE_NLSY_2012", "HH_SIZE_NLSY_2014",
                   "HH_SIZE_NLSY_2016", "HH_SIZE_NLSY_2018")

#recode negative value into NA
hhsize[, 3:30] <- lapply(hhsize[, 3:30], function(x) ifelse(x<0, NA, x))
table(hhsize$HH_SIZE_NLSY_1979, exclude = NULL)

#drop the sample ID
hhsize <- hhsize[, -c(1, 2)]
colnames(hhsize)

#combine income.all and hhsize so that can create hh size adjusted income variables
income.hh<-cbind(income.all, hhsize)

#adjust all income measures for hh size (.hh.) and also make measures equivalized for a family of 2 (.hh2.) in case want to examine income class relative to poverty threshold for a family of 2)
#first create empty cells for variables equivalizing on hh size
empty_cols.79 = c("adj79.hh.inc.79","adj79.hh.inc.80","adj79.hh.inc.81","adj79.hh.inc.82","adj79.hh.inc.83",
               "adj79.hh.inc.84","adj79.hh.inc.85","adj79.hh.inc.86","adj79.hh.inc.87","adj79.hh.inc.88",
               "adj79.hh.inc.89","adj79.hh.inc.90","adj79.hh.inc.91","adj79.hh.inc.92","adj79.hh.inc.93",
               "adj79.hh.inc.94","adj79.hh.inc.96","adj79.hh.inc.98","adj79.hh.inc.00","adj79.hh.inc.02",
               "adj79.hh.inc.04","adj79.hh.inc.06","adj79.hh.inc.08","adj79.hh.inc.10","adj79.hh.inc.12",
               "adj79.hh.inc.14","adj79.hh.inc.16","adj79.hh.inc.18")
income.hh[, empty_cols.79]<-NA 

empty_cols.10 = c("adj10.hh.inc.79","adj10.hh.inc.80","adj10.hh.inc.81","adj10.hh.inc.82","adj10.hh.inc.83",
                  "adj10.hh.inc.84","adj10.hh.inc.85","adj10.hh.inc.86","adj10.hh.inc.87","adj10.hh.inc.88",
                  "adj10.hh.inc.89","adj10.hh.inc.90","adj10.hh.inc.91","adj10.hh.inc.92","adj10.hh.inc.93",
                  "adj10.hh.inc.94","adj10.hh.inc.96","adj10.hh.inc.98","adj10.hh.inc.00","adj10.hh.inc.02",
                  "adj10.hh.inc.04","adj10.hh.inc.06","adj10.hh.inc.08","adj10.hh.inc.10","adj10.hh.inc.12",
                  "adj10.hh.inc.14","adj10.hh.inc.16","adj10.hh.inc.18")
income.hh[, empty_cols.10]<-NA

empty_cols.86 = c("adj86.hh.inc.79","adj86.hh.inc.80","adj86.hh.inc.81","adj86.hh.inc.82","adj86.hh.inc.83",
                  "adj86.hh.inc.84","adj86.hh.inc.85","adj86.hh.inc.86","adj86.hh.inc.87","adj86.hh.inc.88",
                  "adj86.hh.inc.89","adj86.hh.inc.90","adj86.hh.inc.91","adj86.hh.inc.92","adj86.hh.inc.93",
                  "adj86.hh.inc.94","adj86.hh.inc.96","adj86.hh.inc.98","adj86.hh.inc.00","adj86.hh.inc.02",
                  "adj86.hh.inc.04","adj86.hh.inc.06","adj86.hh.inc.08","adj86.hh.inc.10","adj86.hh.inc.12",
                  "adj86.hh.inc.14","adj86.hh.inc.16","adj86.hh.inc.18")
income.hh[, empty_cols.86]<-NA

empty_cols.90 = c("adj90.hh.inc.79","adj90.hh.inc.80","adj90.hh.inc.81","adj90.hh.inc.82","adj90.hh.inc.83",
                  "adj90.hh.inc.84","adj90.hh.inc.85","adj90.hh.inc.86","adj90.hh.inc.87","adj90.hh.inc.88",
                  "adj90.hh.inc.89","adj90.hh.inc.90","adj90.hh.inc.91","adj90.hh.inc.92","adj90.hh.inc.93",
                  "adj90.hh.inc.94","adj90.hh.inc.96","adj90.hh.inc.98","adj90.hh.inc.00","adj90.hh.inc.02",
                  "adj90.hh.inc.04","adj90.hh.inc.06","adj90.hh.inc.08","adj90.hh.inc.10","adj90.hh.inc.12",
                  "adj90.hh.inc.14","adj90.hh.inc.16","adj90.hh.inc.18")
income.hh[, empty_cols.90]<-NA

#then create empty columns for converting equivalized income to income for hh size of 2
empty_cols.79.hh2 = c("adj79.hh2.inc.79","adj79.hh2.inc.80","adj79.hh2.inc.81","adj79.hh2.inc.82","adj79.hh2.inc.83",
                  "adj79.hh2.inc.84","adj79.hh2.inc.85","adj79.hh2.inc.86","adj79.hh2.inc.87","adj79.hh2.inc.88",
                  "adj79.hh2.inc.89","adj79.hh2.inc.90","adj79.hh2.inc.91","adj79.hh2.inc.92","adj79.hh2.inc.93",
                  "adj79.hh2.inc.94","adj79.hh2.inc.96","adj79.hh2.inc.98","adj79.hh2.inc.00","adj79.hh2.inc.02",
                  "adj79.hh2.inc.04","adj79.hh2.inc.06","adj79.hh2.inc.08","adj79.hh2.inc.10","adj79.hh2.inc.12",
                  "adj79.hh2.inc.14","adj79.hh2.inc.16","adj79.hh2.inc.18")
income.hh[, empty_cols.79.hh2]<-NA

empty_cols.10.hh22 = c("adj10.hh2.inc.79","adj10.hh2.inc.80","adj10.hh2.inc.81","adj10.hh2.inc.82","adj10.hh2.inc.83",
                  "adj10.hh2.inc.84","adj10.hh2.inc.85","adj10.hh2.inc.86","adj10.hh2.inc.87","adj10.hh2.inc.88",
                  "adj10.hh2.inc.89","adj10.hh2.inc.90","adj10.hh2.inc.91","adj10.hh2.inc.92","adj10.hh2.inc.93",
                  "adj10.hh2.inc.94","adj10.hh2.inc.96","adj10.hh2.inc.98","adj10.hh2.inc.00","adj10.hh2.inc.02",
                  "adj10.hh2.inc.04","adj10.hh2.inc.06","adj10.hh2.inc.08","adj10.hh2.inc.10","adj10.hh2.inc.12",
                  "adj10.hh2.inc.14","adj10.hh2.inc.16","adj10.hh2.inc.18")
income.hh[, empty_cols.10.hh22]<-NA

empty_cols.86.hh22 = c("adj86.hh2.inc.79","adj86.hh2.inc.80","adj86.hh2.inc.81","adj86.hh2.inc.82","adj86.hh2.inc.83",
                       "adj86.hh2.inc.84","adj86.hh2.inc.85","adj86.hh2.inc.86","adj86.hh2.inc.87","adj86.hh2.inc.88",
                       "adj86.hh2.inc.89","adj86.hh2.inc.90","adj86.hh2.inc.91","adj86.hh2.inc.92","adj86.hh2.inc.93",
                       "adj86.hh2.inc.94","adj86.hh2.inc.96","adj86.hh2.inc.98","adj86.hh2.inc.00","adj86.hh2.inc.02",
                       "adj86.hh2.inc.04","adj86.hh2.inc.06","adj86.hh2.inc.08","adj86.hh2.inc.10","adj86.hh2.inc.12",
                       "adj86.hh2.inc.14","adj86.hh2.inc.16","adj86.hh2.inc.18")
income.hh[, empty_cols.86.hh22]<-NA

empty_cols.90.hh22 = c("adj90.hh2.inc.79","adj90.hh2.inc.80","adj90.hh2.inc.81","adj90.hh2.inc.82","adj90.hh2.inc.83",
                       "adj90.hh2.inc.84","adj90.hh2.inc.85","adj90.hh2.inc.86","adj90.hh2.inc.87","adj90.hh2.inc.88",
                       "adj90.hh2.inc.89","adj90.hh2.inc.90","adj90.hh2.inc.91","adj90.hh2.inc.92","adj90.hh2.inc.93",
                       "adj90.hh2.inc.94","adj90.hh2.inc.96","adj90.hh2.inc.98","adj90.hh2.inc.00","adj90.hh2.inc.02",
                       "adj90.hh2.inc.04","adj90.hh2.inc.06","adj90.hh2.inc.08","adj90.hh2.inc.10","adj90.hh2.inc.12",
                       "adj90.hh2.inc.14","adj90.hh2.inc.16","adj90.hh2.inc.18")
income.hh[, empty_cols.90.hh22]<-NA


#convert to income adj hh size
colnames(income.hh)
sqrt.hhsize.list<-as.list(sqrt(income.hh[,145:172]))
income.hh[, 173:200] <- income.hh[, 33:60]/sqrt.hhsize.list #1979 dollars equiv on HH size
income.hh[, 201:228] <- income.hh[, 61:88]/sqrt.hhsize.list #2010 dollars equiv on HH size
income.hh[, 229:256] <- income.hh[, 89:116]/sqrt.hhsize.list #1986 dollars equiv on HH size
income.hh[, 257:284] <- income.hh[, 117:144]/sqrt.hhsize.list #1990 dollars equiv on HH size

  summary(income.hh$income79.adj79)
  summary(income.hh$HH_SIZE_NLSY_1979)
  summary(income.hh$adj79.hh.inc.79) #method deals with missing data appropriately (remains missing)

#test via hand calculating that the syntax above is doing conversions as expected
head(income.hh[, c(33,145,173)]) #conversion worked
  30000/sqrt(5)
  20000/sqrt(5)

#now convert inflation+household size adjusted incomes so all are for a household size of 2 (by multiplying by sqrt(2)) to enable comparisons with federal median for a household of 2
colnames(income.hh)
income.hh[, 285:312] <- income.hh[, 173:200]*sqrt(2) #convert adj 1979 income to hh of 2 for all and compare to federal median for hh of 2 for 1979
income.hh[, 313:340] <- income.hh[, 201:228]*sqrt(2) #convert adj 2010 income to hh of 2 for all and compare to federal median for hh of 2 for 2010
income.hh[, 341:368] <- income.hh[, 229:256]*sqrt(2) #convert adj 1986 income to hh of 2 for all and compare to federal median for hh of 2 for 1986
income.hh[, 369:396] <- income.hh[, 257:284]*sqrt(2) #convert adj 1990 income to hh of 2 for all and compare to federal median for hh of 2 for 1990

#again, test via hand calculating that the syntax works/is doing what we think it is
head(income.hh[, c(5,33,145,173,285)]) #conversion worked
  13416.408*sqrt(2)
  18000*sqrt(2)

#data check: do variables look okay? do the years that should not be converted look the same as the original?
  summary(income.hh$TOT_INCOME_NLSY_1979)  
  summary(income.hh$income79.adj79) #yes
  summary(income.hh$TOT_INCOME_NLSY_2010)  
  summary(income.hh$income10.adj10) #yes
  
#Income class exposure/income variable of potential interest in future NLSY79 analyses - go ahead and create now
#low, middle, and high income categories using low/high cut points and federal median income for a household of 2 in 2010 and 1979
income.hh <- income.hh %>%
  mutate(
    low.mi.cut.hh2.1979 = 16103*(2/3), #federal median for hh2 in 1979
    high.mi.cut.hh2.1979 = 16103*(2), #federal median for hh2 in 1979
    low.mi.cut.hh2.2010 = 54507*(2/3), #federal median for hh2 in 2010
    high.mi.cut.hh2.2010 = 54507*(2), #federal median for hh2 in 2010
    low.mi.cut.hh2.1986 = 25293*(2/3), #federal median for hh2 in 1986
    high.mi.cut.hh2.1986 = 25293*(2), #federal median for hh2 in 1986
    low.mi.cut.hh2.1990 = 31358*(2/3), #federal median for hh2 in 1990
    high.mi.cut.hh2.1990 = 31358*(2) #federal median for hh2 in 1990
)

#create empty columns for new income categories variables for each year
empty_cols.inccat79 = c("inc79.cat.79","inc79.cat.80","inc79.cat.81","inc79.cat.82","inc79.cat.83",
                      "inc79.cat.84","inc79.cat.85","inc79.cat.86","inc79.cat.87","inc79.cat.88",
                      "inc79.cat.89","inc79.cat.90","inc79.cat.91","inc79.cat.92","inc79.cat.93",
                      "inc79.cat.94","inc79.cat.96","inc79.cat.98","inc79.cat.00","inc79.cat.02",
                      "inc79.cat.04","inc79.cat.06","inc79.cat.08","inc79.cat.10","inc79.cat.12",
                      "inc79.cat.14","inc79.cat.16","inc79.cat.18")
income.hh[, empty_cols.inccat79]<-NA

empty_cols.inccat10 = c("inc10.cat.79","inc10.cat.80","inc10.cat.81","inc10.cat.82","inc10.cat.83",
                       "inc10.cat.84","inc10.cat.85","inc10.cat.86","inc10.cat.87","inc10.cat.88",
                       "inc10.cat.89","inc10.cat.90","inc10.cat.91","inc10.cat.92","inc10.cat.93",
                       "inc10.cat.94","inc10.cat.96","inc10.cat.98","inc10.cat.00","inc10.cat.02",
                       "inc10.cat.04","inc10.cat.06","inc10.cat.08","inc10.cat.10","inc10.cat.12",
                       "inc10.cat.14","inc10.cat.16","inc10.cat.18")
income.hh[, empty_cols.inccat10]<-NA

empty_cols.inccat86 = c("inc86.cat.79","inc86.cat.80","inc86.cat.81","inc86.cat.82","inc86.cat.83",
                        "inc86.cat.84","inc86.cat.85","inc86.cat.86","inc86.cat.87","inc86.cat.88",
                        "inc86.cat.89","inc86.cat.90","inc86.cat.91","inc86.cat.92","inc86.cat.93",
                        "inc86.cat.94","inc86.cat.96","inc86.cat.98","inc86.cat.00","inc86.cat.02",
                        "inc86.cat.04","inc86.cat.06","inc86.cat.08","inc86.cat.10","inc86.cat.12",
                        "inc86.cat.14","inc86.cat.16","inc86.cat.18")
income.hh[, empty_cols.inccat86]<-NA

empty_cols.inccat90 = c("inc90.cat.79","inc90.cat.80","inc90.cat.81","inc90.cat.82","inc90.cat.83",
                        "inc90.cat.84","inc90.cat.85","inc90.cat.86","inc90.cat.87","inc90.cat.88",
                        "inc90.cat.89","inc90.cat.90","inc90.cat.91","inc90.cat.92","inc90.cat.93",
                        "inc90.cat.94","inc90.cat.96","inc90.cat.98","inc90.cat.00","inc90.cat.02",
                        "inc90.cat.04","inc90.cat.06","inc90.cat.08","inc90.cat.10","inc90.cat.12",
                        "inc90.cat.14","inc90.cat.16","inc90.cat.18")
income.hh[, empty_cols.inccat90]<-NA

colnames(income.hh) #1979 vars: 405:432 ; 2010 vars: 433:460 ; 1986 vars: 461:488 ; 1990 vars: 489:516
                    #hh2 income variables to be categories: 285:312 ; 313:340 ; 341:368 ; 369:396 

income.hh[, 405:432]<- ifelse(income.hh[, 285:312]<income.hh$low.mi.cut.hh2.1979, 0,
                              ifelse(income.hh[, 285:312]>=income.hh$low.mi.cut.hh2.1979&income.hh[, 285:312]<=income.hh$high.mi.cut.hh2.1979, 1,
                                     ifelse(income.hh[, 285:312]>income.hh$high.mi.cut.hh2.1979, 2, NA)))
income.hh[, 433:460]<- ifelse(income.hh[, 313:340]<income.hh$low.mi.cut.hh2.2010, 0,
                              ifelse(income.hh[, 313:340]>=income.hh$low.mi.cut.hh2.2010&income.hh[, 313:340]<=income.hh$high.mi.cut.hh2.2010, 1,
                                     ifelse(income.hh[, 313:340]>income.hh$high.mi.cut.hh2.2010, 2, NA)))
income.hh[, 461:488]<- ifelse(income.hh[, 341:368]<income.hh$low.mi.cut.hh2.1986, 0,
                              ifelse(income.hh[, 341:368]>=income.hh$low.mi.cut.hh2.1986&income.hh[, 341:368]<=income.hh$high.mi.cut.hh2.1986, 1,
                                     ifelse(income.hh[, 341:368]>income.hh$high.mi.cut.hh2.1986, 2, NA)))
income.hh[, 489:516]<- ifelse(income.hh[, 369:396]<income.hh$low.mi.cut.hh2.1990, 0,
                              ifelse(income.hh[, 369:396]>=income.hh$low.mi.cut.hh2.1990&income.hh[, 369:396]<=income.hh$high.mi.cut.hh2.1990, 1,
                                     ifelse(income.hh[, 369:396]>income.hh$high.mi.cut.hh2.1990, 2, NA)))

#stop here and do some data checking - make sure all the correct variables used and calculations are correct
head(income.hh[, c(33,145,173,285,397,398,405)],n=20) #loos good/places people into correct categories

#in 1990 (baseline) how many people low, middle, or high income
round(prop.table(table(income.hh$inc79.cat.90)),3)*100
round(prop.table(table(income.hh$inc10.cat.90)),3)*100  
round(prop.table(table(income.hh$inc86.cat.90)),3)*100
round(prop.table(table(income.hh$inc90.cat.90)),3)*100

#in 2010 (end of exposure follow-up) how many people low, middle, or high income
round(prop.table(table(income.hh$inc79.cat.10)),3)*100
round(prop.table(table(income.hh$inc10.cat.10)),3)*100 
round(prop.table(table(income.hh$inc86.cat.10)),3)*100
round(prop.table(table(income.hh$inc90.cat.10)),3)*100

#look at dist of a few income variables over time
hist(income.hh$adj90.hh2.inc.10)
hist(income.hh$adj10.hh2.inc.10)

hist(income.hh$adj90.hh2.inc.90)
hist(income.hh$adj10.hh2.inc.90)

hist(income.hh$adj90.hh2.inc.16)
hist(income.hh$adj10.hh2.inc.16)

#maybe better to compare someone's actual income in a year to federal median *for that year* instead of using the inflation adjusted ones?
#can explore that in future analysis if this comparison to federal median ends up being important; for now, just store federal median values in data.frame to have on hand but do not create new income class variables
fedmedshh2<-as.data.frame(matrix(nrow=12686,ncol=28))
colnames(fedmedshh2)<-c("FedMed1979","FedMed1980","FedMed1981","FedMed1982","FedMed1983","FedMed1984","FedMed1985","FedMed1986",
                        "FedMed1987","FedMed1988","FedMed1989","FedMed1990","FedMed1991","FedMed1992","FedMed1993","FedMed1994",
                        "FedMed1996","FedMed1998","FedMed2000","FedMed2002","FedMed2004","FedMed2006","FedMed2008","FedMed2010",
                        "FedMed2012","FedMed2014","FedMed2016","FedMed2018")
fedmedshh2[,c(1:28)]<-c(replicate(n=12686, 16103), replicate(n=12686, 17506), replicate(n=12686, 18908), replicate(n=12686, 20200), replicate(n=12686, 21064), 
    replicate(n=12686, 22614), replicate(n=12686, 23867), replicate(n=12686, 25293), replicate(n=12686, 26522), replicate(n=12686, 28021), 
    replicate(n=12686, 29862), replicate(n=12686, 31358), replicate(n=12686, 31221), replicate(n=12686, 31816), replicate(n=12686, 32434), 
    replicate(n=12686, 33955), replicate(n=12686, 37283), replicate(n=12686, 41512), replicate(n=12686, 44459), replicate(n=12686, 45556), 
    replicate(n=12686, 47160), replicate(n=12686, 51536), replicate(n=12686, 55418), replicate(n=12686, 54507), replicate(n=12686, 56727), 
    replicate(n=12686, 60406), replicate(n=12686, 65627), replicate(n=12686, 70870))

#create low cut point for low income
fedmedshh2[,c(29:56)]<-fedmedshh2[,c(1:28)]*(2/3)
colnames(fedmedshh2)[c(29:56)]<-paste("lowcut", colnames(fedmedshh2)[c(1:28)], sep=".")

#create high cut point for high income
fedmedshh2[,c(57:84)]<-fedmedshh2[,c(1:28)]*(2)
colnames(fedmedshh2)[c(57:84)]<-paste("hicut", colnames(fedmedshh2)[c(1:28)], sep=".")

#merge federal medians over time into dataset with incomes over time
income.hh<-cbind(income.hh,fedmedshh2)
#now - if needed - dataset contains the relevant variables to create low/middle/high income categories based on real dollars, not inflation adjusted incomes

#-------------------------------------------------------------------------------
#Income volatility - exposure of interest for synthetic cohort analysis
  #create this in a separate step/after combining all of the datasets together (within cohorts and then stacking cohorts) and dealing with missing data (syntax file 01)
  #then create income volatility exposure in stacked dataset (it will only get created for the NLSY participants with data on these variables)
  #will be created in file 01_Combine_NLSY&HRS

colnames(income.hh)
income.hh <- income.hh[, -c(2,3,4)] #will cbind all data frames together at end, so remove these variables for now (create race and sex variables in later step)

#-------------------------------------------------------------------------------
#POVERTY STATUS
#These variables reflect the respondent's household's actual status with respect to the poverty level for his/her family size.
poverty <- read.csv("poverty.csv")

#rename
names(poverty) <- c("NLSY_CASE_ID", "NLSY_SAMPLE_ID", 
                    "POV_NLSY_1979",  "POV_NLSY_1980",
                    "POV_NLSY_1981", "POV_NLSY_1982",
                    "POV_NLSY_1983", "POV_NLSY_1984",
                    "POV_NLSY_1985", "POV_NLSY_1986",
                    "POV_NLSY_1987", "POV_NLSY_1988",
                    "POV_NLSY_1989", "POV_NLSY_1990",
                    "POV_NLSY_1991", "POV_NLSY_1992",
                    "POV_NLSY_1993", "POV_NLSY_1994",#starting 1994, biennial survey
                    "POV_NLSY_1996", "POV_NLSY_1998",
                    "POV_NLSY_2000", "POV_NLSY_2002",
                    "POV_NLSY_2004", "POV_NLSY_2006",
                    "POV_NLSY_2008", "POV_NLSY_2010",
                    "POV_NLSY_2012", "POV_NLSY_2014",
                    "POV_NLSY_2016", "POV_NLSY_2018")

#recode negative value into NA
#1 IN POVERTY; 0 NOT IN POVERTY
table(poverty$POV_NLSY_1979, exclude = NULL)
poverty[, 3:30] <- lapply(poverty[, 3:30], function(x) ifelse(x<0, NA, x))

table(poverty$POV_NLSY_1979, useNA="ifany")

poverty$poverty_2010<-poverty$POV_NLSY_2010 #consistent with HRS naming for matching variables

#drop the sample ID
poverty <- poverty[, -2]
colnames(poverty)


#####################################################################################
#CANDIDATE MATCHING VARIABLES (POTENTIAL CONFOUNDERS)

#-------------------------------------------------------------------------------
#race & ethnicity & sex
race.exercise<-read.csv("race_exercise.csv")
colnames(race.exercise)
race<-race.exercise[,c(1:7)]
names(race)<-c("NLSY_CASE_ID","RACE_FIRST_ID","RACE_SECOND_ID","RACE_MOST_ID","SAMPLE_ID","SAMPLE_RACE","SAMPLE_SEX")

#clean NA values
race[, 2:7] <- lapply(race[, 2:7], function(x) ifelse(x<0, NA, x))

# If respondent either most closely identified with OR identified with a Hispanic background as first or only ethnicity, they were coded as Hispanic
# Otherwise, coded as non-hispanic
table(race$RACE_FIRST_ID, useNA="ifany")
table(race$RACE_SECOND_ID, useNA="ifany")
table(race$RACE_MOST_ID, useNA="ifany")
table(race$SAMPLE_RACE, useNA="ifany")
table(race$RACE_FIRST_ID, race$RACE_MOST_ID, exclude=NULL)

race1 <- race %>%
  mutate(
    eth = ifelse(RACE_MOST_ID %in% c(15,16,17,18,19,20,21),1, #1=Hispanic
                 ifelse(RACE_FIRST_ID %in% c(15,16,17,18,19,20,21),1,0)), #1=Hispanic, 0=Non-Hispanic
    eth = factor(eth, levels = c(0,1), labels = c("Non-hispanic", "Hispanic")))

race1$eth[is.na(race1$RACE_FIRST_ID)&is.na(race1$RACE_MOST_ID)]<-NA
  table(race1$eth, exclude=NULL)

#can recover some missing data by using SAMPLE_RACE variable
race1$eth[is.na(race1$eth)&race1$SAMPLE_RACE==1]<-"Hispanic"
race1$eth[is.na(race1$eth)&race1$SAMPLE_RACE==2]<-"Non-hispanic"
race1$eth[is.na(race1$eth)&race1$SAMPLE_RACE==3]<-"Non-hispanic"
  table(race1$eth, race1$SAMPLE_RACE, exclude=NULL)

# Using a similar schema; if the race they most identified with (or only identified with) was Black, they were coded as Black
# If they most identified with a White identity, they were coded as White
# Otherwise, they were coded as "Other"
race2 <- race1 %>%
  mutate(
    race = case_when(
      (RACE_MOST_ID == 1 | RACE_FIRST_ID == 1) ~ 2, #Black
      (RACE_MOST_ID %in% c(3,5,6,7,11,12,22,23,24,25,27) | 
         RACE_FIRST_ID %in% c(3,5,6,7,11,12,22,23,24,25,27)) ~ 1,#White
      (RACE_MOST_ID == 29 & SAMPLE_RACE == 2) ~ 2, #Black
      TRUE ~ 3), #Other
    SAMPLE_RACE = factor(SAMPLE_RACE, levels = c(1,2,3), labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
    race = factor(race, levels = c(1,2,3), labels = c("White", "Black", "Other"))#,
)
table(race2$race, exclude=NULL)

race2$race[is.na(race1$RACE_FIRST_ID)&is.na(race1$RACE_MOST_ID)]<-NA
  table(race2$race, exclude=NULL)
  table(race2$race, race2$SAMPLE_RACE, exclude=NULL)  
race2$race[is.na(race2$RACE_FIRST_ID)&is.na(race2$RACE_MOST_ID)&race2$SAMPLE_RACE=="Hispanic"]<-"Other"
race2$race[is.na(race2$RACE_FIRST_ID)&is.na(race2$RACE_MOST_ID)&race2$SAMPLE_RACE=="Black"]<-"Black"

#leave rest as missing
#because of difficulties defining race here super similar to HRS, may need to create a combined race/ethnicity variable (SAMPLE_RACE) in HRS (harmonize HRS to NLSY79)
table(race2$race, exclude=NULL)


#sex
table(race2$SAMPLE_SEX, exclude=NULL) #1=male , 2=female
race2$female<-ifelse(race2$SAMPLE_SEX==2,1,0)
table(race2$female)  
race2$female = factor(race2$female, levels = c(0, 1), labels = c("Male", "Female"))

race<-race2

colnames(race)
race<-race[,-c(5)]

#-------------------------------------------------------------------------------
#nativity
nativity <- read.csv("nativity.csv")
nativity <- as.data.frame(nativity)
names(nativity) <- c("NLSY_CASE_ID","nativity")

#recode negative value into NA (n=1)
nativity$nativity <- ifelse(nativity$nativity<0, NA, nativity$nativity)

#recode 2 IN OTHER COUNTRY as 0
nativity$usbirth <- ifelse(nativity$nativity==2, 0, nativity$nativity)
nativity$usbirth <- factor(nativity$usbirth, levels = c(0,1), labels = c("non-US birth", "US birth"))
table(nativity$usbirth, exclude=NULL)

colnames(nativity)

#-------------------------------------------------------------------------------
#self education
#https://nlsinfo.org/content/cohorts/nlsy79/topical-guide/education/educational-attainment-school-enrollment
education <- read.csv("education.csv")
education <- as.data.frame(education)

names(education) <- c("NLSY_CASE_ID","EDUCATION_NLSY") #highest grade ever completed

#recode negative values as NA
education$EDUCATION_NLSY <- ifelse(education$EDUCATION_NLSY<0, NA, education$EDUCATION_NLSY)

table(education$EDUCATION_NLSY, exclude=NULL)

#years of education
#topcoded those above 17 to be consistent with HRS
education$edu_yrs <- ifelse(education$EDUCATION_NLSY>=17, 17, education$EDUCATION_NLSY)
table(education$edu_yrs, useNA="ifany")

colnames(education)

#-------------------------------------------------------------------------------
#parental SES
parent <- read.csv("parent.csv")
colnames(parent)

#rename
names(parent) <- c("NLSY_CASE_ID", 
                   "MOM_EDU_NLSY", #HIGHEST GRADE COMPLETED BY R'S MOTHER
                   "MOM_WORK_NLSY_1978",#DID MOTHER/STEPMOTHER WORK FOR PAY ALL OF 1978, PART, OR NOT AT  ALL?
                   "MOM_OCCUPATION_NLSY_1978",#OCCUPATION OF LONGEST JOB IN 1978, R'S MOTHER/STEPMOTHER (CENSUS 3 DIGIT)
                   "MOM_WORK35HRS_NLSY_1978",#DID MOTHER/STEPMOTHER WORK > 35 HOURS PER WEEK IN 1978?
                   "LIVE_WITH_MOM_NLSY_1979",#DO R AND MOTHER/STEPMOTHER LIVE SEPARATELY?
                   "DAD_EDU_NLSY", #HIGHEST GRADE COMPLETED BY R'S FATHER
                   "DAD_WORK_NLSY_1978",#DID  FATHER/STEPFATHER WORK FOR PAY ALL OF 1978, PART, OR NOT AT  ALL?
                   "DAD_OCCUPATION_NLSY_1978",#OCCUPATION OF LONGEST JOB IN 1978, R'S FATHER/STEPFATHER (CENSUS 3 DIGIT)
                   "DAD_WORK35HRS_NLSY_1978",#DID FATHER/STEPFATHER WORK > 35 HOURS PER WEEK IN 1978?
                   "LIVE_WITH_DAD_NLSY_1979",# DO R AND FATHER/STEPFATHER LIVE SEPARATELY?
                   "NLSY_SAMPLE_ID"
                   )

#recode negative value into NA
parent[, 2:11] <- lapply(parent[, 2:11], function(x) ifelse(x<0, NA, x)) 

#create new measure: highest parent edu
table(parent$MOM_EDU_NLSY, parent$DAD_EDU_NLSY)
parent$pedu_yrs <- ifelse(parent$MOM_EDU_NLSY<parent$DAD_EDU_NLSY, parent$DAD_EDU_NLSY, 
                                 parent$MOM_EDU_NLSY)
table(parent$pedu_yrs, exclude = NULL)

parent <- parent %>% 
  mutate(
    pedu_cat = case_when(pedu_yrs<8 ~ 1,
                         pedu_yrs>=8&pedu_yrs<12 ~ 2,
                         pedu_yrs==12 ~ 3,
                         pedu_yrs>12 ~ 4,
                          TRUE ~ 5),
    pedu_cat = factor(pedu_cat, levels = c(1,2,3,4,5), labels = c("<8 years", "8-<12 years","12 years",">12 years","Missing"))
    
  )

table(parent$pedu_cat)

#**Will just use parental education because the parent work variables are not exactly the same in both datasets (e.g., dissimilar time frame). Also, in HRS the parent working during childhood variables were not super predictive of either income or cognition

#Only keep the variables we need
colnames(parent)
parent <- parent[, c(1,13,14)]


#-------------------------------------------------------------------------------
#geography: if born in South, if lived in South at age of 14
geography <- read.csv("geography.csv")

#rename
names(geography) <- c("NLSY_CASE_ID", 
                      "bornsouth", #SOUTH-NONSOUTH PLACE OF BIRTH IN U.S.
                      "childhoodsouth", #SOUTH-NONSOUTH RESIDENCE IN U.S. AT AGE 14	
                      "NLSY_SAMPLE_ID")

#recode negative value into 0
geography[, 2:3] <- lapply(geography[, 2:3], function(x) ifelse(x<0, NA, x))

#if born in South
#0 NONSOUTH; 1 SOUTH
table(geography$bornsouth, exclude = NULL)

#if lived in South at age of 14
#1 YES; 0 NO
table(geography$childhoodsouth, exclude = NULL)


#only keep the variables we need
colnames(geography)
geography <- geography[, -4]

table(geography$bornsouth, exclude = NULL)#1090 NA

#create a measure: born in South OR lived in South as a child (try to preserve as much N as possible)
geography$born.child.south <- ifelse(is.na(geography$bornsouth), geography$childhoodsouth,
                                    geography$bornsouth)
table(geography$born.child.south, exclude = NULL)# 318 NA

 
#-------------------------------------------------------------------------------
#birth year
birthdate <- read.csv("birthdate.csv")
birthdate <- as.data.frame(birthdate)
names(birthdate) <- c("NLSY_CASE_ID","birthmonth", "BIRTH_YEAR_NLSY")
birthdate$birthyear <- 1900+birthdate$BIRTH_YEAR_NLSY
  summary(birthdate$birthyear)#No NA
  summary(birthdate$birthmonth)#no NA

colnames(birthdate)
birthdate<-birthdate[,-3]


#-------------------------------------------------------------------------------
#AGE OF R AT INTERVIEW DATE
age <- read.csv("AgeInterviewDate.csv")

#rename
names(age) <- c("NLSY_CASE_ID",  
                "age_1979",  "age_1980",
                "age_1981", "age_1982",
                "age_1983", "age_1984",
                "age_1985", "age_1986",
                "age_1987", "age_1988",
                "age_1989", "age_1990",
                "age_1991", "age_1992",
                "age_1993", "age_1994",#starting 1994, biennial survey
                "age_1996", "age_1998",
                "age_2000", "age_2002",
                "age_2004", "age_2006",
                "age_2008", "age_2010",
                "age_2012", "age_2014",
                "age_2016", "age_2018")

#recode negative value into 0
age[, 2:29] <- lapply(age[, 2:29], function(x) ifelse(x<0, NA, x))
summary(age$age_2010)

colnames(age)


#-------------------------------------------------------------------------------
#age at 1st marriage
Age1stMarriage <- read.csv("age1stmarriage.csv")

#rename
names(Age1stMarriage) <- c("NLSY_CASE_ID", "age.1stmarriage")
table(Age1stMarriage$age.1stmarriage)

#recode value=-1, -2, -3 into NA
Age1stMarriage$age.1stmarriage <- ifelse(Age1stMarriage$age.1stmarriage<0&Age1stMarriage$age.1stmarriage>=-3, NA, Age1stMarriage$age.1stmarriage)
table(Age1stMarriage$age.1stmarriage, exclude = NULL)

#recode into categorical
#missing, <20, 20-29, 30-39, 40+, -999=never married
Age1stMarriage$age1stmarried.cat <- ifelse(Age1stMarriage$age.1stmarriage==-999, "never married",
                                              ifelse(Age1stMarriage$age.1stmarriage<20, "<20",
                                                     ifelse(Age1stMarriage$age.1stmarriage>=20&
                                                              Age1stMarriage$age.1stmarriage<30, "20-29",
                                                            ifelse(Age1stMarriage$age.1stmarriage>=30&
                                                                     Age1stMarriage$age.1stmarriage<40, "30-39",
                                                                   ifelse(Age1stMarriage$age.1stmarriage>=40, "40+", NA)))))
Age1stMarriage$age1stmarried.cat[is.na(Age1stMarriage$age1stmarried.cat)] <- "missing"
Age1stMarriage$age1stmarried.cat <- factor(Age1stMarriage$age1stmarried.cat, levels = c("never married", "<20", "20-29", "30-39", "40+", "missing"))

table(Age1stMarriage$age1stmarried.cat)
round(prop.table(table(Age1stMarriage$age1stmarried.cat))*100, 2)

colnames(Age1stMarriage)


#-------------------------------------------------------------------------------
#Earlier-life cognition (AFQT scores)

#merge in data on baseline cognition - to try to partly control for it
#import baseline cognition
basecog<-read.csv("baseline_cog.csv")
names(basecog)<-c("NLSY_CASE_ID","AFQTpctlscore80.81","AFQTpctlscore89rev.81","AFQTpctlscore06rev.81","ASVAB.arithmath.z","ASVAB.arithmath.wtpct",
                  "ASVAB.wordpara.z","ASVAB.wordpara.wtpct","ASVAB.arith.z","ASVAB.arith.wtpct","ASVAB.word.z","ASVAB.word.wtpct",
                  "ASVAB.para.z","ASVAB.para.wtpct","ASVAB.math.z","ASVAB.math.wtpct")
basecog <- lapply(basecog, function(x) ifelse(x<0, NA, x))
basecog<-as.data.frame(basecog)

summary(basecog$AFQTpctlscore80.81)
summary(basecog$AFQTpctlscore89rev.81)
summary(basecog$AFQTpctlscore06rev.81)/1000

#06 revised has 3 implied decimal places so divide variable by 1000
basecog$AFQTpctlscore06rev.81.r<-basecog$AFQTpctlscore06rev.81/1000
summary(basecog$AFQTpctlscore06rev.81.r)

names(basecog)
basecog <- basecog[, c(1:4, 17)]

##########################################################################################################################################################################
#CANDIDATE MATCHING VARIABLES (MEDIATORS AT Tm, pull any year between 2010 and 2018; prioritize matching in 2010 use others for carry forward/back as needed)

#region of current residence
currentregion <- read.csv("currentregion.csv")

#REGION OF RESIDENCE
names(currentregion) <- c("NLSY_CASE_ID","currentregion_2010", "currentregion_2012",
                           "currentregion_2014", "currentregion_2016",
                           "currentregion_2018")

#recode negative value into NA
currentregion[, 2:6] <- lapply(currentregion[, 2:6], function(x) ifelse(x<0, NA, x))
#1: NORTHEAST; 2: NORTH CENTRAL (aka midwest); 3: SOUTH; 4: WEST

table(currentregion$currentregion_2010)

colnames(currentregion)

#-------------------------------------------------------------------------------
#family wealth; only available in 2012, 2016 NLSY
#https://nlsinfo.org/content/cohorts/nlsy79/topical-guide/income/assets
wealth <- read.csv("wealth.csv")

# FAMILY NET WEALTH (TRUNC) *KEY*
names(wealth) <- c("NLSY_CASE_ID","wealth_2008", "wealth_2012", "wealth_2016")

#recode negative values into NA
wealth[, 2:4] <- lapply(wealth[, 2:4], function(x) ifelse(x<0, NA, x))

wealth$wealth10.2008 <- wealth$wealth_2008*1.01 #convert 2008 wealth to 2010 dollars, in case need inflation adjusted in matching
wealth$wealth10.2012 <- wealth$wealth_2012*0.95 #convert 2012 wealth to 2010 dollars, in case need inflation adjusted in matching
wealth$wealth10.2016 <- wealth$wealth_2016*0.91 #convert 2016 wealth to 2010 dollars, in case need inflation adjusted in matching
  summary(wealth$wealth10.2008)  
  summary(wealth$wealth10.2012)
  summary(wealth$wealth10.2016)

wealth$wealth_2010<-(wealth$wealth10.2008+wealth$wealth10.2012)/2
  summary(wealth$wealth_2010)
wealth$wealth_2010<-ifelse(is.na(wealth$wealth_2010),wealth$wealth_2008,wealth$wealth_2010) #if NA use 2008 value first
  summary(wealth$wealth_2010)
wealth$wealth_2010<-ifelse(is.na(wealth$wealth_2010),wealth$wealth_2012,wealth$wealth_2010) #if still NA use 2012 value
  summary(wealth$wealth_2010)
  
colnames(wealth)


#-------------------------------------------------------------------------------
#employment
employment <- read.csv("employment.csv")

#DATE OF INTERVIEW STATUS - EMPLOYED; 1  YES; 0  NO
#DATE OF INTERVIEW STATUS - HOURS WORKED; 0: LESS THAN 30 HOURS; 1: 30 HOURS OR MORE
#DATE OF INTERVIEW STATUS - RETIRED; 1  YES; 0  NO; RETIRED MILITARY
#DATE OF INTERVIEW STATUS - DISABLED; 1  YES; 0  NO

names(employment) <- c("NLSY_CASE_ID", "EMPLOYED_2010", "HRS_WORKED_2010", "RETIRED_2010", "DISABLED_2010",
                       "EMPLOYED_2012", "HRS_WORKED_2012", "RETIRED_2012", "DISABLED_2012",
                       "EMPLOYED_2014", "HRS_WORKED_2014", "RETIRED_2014", "DISABLED_2014",
                       "EMPLOYED_2016", "HRS_WORKED_2016", "RETIRED_2016", "DISABLED_2016",
                       "EMPLOYED_2018", "HRS_WORKED_2018", "RETIRED_2018", "DISABLED_2018")

empl.istat<-left_join(iwstatus, employment, by="NLSY_CASE_ID") #iwsstatus gives info on who is in the study (vs true NA) for defining not in LbRF consistent with
colnames(empl.istat)

#recode negative value as NA
empl.istat[, 1:25] <- lapply(empl.istat[, 1:25], function(x) ifelse(x<0, NA, x))

#recode new labor force status 
#yr 2010
table(empl.istat$iwstatus_2010)
table(empl.istat$RETIRED_2010, exclude = NULL)
table(empl.istat$DISABLED_2010, exclude = NULL)
empl.istat$LBRF_2010 <- rep("Not in LbrF", nrow(empl.istat))
  table(empl.istat$LBRF_2010, exclude = NULL)
empl.istat$LBRF_2010[empl.istat$iwstatus_2010==0]<-NA
empl.istat$LBRF_2010[empl.istat$EMPLOYED_2010==1&empl.istat$HRS_WORKED_2010==1] <- "Work FT"
empl.istat$LBRF_2010[empl.istat$EMPLOYED_2010==1&empl.istat$HRS_WORKED_2010==0] <- "Work PT"
empl.istat$LBRF_2010[empl.istat$EMPLOYED_2010==0] <- "Unemployed"
empl.istat$LBRF_2010[empl.istat$RETIRED_2010==1] <- "Retired"
empl.istat$LBRF_2010[empl.istat$DISABLED_2010==1] <- "Disabled"
table(empl.istat$LBRF_2010, exclude = NULL)

#yr 2012
table(empl.istat$RETIRED_2012, exclude = NULL)
table(empl.istat$DISABLED_2012, exclude = NULL)
empl.istat$LBRF_2012 <- rep("Not in LbrF", nrow(empl.istat))
table(empl.istat$LBRF_2012, exclude = NULL)
  empl.istat$LBRF_2012[empl.istat$iwstatus_2012==0]<-NA
empl.istat$LBRF_2012[empl.istat$EMPLOYED_2012==1&empl.istat$HRS_WORKED_2012==1] <- "Work FT"
empl.istat$LBRF_2012[empl.istat$EMPLOYED_2012==1&empl.istat$HRS_WORKED_2012==0] <- "Work PT"
empl.istat$LBRF_2012[empl.istat$EMPLOYED_2012==0] <- "Unemployed"
empl.istat$LBRF_2012[empl.istat$RETIRED_2012==1] <- "Retired"
empl.istat$LBRF_2012[empl.istat$DISABLED_2012==1] <- "Disabled"
table(empl.istat$LBRF_2012, exclude = NULL)

#yr 2014
table(empl.istat$RETIRED_2014, exclude = NULL)
table(empl.istat$DISABLED_2014, exclude = NULL)
empl.istat$LBRF_2014 <- rep("Not in LbrF", nrow(empl.istat))
  table(empl.istat$LBRF_2014, exclude = NULL)
empl.istat$LBRF_2014[empl.istat$iwstatus_2014==0]<-NA
empl.istat$LBRF_2014[empl.istat$EMPLOYED_2014==1&empl.istat$HRS_WORKED_2014==1] <- "Work FT"
empl.istat$LBRF_2014[empl.istat$EMPLOYED_2014==1&empl.istat$HRS_WORKED_2014==0] <- "Work PT"
empl.istat$LBRF_2014[empl.istat$EMPLOYED_2014==0] <- "Unemployed"
empl.istat$LBRF_2014[empl.istat$RETIRED_2014==1] <- "Retired"
empl.istat$LBRF_2014[empl.istat$DISABLED_2014==1] <- "Disabled"
table(empl.istat$LBRF_2014, exclude = NULL)

#yr 2016
table(empl.istat$RETIRED_2016, exclude = NULL)
table(empl.istat$DISABLED_2016, exclude = NULL)
empl.istat$LBRF_2016 <- rep("Not in LbrF", nrow(empl.istat))
  table(empl.istat$LBRF_2016, exclude = NULL)
empl.istat$LBRF_2016[empl.istat$iwstatus_2016==0]<-NA
empl.istat$LBRF_2016[empl.istat$EMPLOYED_2016==1&empl.istat$HRS_WORKED_2016==1] <- "Work FT"
empl.istat$LBRF_2016[empl.istat$EMPLOYED_2016==1&empl.istat$HRS_WORKED_2016==0] <- "Work PT"
empl.istat$LBRF_2016[empl.istat$EMPLOYED_2016==0] <- "Unemployed"
empl.istat$LBRF_2016[empl.istat$RETIRED_2016==1] <- "Retired"
empl.istat$LBRF_2016[empl.istat$DISABLED_2016==1] <- "Disabled"
table(empl.istat$LBRF_2016, exclude = NULL)

#yr 2018
table(empl.istat$RETIRED_2018, exclude = NULL)
table(empl.istat$DISABLED_2018, exclude = NULL)
empl.istat$LBRF_2018 <- rep("Not in LbrF", nrow(empl.istat))
  table(empl.istat$LBRF_2018, exclude = NULL)
empl.istat$LBRF_2018[empl.istat$iwstatus_2018==0]<-NA
empl.istat$LBRF_2018[empl.istat$EMPLOYED_2018==1&empl.istat$HRS_WORKED_2018==1] <- "Work FT"
empl.istat$LBRF_2018[empl.istat$EMPLOYED_2018==1&empl.istat$HRS_WORKED_2018==0] <- "Work PT"
empl.istat$LBRF_2018[empl.istat$EMPLOYED_2018==0] <- "Unemployed"
empl.istat$LBRF_2018[empl.istat$RETIRED_2018==1] <- "Retired"
empl.istat$LBRF_2018[empl.istat$DISABLED_2018==1] <- "Disabled"
table(empl.istat$LBRF_2018, exclude = NULL)

#drop the raw measures
colnames(empl.istat)
employment <- empl.istat[, c(1,27:31)]

#relevel for all LBRF measures
employment[, 2:6] <- lapply(employment[, 2:6], function(x) factor(x, levels = c("Work FT", "Work PT",
                                                                                "Unemployed", "Retired", 
                                                                                "Disabled", "Not in LbrF")))
table(employment$LBRF_2010, exclude = NULL)


colnames(employment)

#-------------------------------------------------------------------------------
#occupation
occupation <- read.csv("occupation.csv")

names(occupation) <- c("NLSY_CASE_ID", "OCCUPATION_2010",  "OCCUPATION_2012",
                       "OCCUPATION_2014",  "OCCUPATION_2016",
                       "OCCUPATION_2018")

#recode negative values into NA
occupation[, 2:6] <- lapply(occupation[, 2:6], function(x) ifelse(x<0, NA, x))
occupation[, 2:6] <- lapply(occupation[, 2:6], function(x) ifelse(x==9990, NA, x))

table(occupation$OCCUPATION_2010, exclude=NULL)
summary(occupation$OCCUPATION_2010)

###(??): decide later if will create a low skill occupation variable or if just matching on employment status is sufficient
#https://www.census.gov/topics/employment/industry-occupation/guidance/code-lists.html
#NLSY79 uses CENSUS categorizations (2000 codes)
#high-skilled employment (ISCO-08 Skill Level 3 or 4), lower skilled employment (ISCO-08 Skill Levels 1 or 2), or non-employment. Although nonemployment could be further decomposed into several categories (e.g. unemployment, “housewife”, student, retiree, or permanently restricted from working due to disability), these data were not collected for spouses until 1979 and several of the states were experienced rarely in this sample (<5% of measured person-years). Non-employment was therefore treated as a single state.
#use the major groupings here: https://ilostat.ilo.org/methods/concepts-and-definitions/classification-occupation/

#Makes most sense to group as high skilled (major groups 3&4) vs rest
occupation[, 2:6] <- lapply(occupation[, 2:6], function(x) ifelse(x>0&x<3700, 2, x)) #higher skilled
occupation[, 2:6] <- lapply(occupation[, 2:6], function(x) ifelse(x>=3700&x<9990, 1, x)) #lower skilled

table(occupation$OCCUPATION_2010, useNA="ifany") #should be 6422 NAs - yes
table(occupation$OCCUPATION_2012, useNA="ifany")
table(occupation$OCCUPATION_2014, useNA="ifany")
table(occupation$OCCUPATION_2016, useNA="ifany")
table(occupation$OCCUPATION_2018, useNA="ifany")
  #2=major group 3&4 (higher skill); 1=lower skill

occupation <- occupation %>% 
  rename(occuskill_2010=OCCUPATION_2010, occuskill_2012=OCCUPATION_2012, occuskill_2014=OCCUPATION_2014, occuskill_2016=OCCUPATION_2016, occuskill_2018=OCCUPATION_2018)

colnames(occupation)
#need to in NLSY dataset make sure missing category for people who weren't employed (do in later step once all datasets are combined)

#-------------------------------------------------------------------------------
#health insurance (employer-provided)
health_ins <- read.csv("ephealth_ins.csv")

##SOURCE OF HEALTH/HOSPITALIZATION (RESPONDENT) - PRIVATE INSURANCE R'S CURRENT/PREVIOUS EMPLOYER
names(health_ins) <- c("NLSY_CASE_ID", "insurance_2010","epinsurance_rcbr_2010","epinsurance_rcbs_2010","insurance_2012","epinsurance_rcbr_2012","epinsurance_rcbs_2012",
                       "insurance_2014","epinsurance_rcbcr_2014","epinsurance_rcbpr_2014","epinsurance_rcbcs_2014","epinsurance_rcbps_2014",
                       "insurance_2016", "epinsurance_rcbrs_2016", "insurance_2018", "epinsurance_rcbrs_2018")

health_ins[, 2:16] <- lapply(health_ins[, 2:16], function(x) ifelse(x<0, NA, x))


#lead in question is "R COVERED BY ANY HEALTH/HOSPITALIZATION PLAN" and 1429 said "no"
table(health_ins$insurance_2010, exclude=NULL)
table(health_ins$epinsurance_rcbr_2010, exclude=NULL)
table(health_ins$epinsurance_rcbs_2010, exclude=NULL)
table(health_ins$epinsurance_rcbr_2010,health_ins$epinsurance_rcbs_2010, exclude=NULL)

table(health_ins$epinsurance_rcbr_2012,health_ins$epinsurance_rcbs_2012, exclude=NULL)
table(health_ins$epinsurance_rcbcr_2014,health_ins$epinsurance_rcbpr_2014,health_ins$epinsurance_rcbcs_2014,health_ins$epinsurance_rcbps_2014, exclude=NULL)

#private health insurance through own/spouses current or former employer (=1); through other means or no insurance (=0) (consistent with HRS)
health_ins <- health_ins %>%
  mutate(
    epinsurance_2010 = ifelse(insurance_2010==0,0,
                              ifelse(epinsurance_rcbr_2010==1|epinsurance_rcbs_2010==1,1,
                                     ifelse(epinsurance_rcbr_2010==0&epinsurance_rcbs_2010==0,0,NA))),
    epinsurance_2012 = ifelse(insurance_2012==0,0,
                              ifelse(epinsurance_rcbr_2012==1|epinsurance_rcbs_2012==1,1,
                                     ifelse(epinsurance_rcbr_2012==0&epinsurance_rcbs_2012==0,0,NA))),
    epinsurance_2014 = ifelse(insurance_2014==0,0,
                              ifelse(epinsurance_rcbcr_2014==1|epinsurance_rcbpr_2014==1|epinsurance_rcbcs_2014==1|epinsurance_rcbps_2014==1,1,
                                     ifelse(epinsurance_rcbcr_2014==0&epinsurance_rcbpr_2014==0&epinsurance_rcbcs_2014==0&epinsurance_rcbps_2014==0,0,NA))),
    epinsurance_2016 = ifelse(insurance_2016==0,0,
                              ifelse(epinsurance_rcbrs_2016==1|epinsurance_rcbrs_2016==2|epinsurance_rcbrs_2016==3|epinsurance_rcbrs_2016==4,1,
                                     ifelse(epinsurance_rcbrs_2016>4,0,NA))),
    epinsurance_2018 = ifelse(insurance_2018==0,0,
                              ifelse(epinsurance_rcbrs_2018==1|epinsurance_rcbrs_2018==2|epinsurance_rcbrs_2018==3|epinsurance_rcbrs_2018==4,1,
                                     ifelse(epinsurance_rcbrs_2018>4,0,NA)))
  )

colnames(health_ins)

#measures in year 2010-2014, 0=no, 1=yes
table(health_ins$epinsurance_2010, exclude=NULL) 
table(health_ins$epinsurance_2012, exclude=NULL)
table(health_ins$epinsurance_2014, exclude=NULL)
table(health_ins$epinsurance_2016, exclude=NULL)
table(health_ins$epinsurance_2018, exclude=NULL)

colnames(health_ins)
health_ins<-health_ins[,c(1,17:21)]

#-------------------------------------------------------------------------------
#weight (given pounds, convert to kilograms)
#height (given feet and inches, convert to meters) in 2010-2018
WeightHeight <- read.csv("WeightHeight.csv")

names(WeightHeight) <- c("NLSY_CASE_ID","weight_2010", "height_ft_2010", "height_in_2010",
                         "weight_2012", "height_ft_2012", "height_in_2012",
                         "weight_2014", "height_ft_2014", "height_in_2014",
                         "weight_2016", "height_ft_2016", "height_in_2016",
                         "weight_2018", "height_ft_2018", "height_in_2018")

#recode negative values into NA
WeightHeight[, 2:16] <- lapply(WeightHeight[, 2:16], function(x) ifelse(x<0, NA, x))

#covert weight from lbs to kg
summary(WeightHeight$weight_2010)
WeightHeight[, c(2, 5, 8, 11, 14)] <- lapply(WeightHeight[, c(2, 5, 8, 11, 14)], function(x) x=round(x*0.45359237, 1))

#covert height into m
#m=feet*0.3048 + inches*0.0254
summary(WeightHeight$height_ft_2010)
summary(WeightHeight$height_in_2010)

WeightHeight <- WeightHeight %>%
  mutate(
    height_2010 = height_ft_2010*0.3048+height_in_2010*0.0254,
    height_2012 = height_ft_2012*0.3048+height_in_2012*0.0254,
    height_2014 = height_ft_2014*0.3048+height_in_2014*0.0254,
    height_2016 = height_ft_2016*0.3048+height_in_2016*0.0254,
    height_2018 = height_ft_2018*0.3048+height_in_2018*0.0254,
    BMI_2010 = weight_2010/(height_2010^2),
    BMI_2012 = weight_2012/(height_2012^2),
    BMI_2014 = weight_2014/(height_2014^2),
    BMI_2016 = weight_2016/(height_2016^2),
    BMI_2018 = weight_2018/(height_2018^2)
  )

summary(WeightHeight$height_2010)
summary(WeightHeight$BMI_2018)

#only keep the final variables
WeightHeight<-WeightHeight[,c(1,2,5,8,11,14,17:26)]
names(WeightHeight) 


#-------------------------------------------------------------------------------
#marital status in 2010-2018
mstat <- read.csv("MaritalStatus.csv")

names(mstat) <- c("NLSY_CASE_ID", "marriage_2010", "marriage_2012", 
                  "marriage_2014", "marriage_2016", "marriage_2018")

table(mstat$marriage_2010, exclude=NULL)
#recode negative values as NA
mstat[, 2:6] <- lapply(mstat[, 2:6], function(x) ifelse(x<0, NA, x))

#recode
#0  NEVER MARRIED; 1  MARRIED; 2  SEPARATED; 3  DIVORCED; 6  WIDOWED
table(mstat$marriage_2010)
mstat[, 2:6] <- lapply(mstat[, 2:6], function(x) ifelse(x==0, 0,
                                                        ifelse(x==1, 1,
                                                               ifelse(x==2, 2,
                                                                      ifelse(x==3, 2,
                                                                             ifelse(x==6, 2, NA))))))

mstat[,  2:6] <- lapply(mstat[,  2:6], function(x) factor(x, levels = c(0,1,2), labels = c("never married","married/partnered", "separated/divorced/widowed")))
table(mstat$marriage_2010)

colnames(mstat)


#-------------------------------------------------------------------------------
#memory, only at H50 section
#pull other cog variables for potential use in other projects

cognition <- read.csv("all_cognition.csv")
colnames(cognition)
cognition <- cognition[,-c(2:4)]

names(cognition) <- c("NLSY_CASE_ID","VerbalFluency.2018", "CogTestYear","selfratemem", "selfratemem.change",
                      "WR1.completed","WR1.listnum","WR1.List1","WR1.List2","WR1.List3","WR1.List4",
                      "BC.completed", "BC.1stattempt","BC1.yesnocorrect","BC.2ndattempt","BC2.yesnocorrect", "BC86.completed",
                      "BC86.1stattempt","BC86.1.yesnocorrect","BC86.2ndattempt","BC86.2.yesnocorrect",
                      "ST.100m7", "ST.93m7","ST.86m7","ST.79m7","ST.72m7", "WR2.yesno","WR2.List1","WR2.List2","WR2.List3","WR2.List4")

#recode negative value into NA
cognition[, 2:31] <- lapply(cognition[, 2:31], function(x) ifelse(x<0, NA, x))

#Dist of cog exam year
round(prop.table(table(cognition$CogTestYear)),2)*100 #proportion of people who completed cognitive battery in each year (at age 50)

#Dist of SR Memory
table(cognition$selfratemem, exclude = NULL)
#same coding as HRS
#1 Excellent; 2 Very Good; 3 Good; 4 Fair; 5 Poor

table(cognition$WR1.completed, useNA = "ifany")
table(cognition$WR1.completed, cognition$WR1.listnum)

#take which column is non-NA and assign it as the immediate word recall value
cognition$immedrecall <- ifelse(!is.na(cognition$WR1.List1), cognition$WR1.List1,
                                   ifelse(!is.na(cognition$WR1.List2), cognition$WR1.List2,
                                          ifelse(!is.na(cognition$WR1.List3), cognition$WR1.List3, 
                                                 ifelse(!is.na(cognition$WR1.List4), cognition$WR1.List4, NA))))

table(cognition$WR1.completed, cognition$immedrecall, useNA = "ifany")

table(cognition$WR2.yesno, useNA = "ifany")

#take which column is non-NA and assign it as the delayed word recall value
cognition$delrecall <- ifelse(!is.na(cognition$WR2.List1), cognition$WR2.List1,
                                 ifelse(!is.na(cognition$WR2.List2), cognition$WR2.List2,
                                        ifelse(!is.na(cognition$WR2.List3), cognition$WR2.List3, 
                                               ifelse(!is.na(cognition$WR2.List4), cognition$WR2.List4, NA))))


cognition$avg.memory<-rowMeans(cognition[,c("immedrecall","delrecall")], na.rm=TRUE)
cognition$sum.memory<-rowSums(cognition[,c("immedrecall","delrecall")])
summary(cognition$avg.memory)
summary(cognition$sum.memory)

#backward counting: 1=correct, 2=incorrect

table(cognition$BC86.1.yesnocorrect)
table(cognition$BC86.2.yesnocorrect)
table(cognition$BC1.yesnocorrect, cognition$BC2.yesnocorrect)
table(cognition$BC86.1.yesnocorrect, cognition$BC86.2.yesnocorrect)

cognition$backwardcount <- ifelse(cognition$BC1.yesnocorrect==1|(cognition$BC1.yesnocorrect==6&cognition$BC2.yesnocorrect==1), 1,
                                 ifelse(cognition$BC1.yesnocorrect==5,0,NA))
cognition$backwardcount.86 <- ifelse(cognition$BC86.1.yesnocorrect==1|(cognition$BC86.1.yesnocorrect==6&cognition$BC86.2.yesnocorrect==1), 1,
                                  ifelse(cognition$BC86.1.yesnocorrect==5|(cognition$BC86.1.yesnocorrect==6&cognition$BC86.2.yesnocorrect==5),0,NA))

table(cognition$backwardcount, useNA="ifany")
table(cognition$backwardcount.86, useNA="ifany")

#serial 7s: ST.100m7=93 ; ST.93m7=86 ; ST.86m7=79 ; ST.79m7=72 ; ST.72m7=65
#Ask the patient to take away 7 from 100. Ask them to continue subtracting 7 and continue to a total of 5 subtractions (one point is given for each correct answer to a maximum score of five points).

table(cognition$ST.100m7, useNA="ifany")
cognition <- cognition %>%
  mutate(
    ST.100cat = ifelse(ST.100m7==93,1,0), #first trial only counts if 93 since everyone starts at 100
    ST.93cat = ifelse(ST.100m7-ST.93m7==7,1,0), #second trial either 86 (correct answer) or a difference of 7 between answer on first trial and this trial
    ST.86cat = ifelse(ST.93m7-ST.86m7==7,1,0),
    ST.79cat = ifelse(ST.86m7-ST.79m7==7,1,0),
    ST.72cat = ifelse(ST.79m7-ST.72m7==7,1,0),
    serial7s = ST.100cat+ST.93cat+ST.86cat+ST.79cat+ST.72cat
  )

table(cognition$serial7s, useNA="ifany")

#only extract the cognition var we need
colnames(cognition)

cognition <- cognition[, c(1:5, 32:37, 43)]

#-------------------------------------------------------------------------------
#smoking
smoking <- read.csv("smokinghx.csv")

#rename
names(smoking) <- c("NLSY_CASE_ID", "eversmoke_2010", "eversmokedaily_2010", "currsmokedaily_2010",
                    "eversmoke_2012", "eversmokedaily_2012", "currsmokedaily_2012",
                    "eversmoke_2014", "eversmokedaily_2014", "currsmokedaily_2014",
                    "eversmoke_2018", "eversmokedaily_2018", "currsmokedaily_2018")

table(smoking$eversmoke_2010, exclude=NULL)

#recode negative value into NA
smoking[, 2:13] <- lapply(smoking[, 2:13], function(x) ifelse(x<0, NA, x))

table(smoking$eversmoke_2010, exclude=NULL)
table(smoking$eversmokedaily_2010, exclude=NULL)
table(smoking$eversmoke_2010,smoking$eversmokedaily_2010, exclude=NULL)
  #recode anyone who said 1 to either question as ever smoker
table(smoking$eversmoke_2014, exclude=NULL)

smoking$eversmoke_2010[smoking$eversmoke_2010==0&(smoking$eversmokedaily_2010==1|smoking$currsmokedaily_2010==1|smoking$currsmokedaily_2010==2)]<-1
smoking$eversmoke_2012[smoking$eversmoke_2012==0&(smoking$eversmokedaily_2012==1|smoking$currsmokedaily_2012==1|smoking$currsmokedaily_2012==2)]<-1
smoking$eversmoke_2014[smoking$eversmoke_2014==0&(smoking$eversmokedaily_2014==1|smoking$currsmokedaily_2014==1|smoking$currsmokedaily_2014==2)]<-1
smoking$eversmoke_2018[smoking$eversmoke_2018==0&(smoking$eversmokedaily_2018==1|smoking$currsmokedaily_2018==1|smoking$currsmokedaily_2018==2)]<-1

table(smoking$eversmoke_2010, exclude=NULL)
table(smoking$eversmoke_2012, exclude=NULL)
table(smoking$eversmoke_2014, exclude=NULL)
table(smoking$eversmoke_2018, exclude=NULL)
prop.table(table(smoking$currsmokedaily_2010, exclude=NULL))*100

table(smoking$eversmoke_2010, smoking$currsmokedaily_2010)
table(smoking$eversmoke_2012, smoking$currsmokedaily_2012)
table(smoking$eversmoke_2014, smoking$currsmokedaily_2014)
table(smoking$eversmoke_2018, smoking$currsmokedaily_2018)

#combine two variables so that current smoking reference includes never smokers
smoking <- smoking %>%
  mutate(
    currsmoke_2010 = ifelse(eversmoke_2010==0,0,
                            ifelse(eversmoke_2010==1&(currsmokedaily_2010==1|currsmokedaily_2010==2),1,
                                   ifelse(eversmoke_2010==1&currsmokedaily_2010==3,0,NA))),
    currsmoke_2012 = ifelse(eversmoke_2012==0,0,
                            ifelse(eversmoke_2012==1&(currsmokedaily_2012==1|currsmokedaily_2012==2),1,
                                   ifelse(eversmoke_2012==1&currsmokedaily_2012==3,0,NA))),
    currsmoke_2014 = ifelse(eversmoke_2014==0,0,
                            ifelse(eversmoke_2014==1&(currsmokedaily_2014==1|currsmokedaily_2014==2),1,
                                   ifelse(eversmoke_2014==1&currsmokedaily_2014==3,0,NA))),
    currsmoke_2018 = ifelse(eversmoke_2018==0,0,
                            ifelse(eversmoke_2018==1&(currsmokedaily_2018==1|currsmokedaily_2018==2),1,
                                   ifelse(eversmoke_2018==1&currsmokedaily_2018==3,0,NA)))
    
)
prop.table(table(smoking$currsmoke_2010)) #27.7% - close to HRS!
prop.table(table(smoking$eversmoke_2010)) #58.2% - close to HRS!
prop.table(table(smoking$currsmoke_2012)) #25.8% 
prop.table(table(smoking$eversmoke_2012)) #58.5% 
prop.table(table(smoking$currsmoke_2014)) #23.2% 
prop.table(table(smoking$eversmoke_2014)) #58.3% 
prop.table(table(smoking$currsmoke_2018)) #21.5% 
prop.table(table(smoking$eversmoke_2018)) #58.2%

colnames(smoking)

smoking<-smoking[,-c(3,4,6,7,9,10,12,13)]


#-------------------------------------------------------------------------------
#drinking
drinking <- read.csv("drinking.csv")
colnames(drinking)

#recode negative value into NA
drinking[, 2:5] <- lapply(drinking[, 2:5], function(x) ifelse(x<0, NA, x))

#rename
names(drinking) <- c("NLSY_CASE_ID","drinksalc_2010", "drinksalc_2012", "drinksalc_2014", "drinksalc_2018")

table(drinking$drinksalc_2010, exclude=NULL)

#-------------------------------------------------------------------------------
#current CESD (5 shared items + loneliness)

#current CESD (5 shared items + loneliness) 50+  section
H50_CESD <- read.csv("H50_CESD.csv")
colnames(H50_CESD)<-c("cesd.depressed","cesd.effort","cesd.sleep", "cesd.lonely", "cesd.sad","cesd.going","NLSY_CASE_ID")

#recode negative value into NA
H50_CESD[, 1:6] <- lapply(H50_CESD[, 1:6], function(x) ifelse(x<0, NA, x))

#dichotomize everything in NLSY as yes/no at 2|3; HRS only has "yes/no" and question asks about "much of the time" so include NLSY responses 2 and 3 (~50% of the week)
# 0 Rarely/None of the time/1 Day
#1 Some/A little of the time/1-2 Days
#2 Occasionally/Moderate amount of the time/3-4 Days
#3 Most/All of the time/5-7 Days
table(H50_CESD$cesd.depressed, exclude = NULL)
H50_CESD[, 1:6] <- lapply(H50_CESD[, 1:6], function(x) ifelse(x==2|x==3, 1, 0))

#prop.table(table(mbb$r10depres))*100 #16.7% - hrs mbb sample
prop.table(table(H50_CESD$cesd.depressed))*100 #11.2%

#sum all 6 items
H50_CESD$cesd <- rowSums(H50_CESD[, 1:6])
table(H50_CESD$cesd, exclude = NULL)
prop.table(table(H50_CESD$cesd))*100
#prop.table(table(mbb$cesd))*100 #there is decent overlap between NLSY and HRS at each level

#only use the sum score
H50_CESD <- subset(H50_CESD, select=c(NLSY_CASE_ID,cesd))

#current CESD (5 shared items + loneliness) 60+  section
H60_CESD <- read.csv("H60_CESD.csv")
colnames(H60_CESD)<-c("cesd.depressed","cesd.effort","cesd.sleep", "cesd.lonely", "cesd.sad","cesd.going","NLSY_CASE_ID")

#recode negative value into NA
H60_CESD[, 1:6] <- lapply(H60_CESD[, 1:6], function(x) ifelse(x<0, NA, x))

#dichotomize everything in NLSY as yes/no at the “Most or all of the time (5-7 days)” level? 
table(H60_CESD$cesd.depressed, exclude = NULL)
H60_CESD[, 1:6] <- lapply(H60_CESD[, 1:6], function(x) ifelse(x==2|x==3, 1, 0))

#sum all 6 items
H60_CESD$cesd <- rowSums(H60_CESD[, 1:6])
table(H60_CESD$cesd, exclude = NULL)
prop.table(table(H60_CESD$cesd))*100

#only use the sum score
H60_CESD <- subset(H60_CESD, select=c(NLSY_CASE_ID,cesd))

#-------------------------------------------------------------------------------
#SRH
H50_SRH <- read.csv("H50_SRH.csv")[,1:2]
names(H50_SRH)[c(1,2)] <- c("srh","NLSY_CASE_ID")
H50_SRH$srh<-ifelse(H50_SRH$srh<0,NA,H50_SRH$srh)

table(H50_SRH$srh, exclude=NULL)
colnames(H50_SRH)

#-------------------------------------------------------------------------------
#History of diabetes (y/n)
diabetes <- read.csv("diabetes.csv")

colnames(diabetes)<-c("everdiab1","everdiab2","everdiab3","NLSY_CASE_ID")

#DOCTOR EVER SAID R HAS DIABETES/HIGH BLOOD SUGAR in health 40+, 50+, 60+ sections
table(diabetes$everdiab1,diabetes$everdiab2)

#recode negative value into NA
diabetes[, 1:3] <- lapply(diabetes[, 1:3], function(x) ifelse(x<0, NA, x))

table(diabetes$everdiab1, exclude=NULL)
table(diabetes$everdiab2, exclude=NULL)
table(diabetes$everdiab1, diabetes$everdiab2, exclude=NULL)
table(diabetes$everdiab1[is.na(diabetes$everdiab2)])
table(diabetes$everdiab2[is.na(diabetes$everdiab1)])

diabetes$everdiab12<-ifelse(diabetes$everdiab1==0&diabetes$everdiab2==0,0,
                            ifelse(diabetes$everdiab1==1|diabetes$everdiab2==1,1,NA))
diabetes$everdiab12[is.na(diabetes$everdiab1)&diabetes$everdiab2==0]<-0
diabetes$everdiab12[is.na(diabetes$everdiab2)&diabetes$everdiab1==0]<-0

table(diabetes$everdiab12, exclude=NULL)
table(diabetes$everdiab3, exclude=NULL)
table(diabetes$everdiab12, diabetes$everdiab3, exclude=NULL)
table(diabetes$everdiab12[is.na(diabetes$everdiab3)])
table(diabetes$everdiab3[is.na(diabetes$everdiab12)])

diabetes$everdiab<-ifelse(diabetes$everdiab12==0&diabetes$everdiab3==0,0,
                            ifelse(diabetes$everdiab12==1|diabetes$everdiab3==1,1,NA))
diabetes$everdiab[is.na(diabetes$everdiab12)&diabetes$everdiab3==0]<-0
diabetes$everdiab[is.na(diabetes$everdiab3)&diabetes$everdiab12==0]<-0

table(diabetes$everdiab, diabetes$everdiab3, exclude=NULL)
table(diabetes$everdiab, exclude=NULL)

#recode NA into 0
table(diabetes$everdiab, exclude = NULL)
#1590 cases are diagnosed of diabetes

diabetes <- subset(diabetes, select=c(NLSY_CASE_ID,everdiab))

#-------------------------------------------------------------------------------
#History of cardiovascular condition (HBP + stroke + cogestive heart failure + heart problem)
CVD <- read.csv("CVD.csv")

colnames(CVD)

#In health 40+, 50+, 60+ sections
#CCR - DOCTOR EVER DIAGNOSED HIGH BLOOD PRESSURE OR HYPERTENSION?
#CCR - DOCTOR EVER DIAGNOSED HEART PROBLEMS?
#CCR - DOCTOR EVER DIAGNOSED CONGESTIVE HEART FAILURE?
#CCR - DOCTOR EVER DIAGNOSED A STROKE?
colnames(CVD)<-c("hbp1","heart1","chf1","stroke1","hbp2","heart2","chf2","stroke2","hbp3","heart3","chf3","stroke3","NLSY_CASE_ID")

#recode negative value into NA
CVD[, 1:12] <- lapply(CVD[, 1:12], function(x) ifelse(x<0, NA, x))
table(CVD$hbp1, CVD$hbp2, exclude=NULL)
#NAs function same way as with diabetes so use same coding here

CVD$hbp12<-ifelse(CVD$hbp1==0&CVD$hbp2==0,0,
           ifelse(CVD$hbp1==1|CVD$hbp2==1,1,NA))
CVD$hbp12[is.na(CVD$hbp1)&CVD$hbp2==0]<-0
CVD$hbp12[is.na(CVD$hbp2)&CVD$hbp1==0]<-0

table(CVD$hbp12, exclude=NULL)
  1599+101+1457
  4722+171+666

CVD$hbp<-ifelse(CVD$hbp12==0&CVD$hbp3==0,0,
         ifelse(CVD$hbp12==1|CVD$hbp3==1,1,NA))
CVD$hbp[is.na(CVD$hbp12)&CVD$hbp3==0]<-0
CVD$hbp[is.na(CVD$hbp3)&CVD$hbp12==0]<-0

table(CVD$hbp12, CVD$hbp3, exclude=NULL)
table(CVD$hbp, exclude=NULL)
  1356+8+3572
  631+11+3157

#coding works and NAs similar to NAs from diabetes question - do same coding for other heart problem indicators

#heart disease
CVD$heart12<-ifelse(CVD$heart1==0&CVD$heart2==0,0,
           ifelse(CVD$heart1==1|CVD$heart2==1,1,NA))
CVD$heart12[is.na(CVD$heart1)&CVD$heart2==0]<-0
CVD$heart12[is.na(CVD$heart2)&CVD$heart1==0]<-0
CVD$heart<-ifelse(CVD$heart12==0&CVD$heart3==0,0,
                ifelse(CVD$heart12==1|CVD$heart3==1,1,NA))
CVD$heart[is.na(CVD$heart12)&CVD$heart3==0]<-0
CVD$heart[is.na(CVD$heart3)&CVD$heart12==0]<-0

table(CVD$heart, exclude=NULL)

#CHF
CVD$chf12<-ifelse(CVD$chf1==0&CVD$chf2==0,0,
                  ifelse(CVD$chf1==1|CVD$chf2==1,1,NA))
CVD$chf12[is.na(CVD$chf1)&CVD$chf2==0]<-0
CVD$chf12[is.na(CVD$chf2)&CVD$chf1==0]<-0
#table(CVD$chf12, CVD$chf1, exclude=NULL)
CVD$chf<-ifelse(CVD$chf12==0&CVD$chf3==0,0,
         ifelse(CVD$chf12==1|CVD$chf3==1,1,NA))
CVD$chf[is.na(CVD$chf12)&CVD$chf3==0]<-0
CVD$chf[is.na(CVD$chf3)&CVD$chf12==0]<-0
#table(CVD$chf, CVD$chf3, exclude=NULL)

table(CVD$chf, exclude=NULL)

#stroke
CVD$stroke12<-ifelse(CVD$stroke1==0&CVD$stroke2==0,0,
                  ifelse(CVD$stroke1==1|CVD$stroke2==1,1,NA))
CVD$stroke12[is.na(CVD$stroke1)&CVD$stroke2==0]<-0
CVD$stroke12[is.na(CVD$stroke2)&CVD$stroke1==0]<-0
CVD$stroke<-ifelse(CVD$stroke12==0&CVD$stroke3==0,0,
                ifelse(CVD$stroke12==1|CVD$stroke3==1,1,NA))
CVD$stroke[is.na(CVD$stroke12)&CVD$stroke3==0]<-0
CVD$stroke[is.na(CVD$stroke3)&CVD$stroke12==0]<-0

table(CVD$stroke, exclude=NULL)
prop.table(table(CVD$stroke))

#if 1=yes to any 40+, 50+, or 60+ health section, code it as 1 and take sum
colnames(CVD)
CVD<-CVD[,c(13,15,17,19,21)]

#in HRS congestive heart failure is part of "ever heart problem" question so do that here so sums are on the same scale
CVD$heart_chf<-ifelse(CVD$heart==1|CVD$chf==1,1,0)
  table(CVD$heart, CVD$chf)
  prop.table(table(CVD$heart_chf))
  prop.table(table(CVD$stroke))
  prop.table(table(CVD$hbp))

colnames(CVD)
CVD$countCVD <- rowSums(CVD[, c(2,5,6)]) 
CVD$everCVD <- ifelse(CVD$countCVD>0,1,0)
CVD$everhibp<-CVD$hbp

table(CVD$countCVD, exclude=NULL)
prop.table(table(CVD$countCVD, exclude=NULL))
table(CVD$everCVD, exclude=NULL)
prop.table(table(CVD$everCVD))
#4089 cases are diagnosed of CVD

colnames(CVD)

#-------------------------------------------------------------------------------
#History of psych illness or depression (y/n)
psy_ill <- read.csv("psy_ill.csv")
names(psy_ill)<-c("psych40","everdiagdep50","otherpsych50","everdiagdep60","otherpsych60","NLSY_CASE_ID")

#health 40+ section
#CCR - DOCTOR EVER DIAGNOSED EMOTIONAL, NERVOUS, OR PSYCHIATRIC PROBLEMS?
#health 50+, 60+ section
#DOCTOR EVER DIAGNOSED R SUFFERING FROM DEPRESSION
#CCR - DOCTOR EVER DIAGNOSED EMOTIONAL, NERVOUS, OR PSYCHIATRIC PROBLEMS OTHER THAN DEPRESSION?

#recode negative value into NA
psy_ill[, 1:5] <- lapply(psy_ill[, 1:5], function(x) ifelse(x<0, NA, x))

table(psy_ill$everdiagdep50, psy_ill$otherpsych50, exclude=NULL)

#if respondent choose 1=yes to doctor-diagnosed depression or other psych problems in any health section,
#they are considered having psych ill

#first collapse/combine the two health50 psych variables
psy_ill$psych50<-ifelse(psy_ill$everdiagdep50==1|psy_ill$otherpsych50==1,1,
                        ifelse(psy_ill$everdiagdep50==0&psy_ill$otherpsych50==0,0,NA))
psy_ill$psych50[is.na(psy_ill$everdiagdep50)&psy_ill$otherpsych50==0]<-0
psy_ill$psych50[is.na(psy_ill$otherpsych50)&psy_ill$everdiagdep50==0]<-0

table(psy_ill$psych50, exclude=NULL)
6348+190+12
691+193+381+71

#then collapse/combine the two health60 psych variables
psy_ill$psych60<-ifelse(psy_ill$everdiagdep60==1|psy_ill$otherpsych60==1,1,
                        ifelse(psy_ill$everdiagdep60==0&psy_ill$otherpsych60==0,0,NA))
psy_ill$psych60[is.na(psy_ill$everdiagdep60)&psy_ill$otherpsych60==0]<-0
psy_ill$psych60[is.na(psy_ill$otherpsych60)&psy_ill$everdiagdep60==0]<-0

table(psy_ill$everdiagdep60, psy_ill$otherpsych60, exclude=NULL)
table(psy_ill$psych60, exclude=NULL)
2471+2+142
312+16+36+171

#
psy_ill$psych12<-ifelse(psy_ill$psych40==0&psy_ill$psych50==0,0,
                     ifelse(psy_ill$psych40==1|psy_ill$psych50==1,1,NA))
psy_ill$psych12[is.na(psy_ill$psych40)&psy_ill$psych50==0]<-0
psy_ill$psych12[is.na(psy_ill$psych50)&psy_ill$psych40==0]<-0

table(psy_ill$psych40, psy_ill$psych50, exclude=NULL)
table(psy_ill$psych12, exclude=NULL)
  6140+225+766
  376+185+911+64+49
  
psy_ill$psych<-ifelse(psy_ill$psych12==0&psy_ill$psych60==0,0,
                   ifelse(psy_ill$psych12==1|psy_ill$psych60==1,1,NA))
psy_ill$psych[is.na(psy_ill$psych12)&psy_ill$psych60==0]<-0
psy_ill$psych[is.na(psy_ill$psych60)&psy_ill$psych12==0]<-0

table(psy_ill$psych12, psy_ill$psych60, exclude=NULL)
table(psy_ill$psych, exclude=NULL)
  2317+18+4586
  228+280+306+1+999

#1814 had history of psy illness
psy_ill$everpsyche<-psy_ill$psych
colnames(psy_ill)
psy_ill <- subset(psy_ill, select = c(NLSY_CASE_ID,everpsyche))


#-------------------------------------------------------------------------------
#physical activity
race.exercise<-read.csv("race_exercise.csv")
exercise<-race.exercise[,c(1,8:49)]
test<-exercise[exercise$T3024901>3&exercise$T3025000==1,]

exercise<-exercise[,c(1,2,3,4,7,8,9,12,13,14,17,18,19,22,23,
                     24,27,28,29,32,33,34,37,38,39,42,43)]
names(exercise)<-c("NLSY_CASE_ID","freq_vigact_2010","unit_vigact_2010","unable_vigact_2010","freq_lmodact_2010","unit_lmodact_2010","unable_lmodact_2010",
                   "freq_vigact_2012","unit_vigact_2012","unable_vigact_2012","freq_lmodact_2012","unit_lmodact_2012","unable_lmodact_2012",
                   "freq_vigact_2014","unit_vigact_2014","unable_vigact_2014","freq_lmodact_2014","unit_lmodact_2014","unable_lmodact_2014",
                   "freq_vigact_2016","unit_vigact_2016","unable_vigact_2016","freq_lmodact_2016","unit_lmodact_2016","unable_lmodact_2016",
                   "freq_vigact_2018","freq_lmodact_2018")

exercise[, 2:27] <- lapply(exercise[, 2:27], function(x) ifelse(x<0, NA, x))

table(exercise$freq_vigact_2010, useNA="ifany")
table(exercise$unit_vigact_2010, useNA="ifany")#consistent with NLSY codebook  
table(exercise$freq_vigact_2010, exercise$unit_vigact_2010, useNA="ifany")
table(exercise$freq_vigact_2012)
table(exercise$freq_vigact_2014)
table(exercise$freq_vigact_2016)
table(exercise$freq_vigact_2018) #different coding

table(exercise$freq_lmodact_2010, useNA="ifany")
table(exercise$unit_lmodact_2010, useNA="ifany")#consistent with NLSY codebook  
table(exercise$freq_lmodact_2010, exercise$unit_lmodact_2010, useNA="ifany")

exercise.cat <- exercise %>%
  mutate(
    vigex.2010 = case_when(
      freq_vigact_2010 == 0 | freq_vigact_2010 == 996 | unit_vigact_2010 ==5  ~ 5, #unable to do/never
      freq_vigact_2010 >=1 & unit_vigact_2010 ==1 ~ 1,# >= everyday
      freq_vigact_2010 >=7 & unit_vigact_2010 ==2 ~ 1,# >= everyday
      freq_vigact_2010 >=30 & unit_vigact_2010 ==3 ~ 1,# >= everyday
      freq_vigact_2010 >=365 & unit_vigact_2010 ==4 ~ 1,# >= everyday
      (freq_vigact_2010 >1 & freq_vigact_2010 <7) & unit_vigact_2010 ==2 ~ 2,# >1 times per week
      (freq_vigact_2010 >4 & freq_vigact_2010 <30) & unit_vigact_2010 ==3 ~ 2,# >1 times per week
      (freq_vigact_2010 >12 & freq_vigact_2010 <365) & unit_vigact_2010 ==4 ~ 2,# >1 times per week
      freq_vigact_2010 ==1 & unit_vigact_2010 ==2 ~ 3,# 1 per week
      freq_vigact_2010 ==4 & unit_vigact_2010 ==3 ~ 3,# 1 per week
      freq_vigact_2010 ==12 & unit_vigact_2010 ==4 ~ 3,# 1 per week
      (freq_vigact_2010 >0 & freq_vigact_2010 <4) & unit_vigact_2010 ==3 ~ 4,# 1-3 per month (or <1 per week)
      (freq_vigact_2010 >0 & freq_vigact_2010 <12) & unit_vigact_2010 ==4 ~ 4),# 1-3 per month (or <1 per week)
    vigex.2012 = case_when(
      freq_vigact_2012 == 0 | freq_vigact_2012 == 996 | unit_vigact_2012 ==5  ~ 5, #unable to do/never
      freq_vigact_2012 >=1 & unit_vigact_2012 ==1 ~ 1,# >= everyday
      freq_vigact_2012 >=7 & unit_vigact_2012 ==2 ~ 1,# >= everyday
      freq_vigact_2012 >=30 & unit_vigact_2012 ==3 ~ 1,# >= everyday
      freq_vigact_2012 >=365 & unit_vigact_2012 ==4 ~ 1,# >= everyday
      (freq_vigact_2012 >1 & freq_vigact_2012 <7) & unit_vigact_2012 ==2 ~ 2,# >1 times per week
      (freq_vigact_2012 >4 & freq_vigact_2012 <30) & unit_vigact_2012 ==3 ~ 2,# >1 times per week
      (freq_vigact_2012 >12 & freq_vigact_2012 <365) & unit_vigact_2012 ==4 ~ 2,# >1 times per week
      freq_vigact_2012 ==1 & unit_vigact_2012 ==2 ~ 3,# 1 per week
      freq_vigact_2012 ==4 & unit_vigact_2012 ==3 ~ 3,# 1 per week
      freq_vigact_2012 ==12 & unit_vigact_2012 ==4 ~ 3,# 1 per week
      (freq_vigact_2012 >0 & freq_vigact_2012 <4) & unit_vigact_2012 ==3 ~ 4,# 1-3 per month (or <1 per week)
      (freq_vigact_2012 >0 & freq_vigact_2012 <12) & unit_vigact_2012 ==4 ~ 4),# 1-3 per month (or <1 per week)
    vigex.2014 = case_when(
      freq_vigact_2014 == 0 | freq_vigact_2014 == 996 | unit_vigact_2014 ==5  ~ 5, #unable to do/never
      freq_vigact_2014 >=1 & unit_vigact_2014 ==1 ~ 1,# >= everyday
      freq_vigact_2014 >=7 & unit_vigact_2014 ==2 ~ 1,# >= everyday
      freq_vigact_2014 >=30 & unit_vigact_2014 ==3 ~ 1,# >= everyday
      freq_vigact_2014 >=365 & unit_vigact_2014 ==4 ~ 1,# >= everyday
      (freq_vigact_2014 >1 & freq_vigact_2014 <7) & unit_vigact_2014 ==2 ~ 2,# >1 times per week
      (freq_vigact_2014 >4 & freq_vigact_2014 <30) & unit_vigact_2014 ==3 ~ 2,# >1 times per week
      (freq_vigact_2014 >12 & freq_vigact_2014 <365) & unit_vigact_2014 ==4 ~ 2,# >1 times per week
      freq_vigact_2014 ==1 & unit_vigact_2014 ==2 ~ 3,# 1 per week
      freq_vigact_2014 ==4 & unit_vigact_2014 ==3 ~ 3,# 1 per week
      freq_vigact_2014 ==12 & unit_vigact_2014 ==4 ~ 3,# 1 per week
      (freq_vigact_2014 >0 & freq_vigact_2014 <4) & unit_vigact_2014 ==3 ~ 4,# 1-3 per month (or <1 per week)
      (freq_vigact_2014 >0 & freq_vigact_2014 <12) & unit_vigact_2014 ==4 ~ 4),# 1-3 per month (or <1 per week)
    vigex.2016 = case_when(
      freq_vigact_2016 == 0 | freq_vigact_2016 == 996 | unit_vigact_2016 ==5  ~ 5, #unable to do/never
      freq_vigact_2016 >=1 & unit_vigact_2016 ==1 ~ 1,# >= everyday
      freq_vigact_2016 >=7 & unit_vigact_2016 ==2 ~ 1,# >= everyday
      freq_vigact_2016 >=30 & unit_vigact_2016 ==3 ~ 1,# >= everyday
      freq_vigact_2016 >=365 & unit_vigact_2016 ==4 ~ 1,# >= everyday
      (freq_vigact_2016 >1 & freq_vigact_2016 <7) & unit_vigact_2016 ==2 ~ 2,# >1 times per week
      (freq_vigact_2016 >4 & freq_vigact_2016 <30) & unit_vigact_2016 ==3 ~ 2,# >1 times per week
      (freq_vigact_2016 >12 & freq_vigact_2016 <365) & unit_vigact_2016 ==4 ~ 2,# >1 times per week
      freq_vigact_2016 ==1 & unit_vigact_2016 ==2 ~ 3,# 1 per week
      freq_vigact_2016 ==4 & unit_vigact_2016 ==3 ~ 3,# 1 per week
      freq_vigact_2016 ==12 & unit_vigact_2016 ==4 ~ 3,# 1 per week
      (freq_vigact_2016 >0 & freq_vigact_2016 <4) & unit_vigact_2016 ==3 ~ 4,# 1-3 per month (or <1 per week)
      (freq_vigact_2016 >0 & freq_vigact_2016 <12) & unit_vigact_2016 ==4 ~ 4), # 1-3 per month (or <1 per week)
    vigex.2018 = case_when(
      freq_vigact_2018 == 9 | freq_vigact_2018 == 4 ~ 5, #unable to do/never
      freq_vigact_2018 == 7 ~ 1,# >= everyday
      freq_vigact_2018 == 1 ~ 2,# >1 times per week
      freq_vigact_2018 == 2  ~ 3,# 1 per week
      freq_vigact_2018 == 3 ~ 4), # 1-3 per month (or <1 per week)
 #light/moderate
 lmodex.2010 = case_when(
   freq_lmodact_2010 == 0 | freq_lmodact_2010 == 996 | unit_lmodact_2010 ==5  ~ 5, #unable to do/never
   freq_lmodact_2010 >=1 & unit_lmodact_2010 ==1 ~ 1,# >= everyday
   freq_lmodact_2010 >=7 & unit_lmodact_2010 ==2 ~ 1,# >= everyday
   freq_lmodact_2010 >=30 & unit_lmodact_2010 ==3 ~ 1,# >= everyday
   freq_lmodact_2010 >=365 & unit_lmodact_2010 ==4 ~ 1,# >= everyday
   (freq_lmodact_2010 >1 & freq_lmodact_2010 <7) & unit_lmodact_2010 ==2 ~ 2,# >1 times per week
   (freq_lmodact_2010 >4 & freq_lmodact_2010 <30) & unit_lmodact_2010 ==3 ~ 2,# >1 times per week
   (freq_lmodact_2010 >12 & freq_lmodact_2010 <365) & unit_lmodact_2010 ==4 ~ 2,# >1 times per week
   freq_lmodact_2010 ==1 & unit_lmodact_2010 ==2 ~ 3,# 1 per week
   freq_lmodact_2010 ==4 & unit_lmodact_2010 ==3 ~ 3,# 1 per week
   freq_lmodact_2010 ==12 & unit_lmodact_2010 ==4 ~ 3,# 1 per week
   (freq_lmodact_2010 >0 & freq_lmodact_2010 <4) & unit_lmodact_2010 ==3 ~ 4,# 1-3 per month (or <1 per week)
   (freq_lmodact_2010 >0 & freq_lmodact_2010 <12) & unit_lmodact_2010 ==4 ~ 4),# 1-3 per month (or <1 per week)
 lmodex.2012 = case_when(
   freq_lmodact_2012 == 0 | freq_lmodact_2012 == 996 | unit_lmodact_2012 ==5  ~ 5, #unable to do/never
   freq_lmodact_2012 >=1 & unit_lmodact_2012 ==1 ~ 1,# >= everyday
   freq_lmodact_2012 >=7 & unit_lmodact_2012 ==2 ~ 1,# >= everyday
   freq_lmodact_2012 >=30 & unit_lmodact_2012 ==3 ~ 1,# >= everyday
   freq_lmodact_2012 >=365 & unit_lmodact_2012 ==4 ~ 1,# >= everyday
   (freq_lmodact_2012 >1 & freq_lmodact_2012 <7) & unit_lmodact_2012 ==2 ~ 2,# >1 times per week
   (freq_lmodact_2012 >4 & freq_lmodact_2012 <30) & unit_lmodact_2012 ==3 ~ 2,# >1 times per week
   (freq_lmodact_2012 >12 & freq_lmodact_2012 <365) & unit_lmodact_2012 ==4 ~ 2,# >1 times per week
   freq_lmodact_2012 ==1 & unit_lmodact_2012 ==2 ~ 3,# 1 per week
   freq_lmodact_2012 ==4 & unit_lmodact_2012 ==3 ~ 3,# 1 per week
   freq_lmodact_2012 ==12 & unit_lmodact_2012 ==4 ~ 3,# 1 per week
   (freq_lmodact_2012 >0 & freq_lmodact_2012 <4) & unit_lmodact_2012 ==3 ~ 4,# 1-3 per month (or <1 per week)
   (freq_lmodact_2012 >0 & freq_lmodact_2012 <12) & unit_lmodact_2012 ==4 ~ 4),# 1-3 per month (or <1 per week)
 lmodex.2014 = case_when(
   freq_lmodact_2014 == 0 | freq_lmodact_2014 == 996 | unit_lmodact_2014 ==5  ~ 5, #unable to do/never
   freq_lmodact_2014 >=1 & unit_lmodact_2014 ==1 ~ 1,# >= everyday
   freq_lmodact_2014 >=7 & unit_lmodact_2014 ==2 ~ 1,# >= everyday
   freq_lmodact_2014 >=30 & unit_lmodact_2014 ==3 ~ 1,# >= everyday
   freq_lmodact_2014 >=365 & unit_lmodact_2014 ==4 ~ 1,# >= everyday
   (freq_lmodact_2014 >1 & freq_lmodact_2014 <7) & unit_lmodact_2014 ==2 ~ 2,# >1 times per week
   (freq_lmodact_2014 >4 & freq_lmodact_2014 <30) & unit_lmodact_2014 ==3 ~ 2,# >1 times per week
   (freq_lmodact_2014 >12 & freq_lmodact_2014 <365) & unit_lmodact_2014 ==4 ~ 2,# >1 times per week
   freq_lmodact_2014 ==1 & unit_lmodact_2014 ==2 ~ 3,# 1 per week
   freq_lmodact_2014 ==4 & unit_lmodact_2014 ==3 ~ 3,# 1 per week
   freq_lmodact_2014 ==12 & unit_lmodact_2014 ==4 ~ 3,# 1 per week
   (freq_lmodact_2014 >0 & freq_lmodact_2014 <4) & unit_lmodact_2014 ==3 ~ 4,# 1-3 per month (or <1 per week)
   (freq_lmodact_2014 >0 & freq_lmodact_2014 <12) & unit_lmodact_2014 ==4 ~ 4),# 1-3 per month (or <1 per week)
 lmodex.2016 = case_when(
   freq_lmodact_2016 == 0 | freq_lmodact_2016 == 996 | unit_lmodact_2016 ==5  ~ 5, #unable to do/never
   freq_lmodact_2016 >=1 & unit_lmodact_2016 ==1 ~ 1,# >= everyday
   freq_lmodact_2016 >=7 & unit_lmodact_2016 ==2 ~ 1,# >= everyday
   freq_lmodact_2016 >=30 & unit_lmodact_2016 ==3 ~ 1,# >= everyday
   freq_lmodact_2016 >=365 & unit_lmodact_2016 ==4 ~ 1,# >= everyday
   (freq_lmodact_2016 >1 & freq_lmodact_2016 <7) & unit_lmodact_2016 ==2 ~ 2,# >1 times per week
   (freq_lmodact_2016 >4 & freq_lmodact_2016 <30) & unit_lmodact_2016 ==3 ~ 2,# >1 times per week
   (freq_lmodact_2016 >12 & freq_lmodact_2016 <365) & unit_lmodact_2016 ==4 ~ 2,# >1 times per week
   freq_lmodact_2016 ==1 & unit_lmodact_2016 ==2 ~ 3,# 1 per week
   freq_lmodact_2016 ==4 & unit_lmodact_2016 ==3 ~ 3,# 1 per week
   freq_lmodact_2016 ==12 & unit_lmodact_2016 ==4 ~ 3,# 1 per week
   (freq_lmodact_2016 >0 & freq_lmodact_2016 <4) & unit_lmodact_2016 ==3 ~ 4,# 1-3 per month (or <1 per week)
   (freq_lmodact_2016 >0 & freq_lmodact_2016 <12) & unit_lmodact_2016 ==4 ~ 4), # 1-3 per month (or <1 per week)
 lmodex.2018 = case_when(
   freq_lmodact_2018 == 9 | freq_lmodact_2018 == 4 ~ 5, #unable to do/never
   freq_lmodact_2018 == 7 ~ 1,# >= everyday
   freq_lmodact_2018 == 1 ~ 2,# >1 times per week
   freq_lmodact_2018 == 2  ~ 3,# 1 per week
   freq_lmodact_2018 == 3 ~ 4) # 1-3 per month (or <1 per week)
     )

table(exercise.cat$vigex.2010, useNA="ifany") 
table(exercise.cat$vigex.2012, useNA="ifany") 
table(exercise.cat$vigex.2014, useNA="ifany") 
table(exercise.cat$vigex.2016, useNA="ifany") 
table(exercise.cat$vigex.2018, useNA="ifany") 

table(exercise.cat$lmodex.2010, useNA="ifany") 
table(exercise.cat$lmodex.2012, useNA="ifany") 
table(exercise.cat$lmodex.2014, useNA="ifany") 
table(exercise.cat$lmodex.2016, useNA="ifany") 
table(exercise.cat$lmodex.2018, useNA="ifany") 

table(exercise.cat$vigex.2010, exercise.cat$freq_vigact_2010, useNA="ifany")
table(exercise.cat$vigex.2010[!is.na(exercise.cat$unit_vigact_2010)], exercise.cat$freq_vigact_2010[!is.na(exercise.cat$unit_vigact_2010)], useNA="ifany")
 #all other NAs are because the unit is missing

colnames(exercise.cat)
exercise.final<-exercise.cat[,-c(2:27)]

colnames(exercise.final)

#-------------------------------------------------------------------------------------
#merge all variables

NLSY <- iwstatus %>%
  left_join(income.hh, by='NLSY_CASE_ID') %>%
  left_join(poverty, by='NLSY_CASE_ID') %>%
  left_join(race, by='NLSY_CASE_ID') %>%
  left_join(nativity, by='NLSY_CASE_ID') %>%
  left_join(education, by='NLSY_CASE_ID') %>%
  left_join(parent, by='NLSY_CASE_ID') %>%
  left_join(geography, by='NLSY_CASE_ID') %>%
  left_join(birthdate, by='NLSY_CASE_ID') %>%
  left_join(age, by='NLSY_CASE_ID') %>%
  left_join(Age1stMarriage, by='NLSY_CASE_ID') %>%
  left_join(currentregion, by='NLSY_CASE_ID') %>%
  left_join(wealth, by='NLSY_CASE_ID') %>%
  left_join(employment, by='NLSY_CASE_ID') %>%
  left_join(occupation, by='NLSY_CASE_ID') %>%
  left_join(health_ins, by='NLSY_CASE_ID') %>%
  left_join(WeightHeight, by='NLSY_CASE_ID') %>%
  left_join(mstat, by='NLSY_CASE_ID') %>%
  left_join(basecog, by='NLSY_CASE_ID') %>%
  left_join(cognition, by='NLSY_CASE_ID') %>%
  left_join(smoking, by='NLSY_CASE_ID') %>%
  left_join(drinking, by='NLSY_CASE_ID') %>%
  left_join(H50_CESD, by='NLSY_CASE_ID') %>%
  left_join(H60_CESD, by='NLSY_CASE_ID') %>%
  left_join(H50_SRH, by='NLSY_CASE_ID') %>%
  left_join(diabetes, by='NLSY_CASE_ID') %>%
  left_join(CVD, by='NLSY_CASE_ID') %>%
  left_join(psy_ill, by='NLSY_CASE_ID') %>%
  left_join(exercise.final, by='NLSY_CASE_ID') 

colnames(NLSY)

#combine info from cesd at age 50 and 60 exams into one variable
NLSY$cesd<-ifelse(!is.na(NLSY$cesd.x)&!is.na(NLSY$cesd.y)&NLSY$age_2010<60,NLSY$cesd.x,
                  ifelse(!is.na(NLSY$cesd.x)&!is.na(NLSY$cesd.y)&NLSY$age_2010>=60,NLSY$cesd.y,
                         ifelse(is.na(NLSY$cesd.x)&!is.na(NLSY$cesd.y),NLSY$cesd.y,
                                ifelse(!is.na(NLSY$cesd.x)&is.na(NLSY$cesd.y),NLSY$cesd.x,NA))))

#remove the two cesd variables after combining 
NLSY=NLSY %>% select_if(!names(.) %in% c("cesd.x","cesd.y"))

#update occupational skill variable now that employment status and occupation class are in same data frame
table(NLSY$LBRF_2010, NLSY$occuskill_2010, exclude=NULL)
table(NLSY$LBRF_2010, exclude=NULL)
table(NLSY$occuskill_2010, exclude=NULL) #1-3934, 2-2330, NA-6422

#recategorize/relabel occupationa skill 
NLSY = NLSY %>%
  mutate(
    occuskill_2010 = case_when(occuskill_2010==2 ~ 2, # high skill
                              occuskill_2010==1~1, #low skill 
                              LBRF_2010 %in% c("Unemployed","Retired","Disabled","Not in LbrF")~0,
                              TRUE ~ 3),
    occuskill_2010 = factor(occuskill_2010, levels = c(0,1,2,3), labels = c("not working","lower skill","higher skill", "missing")),
    
    occuskill_2012 = case_when(occuskill_2012==2 ~ 2, # high skill
                               occuskill_2012==1~1, #low skill 
                               LBRF_2012 %in% c("Unemployed","Retired","Disabled","Not in LbrF")~0,
                               TRUE ~ 3),
    occuskill_2012 = factor(occuskill_2012, levels = c(0,1,2,3), labels = c("not working","lower skill","higher skill", "missing")),
    
    occuskill_2014 = case_when(occuskill_2014==2 ~ 2, # high skill
                               occuskill_2014==1~1, #low skill 
                               LBRF_2014 %in% c("Unemployed","Retired","Disabled","Not in LbrF")~0,
                               TRUE ~ 3),
    occuskill_2014 = factor(occuskill_2014, levels = c(0,1,2,3), labels = c("not working","lower skill","higher skill", "missing")),
    
    occuskill_2016 = case_when(occuskill_2016==2 ~ 2, # high skill
                               occuskill_2016==1~1, #low skill 
                               LBRF_2016 %in% c("Unemployed","Retired","Disabled","Not in LbrF")~0,
                               TRUE ~ 3),
    occuskill_2016 = factor(occuskill_2016, levels = c(0,1,2,3), labels = c("not working","lower skill","higher skill", "missing")),
    
    occuskill_2018 = case_when(occuskill_2018==2 ~ 2, # high skill
                               occuskill_2018==1~1, #low skill 
                               LBRF_2018 %in% c("Unemployed","Retired","Disabled","Not in LbrF")~0,
                               TRUE ~ 3),
    occuskill_2018 = factor(occuskill_2018, levels = c(0,1,2,3), labels = c("not working","lower skill","higher skill", "missing")))

table(NLSY$occuskill_2010, exclude=NULL)    
table(NLSY$LBRF_2010, NLSY$occuskill_2010, exclude=NULL)
table(NLSY$LBRF_2018, NLSY$occuskill_2018, exclude=NULL)

#fix discordance with 'never married' in age1stmarried.cat and marriage_2010
table(NLSY$age1stmarried.cat, NLSY$marriage_2010, exclude=NULL)

NLSY$marriage_2010[!is.na(NLSY$marriage_2010)&NLSY$age1stmarried.cat=="never married"]<-"never married"
NLSY$age1stmarried.cat[!is.na(NLSY$marriage_2010)&NLSY$marriage_2010=="never married"]<-"never married"
NLSY$marriage_2010[is.na(NLSY$marriage_2010)&NLSY$age1stmarried.cat=="never married"]<-"never married"

#save dataset
write.csv(NLSY,"/Users/xxxxx/code/For publication/Data/NLSY_data_final.csv")
