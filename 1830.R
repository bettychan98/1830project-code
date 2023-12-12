library(dplyr)
library(foreign)
library(ggplot2)
library(table1)
library(car)
library(purrr)

#read in data
#put the raw file in the same folder as the R file
#outcome: smoking cessation attempts
#input: demographic(gender, race, age, marital status, education level, citizenship), income, number of household smokers at home,
#input2: alcohol use, diabetes, drug use, age #cigarettes/day during past 30(5-SMQ 720) days, hypertension, household information and diabetes.
#author: Jiaoyang
demographic = read.xport('DEMO_J.XPT')
income = read.xport('INQ_J.XPT')
self_smoking = read.xport('SMQ_J.XPT')
family_smoking = read.xport('SMQFAM_J.XPT')
depression = read.xport('DPQ_J.XPT')
alcohol = read.xport('ALQ_J.XPT')
diabetes = read.xport('DIQ_J.XPT')
drug = read.xport('DUQ_J.XPT')
recent_smoke = read.xport('SMQRTU_J.XPT')
hypertension = read.xport('BPQ_J.XPT')

#View data
#author:Jiaoyang
self_smoking$cursmoker = ifelse(self_smoking$SMQ040 %in% c(1,2), 1, ifelse(self_smoking$SMQ040 == 3,0,NA))
#do you now smoke cigarettes? 1-everyday, 2-somedays, 3-not at all, 7-refused, 9-don't know
table(self_smoking$cursmoker) # this gives us 1021 current smokers

self_smoking$allsmoker = ifelse(self_smoking$SMD030<=76&self_smoking$SMD030>=7, 1, ifelse(self_smoking$SMD030 == 0,0,NA))
table(self_smoking$allsmoker, useNA = "ifany") #this gives us 2285 people who have ever smoked

table(self_smoking$SMQ670) #tried to quit smoking 1-yes 2-no
table(family_smoking$SMD470) #this gives us numbers of families who smoke at home
##variables to control for:
table(demographic$RIAGENDR) #gender 1-male 2-female
table(demographic$RIDAGEYR) #age in years
table(demographic$RIDRETH3) #race 1-Mexican American 2-Other Hispanic 3-White 4-Black 6-Asian 7-other
table(demographic$DMDCITZN) #citizenship 1-citizen 2-not a citizen of US
table(demographic$DMDMARTL) #marital status 1-married 2-widowed 3-divorced 4-separated 5-never married 6-living with a partner
table(demographic$DMDEDUC2) #adults 20+  5-college graduate or above; 1-less than 9th grade
table(income$INDFMMPI) #family monthly poverty level index
table(alcohol$ALQ121) #alcohol use in past 12 months
table(hypertension$BPQ020) #ever told you have high blood pressure 1-yes 2-no
table(diabetes$DIQ010) #doctor told you have diabetes 1-yes 2-no
table(drug$DUQ200) #ever used marijuana or hashish 1-yes 2-no
table(recent_smoke$SMQ720) #cigarettes/day during past 5 days
table(self_smoking$SMD650) #Avg # cigarettes/day during past 30 days

#Clean data
#author: Jiaoyang
##merge the 10 datasets onto one for convenience
list_df = list(self_smoking,family_smoking,demographic,hypertension,income,alcohol,diabetes,drug,recent_smoke,depression)
merged = reduce(list_df, full_join, by = "SEQN")
##select the needed columns
selected  = merged %>%
  dplyr::select(ALQ121,BPQ020,DIQ010,DUQ200,SMQ720,SMD650,cursmoker, allsmoker, SMQ670, RIDRETH3,SMQ848, SMD470, RIAGENDR, RIDAGEYR, DMDCITZN, DMDMARTL, DMDEDUC2, INDFMMPI,DMDHHSIZ,DMDHHSZA,DMDHHSZB)
selected = rename(selected, TriedQuit = SMQ670,
                  TimesStop = SMQ848,
                  FaNu_smokeAtHome = SMD470,
                  gender = RIAGENDR,
                  age = RIDAGEYR,
                  citizenship = DMDCITZN,
                  marital_sta = DMDMARTL,
                  education = DMDEDUC2,
                  poverty = INDFMMPI,  #Family monthly poverty level index, a ratio of monthly family income to the HHS poverty guidelines specific to family size.
                  race = RIDRETH3,
                  household = DMDHHSIZ,  #Total number of people in the Family
                  less5child = DMDHHSZA,
                  olderchild = DMDHHSZB, #number of children 6-17 years old in HH
                  alcohol_12 = ALQ121,
                  hypertension_yn = BPQ020,
                  diabete_yn = DIQ010,
                  drug_yn = DUQ200,
                  cigarette_5 = SMQ720,
                  cigarette_30 = SMD650
)
##clean the yes/no questions
selected$hypertension_yn = ifelse(selected$hypertension_yn == 1, 1, ifelse(selected$hypertension_yn == 2, 0, NA))
selected$diabete_yn = ifelse(selected$diabete_yn == 1, 1, ifelse(selected$diabete_yn == 2, 0,NA))
selected$drug_yn = ifelse(selected$drug_yn == 1, 1, ifelse(selected$drug_yn == 2, 0,NA))
table(selected$drug_yn)

current_smoker = selected[which(selected$cursmoker == 1),]

##check the percentage of missing values
sapply(current_smoker, function(x) sum(length(which(is.na(x)))))/nrow(current_smoker)

#load package
library(missForest)
library(visdat)

current_smoker = current_smoker %>%
  mutate(hypertension_yn = as.factor(hypertension_yn),
         diabete_yn = as.factor(diabete_yn),
         drug_yn = as.factor(drug_yn),
         TriedQuit = as.factor(TriedQuit),
         gender = as.factor(gender),
         citizenship = as.factor(citizenship),
         education = as.factor(education))
vis_miss(current_smoker)

#how do we find the missing pattern?

#imputate the missing data
temp = missForest(current_smoker)$ximp 
current_smoker = temp
current_smoker$alcohol_12 = round(current_smoker$alcohol_12)
current_smoker$cigarette_5 = round(current_smoker$cigarette_5)
current_smoker$cigarette_30 = round(current_smoker$cigarette_30)
current_smoker$TimesStop = round(current_smoker$TimesStop)
current_smoker$FaNu_smokeAtHome = round(current_smoker$FaNu_smokeAtHome)

