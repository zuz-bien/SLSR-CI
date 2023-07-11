#1. Install packages and load data


library(tidyverse)
library(scales)
library(kableExtra)
library(lubridate)
library(table1)

slsr <- read.csv("/Users/Zuzanna_Bien/Desktop/ACF Public Health/Cognitive impairment in stroke/SLSR-CI/SLSR_raw_data/ds_202305.csv")
slsr <- slsr %>% filter (dtstrk < 21946) #Filter out everyone registered after Feb 1st, 2020
nrow(slsr) #n = 6852

#-------------------------------------------------------------------------------------------------------------------------
#2. CLEAN DATA 

#2.1 Calculate the number of people who completed the interview at each timepoint
n0 <- sum(!is.na(slsr$id))
n0.25<- sum(!is.na(slsr$dtint0.25))
n1<- sum(!is.na(slsr$dtint1))
n5 <- sum(!is.na(slsr$dtint5))

#2.2 Calculate missing mtot variables
slsr <- slsr %>% mutate (mtot0.25 = mt1_0.25 + mt2_0.25 + mt3_0.25 + mt4_0.25 + mt5_0.25 + mt6_0.25 + mt7_0.25 + mt8_0.25 +mt9_0.25 + mt10_0.25)
slsr <- slsr %>% mutate (mtot1 = mt1_1 + mt2_1 + mt3_1 + mt4_1 + mt5_1 + mt6_1 + mt7_1 + mt8_1 +mt9_1 + mt10_1)
#NB mt5_5 was misnamed as mt5_1.1
slsr <- slsr %>% mutate(mt5_5 = mt5_1.1) %>% mutate (mtot5 = mt1_5 + mt2_5 + mt3_5 + mt4_5 + mt5_5 + mt6_5 + mt7_5 + mt8_5 +mt9_5 + mt10_5)

#2.3. Create 'cog'variable to amalgamate cognitive scores 
#cog 1=intact 2=impaired 

#initial interview 
slsr <- slsr %>% mutate (cog = case_when(
  cogcat == 2 | mtot < 8 ~ 2,
  cogcat == 1 & mtot >=8  ~ 1,
  cogcat == 1 & is.na(mtot)~ 1,
  is.na(cogcat)& mtot >=8  ~ 1,
  is.na(mtot) & is.na(cogcat) ~ NA)) 

#3m follow-up
slsr <- slsr %>% mutate (cog0.25 = case_when(
  cogcat0.25 == 2 | mtot0.25 < 8 ~ 2,
  cogcat0.25 == 1 & mtot0.25 >=8  ~ 1,
  cogcat0.25 == 1 & is.na(mtot0.25)~ 1,
  is.na(cogcat0.25)& mtot0.25 >=8  ~ 1,
  is.na(mtot0.25) & is.na(cogcat0.25) ~ NA)) 

#1 year follow-up
slsr <- slsr %>% mutate (cog1 = case_when(
  cogcat1 == 2 | mtot1 < 8 ~ 2,
  cogcat1 == 1 & mtot1 >=8  ~ 1,
  cogcat1 == 1 & is.na(mtot1)~ 1,
  is.na(cogcat1)& mtot1 >=8  ~ 1,
  is.na(mtot1) & is.na(cogcat1) ~ NA)) 

#5-year follow-up 
slsr <- slsr %>% mutate (cog5 = case_when(
  cogcat5 == 2 | mtot5 < 8 ~ 2,
  cogcat5 == 1 & mtot5 >=8  ~ 1,
  cogcat5 == 1 & is.na(mtot5)~ 1,
  is.na(cogcat5)& mtot5 >=8  ~ 1,
  is.na(mtot5) & is.na(cogcat5) ~ NA)) 

#2.4 Impute cog0.25 based on surrounding timepoints (if cog == cog1, and cog0.25 is NA)
slsr <- slsr %>% mutate(cog0.25 = ifelse (
  (cog == cog1 & is.na(cog0.25)), cog, cog0.25 
))

#2.5 Recode mtndn = 4 (cognitively impaired) as cog = 2 (cognitively impaired)
slsr <- slsr %>%
  mutate(cog0.25 = ifelse(is.na(mtndn_0.25), cog0.25, ifelse(mtndn_0.25 == 4, 2, cog0.25)))

#2.6 Make a recur0.25 variable to amalgamate anstrk3m (known recurrence; 1=No, 2=Yes) and recevid3m (evidence of recurrence, 1=No, 2=Yes)
#slsr <- slsr %>% 
#  mutate(recur0.25 = case_when(
#    #positive
 #   anstrk0.25 == 2 ~ 2,  #known recurrence
  #  anstrk0.25 == 1 & recevid0.25 == 2 ~ 2,#no known recurrence; suspected recurrence
   # is.na(anstrk0.25) & recevid0.25 == 2 ~ 2, #no info re: known recurrence; suspected recurrence
    #negative
  #  anstrk0.25 == 1 & recevid0.25 == 1 ~ 1,
   # anstrk0.25 == 1 & is.na(recevid0.25) ~ 1,
    #is.na(anstrk0.25) & recevid0.25 == 1 ~ 1,
        #unknown
  #  is.na(anstrk0.25) & is.na(recevid0.25) ~ NA)) #no info on either
    


#-------------------------------------------------------------------------------------------------------------------------
#3. FILTER BASED ONE EXCLUSION CRITERIA 

#3.1 Dead before having 3mfu (window up to 6 months)
died_before_3mfu <- slsr %>% filter (surv<0.5 & is.na(dtint0.25) == TRUE) #1640 died before 6 months and had no 3mfu data
slsr <- slsr %>% filter (!id %in% died_before_3mfu$id)
nrow(slsr)
#n = 5212


#3.2 Late registration 
#Exclude patients who were registered more than 6 months post-stroke (after the 3-month follow-up period elapsed)
slsr <- slsr %>% filter (dtint - dtstrk < 182.5 | is.na(dtint-dtstrk) == TRUE) 
nrow(slsr)
#n = 4400

#3.3 Pre-existing dementia
#recode dement_rec == 0 and dement_rec == 3 as NAs, because they have no explanation 
slsr <- slsr %>% mutate (dement_rec = as.factor(dement_rec)) %>% 
  mutate (dement_rec = case_when(
    dement_rec == 0 ~ NA,
    dement_rec == 1 ~ 1,
    dement_rec == 2 ~ 2,
    dement_rec == 3 ~ NA,
    is.na(dement_rec) ~ NA
  )) 

slsr <- slsr %>% filter (dement_rec == 1 | is.na(dement_rec)) 
nrow(slsr)
#n = 4313

#3.4 Calculate how many completed 3-month follow-up  
slsr_3mfu <- slsr %>% filter(!is.na(dtint0.25)) 
nrow(slsr_3mfu)
#n = 3237 


#--------------------------------------------------------------------------------------------
#4. EXPLORE REASONS FOR MISSINGNESS

#4.1 'mtndn' variable gives reasons for not completing. Find out how many patients have a reason stated. 
sum(!is.na(slsr$mtndn_0.25)) #87 patients have reason stated at 3 months
table (slsr$mtndn_0.25)

#4.2 Create a new variable (why_mtndn) which will aggregate all the reasons 
slsr <- slsr %>% mutate (why_mtndn0.25 = ifelse (!is.na(mtndn_0.25), mtndn_0.25, NA))

#4.3 Encode those with speech disorder (spehcur = 2) as why_mtndn = 3
sum(slsr$spehcur025 == 2, na.rm = TRUE) #585 people have slurred speech/trouble articulating on 3 month follow-up

#need to add, only in those with mtot0.25 == NA
slsr <- slsr %>% 
  #if spehcur is NA, leave the why_mtndn0.25 value unchanged
  mutate(why_mtndn0.25 = ifelse(is.na(spehcur025), why_mtndn0.25, 
                                #if spehcur is anything other than NA
                                #if spehcur is equal to 2, mtot0.25 is missing,  and there was no value in why_mtndn0.25 previously
                                #/the value was 5 ('other'), change why_mtndn0.25 to 3
                                ifelse(spehcur025 == 2 & is.na(mtot0.25) & ((is.na(why_mtndn0.25) | why_mtndn0.25 == 5)), 3, 
                                       #otherwise leave it unchanged
                                       why_mtndn0.25)))

slsr <- slsr %>% 
  mutate(why_mtndn0.25 = ifelse(is.na(spehcur025), why_mtndn0.25, 
                                ifelse(spehcur025 == 2 & is.na(mtot0.25) & (is.na(why_mtndn0.25) | why_mtndn0.25 == 5), 3, why_mtndn0.25)))


#why_mtndn
#1=refused, 2=language barrier, 3=aphasia, 4=too cognitively impaired
table(slsr$why_mtndn0.25)

## -------------------------------------------------------------------------------------------------------------------------
#5. EXPLORE REASONS FOR LOSS-TO-FOLLOW-UP AT 3 MONTHS 

#888 were eligible but did not have interview 
lost_to_3mfu <- slsr %>% filter (is.na(dtint0.25) & is.na(cog0.25)) #n = 888 

nrow(slsr)
## -------------------------------------------------------------------------------------------------------------------------
#6. Save the dataframe

write.csv(slsr, "/Users/Zuzanna_Bien/Desktop/ACF Public Health/Cognitive impairment in stroke/SLSR-CI/SLSR_raw_data/slsr(pre_2020).csv", row.names = FALSE)


