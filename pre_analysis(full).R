#nb this is for all of SLSR up to present date 
#if this is used in the future, it should be limited to everything before April 1st, 2022, when MOCA started being used

#1. Install packages and load data

library(tidyverse)
library(scales)
library(kableExtra)
library(lubridate)
library(table1)

slsr <- read.csv("/Users/Zuzanna_Bien/Desktop/ACF Public Health/Cognitive impairment in stroke/SLSR-CI/SLSR_raw_data/ds_202305.csv")

#-------------------------------------------------------------------------------------------------------------------------
#2.Clean data

#2.1 Calculate the number of people who completed the interview at each timepoint

#total number of follow-ups at each timepoint 
n0 <- sum(!is.na(slsr$id))
n0.25<- sum(!is.na(slsr$dtint0.25))
n1<- sum(!is.na(slsr$dtint1))
n5 <- sum(!is.na(slsr$dtint5))

#2.2 Calculate missing mtot variables

slsr <- slsr %>% mutate (mtot0.25 = mt1_0.25 + mt2_0.25 + mt3_0.25 + mt4_0.25 + mt5_0.25 + mt6_0.25 + mt7_0.25 + mt8_0.25 +mt9_0.25 + mt10_0.25)
slsr <- slsr %>% mutate (mtot1 = mt1_1 + mt2_1 + mt3_1 + mt4_1 + mt5_1 + mt6_1 + mt7_1 + mt8_1 +mt9_1 + mt10_1)
#NB mt5_5 was misnamed as mt5_1.1
slsr <- slsr %>% mutate(mt5_5 = mt5_1.1) %>% mutate (mtot5 = mt1_5 + mt2_5 + mt3_5 + mt4_5 + mt5_5 + mt6_5 + mt7_5 + mt8_5 +mt9_5 + mt10_5)

#2.3. Amalgamate cognitive scores - create 'cog' variable
#cogcat: 1=intact (AMT 8-10 and MMSE ≥ 24) 2=impaired (AMT 0-7 and MMSE < 24)

#initial interview 
slsr <- slsr %>% mutate (cog = case_when(
  cogcat == 2 | mtot < 8 ~ 2,
  cogcat == 1 & mtot >=8  ~ 1,
  cogcat == 1 & is.na(mtot)~ 1,
  is.na(cogcat)& mtot >=8  ~ 1,
  is.na(mtot) & is.na(cogcat) ~ NA)) 

#2915 patients have either cogcat or mtot recorded at 3m follow-up
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



#-------------------------------------------------------------------------------------------------------------------------
#3. Find out how many patients have data available for cognitive scores 

completion_rates <- rbind (c(
  sum(!is.na(slsr$cog)),
  sum(!is.na(slsr$cog))/sum(!is.na(slsr$dtint)), 
  sum(!is.na(slsr$cog0.25)),
  sum(!is.na(slsr$cog0.25))/sum(!is.na(slsr$dtint0.25)), 
  sum(!is.na(slsr$cog1)),
  sum(!is.na(slsr$cog1))/sum(!is.na(slsr$dtint1)), 
  sum(!is.na(slsr$cog5)),
  sum(!is.na(slsr$cog5))/sum(!is.na(slsr$dtint5))
))

colnames(completion_rates) <- c("n0", "%0", "n0.25", "%0.25", "n1", "%1", "n5", "%5")

completion_table <- completion_rates %>% kable(caption = "Table 1: Absolute number (n) and percentage (%) of patients with non-missing cognitive scores at each follow-up point point (out of patients who completed 3mo follow-up)") %>% kable_styling(bootstrap_options = "striped", full_width = FALSE)



completion_rates_post_2020 <- rbind (c(
  sum(!is.na(slsr_from_2020$cog)),
  sum(!is.na(slsr_from_2020$cog))/sum(!is.na(slsr_from_2020$dtint)), 
  sum(!is.na(slsr_from_2020$cog0.25)),
  sum(!is.na(slsr_from_2020$cog0.25))/sum(!is.na(slsr_from_2020$dtint0.25)), 
  sum(!is.na(slsr_from_2020$cog1)),
  sum(!is.na(slsr_from_2020$cog1))/sum(!is.na(slsr_from_2020$dtint1)), 
  sum(!is.na(slsr_from_2020$cog5)),
  sum(!is.na(slsr_from_2020$cog5))/sum(!is.na(slsr_from_2020$dtint5))
))

completion_rates_pre_2020 <- rbind (c(
  sum(!is.na(slsr_pre_2020$cog)),
  sum(!is.na(slsr_pre_2020$cog))/sum(!is.na(slsr_pre_2020$dtint)), 
  sum(!is.na(slsr_pre_2020$cog0.25)),
  sum(!is.na(slsr_pre_2020$cog0.25))/sum(!is.na(slsr_pre_2020$dtint0.25)), 
  sum(!is.na(slsr_pre_2020$cog1)),
  sum(!is.na(slsr_pre_2020$cog1))/sum(!is.na(slsr_pre_2020$dtint1)), 
  sum(!is.na(slsr_pre_2020$cog5)),
  sum(!is.na(slsr_pre_2020$cog5))/sum(!is.na(slsr_pre_2020$dtint5))
))



#-------------------------------------------------------------------------------------------------------------------------
#4. Explaining missing data 

#4.1 Exclude those who died before 3mo follow-up (window up to 6 months)
died_before_3mfu <- slsr %>% filter (surv<0.5 & is.na(dtint0.25) == TRUE) #1799 died before 2 months and had no 3mfu data
slsr <- slsr %>% filter (!id %in% died_before_3mfu$id)

#n = 6122

#4.2 Exclude those whose stroke was less than 6 months prior to data extraction AND dtint0.25 missing (not yet due for follow-up)
not_yet_eligible <- slsr %>% filter ((23195 - dtint < 183 & is.na(dtint0.25) == TRUE)) %>% select(id)  #further 58 patients registered in the last 6 months without follow-up 
slsr <- slsr %>% filter (!id %in% not_yet_eligible$id)

#n = 6064

#4.3 Late registration 
#Exclude patients who were registered more than 6 months post-stroke (after the 3-month follow-up period elapsed)
slsr <- slsr %>% filter (dtint - dtstrk < 182.5 | is.na(dtint-dtstrk) == TRUE) 

#n = 5173

#4.4 3-month follow-up completed 

slsr_complete_3mfu <- slsr %>% filter (!is.na(dtint0.25))

#n = 3886  

## 4.5 Exclude those with pre-existing dementia------------------------------------------------------------------------------------------------
sum(slsr$dement_rec == 2, na.rm = TRUE) #125 people with pre-existing dementia 

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
#n = 5048


#--------------------------------------------------------------------------------------------
#Finding out reasons for lack of cognitive scores

#4.5 'mtndn' variable gives reasons for not completing. Find out how many patients have a reason stated. 
sum(!is.na(slsr$mtndn_0.25)) #493 patients have reason stated at 3 months
table (slsr$mtndn_0.25)

#4.6 Create a new variable (why_mtndn) which will aggregate all the reasons we infered from elsewhere together with the 
# original mtndn - 'why_mtndn'
slsr <- slsr %>% mutate (why_mtndn0.25 = ifelse (!is.na(mtndn_0.25), mtndn_0.25, NA))

#4.7 Recode mtndn = 4 (n = 77; cognitively impaired) as cogcat 2 (cognitively impaired)
#NB number of complete cogcat0.25 increased from 2875 to 2951 by 76 (not 77) because one person already had cogcat0.25 (as well as mtndn_0.25)
slsr <- slsr %>%
  mutate(cog0.25 = ifelse(is.na(mtndn_0.25), cog0.25, ifelse(mtndn_0.25 == 4, 2, cog0.25)))

#4.8 Encode those with speech disorder (spehcur = 2) as why_mtndn = 3
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
                 
## -------------------------------------------------------------------------------------------------------------------------
## 6. Account for those who had their 3mfu but do not have cognitive scores 

#How many actually ended up with 3-month follow-up?
sum(!is.na(slsr$dtint0.25)) #n = 3795

#create slsr_3mfu data frame 
slsr_3mfu <- slsr %>% filter(!is.na(dtint0.25)) 

#how many have cognitive scores available at 3 months? 
sum(is.na(slsr_3mfu$cog0.25))

#find out reasons why they don't have cogitive scores  
#1=refused, 2=language barrier, 3=aphasia, 4=too cognitively impaired
table(slsr_3mfu$why_mtndn0.25)
#174 1 = refused 
#70 2 = language barrier
#172 3 = aphasia
#50 4 = too cognitively impaired
#45 5 = other 

## -------------------------------------------------------------------------------------------------------------------------
#5. Analysis 

#how many people who were unimpaired (1) on initial assessment, were later impaired (2) on 3mfu? 
slsr %>% filter (cog == 1 & cog0.25 == 2) %>% nrow() #160 went from unimpaired to impaired (worsened)
sum(is.na(slsr$cog))
sum(is.na(slsr$cog0.25))
slsr %>% filter (cog == 2 & cog0.25 == 1) %>% nrow() #266 went from impaired to unimpaired (recovered)

#how many people who were unimpaired (1) on on 3mfu were unimpaired later? 
slsr %>% filter (cog1 == 1 & cog0.25 == 2) %>% nrow() #125 went from impaired on 3m to unimpaired on 1y (recovered)
slsr %>% filter (cog1 == 2 & cog0.25 == 1) %>% nrow() #168 went from unimpaired to impaired (worsened)

#how many people have initial scores & 1 year, but not 3 months 
missing_3m <- slsr %>% filter (!is.na(cog) & is.na(cog0.25) & !is.na(cog1)) #349
missing_3m %>% filter (cog == cog1) %>% nrow() #in 266, cog is the same at initial and 1 year 

test <- slsr %>% mutate(cog0.25 = ifelse (
  (cog == cog1 & is.na(cog0.25)), cog, cog0.25 
))

test %>% select (id, cog, cog0.25, cog1) %>% view()

#check if missingness rate got worse since Covid 
slsr_from_2020 <- slsr %>% filter (strk_y >= 2020) 
slsr_pre_2020 <- slsr %>% filter (strk_y < 2020) 

sum(!is.na(slsr_from_2020$cog0.25))/sum(!is.na(slsr_from_2020$dtint0.25))
sum(!is.na(slsr_pre_2020$cog0.25))/sum(!is.na(slsr_pre_2020$dtint0.25))

nrow(slsr)

## -------------------------------------------------------------------------------------------------------------------------
#6. Write to file 

write.csv(slsr, "/Users/Zuzanna_Bien/Desktop/ACF Public Health/Cognitive impairment in stroke/SLSR-CI/SLSR_raw_data/slsr(full).csv", row.names = FALSE)
