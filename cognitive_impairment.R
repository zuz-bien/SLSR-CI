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

#2.2 Recode mtndn = 

#


#-------------------------------------------------------------------------------------------------------------------------
#3. Find out how many patients have data available for cognitive scores 


#3.1 Calculate the number of patients who have cognitive scores at each follow-up point

#3.1.1 Initial assessment 
#number of patients with complete observations for mtot on initial assessment
n_mtot <- sum(is.na(slsr$mtot) == FALSE)

#number of patients with complete observations for cogcat on initial assessment
n_cogcat <- sum(is.na(slsr$cogcat) == FALSE)

#3.1.2 3-month follow-up 
#number of patients with complete observations for mtot at 3mo
n_mtot0.25 <- sum(is.na(slsr$mtot0.25) == FALSE)

#number of patients with complete observations for cogcat at 3mo
n_cogcat0.25<- sum(is.na(slsr$cogcat0.25) == FALSE)

#3.1.3 1 year follow-up

#number of patients with complete observations for mtot at 1y
n_mtot1 <- sum(is.na(slsr$mtot1) == FALSE)
#number of patients with complete observations for cogcat at 1y
n_cogcat1 <-sum(is.na(slsr$cogcat1) == FALSE)

#3.1.4 5-year follow-up
#number of patients with complete observations for mtot at 5y
n_mtot5 <- sum(is.na(slsr$mtot5) == FALSE)

#number of patients with complete observations for cogcat at 5y
n_cogcat5 <- sum(is.na(slsr$cogcat5) == FALSE)

#3.1.5 Summary table: complete cognitive score observations 

#create a dataframe with number of poatients who have mtot and cogcat scores 
n_mtots <- c(n_mtot, n_mtot0.25, n_mtot1, n_mtot5)
n_cogcats <- c( n_cogcat, n_cogcat0.25, n_cogcat1, n_cogcat5)
n_cognitive_scores <- rbind(n_mtots, n_cogcats) %>% as.data.frame()
names(n_cognitive_scores) = c("t0", "t0.25", "t1", "t5")

#add completion rate (denominator = n patients who completed interview at each follow-up time)
#NB this is currently wrong because dtint1 is wrong 
n_cognitive_scores <- n_cognitive_scores %>% 
  mutate (t0_completion_rate = (t0*100/n0), t0.25_completion_rate = (t0.25*100/n0.25), t1_completion_rate = (t1*100/n1), t5_completion_rate = (t5*100/n5)) %>% 
  mutate(across(c(t0_completion_rate,t0.25_completion_rate, t1_completion_rate, t5_completion_rate),round, 2))  %>%
  select(t0, t0_completion_rate, t0.25, t0.25_completion_rate, t1, t1_completion_rate, t5, t5_completion_rate) 

#create a nice table of this 
n_cognitive_scores %>% kable(caption = "Table 1: Number of patients with non-missing cognitive scores at each follow-up point") %>% kable_styling(bootstrap_options = "striped", full_width = FALSE)


#3.2 Calculate the number of patients missing cognitive scores are each timepoint 
mtot_incomplete <- n0 - n_mtot 
mtot0.25_incomplete <- n0.25 - n_mtot0.25
mtot1_incomplete <- n1 - n_mtot1
mtot5_incomplete <-n5 - n_mtot5

cogcat_incomplete <- n0 - n_cogcat
cogcat0.25_incomplete <- n0.25 - n_cogcat0.25
cogcat1_incomplete <- n1 - n_cogcat1
cogcat5_incomplete <- n5 - n_cogcat5

#summary table 
n_cognitive_scores_incomplete <- rbind(c(mtot_incomplete, mtot0.25_incomplete, mtot1_incomplete, mtot5_incomplete), c(cogcat_incomplete,cogcat0.25_incomplete,cogcat1_incomplete, cogcat5_incomplete)) %>% as.data.frame() 
colnames(n_cognitive_scores_incomplete) <- c("t0", "t0.25", "t1", "t5")

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
  mutate(cogcat0.25 = ifelse(is.na(mtndn_0.25), cogcat0.25, ifelse(mtndn_0.25 == 4, 2, cogcat0.25)))

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
#5. Amalgamate cognitive scores - create 'cog' variable
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

#We will be working on the slsr_3mfu dataframe




# create a dataframe with only people who have cognitive scores 
slsr_3mfu_cog <- slsr_3mfu %>% filter (!is.na(cog0.25)) %>% 
  mutate (sex = as.factor(sex),
          eth6cat = as.factor(eth6cat), 
          cog0.25 = as.factor(cog0.25)) 

table(slsr_3mfu_cog$eth6cat)

#Recode factors:
#Cognitive status: 1=cognitively intact (8-10 AMT and 24+ MMSE) 2=cognitively impaired (0-7 AMT and <24 MMSE)
slsr_3mfu_cog$cog0.25 <- recode_factor(slsr_3mfu_cog$cog0.25, "1" = "Cognitively intact", "2" = "Cognitively impaired")
#Ethnic category: 1=White 2=Black Carribean 3=Black African 4=Black Other 5=Other 99= Unknown/not stated
slsr_3mfu_cog$eth6cat <- recode_factor(slsr_3mfu_cog$eth6cat, "1" = "White", "2" = "Black Carribean", "3" = "Black African", "4" = "Black other", "5" = "Other", "99" = "Unknown") 
#Sex: 1=Male 2=Female
slsr_3mfu_cog$sex <- recode_factor(slsr_3mfu_cog$sex, "1" = "Male", "2" = "Female")
#Social class: 1=Non manual 2=skilled manual 88=Army
slsr_3mfu_cog$socat <- recode_factor(slsr_3mfu_cog$socat, "1" = "Non-manual work", "2" = "Manual work", "88" = "Army") 

#Make nice labels for the table: 
label(slsr_3mfu_cog$sex)       <- "Sex"
label(slsr_3mfu_cog$age)       <- "Age"
label(slsr_3mfu_cog$eth6cat)       <- "Ethnic category"
label(slsr_3mfu_cog$socat)       <- "Socioeconomic category"



table1(~ sex + age + eth6cat + socat | cog0.25, data=slsr_3mfu_cog)


slsr_3mfu$age


## ----absolute number & proportion of cognitively impaired at each timepoint-----------------------------------------------

#1=cognitively intact (8-10 AMT and 24+ MMSE) 2=cognitively impaired (0-7 AMT and <24 MMSE)

cogcat <- slsr %>% select(id, cogcat, cogcat0.25, cogcat1, cogcat5) %>% 
summarise (
cogcat_impaired = sum(cogcat == 2, na.rm = TRUE),
cogcat_unimpaired = sum(cogcat == 1, na.rm = TRUE),
cogcat_tot = sum(is.na(cogcat) == FALSE), 
cogcat_impaired_prop = cogcat_impaired*100/cogcat_tot,
cogcat_unimpaired_prop = cogcat_unimpaired*100/cogcat_tot,
cogcat0.25_impaired = sum(cogcat0.25 == 2, na.rm = TRUE),
cogcat0.25_unimpaired = sum(cogcat0.25 == 1, na.rm = TRUE),
cogcat0.25_tot = sum(is.na(cogcat0.25) == FALSE), 
cogcat0.25_impaired_prop = cogcat0.25_impaired*100/cogcat0.25_tot,
cogcat0.25_unimpaired_prop = cogcat0.25_unimpaired*100/cogcat0.25_tot,
cogcat1_impaired = sum(cogcat1 == 2, na.rm = TRUE),
cogcat1_unimpaired = sum(cogcat1 == 1, na.rm = TRUE),
cogcat1_tot = sum(is.na(cogcat1) == FALSE), 
cogcat1_impaired_prop = cogcat1_impaired*100/cogcat1_tot,
cogcat1_unimpaired_prop = cogcat1_unimpaired*100/cogcat1_tot,
cogcat5_impaired = sum(cogcat5 == 2, na.rm = TRUE),
cogcat5_unimpaired = sum(cogcat5 == 1, na.rm = TRUE),
cogcat5_tot = sum(is.na(cogcat5) == FALSE),
cogcat5_impaired_prop = cogcat5_impaired*100/cogcat5_tot,
cogcat5_unimpaired_prop = cogcat5_unimpaired*100/cogcat5_tot) %>% view()

print(cogcat)


