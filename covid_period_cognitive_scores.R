#CALCULATING MISSINGNESS RATE DURING COVID - FEB 1ST, 2020 TO MARCH 31ST, 2022

library(tidyverse)
library(scales)
library(kableExtra)
library(lubridate)
library(table1)

slsr <- read.csv("/Users/Zuzanna_Bien/Desktop/ACF Public Health/Cognitive impairment in stroke/SLSR-CI/SLSR_raw_data/ds_202305.csv")
slsr <- slsr %>% filter (dtstrk > 21946 & dtstrk < 22736)  

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

#Calculate the rate of non-completion during Covid 
sum(!is.na(slsr$cog0.25))/sum(!is.na(slsr$dtint0.25)) 

#46.7% OF PATIENTS WHO COMPLETED A 3-MONTH FOLLOW-UP INTERVIEW DURING COVID HAD COGNITIVE SCORE AVAILABLE (WITHOUT IMPUTATION)
