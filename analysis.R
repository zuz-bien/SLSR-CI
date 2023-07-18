#1. COGNITIVE IMPAIRMENT POST-STROKE: ANALYSIS  

## -------------------------------------------------------------------------------------------------------------------------
#1.1 Table 1 


library(tidyverse)
library(scales)
library(kableExtra)
library(lubridate)
library(table1)

slsr <- read.csv("/Users/Zuzanna_Bien/Desktop/ACF Public Health/Cognitive impairment in stroke/SLSR-CI/SLSR_raw_data/slsr(pre_2020).csv")

# create a dataframe with only people who have cognitive scores (table1 does not accept NAs in the classifying variable)
slsr_table1 <- slsr %>% filter (!is.na(cog0.25)) %>% 
  #demographic
  mutate(frcat0.25 = cut(frtot0.25, breaks = c(0, 14, 30, 45), labels = c("Inactive (0-14)", "Moderate (15-30)", "Active (31-45)"))) %>%
  mutate(anstrk0.25 = ifelse(anstrk0.25 == 3, NA, anstrk0.25)) %>% 
  mutate (sex = as.factor(sex),
          eth6cat = as.factor(eth6cat), 
          cog0.25 = as.factor(cog0.25), 
          educat = as.factor(educat), 
          #co-morbidities
          rfpdep = as.factor(rfpdep),
          bi3m = as.factor(bi0.25),
          frcat0.25 = as.factor(frcat0.25), 
          anstrk0.25 = as.factor(anstrk0.25),
          #stroke characteristics
          subtype = as.factor(subtype),
          mtaet6 = as.factor(mtaet6)
  ) 

#Recode factors:
#Cognitive status: 1=cognitively intact (8-10 AMT and 24+ MMSE) 2=cognitively impaired (0-7 AMT and <24 MMSE)
slsr_table1$cog0.25 <- recode_factor(slsr_table1$cog0.25, "1" = "Cognitively intact", "2" = "Cognitively impaired")
#Ethnic category: 1=White 2=Black Carribean 3=Black African 4=Black Other 5=Other 99= Unknown/not stated
slsr_table1$eth6cat <- recode_factor(slsr_table1$eth6cat, "1" = "White", "2" = "Black Carribean", "3" = "Black African", "4" = "Black other", "5" = "Other", "99" = "Unknown") 
#Sex: 1=Male 2=Female
slsr_table1$sex <- recode_factor(slsr_table1$sex, "1" = "Male", "2" = "Female")
#Social class: 1=Non manual 2=skilled manual 88=Army
slsr_table1$socat <- recode_factor(slsr_table1$socat, "1" = "Non-manual work", "2" = "Manual work", "88" = "Army") 
#Educational attainment: 0=no formal education 1=primary 2=lower secondary 3=upper secondary 4=post secondary non tertiary 5=first stage of tertiary 6=secondary stage of tertiary
slsr_table1$educat <- recode_factor(slsr_table1$educat, "0" = "No formal education", "1" = "Primary", "2" = "Secondary", "3" = "Secondary", "4" = "Post-secondary", "5" = "Post-secondary", "6" = "Post-secondary") 

#Depression 1=No, 2=Yes
slsr_table1$rfpdep <- recode_factor(slsr_table1$rfpdep, "1" = "No", "2" = "Yes")
#Barthel index (1=">=15"  2="<15")
slsr_table1$bi0.25 <- recode_factor(slsr_table1$bi0.25, "1" = "≥15", "2" = "<15")
#Frenchey score 1=inactive (0-15), 2=moderate (16-30), 3=active (31-45)
#pre-coded
#Recurrence 1=No, 2=Yes
slsr_table1$anstrk0.25 <- recode_factor(slsr_table1$anstrk0.25, "1" = "No", "2" = "Yes")

#OCSP 1=TACI 2=PACI 3=POCI 4=LACI 5=Infarct unspecified 6=PICH 7=SAH 8=Unclassified 9=Unknown
slsr_table1$subtype <- recode_factor(slsr_table1$subtype, "1" = "TACI", "2" = "PACI", "3" = "POCI", "4" = "LACI", "5" = "Other/unknown", "6" = "Primary intracerebral haemorrhage", "7" = "Subarachnoid haemorrhage", "8" = "Other/unknown", "9" = "Other/unknown")
#TOAST 6-subtypes 1=LAA 2=CE 3=SVO 4=OTH 5=UND 6=CONC 77=PICH 88=SAH
slsr_table1$mtaet6 <- recode_factor(slsr_table1$mtaet6, "1" = "Large‐artery atherosclerosis", "2" = "Cardioembolism", "3" = "Small vessel occlusion ", "4" = "Other/unknown", "5" = "Other/unknown", "6" = "Other/unknown", "77" = "Primary intracerebral haemorrhage", "88" = "Subarachnoid haemorrhage")


#Make nice labels for the table: 
label(slsr_table1$sex)       <- "Sex"
label(slsr_table1$age)       <- "Age"
label(slsr_table1$eth6cat)       <- "Ethnic category"
label(slsr_table1$socat)       <- "Socioeconomic category"
label(slsr_table1$educat)       <- "Educational attainment"

label(slsr_table1$rfpdep)       <- "Depression"
label(slsr_table1$bi0.25)       <- "Barthel score"
label(slsr_table1$frcat0.25)    <- "Frenchay Activity Index"
label(slsr_table1$anstrk0.25)    <- "Recurrence"

label(slsr_table1$subtype)   <- "OSCP classification"
label(slsr_table1$mtaet6) <- "Modified TOAST 6-class (collected since 1999)"


#Create table1 
table1(~ sex + age + eth6cat + socat + educat + rfpdep + bi0.25 + frcat0.25 + anstrk0.25  + subtype + mtaet6| cog0.25, overall = F, data=slsr_table1)

#note: possibly will have to add aggregated stroke subtype (haemorrhage/infarct/SAH). Currently variable strksub has
#a very high  degree of missingness, but could fill it in with data from TOAST/OSCP if needed 

## -------------------------------------------------------------------------------------------------------------------------
#2.2 Prevalence of cognitive impairment after stroke 
#Age-adjusted? 

#Prevalence of cognitive impairment at 7 days, 3 months, 5 years, and 15 years 
#cog 1=intact 2=impaired 


slsr %>% summarise (cog0n = sum(cog == 2, na.rm = TRUE), n0 = sum(!is.na(dtint)), cog0p = cog0n/n0,
                    cog0.25n = sum(cog0.25 == 2, na.rm = TRUE), n0.25 = sum(!is.na(dtint0.25)), cog0.25p = cog0.25n/n0.25,
                    cog1n = sum(cog1 == 2, na.rm = TRUE), n1 = sum(!is.na(dtint1)), cog1p = cog1n/n1,
                    cog5n = sum(cog5 == 2, na.rm = TRUE), n5 = sum(!is.na(dtint5)), cog5p = cog5n/n5
                    ) %>% 
  kable(caption = "Table 1: Prevalence of cognitive impairment") %>% kable_styling(bootstrap_options = "striped", full_width = FALSE)


#Age-standardised prevalence

#create age categories 
slsr <- slsr %>% mutate (age_cat = case_when(
                  age < 65 ~ "under 65",
                  age >= 65 & age <75 ~ "65-74", 
                  age >=75 & age < 85 ~ "75-84",
                  age>= 85 ~ "85+"
))

slsr$age_cat <- factor(slsr$age_cat, levels = c("under 65", "65-74", "75-84", "85+"))

#create a summary df with nCI by age and sex 
 
slsr_age <- slsr %>% group_by(age_cat) %>% summarise (cog0n = sum(cog == 2, na.rm = TRUE), n0 = sum(!is.na(dtint)), cog0p = cog0n/n0,
                                               cog0.25n = sum(cog0.25 == 2, na.rm = TRUE), n0.25 = sum(!is.na(dtint0.25)), cog0.25p = cog0.25n/n0.25,
                                               cog1n = sum(cog1 == 2, na.rm = TRUE), n1 = sum(!is.na(dtint1)), cog1p = cog1n/n1,
                                               cog5n = sum(cog5 == 2, na.rm = TRUE), n5 = sum(!is.na(dtint5)), cog5p = cog5n/n5)

slsr_age


#load European standard population data according to our age categories 
esp_age_cat <- read.csv("/Users/Zuzanna_Bien/Desktop/ACF Public Health/Cognitive impairment in stroke/SLSR-CI/SLSR_raw_data/esp_age_cat.csv")
slsr_age$esp <- esp_age_cat$n
slsr_age$esp_p <- esp_age_cat$p

slsr_age %>% mutate (age_s_cog0p = (cog0p * esp)/100000,
                     age_s_cog0.25p = (cog0.25p * esp)/100000,
                     age_s_cog1p = (cog1p * esp)/100000,
                     age_s_cog5p = (cog5p * esp)/100000) %>%
            summarise(age_s_cog0p = mean(age_s_cog0p), age_s_cog0.25p = mean(age_s_cog0.25p), age_s_cog1p = mean(age_s_cog1p), age_s_cog5p = mean(age_s_cog5p) ) 
         

#

#Prevalence ratios (PRs) between patient groups were used for comparison. 
#All estimates were applied to the standard European population using the direct method with 4 age groups
#0 to 64, 65 to 74, 75 to 84, and 85+. Ninety-five percent confidence intervals of age-standardized rates and ratios 
#were calculated using the percentile bootstrap technique with 10 000 replications.
            
#Age-standardised only for *comparing* prevalence between groups. 

## -------------------------------------------------------------------------------------------------------------------------

#Sankey diagram 
library(ggsankey)
library(viridis)
            
            
#NB survival status sstat 0=alive (no record of death) 1=dead **Data library says 2=dead but this is incorrect**
            
            
test <- slsr %>%
  select(id, surv, sstat, dtint0.25, dtint1, dtint5, cog, cog0.25, cog1, cog5) %>% 
  mutate(cog = case_when(
    cog == 1 ~ "No PSCI", 
    cog == 2 ~ "PSCI"),
  
  cog0.25 = case_when(
    cog0.25 == 1 ~ "No PSCI", 
    cog0.25 == 2 ~ "PSCI",
    is.na(dtint0.25) & surv < 0.5 ~ "Dead"), #label those who died before 3m interview as dead),
  
  cog1 = case_when(
    cog1 == 1 ~ "No PSCI", 
    cog1 == 2 ~ "PSCI",
    surv < 1.5 & is.na(dtint1) ~ "Dead"),
    
  cog5 = case_when(
      cog5 == 1 ~ "No PSCI", 
      cog5 == 2 ~ "PSCI",
      is.na(dtint5) & surv < 5.5 ~ "Dead")
  
  ) %>% 
  rename ("7 days" = cog, "3 months" = cog0.25, "1 year" = cog1, "5 years" = cog5) %>% 
  ggsankey::make_long("7 days", "3 months", "1 year", "5 years")

ggplot(test, aes(x = x, next_x = next_x, 
                node = node, next_node = next_node, 
                fill = factor(node), 
                label = node
                #label = paste0(node," n=", n)
  geom_sankey(flow.alpha = 0.5, node.color = "grey50") +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  #geom_sankey_text(aes(label = count), size = 3.5, vjust = -1.5, check_overlap = TRUE) +
  scale_fill_viridis_d(alpha = 0.5) +
  theme_sankey(base_size = 14) +
  labs(x = "Follow-up time") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5), 
        axis.title = element_text(size=14))
            
#export image 
ggsave("sankey_deaths.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#we need the following info in the data-frame - 