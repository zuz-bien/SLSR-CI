#1. COGNITIVE IMPAIRMENT POST-STROKE: ANALYSIS  
#Table of contents:
#1. Table 1
#2 Prevalence of cognitive impairment after stroke 
#3. Incidence 
#4. Recovery 
#5. Sankey diagram 
#6. Survival 
#7. Risk factors
#8. Mixed effects model (trajectory) 
#9. Predictors of recovery


## -------------------------------------------------------------------------------------------------------------------------
#1 Table 1 


library(tidyverse)
library(scales)
library(kableExtra)
library(lubridate)
library(table1)
library(boot)
library(gridExtra)
library(survival)
library(survminer)
library(broom)

slsr <- read.csv("/Users/Zuzanna_Bien/Desktop/ACF Public Health/Cognitive impairment in stroke/SLSR-CI/SLSR_raw_data/slsr(pre_2020).csv")

# create a dataframe with only people who have cognitive scores (table1 does not accept NAs in the classifying variable)
slsr_table1 <- slsr %>% filter (!is.na(cog0.25)) %>% 
  #demographic
  mutate(frcat0.25 = cut(frtot0.25, breaks = c(0, 14, 30, 45), labels = c("Inactive (0-14)", "Moderate (15-30)", "Active (31-45)"))) %>%
  mutate(anstrk0.25 = ifelse(anstrk0.25 == 3, NA, anstrk0.25)) %>% 
  #1=TACI 2=PACI 3=POCI 4=LACI 5=Infarct unspecified 6=PICH 7=SAH 8=Unclassified 9=Unknown
  mutate(strk_cat = case_when(
    subtype == 1 ~ "Ischaemic stroke", 
    subtype == 2 ~ "Ischaemic stroke", 
    subtype == 3 ~ "Ischaemic stroke", 
    subtype == 4 ~ "Ischaemic stroke", 
    subtype == 5 ~ "Ischaemic stroke", 
    subtype == 6 ~ "Intracerebral haemorrhage", 
    subtype == 7 ~ "Subarachnoid haemorrhage",
    subtype == 8 ~ NA,
    subtype == 9 ~ NA)) %>%
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
          mtaet6 = as.factor(mtaet6),
          strk_cat = as.factor(strk_cat)
  ) 

#Recode factors:
#Cognitive status: 1=cognitively intact (8-10 AMT and 24+ MMSE) 2=cognitively impaired (0-7 AMT and <24 MMSE)
slsr_table1$cog0.25 <- recode_factor(slsr_table1$cog0.25, "1" = "Cognitively intact", "2" = "Cognitively impaired")
#Ethnic category: 1=White 2=Black Carribean 3=Black African 4=Black Other 5=Other 99= Unknown/not stated
slsr_table1$eth6cat <- recode_factor(slsr_table1$eth6cat, "1" = "White", "2" = "Black Carribean", "3" = "Black African", "4" = "Black other", "5" = "Other", "99" = "Unknown") 
#Sex: 1=Male 2=Female
slsr_table1$sex <- recode_factor(slsr_table1$sex, "1" = "Male", "2" = "Female")
#Social class: 1=Non manual 2=skilled manual 88=Army
slsr_table1$socat <- recode_factor(slsr_table1$socat, "1" = "Non-manual work", "2" = "Manual work", "88" = "Non-manual work") 
#Educational attainment: 0=no formal education 1=primary 2=lower secondary 3=upper secondary 4=post secondary non tertiary 5=first stage of tertiary 6=secondary stage of tertiary
slsr_table1$educat <- recode_factor(slsr_table1$educat, "0" = "Primary or less", "1" = "Primary or less", "2" = "Secondary", "3" = "Secondary", "4" = "Tertiary", "5" = "Tertiary", "6" = "Tertiary") 

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

label(slsr_table1$strk_cat) <- "Stroke category"
label(slsr_table1$subtype)   <- "OSCP classification"
label(slsr_table1$mtaet6) <- "Modified TOAST 6-class (collected since 1999)"


#Create table1 
table1 <- table1(~ sex + age + eth6cat + socat + educat + rfpdep + bi0.25 + frcat0.25 + anstrk0.25  + strk_cat + mtaet6| cog0.25, overall = F, data=slsr_table1)
write.csv(table1(~ sex + age + eth6cat + socat + educat + rfpdep + bi0.25 + frcat0.25 + anstrk0.25 + subtype + mtaet6 | cog0.25, overall = F, data=slsr_table1), "table1.csv")

#Add p-values

#testing for normality
shapiro.test(slsr_table1$age[slsr_table1$cog0.25 == "Cognitively impaired"]) 
#shapiro test indicates age is not normally distributed = therefore we will use Wilcox instead of t-test

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform Wilcox test (as they are not normally distributed)
    print(str(y))
    p <- wilcox.test(y ~ g)$p.value
    print(c("is numeric", p))
  } else {
    # For categorical variables, perform a chi-squared test of independence
    print(str(y))
    p <- chisq.test(table(y, g))$p.value
    print(c("is categorical", p))
  }
  # Format the p-value, using an HTML entity (&lt) for the less-than sign (<)
  # The initial empty string places the output on the line below the variable label.
  c("", format.pval(p, digits=3, eps=0.001))
}

table1(~ sex + age + eth6cat + socat + educat + rfpdep + bi0.25 + frcat0.25 + anstrk0.25  + strk_cat + mtaet6| cog0.25,
       data=slsr_table1, 
       overall = F,  
       extra.col=list(`P-value`=pvalue))


## -------------------------------------------------------------------------------------------------------------------------
#2 Prevalence of cognitive impairment after stroke 

#2.1 Calculate crude prevalence rates of cognitive impairment at 7 days, 3 months, 5 years, and 15 years 
#cog 1=intact 2=impaired 

#cog0n = number of cognitively impaired at initial assessment
#cog0p = proportion of cognitively impaired at initial assessment, of those who completed the assessment

crude_rate <- slsr %>% summarise (cog0n = sum(cog == 2, na.rm = TRUE), n0 = sum(!is.na(dtint)), cog0p = round(cog0n*100/n0,2),
                                  cog0.25n = sum(cog0.25 == 2, na.rm = TRUE), n0.25 = sum(!is.na(dtint0.25)), cog0.25p = round(cog0.25n*100/n0.25,2),
                                  cog1n = sum(cog1 == 2, na.rm = TRUE), n1 = sum(!is.na(dtint1)), cog1p = round(cog1n*100/n1,2),
                                  cog5n = sum(cog5 == 2, na.rm = TRUE), n5 = sum(!is.na(dtint5)), cog5p = round(cog5n*100/n5,2)
) 

write.csv(crude_rate, "crude_rate.csv")
#note you can also use bootstrapping for the confidence intervals on the crude rate (idk where the code for this has gone)

#2.2 Prepare the data-frame with the shifting age-categories 
#NB I've created shifting categories to account for the fact that patients age

slsr_esp <- slsr %>% mutate(age_cat_esp = case_when(
  age <5 ~ "0-4 years", 
  age >=5 & age <10 ~ "5-9 years", 
  age >=10 & age <15 ~ "10-14 years", 
  age >=15 & age <20 ~ "15-19 years", 
  age >=20 & age <25 ~ "20-24 years", 
  age >=25 & age <30  ~ "25-29 years", 
  age >=30 & age <35 ~ "30-34 years", 
  age >=35 & age <40 ~ "35-39 years", 
  age >= 40 & age <45 ~ "40-44 years", 
  age >= 45 & age <50 ~ "45-49 years", 
  age >= 50 & age <55 ~ "50-54 years", 
  age >= 55 & age <60 ~ "55-59 years", 
  age >= 60 & age <65 ~ "60-64 years", 
  age >= 65 & age <70 ~ "65-69 years", 
  age >= 70 & age <75 ~ "70-74 years", 
  age >= 75 & age <80 ~ "75-79 years", 
  age >= 80 & age <85 ~ "80-84 years", 
  age >= 85 & age <90 ~ "85-89 years", 
  age >= 90 ~ "90plus years"), 
  
  age_cat_esp0.25 = case_when(
    age+0.25 <5 ~ "0-4 years", 
    age+0.25 >=5 & age+0.25 <10 ~ "5-9 years", 
    age+0.25 >=10 & age+0.25 <15 ~ "10-14 years", 
    age+0.25 >=15 & age+0.25 <20 ~ "15-19 years", 
    age+0.25 >=20 & age+0.25 <25 ~ "20-24 years", 
    age+0.25 >=25 & age+0.25 <30  ~ "25-29 years", 
    age+0.25 >=30 & age+0.25 <35 ~ "30-34 years", 
    age+0.25 >=35 & age+0.25 <40 ~ "35-39 years", 
    age+0.25 >= 40 & age+0.25 <45 ~ "40-44 years", 
    age+0.25 >= 45 & age+0.25 <50 ~ "45-49 years", 
    age+0.25 >= 50 & age+0.25 <55 ~ "50-54 years", 
    age+0.25 >= 55 & age+0.25 <60 ~ "55-59 years", 
    age+0.25 >= 60 & age+0.25 <65 ~ "60-64 years", 
    age+0.25 >= 65 & age+0.25 <70 ~ "65-69 years", 
    age+0.25 >= 70 & age+0.25 <75 ~ "70-74 years", 
    age+0.25 >= 75 & age+0.25 <80 ~ "75-79 years", 
    age+0.25 >= 80 & age+0.25 <85 ~ "80-84 years", 
    age+0.25 >= 85 & age+0.25 <90 ~ "85-89 years", 
    age+0.25 >= 90 ~ "90plus years"), 
  
  age_cat_esp1 = case_when(
    age+1 <5 ~ "0-4 years", 
    age+1 >=5 & age+1 <10 ~ "5-9 years", 
    age+1 >=10 & age+1 <15 ~ "10-14 years", 
    age+1 >=15 & age+1 <20 ~ "15-19 years", 
    age+1 >=20 & age+1 <25 ~ "20-24 years", 
    age+1 >=25 & age+1 <30  ~ "25-29 years", 
    age+1 >=30 & age+1 <35 ~ "30-34 years", 
    age+1 >=35 & age+1 <40 ~ "35-39 years", 
    age+1 >= 40 & age+1 <45 ~ "40-44 years", 
    age+1 >= 45 & age+1 <50 ~ "45-49 years", 
    age+1 >= 50 & age+1 <55 ~ "50-54 years", 
    age+1 >= 55 & age+1 <60 ~ "55-59 years", 
    age+1 >= 60 & age+1 <65 ~ "60-64 years", 
    age+1 >= 65 & age+1 <70 ~ "65-69 years", 
    age+1 >= 70 & age+1 <75 ~ "70-74 years", 
    age+1 >= 75 & age+1 <80 ~ "75-79 years", 
    age+1 >= 80 & age+1 <85 ~ "80-84 years", 
    age+1 >= 85 & age+1 <90 ~ "85-89 years", 
    age+1 >= 90 ~ "90plus years"), 
  
  
  age_cat_esp5 = case_when(
    age+5 <5 ~ "0-4 years", 
    age+5 >=5 & age+5 <10 ~ "5-9 years", 
    age+5 >=10 & age+5 <15 ~ "10-14 years", 
    age+5 >=15 & age+5 <20 ~ "15-19 years", 
    age+5 >=20 & age+5 <25 ~ "20-24 years", 
    age+5 >=25 & age+5 <30  ~ "25-29 years", 
    age+5 >=30 & age+5 <35 ~ "30-34 years", 
    age+5 >=35 & age+5 <40 ~ "35-39 years", 
    age+5 >= 40 & age+5 <45 ~ "40-44 years", 
    age+5 >= 45 & age+5 <50 ~ "45-49 years", 
    age+5 >= 50 & age+5 <55 ~ "50-54 years", 
    age+5 >= 55 & age+5 <60 ~ "55-59 years", 
    age+5 >= 60 & age+5 <65 ~ "60-64 years", 
    age+5 >= 65 & age+5 <70 ~ "65-69 years", 
    age+5 >= 70 & age+5 <75 ~ "70-74 years", 
    age+5 >= 75 & age+5 <80 ~ "75-79 years", 
    age+5 >= 80 & age+5 <85 ~ "80-84 years", 
    age+5 >= 85 & age+5 <90 ~ "85-89 years", 
    age+5 >= 90 ~ "90plus years"))

slsr_age_0<- slsr_esp %>% group_by(age_cat_esp) %>% summarise (cog0n = sum(cog == 2, na.rm = TRUE), n0 = sum(!is.na(dtint)), cog0p = cog0n/n0)
slsr_age_0.25 <- slsr_esp %>% group_by(age_cat_esp0.25) %>% summarise (cog0.25n = sum(cog0.25 == 2, na.rm = TRUE), n0.25 = sum(!is.na(dtint0.25)), cog0.25p = cog0.25n/n0.25) %>% rename ("age_cat_esp" = "age_cat_esp0.25")
slsr_age_1 <- slsr_esp %>% group_by(age_cat_esp1) %>% summarise (cog1n = sum(cog1 == 2, na.rm = TRUE), n1 = sum(!is.na(dtint1)), cog1p = cog1n/n1)%>% rename ("age_cat_esp" = "age_cat_esp1")
slsr_age_5 <- slsr_esp %>% group_by(age_cat_esp5) %>% summarise (cog5n = sum(cog5 == 2, na.rm = TRUE), n5 = sum(!is.na(dtint5)), cog5p = cog5n/n5) %>% rename ("age_cat_esp" = "age_cat_esp5")

# To merge the above, create a list of data frames
list_of_dfs <- list(slsr_age_0, slsr_age_0.25, slsr_age_1, slsr_age_5)

# Use reduce to recursively merge the data frames
slsr_age_esp <- reduce(list_of_dfs, ~full_join(.x, .y, by = "age_cat_esp"))

#Add ESP weights to this
esp<- read.csv("/Users/Zuzanna_Bien/Desktop/ACF Public Health/Cognitive impairment in stroke/SLSR-CI/SLSR_raw_data/european_standard_population.csv", header = TRUE)
esp <- esp %>% rename ("age_cat_esp" = "age", "esp" = "n") %>% mutate (esp_w = esp/sum(esp))

slsr_age_esp <- merge(slsr_age_esp, esp, by = "age_cat_esp", all = TRUE)
slsr_age_esp[is.na(slsr_age_esp)] <- 0

#2.3 Calculate age-standardised prevalence with bootstrapping intervals 

# Set the number of bootstrap replications
n_replications <- 10000
indices <- 1:nrow(slsr_age_esp)

#-------------------
##Initial assessment 

#CIs for ESP

calc_standardized_rate <- function(data, indices, cases, total, esp_weights) {
  # Subset the data based on the indices
  df <- data[indices, ]
  
  df$rate <- df[[cases]] / df[[total]]
  df$weighted_rate <- df$rate * df[[esp_weights]]
  
  return(sum(df$weighted_rate, na.rm = TRUE))
}

calc_standardized_rate(slsr_age_esp, indices, "cog0n", "n0", "esp_w")

boot_results <- boot(data = slsr_age_esp, 
                     statistic = calc_standardized_rate, 
                     R = n_replications,
                     cases = "cog0n",
                     total = "n0",
                     esp_weights = "esp_w")

boot_ci <- boot.ci(boot.out = boot_results, type = "perc")
print(boot_ci)

#-------------------
###3-month follow-up
#Calculate 95% CIs for age-standardised rate 
calc_standardized_rate(slsr_age_esp, indices, "cog0.25n", "n0.25", "esp_w")

boot_results <- boot(data = slsr_age_esp, 
                     statistic = calc_standardized_rate, 
                     R = n_replications,
                     cases = "cog0.25n",
                     total = "n0.25",
                     esp_weights = "esp_w")

boot_ci <- boot.ci(boot.out = boot_results, type = "perc")
print(boot_ci)

#-------------------
###1-year follow-up 
#Calculate 95% CIs for age-standardised rate 
calc_standardized_rate(slsr_age_esp, indices, "cog1n", "n1", "esp_w")

boot_results <- boot(data = slsr_age_esp, 
                     statistic = calc_standardized_rate, 
                     R = n_replications,
                     cases = "cog1n",
                     total = "n1",
                     esp_weights = "esp_w")

boot_ci <- boot.ci(boot.out = boot_results, type = "perc")
print(boot_ci)


#-------------------
###5-year follow-up

#Calculate CIs for age-standardised rate 
calc_standardized_rate(slsr_age_esp, indices, "cog5n", "n5", "esp_w")

boot_results <- boot(data = slsr_age_esp, 
                     statistic = calc_standardized_rate, 
                     R = n_replications,
                     cases = "cog5n",
                     total = "n5",
                     esp_weights = "esp_w")

boot_ci <- boot.ci(boot.out = boot_results, type = "perc")
print(boot_ci)

## -------------------------------------------------------------------------------------------------------------------------
#3. Incidence 

#Create new variables for 'dead at 3 months', 'dead at 1 year' etc. 
#slsr <- slsr %>% mutate (sstat0.25 = ifelse(surv <0.5 & is.na(dtint0.25) == TRUE, 1, 0),
#                         sstat1 = ifelse(surv <1.5 & is.na(dtint1) == TRUE, 1, 0),
#                         sstat5 = ifelse(surv <5.5 & is.na(dtint5) == TRUE, 1, 0))

incidence0.25 = sum(slsr$cog0.25 == 2 & slsr$cog == 1, na.rm = TRUE)/sum(!is.na(slsr$cog0.25))  
incidence1 = sum(slsr$cog1 == 2 & slsr$cog0.25 == 1, na.rm = TRUE)/sum(!is.na(slsr$cog1))  
incidence5 = sum(slsr$cog5 == 2 & slsr$cog1 == 1, na.rm = TRUE)/sum(!is.na(slsr$cog5))  


# Calculate 95% confidence intervals with bootstrapping

# Define a function to calculate incidence
calc_incidence <- function(data, indices, cog_before, cog_after) {
  # Sample data with replacement
  d <- data[indices,] 
  
  # Calculate incidence
  incidence <- sum(d[[cog_after]] == 2 & d[[cog_before]] == 1, na.rm = TRUE)/sum(!is.na(d[[cog_after]]))
  str
  return(incidence)
}

# Run bootstrap for each time point
boot0.25 <- boot(data = slsr, statistic = calc_incidence, R = 10000, cog_before = "cog", cog_after = "cog0.25")
boot1 <- boot(data = slsr, statistic = calc_incidence, R = 10000, cog_before = "cog0.25", cog_after = "cog1")
boot5 <- boot(data = slsr, statistic = calc_incidence, R = 10000, cog_before = "cog1", cog_after = "cog5")

# Get the 95% confidence intervals
ci0.25 <- boot.ci(boot.out = boot0.25, type = "perc")
ci1 <- boot.ci(boot.out = boot1, type = "perc")
ci5 <- boot.ci(boot.out = boot5, type = "perc")

#Print the incidence & CIs
print(paste0("3-month incidence: ", round(mean(boot0.25$t)*100, 2), 
             "% (95% CI: ", round(ci0.25$perc[4]*100, 2), 
             "% - ", round(ci0.25$perc[5]*100, 2), "%)"))
print(paste0("1-year incidence: ", round(mean(boot1$t)*100, 2), 
             "% (95% CI: ", round(ci1$perc[4]*100, 2), 
             "% - ", round(ci1$perc[5]*100, 2), "%)"))
print(paste0("5-year incidence: ", round(mean(boot5$t)*100, 2), 
             "% (95% CI: ", round(ci5$perc[4]*100, 2), 
             "% - ", round(ci5$perc[5]*100, 2), "%)"))




## -------------------------------------------------------------------------------------------------------------------------
#4. Recovery  



# Calculate 95% confidence intervals with bootstrapping

# Define a function to calculate incidence
calc_recovery <- function(data, indices, cog_before, cog_after) {
  # Sample data with replacement
  d <- data[indices,] 
  
  # Calculate incidence
  recovery <- sum(d[[cog_after]] == 1 & d[[cog_before]] == 2, na.rm = TRUE)/sum(!is.na(d[[cog_after]]))
  str
  return(recovery)
}

# Run bootstrap for each time point
boot0.25 <- boot(data = slsr, statistic = calc_recovery, R = 10000, cog_before = "cog", cog_after = "cog0.25")
boot1 <- boot(data = slsr, statistic = calc_recovery, R = 10000, cog_before = "cog0.25", cog_after = "cog1")
boot5 <- boot(data = slsr, statistic = calc_recovery, R = 10000, cog_before = "cog1", cog_after = "cog5")

# Get the 95% confidence intervals
ci0.25 <- boot.ci(boot.out = boot0.25, type = "perc")
ci1 <- boot.ci(boot.out = boot1, type = "perc")
ci5 <- boot.ci(boot.out = boot5, type = "perc")

#Print the incidence & CIs
print(paste0("3-month recovery: ", round(mean(boot0.25$t)*100, 2), 
             "% (95% CI: ", round(ci0.25$perc[4]*100, 2), 
             "% - ", round(ci0.25$perc[5]*100, 2), "%)"))
print(paste0("1-year recovery: ", round(mean(boot1$t)*100, 2), 
             "% (95% CI: ", round(ci1$perc[4]*100, 2), 
             "% - ", round(ci1$perc[5]*100, 2), "%)"))
print(paste0("5-year recovery: ", round(mean(boot5$t)*100, 2), 
             "% (95% CI: ", round(ci5$perc[4]*100, 2), 
             "% - ", round(ci5$perc[5]*100, 2), "%)"))




## -------------------------------------------------------------------------------------------------------------------------
#5. Sankey diagram 
library(ggsankey)
library(viridis)

slsr_full <- read.csv("/Users/Zuzanna_Bien/Desktop/ACF Public Health/Cognitive impairment in stroke/SLSR-CI/SLSR_raw_data/slsr(including those who died before 3 months).csv")



          
#NB survival status sstat 0=alive (no record of death) 1=dead **Data library says 2=dead but this is incorrect**
          
test <- slsr_full %>%
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
                label = node)) +
                #label = paste0(node," n=", n)
  geom_sankey(flow.alpha = 0.5, node.color = "grey50") +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  #geom_sankey_text(aes(label = count), size = 3.5, vjust = -1.5, check_overlap = TRUE) +
  scale_fill_viridis_d(alpha = 0.5) +
  theme_sankey(base_size = 14) +
  labs(x = "Follow-up time") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5), 
        axis.title = element_text(size=12, colour = "gray40"))
            
#export image 
ggsave("sankey_deaths.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')
ggsave("sankey_deaths.jpeg", units="in", width=5, height=4, dpi=300)


##----------------------------------------------------------------------------------------------------
#6. Survival status 

slsr <- slsr %>% mutate (
  surv_time = (dtdodpi - dtstrk)/365.25, 
  surv_status = case_when(
    sstat == 0 ~ 0, 
    surv_time >= 5 ~ 0,    #(alive at 5 years)
    surv_time < 5 ~ 1  # (dead within 5 years)
  ))




#sum(is.na(slsr$surv_time) & slsr$surv_status == 1)  #n = 0 - so everyone who is dead, has a survival time
#sum(is.na(slsr$surv_time) & slsr$surv_status == 0)  #n = 1891 have no survival time but they are alive - input 5 years + 1 day 

#Input survival time as 5 for those still alive at 5 years 
slsr <- slsr %>% mutate(surv_time = ifelse(
  is.na(surv_time) & (!is.na(surv_status) & surv_status == 0), 5, surv_time))                        
                        

#Cox model
surv <- coxph(Surv(surv_time, surv_status) ~ cog0.25 + age+ sex, data = slsr)
summary (surv)

#Kaplan-Meier curve 
fit <- survfit(Surv(surv_time, surv_status) ~ cog0.25, data = slsr)
ggsurvplot(
  fit,                             # survival curve object
  data = slsr,                     # dataset
  pval = TRUE,                     # adds p-value
  conf.int = TRUE,                 # adds confidence intervals
  xlim = c(0, 5),            # limits x-axis from 0 to 1826.25
  palette = c("#E7B800", "#2E9FDF"),  # custom color palette
  xlab = "Time in years",           # label of the x-axis
  ylab = "Survival probability",   # label of the y-axis
  legend.labs =                    # customize legend labels
    c("No PSCI", "PSCI"),
  break.x.by = 1)

##----------------------------------------------------------------------------------------------------
#7. Risk factors 

# create a dataframe with only people who have cognitive scores (table1 does not accept NAs in the classifying variable)
slsr_glm <- slsr %>% 
  #demographic
  mutate(frcat0.25 = cut(frtot0.25, breaks = c(0, 14, 30, 45), labels = c("Inactive (0-14)", "Moderate (15-30)", "Active (31-45)"))) %>%
  mutate(anstrk0.25 = ifelse(anstrk0.25 == 3, NA, anstrk0.25)) %>% 
  mutate(eth6cat = ifelse(eth6cat == "99", NA, eth6cat)) %>%
  mutate(q3a_ri = ifelse(q3a_ri == "3", NA, q3a_ri)) %>%
  mutate(thromby_ip = ifelse(is.na(thromby_ip) | thromby_ip == "0" | thromby_ip == "3", "1", thromby_ip)) %>%
  mutate (age_cat = case_when(
    age < 65 ~ "under 65",
    age >= 65 & age <75 ~ "65-74", 
    age >=75 & age < 85 ~ "75-84",
    age>= 85 ~ "85+")) %>%
  #mutate(thromby_ip = ifelse(is.na(thromby_ip), 1, thromby_ip)) %>% #find out from Hatem what is up with this variable
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
          mtaet6 = as.factor(mtaet6),
          handed = as.factor(handed), 
          q3a_ri = as.factor(q3a_ri), 
          thromby_ip = as.factor(thromby_ip),
  ) 

#Recode factors:
slsr_glm$age_cat <- factor(slsr_glm$age_cat, levels = c("under 65","65-74", "75-84", "85+"))
#Cognitive status: 1=cognitively intact (8-10 AMT and 24+ MMSE) 2=cognitively impaired (0-7 AMT and <24 MMSE)
slsr_glm$cog0.25 <- recode_factor(slsr_glm$cog0.25, "1" = "Cognitively intact", "2" = "Cognitively impaired")
#Ethnic category: 1=White 2=Black Carribean 3=Black African 4=Black Other 5=Other 99= Unknown/not stated
slsr_glm$eth6cat <- recode_factor(slsr_glm$eth6cat, "1" = "White", "2" = "Black Carribean", "3" = "Black African", "4" = "Black other", "5" = "Other") 
#Sex: 1=Male 2=Female
slsr_glm$sex <- recode_factor(slsr_glm$sex, "1" = "Male", "2" = "Female")
#Social class: 1=Non manual 2=skilled manual 88=Army
slsr_glm$socat <- recode_factor(slsr_glm$socat, "1" = "Non-manual work", "2" = "Manual work", "88" = "Army") 
#Educational attainment: 0=no formal education 1=primary 2=lower secondary 3=upper secondary 4=post secondary non tertiary 5=first stage of tertiary 6=secondary stage of tertiary
slsr_glm$educat <- recode_factor(slsr_glm$educat, "0" = "Primary or less", "1" = "Primary or less", "2" = "Secondary", "3" = "Secondary", "4" = "Tertiary", "5" = "Tertiary", "6" = "Tertiary") 

#Depression 1=No, 2=Yes
slsr_glm$rfpdep <- recode_factor(slsr_glm$rfpdep, "1" = "No", "2" = "Yes")
#Barthel index (1=">=15"  2="<15")
slsr_glm$bi0.25 <- recode_factor(slsr_glm$bi0.25, "1" = "≥15", "2" = "<15")
#Frenchey score 1=inactive (0-15), 2=moderate (16-30), 3=active (31-45)
#pre-coded
#Recurrence 1=No, 2=Yes
slsr_glm$anstrk0.25 <- recode_factor(slsr_glm$anstrk0.25, "1" = "No", "2" = "Yes")

#Handedness 1=Right 2=Left
slsr_glm$handed <- recode_factor(slsr_glm$handed, "1" = "Right", "2"= "Left" )

#OCSP 1=TACI 2=PACI 3=POCI 4=LACI 5=Infarct unspecified 6=PICH 7=SAH 8=Unclassified 9=Unknown
  slsr_glm$subtype <- recode_factor(slsr_glm$subtype, "1" = "TACI", "2" = "PACI", "3" = "POCI", "4" = "LACI", "5" = "Other/unknown", "6" = "Primary intracerebral haemorrhage", "7" = "Subarachnoid haemorrhage", "8" = "Other/unknown", "9" = "Other/unknown")
#TOAST 6-subtypes 1=LAA 2=CE 3=SVO 4=OTH 5=UND 6=CONC 77=PICH 88=SAH
slsr_glm$mtaet6 <- recode_factor(slsr_glm$mtaet6, "1" = "Large‐artery atherosclerosis", "2" = "Cardioembolism", "3" = "Small vessel occlusion ", "4" = "Other/unknown", "5" = "Other/unknown", "6" = "Other/unknown", "77" = "Primary intracerebral haemorrhage", "88" = "Subarachnoid haemorrhage")
#Thrombolysis  1=No, 2=Yes
slsr_glm$q3a_ri <- recode_factor(slsr_glm$q3a_ri, "1" = "No", "2" = "Yes") 
#Thrombectomy 1=No, 2=Yes
slsr_glm$thromby_ip <- recode_factor(slsr_glm$thromby_ip,  "1" = "No", "2" = "Yes") 

#Make nice labels for the table: 
label(slsr_glm$sex)       <- "Sex"
label(slsr_glm$age_cat)       <- "Age"
label(slsr_glm$eth6cat)       <- "Ethnic category"
label(slsr_glm$socat)       <- "Socioeconomic category"
label(slsr_glm$educat)       <- "Educational attainment"

label(slsr_glm$rfpdep)       <- "Depression"
label(slsr_glm$bi0.25)       <- "Barthel score"
label(slsr_glm$frcat0.25)    <- "Frenchay Activity Index"
label(slsr_glm$anstrk0.25)    <- "Recurrence"

label(slsr_glm$subtype)   <- "OSCP classification"
label(slsr_glm$mtaet6) <- "Modified TOAST 6-class (collected since 1999)"
label(slsr_glm$handed) <- "Handedness"

label(slsr_glm$q3a_ri) <- "Thrombolysis"
label(slsr_glm$thromby_ip) <- "Thrombectomy"


# glm model

glm.fit1 <- glm(cog0.25 ~  
                  age_cat +  
                  sex +
                  eth6cat + 
                  socat + 
                  educat + 
                  rfpdep +
                  #q3a_ri,
                  mtaet6, #thrombolysis
                  #thromby_ip, #thrombectomy 
                data = slsr_glm, family = binomial)

summary(glm.fit1)

#forest plot 
#tidy up the results 
tidy_results <- broom::tidy(glm.fit1)

#calculate odds ratios and confidence intervals 
tidy_results <- tidy_results %>%
  mutate(odds_ratio = exp(estimate),
         conf.low = exp(estimate - 1.96 * std.error),
         conf.high = exp(estimate + 1.96 * std.error),
         significant = case_when(
           p.value >=0.05 ~ "",
           p.value < 0.05 & p.value >= 0.01 ~ "*",
           p.value < 0.01 & p.value >= 0.001  ~ "**",
           p.value < 0.001 ~ "***"),
         or_ci = sprintf("%.2f (%.2f, %.2f)", odds_ratio, conf.low, conf.high)) %>% 
  filter (term != "(Intercept)") %>% 
  mutate (term = case_when(
    #term == "(Intercept)" ~ "Intercept",
    
    term == "age_cat65-74" ~ "Age 65-74",
    term == "age_cat75-84" ~ "Age 75-84",
    term == "age_cat85+" ~ "Age 85+",

    term == "sexFemale" ~ "Female sex",
    
    term == "eth6catBlack Carribean" ~ "Black Carribean ethnicity",
    term == "eth6catBlack African" ~ "Black African ethnicity",
    term == "eth6catBlack other" ~ "Black Other ethnicity",
    term == "eth6catOther" ~ "Other ethnicity",
    
    term == "socatManual work" ~ "Manual work",
    term == "educatPrimary" ~ "Primary education",
    term == "educatSecondary" ~ "Secondary education",
    term == "educatTertiary" ~ "Tertiary education",
    
    term == "mtaet6Cardioembolism" ~ "TOAST: Cardioembolic stroke",
    term == "mtaet6Small vessel occlusion " ~ "TOAST: Small vessel occlusion",
    term == "mtaet6Other/unknown" ~ "TOAST: Unknown",
    term == "mtaet6Primary intracerebral haemorrhage" ~ "TOAST: Primary ICH",
    term == "mtaet6Subarachnoid haemorrhage" ~ "TOAST: SAH", 
    #term == "q3a_riYes" ~ "Thrombolysis",
    term == "rfpdepYes" ~ "Pre-stroke depression"
  ))

#order variables correctly 
tidy_results$term <- factor(tidy_results$term, levels = rev(c(
  "Age 65-74", "Age 75-84", "Age 85+", "Female sex", 
  "Black Carribean ethnicity","Black African ethnicity","Black Other ethnicity","Other ethnicity",
  "Manual work", "Primary education", "Secondary education", "Tertiary education", "Pre-stroke depression",
  "TOAST: Cardioembolic stroke", "TOAST: Small vessel occlusion", "TOAST: Unknown", "TOAST: Primary ICH", "TOAST: SAH", 
  "Thrombolysis")
))

# Log-transformed odds 
forest_rf_log <- ggplot(tidy_results, aes(x = odds_ratio, y = term, xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 1, color = "grey10", linetype = "dashed") +
  geom_errorbarh(height = 0.2) +
  geom_point(size = 2) +
  #geom_text(aes(label = c(or_ci, significant), x = 1.1 * conf.high), hjust = 1.6) +
  geom_text(aes(label = significant, x = 1.1 * conf.high), hjust = -1.5) +
  #scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                #abels = scales::trans_format("log10", scales::math_format(.x))) +
  xlab("Odds Ratio (95% CI)") +
  ylab("") +
  theme_minimal()


# Create forest plot with asterisks for significant results and modified labels
forest_rf <- ggplot(tidy_results, aes(x = odds_ratio, y = term, xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 1, color = "grey", linetype = "dashed") +
  geom_errorbarh(height = 0.2) +
  geom_point(size = 2) +
  geom_text(aes(label = significant, x = 1.05 * conf.high), hjust = -0.3) +
  scale_y_discrete(labels = c(age = "Age at Diagnosis", sex = "Gender")) +
  xlab("Odds Ratio (95% CI)") +
  ylab("") +
  theme_minimal()

forest_rf

png("forest_rf.png", units="cm", width=17.1, height=17.1, res=400)
forest_rf 
dev.off()


#round values
write.csv(tidy_results %>% select (term, odds_ratio, conf.low, conf.high, p.value, significant), "risk_factors.csv")

##---------------------------------------------------------------------------------------------------------------------------------------------------
#8. Mixed effects model 
library(lme4)
library(lmerTest)


#1. Make data long

slsr_longer <- slsr %>% select(id, age, sex, ethcat, cog, cog0.25, cog1, cog5) %>% 
  pivot_longer(cols = c(cog, cog0.25, cog1, cog5), names_to = "timepoint", values_to = "cog") %>%
  mutate (cog = case_when(
    cog == 1 ~ 0, # 0 = no cognitive impairment
    cog == 2 ~ 1 #1 = cognitive impairment
  )) %>% mutate (sex = case_when(
    sex == 1 ~ 0, 
    sex == 2 ~ 1
  )) %>% mutate (age_cat = case_when(
    age < 65 ~ "under 65",
    age >= 65 & age <75 ~ "65-74", 
    age >=75 & age < 85 ~ "75-84",
    age>= 85 ~ "85+")) %>%
  mutate (ethcat = ifelse(ethcat == 4, NA, ethcat)) %>% 
  mutate (age_scaled = scale(age))

slsr_longer$timepoint <- as.factor(slsr_longer$timepoint)
slsr_longer$age_cat <- factor(slsr_longer$age_cat, levels = c("under 65","65-74", "75-84", "85+"))
slsr_longer$ethcat <- as.factor(slsr_longer$ethcat)

#2. Fit the model 
model <- glmer(cog ~ sex  + age_cat + ethcat + timepoint + (1|id), data=slsr_longer, family=binomial(link="logit"))

summary(model) #model not converging - not sure why 

#3. Fit the model to continuous scores 

slsr_longer2 <- slsr %>% select(id, age, sex, ethcat, educat, mtot, mtot0.25, mtot1, mtot5) %>% 
  pivot_longer(cols = c(mtot, mtot0.25, mtot1, mtot5), names_to = "timepoint", values_to = "mtot") %>%
  mutate (ethcat = ifelse(ethcat == 4, NA, ethcat))%>% 
  mutate (age_cat = case_when(
    age < 65 ~ "under 65",
    age >= 65 & age <75 ~ "65-74", 
    age >=75 & age < 85 ~ "75-84",
    age>= 85 ~ "85+")) 

#1=White 2=Black 3=Other 4=Unknown/not stated
slsr_longer2$ethcat <- as.factor(slsr_longer2$ethcat)
slsr_longer2$age_cat <- factor(slsr_longer2$age_cat, levels = c("under 65","65-74", "75-84", "85+"))
#Educational attainment: 0=no formal education 1=primary 2=lower secondary 3=upper secondary 4=post secondary non tertiary 5=first stage of tertiary 6=secondary stage of tertiary
slsr_longer2$educat <- recode_factor(slsr_longer2$educat, "0" = "Primary or less", "1" = "Primary or less", "2" = "Secondary", "3" = "Secondary", "4" = "Tertiary", "5" = "Tertiary", "6" = "Tertiary") 


model <- lmer(mtot ~ age_cat + sex + ethcat + educat + timepoint + (1|id), data=slsr_longer2) #converging fine
summary(model)

coef <- coef(summary(model <- lmer(mtot ~ age_cat + sex + ethcat + educat + timepoint + (1 |id), data=slsr_longer2)))
colnames(coef) <- c("estimate", "se", "df", "tval", "pval")
coef <- data.frame(coef)
coef %>% mutate(pval = format.pval(coef$pval, digits=3, eps=0.001))

##---------------------------------------------------------------------------------------------------------------------------------------------------
#9. Predictors of recovery
slsr <- read.csv("/Users/Zuzanna_Bien/Desktop/ACF Public Health/Cognitive impairment in stroke/SLSR-CI/SLSR_raw_data/slsr(pre_2020).csv", stringsAsFactors = TRUE)

#1=cognitively intact (8-10 AMT and 24+ MMSE) 2=cognitively impaired (0-7 AMT and <24 MMSE)

#cog_recov 0 = not recovered, 1 = recovered
#checked - captures all possibilities 
slsr_recovery <- slsr %>% filter (cog == 2) %>%
  mutate (
  cog_recov = case_when(
  cog == 2 & (cog0.25 == 1 | cog1 == 1) ~ 1, #
  cog == 2 & (cog0.25 == 2 & cog1 == 2) ~ 0, #
  cog == 2 & (is.na(cog0.25) & cog1 == 2) ~ 0, #
  cog == 2 & (cog0.25 == 2 & is.na(cog1)) ~ 0, #
  is.na(cog)  ~ NA,
  is.na(cog0.25) & is.na(cog1) ~ NA), 
  
  gcs = ifelse (glas_cs <13, 2, 1),
  
  age_cat = case_when(
    age < 65 ~ "under 65",
    age >= 65 & age <75 ~ "65-74", 
    age >=75 & age < 85 ~ "75-84",
    age>= 85 ~ "85+"), 
  
 ethcat = ifelse(ethcat == 4, NA, ethcat), 
 
 strk_cat = case_when(
   subtype == 1 ~ "Ischaemic stroke", 
   subtype == 2 ~ "Ischaemic stroke", 
   subtype == 3 ~ "Ischaemic stroke", 
   subtype == 4 ~ "Ischaemic stroke", 
   subtype == 5 ~ "Ischaemic stroke", 
   subtype == 6 ~ "Intracerebral haemorrhage", 
   subtype == 7 ~ "Subarachnoid haemorrhage",
   subtype == 8 ~ NA,
   subtype == 9 ~ NA))
    


slsr_recovery$age_cat <- factor(slsr_recovery$age_cat, levels = c("under 65","65-74", "75-84", "85+"))
#Sex: 1=Male 2=Female
slsr_recovery$sex <- recode_factor(slsr_recovery$sex, "1" = "Male", "2" = "Female")
#Educational attainment: 0=no formal education 1=primary 2=lower secondary 3=upper secondary 4=post secondary non tertiary 5=first stage of tertiary 6=secondary stage of tertiary
slsr_recovery$educat <- recode_factor(slsr_recovery$educat, "0" = "Primary or less", "1" = "Primary or less", "2" = "Secondary", "3" = "Secondary", "4" = "Tertiary", "5" = "Tertiary", "6" = "Tertiary") 
#1=White 2=Black 3=Other 4=Unknown/not stated
slsr_recovery$ethcat <- recode_factor(slsr_recovery$ethcat, "1" = "White", "2" = "Black", "3" = "Other")
#NIHSS 1=mild(<=4) 2=moderate(5-20) 3=severe(>20)
slsr_recovery$sex <- recode_factor(slsr_recovery$sex, "1" = "Mild", "2" = "Moderate", "3" = "Severe")
#Stroke type 1=infarct 2=haemorrhage
slsr_recovery$strk_cat <- factor(slsr_recovery$strk_cat, levels = c("Ischaemic stroke", "Intracerebral haemorrhage", "Subarachnoid haemorrhage"))
#Depression
slsr_recovery$rfpdep <- recode_factor(slsr_recovery$rfpdep, "1" = "No", "2" = "Yes")
#NIHSS score 
slsr_recovery$nihss <- recode_factor(slsr_recovery$nihss, "1" = "Mild", "2" = "Moderate", "3" = "Severe")


table(slsr_recovery$cog_recov) #353 not recovered, 334 recovered 
#view(slsr_recovery %>% select (id, cog, cog0.25, cog1, cog_recov))

#NB consider recoving those with cog = 2, cog0.25 = 1, and cog1 = 2 as not recovered (n = 56) 

#predictors of recovery
glm.fit2 <- glm(cog_recov ~  
                  age_cat +  
                  sex +
                  educat + 
                  ethcat + 
                  nihss + 
                  strk_cat + 
                  rfpdep,
                data = slsr_recovery, family = binomial)

summary(glm.fit2)

table(slsr_recovery$ethcat)
sum(is.na(slsr_recovery$anstrk1))

#Plot results
tidy_results2 <- broom::tidy(glm.fit2)

#calculate odds ratios and confidence intervals 
tidy_results2 <- tidy_results2 %>%
  mutate(odds_ratio = exp(estimate),
         conf.low = exp(estimate - 1.96 * std.error),
         conf.high = exp(estimate + 1.96 * std.error),
         significant = case_when(
           p.value >=0.05 ~ "",
           p.value < 0.05 & p.value >= 0.01 ~ "*",
           p.value < 0.01 & p.value >= 0.001  ~ "**",
           p.value < 0.001 ~ "***"),
         or_ci = sprintf("%.2f (%.2f, %.2f)", odds_ratio, conf.low, conf.high)) %>% 
  filter (term != "(Intercept)") %>% 
  mutate (term = case_when(
    #term == "(Intercept)" ~ "Intercept",
    
    term == "age_cat65-74" ~ "Age 65-74",
    term == "age_cat75-84" ~ "Age 75-84",
    term == "age_cat85+" ~ "Age 85+",
    
    term == "sexFemale" ~ "Female sex",
    
    term == "ethcatBlack" ~ "Black ethnicity",
    term == "ethcatOther" ~ "Other ethnicity",
    
    term == "educatSecondary" ~ "Secondary education",
    term == "educatTertiary" ~ "Tertiary education",
    
    term == "nihssModerate" ~ "NIHSS: Moderate",
    term == "nihssSevere" ~ "NIHSS: Severe",
    
    term == "strk_catIntracerebral haemorrhage" ~ "Stroke subtype: Intracerebral haemorrhage",
    term == "strk_catSubarachnoid haemorrhage"~ "Stroke subtype: Subarachnoid haemorrhage",
    term == "rfpdepYes" ~ "Pre-stroke depression"
  ))

#order variables correctly 
tidy_results2$term <- factor(tidy_results2$term, levels = rev(c(
  "Age 65-74", "Age 75-84", "Age 85+", "Female sex", 
  "Black ethnicity","Other ethnicity",
  "Secondary education", "Tertiary education", 
  "Pre-stroke depression",
  "NIHSS: Moderate", "NIHSS: Severe",                            
  "Stroke subtype: Intracerebral haemorrhage", "Stroke subtype: Subarachnoid haemorrhage")))


# Create forest plot with asterisks for significant results and modified labels
forest_rf2 <- ggplot(tidy_results2, aes(x = odds_ratio, y = term, xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 1, color = "grey", linetype = "dashed") +
  geom_errorbarh(height = 0.2) +
  geom_point(size = 2) +
  geom_text(aes(label = significant, x = 1.1 * conf.high), hjust = -1) +
  scale_y_discrete(labels = c(age = "Age at Diagnosis", sex = "Gender")) +
  xlim(-1,12) + 
  xlab("Odds Ratio (95% CI)") +
  ylab("") +
  theme_minimal()

forest_rf2

png("predictors_of_recovery.png", units="cm", width=17.1, height=17.1, res=400)
forest_rf2 
dev.off()









     