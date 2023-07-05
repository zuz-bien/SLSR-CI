#COGNITIVE IMPAIRMENT - ABDEL's CODE

library(tidyverse)
slsr <- read.csv("/Users/Zuzanna_Bien/Desktop/ACF Public Health/Cognitive impairment in stroke/SLSR-CI/SLSR_raw_data/ds_202305.csv")


# cogcat	Memory score category	
# 0=Cognitively impaired (0-7 AMT & <24 MMSE)
# 1=Cognitively intact (8-10 AMT & 24+ MMSE)
# 88=Not on form

slsr$cogcat <- factor(slsr$cogcat)
slsr$cogcat[slsr$cogcat == 88] <- 4

slsr$cogcat[slsr$cog == 2 & slsr$cogcat > 1 & slsr$cogcat == 4] <- 3
slsr$cogcat[slsr$survy < 0.02 & slsr$cogcat3m > 1 & slsr$cogcat == 4] <- 2

# 3m cognition
slsr$cogcat3m <- factor(slsr$cogcat3m)
slsr$cogcat3m[slsr$cog3m == 2 & slsr$cogcat3m > 1 & slsr$cogcat3m == 4] <- 3
slsr$cogcat3m[slsr$survy < 0.25 & slsr$cogcat3m > 1 & slsr$cogcat3m == 4] <- 2

# 1yr cognition
slsr$cogcat1 <- factor(slsr$cogcat1)
slsr$cogcat1[slsr$cog1 == 2 & slsr$cogcat1 > 1 & slsr$cogcat1 == 4] <- 3
slsr$cogcat1[slsr$survy < 1 & slsr$cogcat1 > 1 & slsr$cogcat1 == 4] <- 2

# 2-18 yrs cognition
for (i in 2:18) {
  colname <- paste0("cogcat", i)
  slsr[, colname] <- factor(slsr[, colname])
  slsr[, colname][slsr[, paste0("cog", i)] == 2 & slsr[, colname] > 1 & slsr[, colname] == 4] <- 3
  slsr[, colname][slsr$survy < i & slsr[, colname] > 1 & slsr[, colname] == 4] <- 2
}

# cog	Any one of the memory tests done	
# 1=Done
# 2=Not done
# 3=Unknown
# 88=Not on form

slsr$cog <- factor(slsr$cog)
slsr$cog[slsr$cog == 88 | slsr$cog == 7] <- 3

slsr$cog3m <- factor(slsr$cog3m)
slsr$cog3m[slsr$cog3m == 88 | slsr$cog3m == 7] <- 3

for (i in 1:18) {
  colname <- paste0("cog", i)
  slsr[, colname] <- factor(slsr[, colname])
  slsr[, colname][slsr[, colname] == 88 | slsr[, colname] == 7] <- 3
}

# Output to cogcat.txt
slsr <- slsr[order(slsr$cogcat), ]
write.table(slsr, "cogcat.txt", sep="\t", row.names=FALSE, col.names=FALSE, na="", quote=FALSE)

# Cognition as continuous variable
# 0=Cognitively impaired (0-7 AMT & <24 MMSE)
# 1=Cognitively intact (8-10 AMT & 24+ MMSE)
# 88=Not on form

slsr$cognition <- NA
#min-max scaling of the cognition variable 
slsr$cognition <- ifelse(!is.na(slsr$mtot), (slsr$mtot - min(slsr$mtot)) / (max(slsr$mtot) - min(slsr$mtot)), slsr$cognition)
slsr$cognition <- ifelse(!is.na(slsr$tot_m), (10 * (10 * (slsr$tot_m - min(slsr$tot_m)) / (max(slsr$tot_m) - min(slsr$tot_m)))) / 100, slsr$cognition)

slsr$cognition


slsr$cognition3m <- NA
slsr$cognition3m <- ifelse(!is.na(slsr$mtot_3m), (slsr$mtot_3m - min(slsr$mtot_3m)) / (max(slsr$mtot_3m) - min(slsr$mtot_3m)), slsr$cognition3m)
slsr$cognition3m <- ifelse(!is.na(slsr$tot_m3m), (10 * (10 * (slsr$tot_m3m - min(slsr$tot_m3m)) / (max(slsr$tot_m3m) - min(slsr$tot_m3m)))) / 100, slsr$cognition3m)

for (i in 1:3) {
  colname <- paste0("cognition", i)
  slsr[, colname] <- NA
  slsr[, colname] <- ifelse(!is.na(slsr[, paste0("mtot_", i)]), (slsr[, paste0("mtot_", i)] - min(slsr[, paste0("mtot_", i)])) / (max(slsr[, paste0("mtot_", i)]) - min(slsr[, paste0("mtot_", i)]))), slsr[, colname])
slsr[, colname] <- ifelse(!is.na(slsr[, paste0("tot_m", i)]), (10 * (10 * (slsr[, paste0("tot_m", i)] - min(slsr[, paste0("tot_m", i)])) / (max(slsr[, paste0("tot_m", i)]) - min(slsr[, paste0("tot_m", i)]))))) / 100, slsr[, colname])
}

for (i in 4:18) {
  colname <- paste0("cognition", i)
  slsr[, colname] <- NA
  slsr[, colname] <- ifelse(!is.na(slsr[, paste0("mtot_", i)]), (slsr[, paste0("mtot_", i)] - min(slsr[, paste0("mtot_", i)])) / (max(slsr[, paste0("mtot_", i)]) - min(slsr[, paste0("mtot_", i)]))), slsr[, colname])
}

# Activity level
slsr$activity <- cut(slsr$activity, breaks = c(0, 15, 30, 45), labels = c(1, 2, 3), include.lowest = TRUE)
