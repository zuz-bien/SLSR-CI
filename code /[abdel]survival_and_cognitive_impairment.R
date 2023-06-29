#Survival 

library(tidyverse)
slsr <- read.csv("/Users/Zuzanna_Bien/Desktop/ACF Public Health/Cognitive impairment in stroke/SLSR-CI/SLSR_raw_data/ds_202305.csv")


# Generate survival time in days
slsr$survd <- as.Date(paste(slsr$dodpi_y, slsr$dodpi_m, slsr$dodpi_d, sep = "-")) - 
  as.Date(paste(slsr$strk_y, slsr$strk_m, slsr$strk_d, sep = "-"))

# Generate survival time in years
#slsr$survy <- slsr$survd / 365.25
slsr$surv

# Replace negative surv values with their absolute values
slsr$surv <- ifelse(slsr$surv < 0, abs(slsr$surv), slsr$surv)

# Generate recurrence time in days
slsr$dtrecurrence <- as.Date(paste(slsr$rec_y, slsr$rec_m, slsr$rec_d, sep = "-"))

# Calculate recurrence time in years
slsr$survreccy <- (slsr$dtrecurrence - as.Date(paste(slsr$strk_y, slsr$strk_m, slsr$strk_d, sep = "-"))) / 365.25

# Define labels for variables
slsr$recc <- factor(slsr$recc, labels = c("Baseline", "1 year", "2 years", "3 years", ..., "18 years"))

# Update recc variable based on survreccy values
for (x in c(18, 17, 16, 15, ..., 0.25)) {
  slsr$recc[slsr$survreccy < x] <- x
}
slsr$recc[slsr$survreccy < 0.02] <- 0

# Define labels for yesno variable
yesno_labels <- c("No", "Yes")

# Generate trecc7d variable
slsr$trecc7d <- ifelse(slsr$survreccy < 0.02, 1, 0)

# Generate trecc3m variable
slsr$trecc3m <- ifelse(slsr$survreccy < 0.25, 1, 0)

# Generate trecc1, trecc2, trecc3, ..., trecc18 variables
for (x in 1:18) {
  slsr[[paste0("trecc", x)]] <- ifelse(slsr$survreccy < x, 1, 0)
}

# Define labels for surv variable
slsr$surv <- factor(slsr$surv, labels = c("Baseline", "1 year", "2 years", "3 years", ..., "18 years"))

# Update surv variable based on survy values
for (x in c(18, 17, 16, 15, ..., 0.25)) {
  slsr$surv[slsr$survy < x] <- x
}
slsr$surv[slsr$survy < 0.02] <- 0

# Generate tsurv7d variable
slsr$tsurv7d <- ifelse(slsr$survy < 0.02, 1, 0)

# Generate tsurv3m variable
slsr$tsurv3m <- ifelse(slsr$survy < 0.25, 1, 0)

# Generate tsurv1, tsurv2, tsurv3, ..., tsurv18 variables
for (x in 1:18) {
  slsr[[paste0("tsurv", x)]] <- ifelse(slsr$survy < x, 1, 0)
}
