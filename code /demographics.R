#DEMOGRAPHIC DATA 

library(tidyverse)
slsr <- read.csv("SLSR_raw_data/ds_202305.csv")

#AGE
#Change biologically implausible ages to NA 
slsr <- slsr %>% mutate(age = ifelse((age < 17 | age >110), NA,  age))

#ETHNICITY 



#EDUCATION 