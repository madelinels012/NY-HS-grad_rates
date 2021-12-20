# load libraries
library(tidyverse)
library(readxl)
library(dplyr) 

#test scores
data_1 = read_excel("data/raw/acc_hs_composite_performance.xlsx")

#District Status
data_2 = read_excel("data/raw/accountability_status_subgroup.xlsx")

#CCCR
data_3 = read_excel("data/raw/cccr.xlsx")

#Grad Rates
data_4 = read_excel("data/raw/grad_rate.xlsx")

#Expenditures
data_5 = read_excel("data/raw/expenditures_per_student.xlsx")

#N/RC
data_6 = read_excel("data/raw/NRC.xlsx")

#Staff qualifications
data_7 = read_excel("data/raw/staff.xlsx")

##Clean data
#data 1
data_1_final = data_1 %>%
  filter(YEAR == 2019) %>%
  filter(SUBGROUP_NAME == "All Students") %>%
  select(-`OVERRIDE`, -`COMPOSITE _LEVEL`, -`COMPOSITE_COHORT`) %>%
  pivot_wider(names_from = `SUBJECT`, values_from = `COMPOSITE _INDEX`) %>%
  separate(ENTITY_CD, into = c("first", "second"), sep = 6) %>%
  separate(second, into = c("second", "third"), sep = 2) %>%
  filter(second != 86)

data_1_final$ENTITY_CD = str_c(data_1_final$first, data_1_final$second, data_1_final$third)

data_1_final$third = as.numeric(data_1_final$third)

data_1_final = data_1_final %>%
  filter(third != 0) %>% 
  select(-first, -second, -third) %>%
  relocate(ENTITY_CD) 

data_1_final = as_tibble(data_1_final)

data_1_final$ELA = as.numeric(data_1_final$ELA)
data_1_final$`Social Studies` = as.numeric(data_1_final$`Social Studies`)
data_1_final$Science = as.numeric(data_1_final$Science)
data_1_final$Math = as.numeric(data_1_final$Math)

#clean data 2
data_2_final = data_2 %>%
  filter(SUBGROUP_NAME == 'All Students') %>%
  filter(SCHOOL_TYPE != 'EM') %>%
  filter(YEAR == 2019)

data_2_final = as_tibble(data_2_final)

data_2_final = data_2_final %>%
  select(ENTITY_CD, OVERALL_STATUS)

data_final = inner_join(data_1_final, data_2_final, by = "ENTITY_CD")

#clean data 3
data_3_final = data_3 %>%
  filter(SUBGROUP_NAME == 'All Students') %>%
  filter(YEAR == 2019)

data_3_final = as_tibble(data_3_final)

data_3_final$LEVEL = as.numeric(data_3_final$LEVEL)

data_3_final = data_3_final %>%
  select(ENTITY_CD, `LEVEL`)

data_final = inner_join(data_3_final, data_final, by = "ENTITY_CD")

#clean data 4
data_4_final = data_4 %>%
  filter(SUBGROUP_NAME == 'All Students') %>%
  filter(YEAR == 2019)

data_4_final = as_tibble(data_4_final)

#select columns I want for grad rate
data_4_final = data_4_final %>%
  select(ENTITY_CD, COHORT, GRAD_RATE, COHORT_COUNT, COHORT_LEVEL)

data_4_final$GRAD_RATE = as.numeric(data_4_final$GRAD_RATE)
data_4_final$COHORT_COUNT = as.numeric(data_4_final$COHORT_COUNT)
data_4_final$COHORT_LEVEL = as.numeric(data_4_final$COHORT_LEVEL)

#pivot_wider so I can see all the columns based on 4,5,6 year grad rate
data_4_final = data_4_final %>%
  pivot_wider(names_from = c(COHORT, COHORT, COHORT), values_from = c(GRAD_RATE, COHORT_COUNT, COHORT_LEVEL)) %>%
  select(-COHORT_COUNT_Combined)

data_final = inner_join(data_4_final, data_final, by = "ENTITY_CD")

data_final = data_final %>%
  relocate(ENTITY_NAME,YEAR,SUBGROUP_NAME,) %>%
  relocate(ENTITY_CD)

#clean data 5
data_5_final = data_5 %>%
  filter(YEAR == 2019) 

data_5_final = as_tibble(data_5_final)

data_5_final = data_5_final %>%
  select(ENTITY_CD, PUPIL_COUNT_TOT, PER_FED_STATE_LOCAL_EXP)

data_final = inner_join(data_5_final, data_final, by = "ENTITY_CD")

data_final = data_final %>%
  relocate(ENTITY_NAME,YEAR,SUBGROUP_NAME,) %>%
  relocate(ENTITY_CD)

#clean data 6
data_6_final = data_6 %>%
  filter(YEAR == 2019)

data_6_final = as_tibble(data_6_final)

data_6_final = data_6_final %>%
  select(ENTITY_CD, DISTRICT_CD, DISTRICT_NAME, COUNTY_CD, COUNTY_NAME, NEEDS_INDEX)                    

data_final = inner_join(data_6_final, data_final, by = "ENTITY_CD")

#clean data 7
data_7_final = data_7 %>%
  filter(YEAR == 2019)

data_7_final = as_tibble(data_7_final)

data_7_final = data_7_final %>%
  select(ENTITY_CD, NUM_TEACH, NUM_TEACH_OC, NUM_TEACH_INEXP)

data_7_final$NUM_TEACH_INEXP = as.numeric(data_7_final$NUM_TEACH_INEXP)
data_7_final$NUM_TEACH = as.numeric(data_7_final$NUM_TEACH)
data_7_final$NUM_TEACH_OC = as.numeric(data_7_final$NUM_TEACH_OC)

data_final = inner_join(data_7_final, data_final, by = "ENTITY_CD")

data_final = data_final %>%
  mutate(TEACH_OC = NUM_TEACH_OC / NUM_TEACH) %>%
  mutate(TEACH_INEXP = NUM_TEACH_INEXP / NUM_TEACH) %>%
  select(-NUM_TEACH_INEXP, -NUM_TEACH_OC)

data_final = data_final %>%
  relocate(ENTITY_NAME,YEAR,SUBGROUP_NAME, DISTRICT_CD, DISTRICT_NAME, COUNTY_CD, COUNTY_NAME) %>%
  relocate(ENTITY_CD)

#Delete rows with missing values
data_final = na.omit(data_final)

#rearrange columns, delete some redundant columns
colnames(data_final)

data_final = data_final[, c(1,2,3,5,6,7,8,9,31,32,10,11,12,30,24,25,26,27,29,28,13,14,16,15)]

newnames = c("entity_cd", "entity_name", "year", "district_cd", "district_name", "county_cd", "county_name", "num_teach", "teach_oc", "teach_inexp", "needs_index", "pupil_count", "expenditures", "overall_status", "cccr_level", "ela", "social_studies", "science", "math", "combined_test", "grad_rate_6", "grad_rate_5", "grad_rate_4", "grad_rate_combined")
oldnames = as.character(colnames(data_final))

data_final = data_final %>%
  rename_at(vars(oldnames), ~ newnames)

#make more columns numeric
data_final$num_teach = as.numeric(data_final$num_teach)
data_final$teach_oc = as.numeric(data_final$teach_oc)
data_final$expenditures = as.numeric(data_final$expenditures)
data_final$combined_test = as.numeric(data_final$combined_test)


##write data to a .cvs file
write.csv(data_final, "data\\clean\\data_final.csv", row.names = FALSE)

