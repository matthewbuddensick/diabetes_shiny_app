library(tidyverse)
diabetic_data <- read_csv("diabetic_data.csv", na = "?")
id_mapping <- read_csv("IDs_mapping.csv")

# admission_type_id 
admission_type_data <- id_mapping %>%
  filter(row(id_mapping) >= 1 & row(id_mapping) <= 8) %>% 
  mutate(admission_type_id = as.numeric(admission_type_id)) %>% 
  rename(admission_type = description)

## discharge_disposition_id,description
discharge_disposition_data <- id_mapping %>% 
  filter(row(id_mapping) >= 11 & row(id_mapping) <= 40) %>% 
  rename(discharge_disposition_id = admission_type_id) %>% 
  mutate(discharge_disposition_id = as.numeric(discharge_disposition_id)) %>% 
  rename(discharge_disposition = description)

## admission_source_id
admission_source_data <- id_mapping %>%  
  filter(row(id_mapping) >= 43) %>% 
  rename(admission_source_id = admission_type_id) %>% 
  mutate(admission_source_id = as.numeric(admission_source_id)) %>% 
  rename(admission_source = description)

cleaned_diabetic_data <- diabetic_data %>% 
  left_join(admission_type_data, by = c("admission_type_id")) %>% 
  left_join(discharge_disposition_data, by = c("discharge_disposition_id")) %>% 
  left_join(admission_source_data, by = c("admission_source_id")) %>% 
  select(-c(admission_type_id, discharge_disposition_id, admission_source_id)) %>% 
  mutate(race = ifelse(race == "AfricanAmerican", "African American", race),
         medical_specialty = ifelse(is.na(medical_specialty), "Unknown", medical_specialty),
         readmitted = case_when(
           readmitted == "NO" ~ "No Readmission",
           readmitted == ">30" ~ "Readmission >30 days",
           readmitted == "<30" ~ "Readmission <30 days"),
         race = ifelse(is.na(race), "Unknown", race)) %>%
  write_csv("cleaned_diabetic_data.csv")

