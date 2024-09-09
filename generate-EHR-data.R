### this is a script that quickly generates two simple EHR datasets for a patient data exercise to expose new users to patient-level data

# setup ---- 

library(stringi)   # makes it easier to generate random strings for pt ids
library(tidyverse) # basic data wrangling

# DATASET 1 ----


## generate patients ----

pt_ids <- stri_rand_strings(500, 7, '[0-9]')

pt_gender <- rbinom(n=500, size=1, prob=0.6)

pt_age <- rnorm(500, mean=30, sd = 15)

pts <- as.data.frame(cbind(pt_ids, pt_gender, pt_age))

rm(pt_ids, pt_gender, pt_age)

pts <- pts %>%
  mutate(pt_age = as.numeric(pt_age))

pts <- pts %>%
  mutate(age_at_first_visit = round(pt_age, 2))

pts <- pts %>%
  mutate(sex = case_when(
    pt_gender == 0 ~ "M",
    pt_gender == 1 ~ "F"
  ))

pts <- pts %>%
  select(-pt_gender)

## generate visit dates ----

first_visit_date <- sample(seq(as.Date('2019-01-01'), as.Date('2020-12-31'), by="day"), 500)

pts <- cbind(pts, first_visit_date)


## generate date of birth ----

pts <- pts %>% 
  mutate(age_days = (age_at_first_visit*365))

pts <- pts %>%
  mutate(date_of_birth = as.Date(first_visit_date - age_days))

pts <- pts %>%
  select(-age_days)

## generate second visit date----

pts <- pts %>%
  mutate(days_til_second = sample(1:65, n(), replace = TRUE))

pts <- pts %>%
  mutate(second_visit_date = first_visit_date + days_til_second)

pts <- pts %>%
  mutate(days_til_third = sample(1:35, n(), replace = TRUE))

pts <- pts %>%
  mutate(days_til_third = ifelse(startsWith(pt_ids, "7"), NA, days_til_third))

pts <- pts %>%
  mutate(third_visit_date = second_visit_date + days_til_third)


## generate and assign visit types ----

visit_type <- c("drug pickup", "follow-up", "initial intake", "test results", "antenatal care")

pts <- pts %>%
  mutate(first_visit_type = case_when(
    startsWith(pt_ids, "1") ~ "drug pickup",
    startsWith(pt_ids, "2") ~ "drug pickup",
    startsWith(pt_ids, "3") ~ "follow-up",
    startsWith(pt_ids, "4") ~ "follow-up",
    startsWith(pt_ids, "5") ~ "initial intake",
    startsWith(pt_ids, "6") ~ "test results",
    startsWith(pt_ids, "7") ~ "initial intake",
    startsWith(pt_ids, "8") ~ "follow-up",
    startsWith(pt_ids, "9") ~ "test results",
    startsWith(pt_ids, "0") ~ "initial intake"
  ))


pts <- pts %>%
  mutate(second_visit_type = case_when(
    endsWith(pt_ids, "1") ~ "drug pickup",
    endsWith(pt_ids, "2") ~ "drug pickup",
    endsWith(pt_ids, "3") ~ "follow-up",
    endsWith(pt_ids, "4") ~ "follow-up",
    endsWith(pt_ids, "5") ~ "initial intake",
    endsWith(pt_ids, "6") ~ "test results",
    endsWith(pt_ids, "7") ~ "initial intake",
    endsWith(pt_ids, "8") ~ "follow-up",
    endsWith(pt_ids, "9") ~ "test results",
    endsWith(pt_ids, "0") ~ "initial intake"
  ))

pts <- pts %>%
  mutate(third_visit_date_char = as.character(third_visit_date))


pts <- pts %>%
  mutate(third_visit_type = case_when(
    endsWith(third_visit_date_char, "1") ~ "drug pickup",
    endsWith(third_visit_date_char, "2") ~ "drug pickup",
    endsWith(third_visit_date_char, "3") ~ "follow-up",
    endsWith(third_visit_date_char, "4") ~ "follow-up",
    endsWith(third_visit_date_char, "5") ~ "initial intake",
    endsWith(third_visit_date_char, "6") ~ "test results",
    endsWith(third_visit_date_char, "7") ~ "initial intake",
    endsWith(third_visit_date_char, "8") ~ "follow-up",
    endsWith(third_visit_date_char, "9") ~ "test results",
    endsWith(third_visit_date_char, "0") ~ "initial intake"
  ))

pts <- pts %>%
  mutate(age_at_second_visit = as.numeric(second_visit_date - date_of_birth)/365) %>%
  mutate(age_at_second_visit = round(age_at_second_visit, 2))


pts <- pts %>%
  mutate(age_at_third_visit = as.numeric(third_visit_date - date_of_birth)/365) %>%
  mutate(age_at_third_visit = round(age_at_third_visit, 2))

## generate and assign clinics ----

pts <- pts %>%
  mutate(first_visit_clinic = case_when(
    endsWith(pt_ids, "1") ~ "Pine Clinic",
    endsWith(pt_ids, "2") ~ "Cedar Clinic",
    endsWith(pt_ids, "3") ~ "Redwood Health Center",
    endsWith(pt_ids, "4") ~ "Pine Clinic",
    endsWith(pt_ids, "5") ~ "Spruce Clinic",
    endsWith(pt_ids, "6") ~ "Cedar Clinic",
    endsWith(pt_ids, "7") ~ "Pine Health Center",
    endsWith(pt_ids, "8") ~ "Cedar Health Center",
    endsWith(pt_ids, "9") ~ "Pine Health Center",
    endsWith(pt_ids, "0") ~ "Redwood Hospital"
  ))


pts <- pts %>%
  mutate(second_visit_clinic = case_when(
    endsWith(pt_ids, "1") ~ "Pine Health Center",
    endsWith(pt_ids, "2") ~ "Pine Health Center",
    endsWith(pt_ids, "3") ~ "Spruce Clinic",
    endsWith(pt_ids, "4") ~ "Cedar Clinic",
    endsWith(pt_ids, "5") ~ "Pine Clinic",
    endsWith(pt_ids, "6") ~ "Redwood Health Center",
    endsWith(pt_ids, "7") ~ "Pine Health Center",
    endsWith(pt_ids, "8") ~ "Redwood Hospital",
    endsWith(pt_ids, "9") ~ "Pine Clinic",
    endsWith(pt_ids, "0") ~ "Cedar Clinic"
  ))


pts <- pts %>%
  mutate(third_visit_clinic = case_when(
    endsWith(third_visit_date_char, "1") ~ "Pine Clinic",
    endsWith(third_visit_date_char, "2") ~ "Pine Health Center",
    endsWith(third_visit_date_char, "3") ~ "Spruce Health Center",
    endsWith(third_visit_date_char, "4") ~ "Spruce Clinic",
    endsWith(third_visit_date_char, "5") ~ "Redwood Hospital",
    endsWith(third_visit_date_char, "6") ~ "Redwood Health Center",
    endsWith(third_visit_date_char, "7") ~ "Pine Clinic",
    endsWith(third_visit_date_char, "8") ~ "Cedar Health Center",
    endsWith(third_visit_date_char, "9") ~ "Pine Clinic",
    endsWith(third_visit_date_char, "0") ~ "Cedar Clinic"
  ))


## save ----

pts <- pts %>%
  select(-days_til_second, -days_til_third, -third_visit_date_char, -pt_age)

write_csv(pts, "dataset1.csv")



# DATASET 2 ----



## generate patients ----

pt_ids <- stri_rand_strings(500, 7, '[0-9]')

pt_gender <- rbinom(n=500, size=1, prob=0.6)

pt_age <- rnorm(500, mean=30, sd = 15)

pts <- as.data.frame(cbind(pt_ids, pt_gender, pt_age))

rm(pt_ids, pt_gender, pt_age)

pts <- pts %>%
  mutate(pt_age = as.numeric(pt_age))

pts <- pts %>%
  mutate(age_at_first_visit = round(pt_age, 2))

pts <- pts %>%
  mutate(sex = case_when(
    pt_gender == 0 ~ "Male",
    pt_gender == 1 ~ "Female"
  ))

pts <- pts %>%
  select(-pt_gender)

## generate visit dates ----

first_visit_date <- sample(seq(as.Date('2019-01-01'), as.Date('2020-12-31'), by="day"), 500)

pts <- cbind(pts, first_visit_date)


## generate date of birth ----

pts <- pts %>% 
  mutate(age_days = (age_at_first_visit*365))

pts <- pts %>%
  mutate(date_of_birth = as.Date(first_visit_date - age_days))

pts <- pts %>%
  select(-age_days)

## generate second visit date----

pts <- pts %>%
  mutate(days_til_second = sample(1:65, n(), replace = TRUE))

pts <- pts %>%
  mutate(second_visit_date = first_visit_date + days_til_second)

pts <- pts %>%
  mutate(days_til_third = sample(1:35, n(), replace = TRUE))

pts <- pts %>%
  mutate(days_til_third = ifelse(startsWith(pt_ids, "7"), NA, days_til_third))

pts <- pts %>%
  mutate(third_visit_date = second_visit_date + days_til_third)


## generate and assign visit types ----

visit_type <- c("Drug pickup", "Follow-up", "Intake appointment", "Receiving test results", "antenatal care")

pts <- pts %>%
  mutate(first_visit_type = case_when(
    startsWith(pt_ids, "1") ~ "Drug pickup",
    startsWith(pt_ids, "8") ~ "Drug pickup",
    startsWith(pt_ids, "3") ~ "Follow-up",
    startsWith(pt_ids, "4") ~ "Initial intake appointment",
    startsWith(pt_ids, "5") ~ "Initial intake appointment",
    startsWith(pt_ids, "6") ~ "Receiving test results",
    startsWith(pt_ids, "7") ~ "Initial intake appointment",
    startsWith(pt_ids, "2") ~ "Follow-up",
    startsWith(pt_ids, "9") ~ "Receiving test results",
    startsWith(pt_ids, "0") ~ "Initial intake appointment"
  ))


pts <- pts %>%
  mutate(second_visit_type = case_when(
    endsWith(pt_ids, "1") ~ "Drug pickup",
    endsWith(pt_ids, "2") ~ "Drug pickup",
    endsWith(pt_ids, "3") ~ "Follow-up",
    endsWith(pt_ids, "4") ~ "Follow-up",
    endsWith(pt_ids, "5") ~ "Follow-up",
    endsWith(pt_ids, "6") ~ "Receiving test results",
    endsWith(pt_ids, "7") ~ "Initial intake appointment",
    endsWith(pt_ids, "8") ~ "Follow-up",
    endsWith(pt_ids, "9") ~ "Receiving test results",
    endsWith(pt_ids, "0") ~ "Receiving test results"
  ))

pts <- pts %>%
  mutate(third_visit_date_char = as.character(third_visit_date))


pts <- pts %>%
  mutate(third_visit_type = case_when(
    endsWith(third_visit_date_char, "1") ~ "Drug pickup",
    endsWith(third_visit_date_char, "2") ~ "Drug pickup",
    endsWith(third_visit_date_char, "3") ~ "Follow-up",
    endsWith(third_visit_date_char, "4") ~ "Follow-up",
    endsWith(third_visit_date_char, "5") ~ "Initial intake appointment",
    endsWith(third_visit_date_char, "6") ~ "Receiving test results",
    endsWith(third_visit_date_char, "7") ~ "Drug pickup",
    endsWith(third_visit_date_char, "8") ~ "Follow-up",
    endsWith(third_visit_date_char, "9") ~ "Receiving test results",
    endsWith(third_visit_date_char, "0") ~ "Initial intake appointment"
  ))



pts <- pts %>%
  mutate(age_at_second_visit = as.numeric(second_visit_date - date_of_birth)/365) %>%
  mutate(age_at_second_visit = round(age_at_second_visit, 2))


pts <- pts %>%
  mutate(age_at_third_visit = as.numeric(third_visit_date - date_of_birth)/365) %>%
  mutate(age_at_third_visit = round(age_at_third_visit, 2))



pts <- pts %>%
  filter(age_at_first_visit > 0)

## generate and assign clinics ----

pts <- pts %>%
  mutate(first_visit_clinic = case_when(
    endsWith(pt_ids, "1") ~ "Baobab Clinic",
    endsWith(pt_ids, "2") ~ "Acacia Clinic",
    endsWith(pt_ids, "3") ~ "Juniper Health Center",
    endsWith(pt_ids, "4") ~ "Acacia Clinic",
    endsWith(pt_ids, "5") ~ "Juniper Hospital",
    endsWith(pt_ids, "6") ~ "Baobab Clinic",
    endsWith(pt_ids, "7") ~ "Sandalwood Clinic",
    endsWith(pt_ids, "8") ~ "Mangrove Health Center",
    endsWith(pt_ids, "9") ~ "Sandalwood Clinic",
    endsWith(pt_ids, "0") ~ "Juniper Hospital"
  ))


pts <- pts %>%
  mutate(second_visit_clinic = case_when(
    endsWith(pt_ids, "1") ~ "Juniper Health Center",
    endsWith(pt_ids, "2") ~ "Juniper Health Center",
    endsWith(pt_ids, "3") ~ "Baobab Clinic",
    endsWith(pt_ids, "4") ~ "Acacia Clinic",
    endsWith(pt_ids, "5") ~ "Sandalwood Clinic",
    endsWith(pt_ids, "6") ~ "Mangrove Health Center",
    endsWith(pt_ids, "7") ~ "Mangrove Health Center",
    endsWith(pt_ids, "8") ~ "Juniper Hospital",
    endsWith(pt_ids, "9") ~ "Sandalwood Clinic",
    endsWith(pt_ids, "0") ~ "Baobab Clinic"
  ))


pts <- pts %>%
  mutate(third_visit_clinic = case_when(
    endsWith(third_visit_date_char, "1") ~ "Baobab Clinic",
    endsWith(third_visit_date_char, "2") ~ "Juniper Health Center",
    endsWith(third_visit_date_char, "3") ~ "Mangrove Health Center",
    endsWith(third_visit_date_char, "4") ~ "Sandalwood Clinic",
    endsWith(third_visit_date_char, "5") ~ "Juniper Hospital",
    endsWith(third_visit_date_char, "6") ~ "Juniper Health Center",
    endsWith(third_visit_date_char, "7") ~ "Acacia Clinic",
    endsWith(third_visit_date_char, "8") ~ "Juniper Health Center",
    endsWith(third_visit_date_char, "9") ~ "Baobab Clinic",
    endsWith(third_visit_date_char, "0") ~ "Baobab Clinic"
  ))

## save ----
pts <- pts %>%
  select(-days_til_second, -days_til_third, -third_visit_date_char, -pt_age)

write_csv(pts, "dataset2.csv")


# other scrap code ----


clinic_prefix <- c("Acacia", "Baobab", "Juniper", "Oak", "Pine", "Maple", "Spruce", "Cypress", "Chestnut", "Sequoia", "Mulberry", "Redwood")

hospitals <- paste(clinic_prefix, "Hospital")
clinics <- paste(clinic_prefix, "Clinic")
health_centers <- paste(clinic_prefix, "Health Center")

clinic_names <- c(hospitals, clinics, health_centers)
rm(hospitals, clinics, health_centers, clinic_prefix)


## generate districts ----

district_prefix <- c("Red", "Orange", "Yellow", "Blue", "Green", "Purple")

district_names <- paste0(district, " District")
rm(district_prefix)

