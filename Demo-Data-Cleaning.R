# Loading Packages --------------------------------------------------------

library(tidyverse)
library(googledrive)
library(googlesheets4)
library(janitor)

# Authentication ----------------------------------------------------------

Sys.getenv("GOOGLE_SHEETS_EMAIL")

drive_auth(Sys.getenv("GOOGLE_SHEETS_EMAIL"))

# Import File -----------------------------------------------------------

all_demographics <- read_sheet("https://docs.google.com/spreadsheets/d/1aNWF7AojvdaRg7gaxfztgbog13QVfwIcMqOaiAEiDwM/edit#gid=1265905422",
                               sheet = "data") %>% 
  clean_names()

# Clean Data --------------------------------------------------------------

clean_all_norace_demos <- all_demographics %>%
  select(-test_name) %>% 
  mutate(education = recode(education, "A" = "High School/GED",
                            "B" = "Some College",
                            "C" = "Associates Degree",
                            "D" = "BA/BS in Social Work",
                            "E" = "BA/BS in other Social Sciences",
                            "F" = "BA/BS in non-Social Sciences",
                            "G" = "MA/MS in Social Work",
                            "H" = "MA/MS in other Social Sciences",
                            "I" = "MA/MS in non-Social Sciences",
                            "J" = "Doctorate")) %>% 
  mutate(education = na_if(education, "No answer")) %>% 
  mutate(cwep_student = recode(cwep_student, "A" = "Yes",
                          "B" = "No")) %>% 
  mutate(cwep_student = na_if(cwep_student, "No answer")) %>% 
  mutate(cwep_grad_location = recode(cwep_grad_location, "A" = "Not Applicable",
                             "B" = "Portland State University",
                             "C" = "Other University")) %>% 
  mutate(cwep_grad_location = na_if(cwep_grad_location, "No answer")) %>% 
  mutate(cwep_status = recode(cwep_status, "A" = "Not Applicable",
                              "B" = "Current BSW Student",
                              "C" = "Graduated BSW, in payback",
                              "D" = "Current MSW Student",
                              "E" = "Graduated MSW, in payback",
                              "F" = "Other")) %>% 
  mutate(cwep_status = na_if(cwep_status, "No answer")) %>% 
  mutate(current_role = recode(current_role, "A" = "Screener",
                       "B" = "CPS",
                       "C" = "Permanency",
                       "D" = "Certification/Adoption",
                       "E" = "Not Assigned Yet",
                       "F" = "Tribal Child Welfare Employee",
                       "G" = "Intern/Student",
                       "H" = "Other")) %>% 
  mutate(current_role = na_if(current_role, "No answer")) %>% 
  mutate(cw_other_state = recode(cw_other_state, "A" = "Yes",
                                 "B" = "No")) %>% 
  mutate(cw_other_state = na_if(cw_other_state, "No answer")) %>% 
  mutate(length_cw_other_state = recode(length_cw_other_state, "A" = "Not Applicable",
                                      "B" = "< 1 year",
                                      "C" = "1-2 years",
                                      "D" = "3-5 years",
                                      "E" = "6-10 years",
                                      "F" = "> 10 years")) %>% 
  mutate(length_cw_other_state = na_if(length_cw_other_state, "No answer")) %>%
  mutate(age = as.character(age)) %>% 
  mutate(age = na_if(age, "No answer")) %>% 
  mutate(age = na_if(age, "N/A")) %>% 
  mutate(age = parse_number(age)) %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(age = round(age, digits = 0)) %>% 
  mutate(english = recode(english, "A" = "Yes",
                                  "B" = "No")) %>% 
  mutate(english = na_if(english, "No answer")) %>% 
  mutate(start_date = as.character(start_date)) %>% 
  mutate(start_date = na_if(start_date, "No answer")) %>%
  mutate(start_date = na_if(start_date, "N/A")) %>%
  mutate(start_date = na_if(start_date, "n/a")) %>%
  mutate(start_date = as.numeric (start_date)) %>% 
  mutate(start_date = as.Date(start_date, origin = "1899-12-30")) %>% 
  mutate(sss1 = recode(sss1, "A" = "Yes",
                          "B" = "No")) %>% 
  mutate(sss1 = na_if(sss1, "No answer")) %>% 
  mutate(gender_id = recode(gender_id, "A" = "Cisgender Man",
                            "B" = "Cisgender Woman",
                            "C" = "Transgender Man",
                            "D" = "Transgender Woman",
                            "E" = "Non-binary Transgender",
                            "F" = "Non-binary Non-transgender",
                            "G" = "Prefer Not to Answer",
                            "H" = "Prefer to Describe")) %>% 
  mutate(gender_id = na_if(gender_id, "No answer"))

clean_all_race_demos <- all_demographics %>% 
  select(identifier, asian:white) %>% 
  pivot_longer(cols = -identifier,
               names_to = "racial_group",
               values_to = "selection") %>% 
  mutate(selection = na_if(selection, "No answer")) %>%
  drop_na(selection) %>% 
  separate_longer_position(selection, width = 1) %>% 
  mutate(selection = na_if(selection, "A")) %>% 
  mutate(selection = case_when(
    racial_group == "asian" & selection == "B" ~ "Asian Indian",
    racial_group == "asian" & selection == "C" ~ "Chinese",
    racial_group == "asian" & selection == "D" ~ "Filipino/a",
    racial_group == "asian" & selection == "E" ~ "Japanese",
    racial_group == "asian" & selection == "F" ~ "Korean",
    racial_group == "asian" & selection == "G" ~ "Vietnamese",
    racial_group == "asian" & selection == "H" ~ "Not Listed/Prefer to Self-Describe",
    racial_group == "aian" & selection == "B" ~ "Alaska Native",
    racial_group == "aian" & selection == "C" ~ "American Indian",
    racial_group == "aian" & selection == "D" ~ "Canadian Inuit, Metis, or First Nation",
    racial_group == "aian" & selection == "E" ~ "Indigenous Mexican, Central American, or South American",
    racial_group == "aian" & selection == "F" ~ "Not Listed/Prefer to Self-Describe",
    racial_group == "blaa" & selection == "B" ~ "African American",
    racial_group == "blaa" & selection == "C" ~ "Ethiopian",
    racial_group == "blaa" & selection == "D" ~ "Haitian",
    racial_group == "blaa" & selection == "E" ~ "Jamaican",
    racial_group == "blaa" & selection == "F" ~ "Nigerian",
    racial_group == "blaa" & selection == "G" ~ "Somali",
    racial_group == "blaa" & selection == "H" ~ "Not Listed/Prefer to Self-Describe",
    racial_group == "his_lat" & selection == "B" ~ "Central American",
    racial_group == "his_lat" & selection == "C" ~ "Cuban",
    racial_group == "his_lat" & selection == "D" ~ "Mexican",
    racial_group == "his_lat" & selection == "E" ~ "Mexican-American or Chicano/a",
    racial_group == "his_lat" & selection == "F" ~ "Puerto Rican",
    racial_group == "his_lat" & selection == "G" ~ "South American",
    racial_group == "his_lat" & selection == "H" ~ "Not Listed/Prefer to Self-Describe",
    racial_group == "mid_east" & selection == "B" ~ "Egyptian",
    racial_group == "mid_east" & selection == "C" ~ "Iranian",
    racial_group == "mid_east" & selection == "D" ~ "Iraqi",
    racial_group == "mid_east" & selection == "E" ~ "Israeli",
    racial_group == "mid_east" & selection == "F" ~ "Kurdish",
    racial_group == "mid_east" & selection == "G" ~ "Lebanese",
    racial_group == "mid_east" & selection == "H" ~ "Moroccan",
    racial_group == "mid_east" & selection == "I" ~ "Syrian",
    racial_group == "mid_east" & selection == "J" ~ "Not Listed/Prefer to Self-Describe",
    racial_group == "nhpi" & selection == "B" ~ "Guamanian or Chamorro",
    racial_group == "nhpi" & selection == "C" ~ "Marshallese",
    racial_group == "nhpi" & selection == "D" ~ "Native Hawaiian",
    racial_group == "nhpi" & selection == "E" ~ "Samoan",
    racial_group == "nhpi" & selection == "F" ~ "Not Listed/Prefer to Self-Describe",
    racial_group == "white" & selection == "B" ~ "Eastern European",
    racial_group == "white" & selection == "C" ~ "Slavic",
    racial_group == "white" & selection == "D" ~ "Western European",
    racial_group == "white" & selection == "E" ~ "Not Listed/Prefer to Self-Describe",
    TRUE ~ "N/A or No Answer")) %>% 
  mutate(racial_group = recode(racial_group, "asian" = "asian_named",
                               "aian" = "aian_named",
                               "blaa" = "blaa_named",
                               "his_lat" = "his_lat_named",
                               "mid_east" = "mid_east_named",
                               "nhpi" = "nhpi_named",
                               "white" = "white_named")) %>% 
  pivot_wider(id_cols = identifier,
              names_from = racial_group,
              values_from = selection,
              values_fn = list) %>% 
  mutate(asian_named = as.character(asian_named)) %>% 
  mutate(aian_named = as.character(aian_named)) %>% 
  mutate(blaa_named = as.character(blaa_named)) %>% 
  mutate(his_lat_named = as.character(his_lat_named)) %>% 
  mutate(mid_east_named = as.character(mid_east_named)) %>% 
  mutate(nhpi_named = as.character(nhpi_named)) %>% 
  mutate(white_named = as.character(white_named))

clean_all_demos <- left_join(clean_all_norace_demos,
                                clean_all_race_demos,
                                by = "identifier") %>% 
  select(-(asian:white)) %>% 
  relocate(describe_race, .after = last_col())

# Export Data -------------------------------------------------------------

write_rds(clean_all_demos,
          file = "data/clean-all-demos.rds")
