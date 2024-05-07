# Loading Packages --------------------------------------------------------

library(tidyverse)
library(googledrive)
library(googlesheets4)
library(janitor)


# Create Google Sheets Mega -----------------------------------------------

google_sheets_urls <-
  c(
    "https://docs.google.com/spreadsheets/d/1L7Yw8vIw28AeJ3QnoWBjQtnasOZ1F5pSea2phePbejM/edit#gid=335205396",
    "https://docs.google.com/spreadsheets/d/1eghPRIvGVZABrKHoLKnHILu19QM5N8jYo8KWUigF3dw/edit#gid=1501730987",
    "https://docs.google.com/spreadsheets/d/1te-JuXY9Cy0Zv2N6cfhkwnyNMymF99A1uP9gJq77evw/edit#gid=1947227508",
    "https://docs.google.com/spreadsheets/d/1or2liAYga4IkcZme9_CBGEeJbrF6pQGEF8A8QyQDoPc/edit#gid=1512201892",
    "https://docs.google.com/spreadsheets/d/1szz50CzAS9dwJR-Uqljt7YKeaEE5qCpip-dUCfXD24M/edit#gid=1903222710",
    "https://docs.google.com/spreadsheets/d/1kW9L8iASwTzyFAFO1fv8acFT-bc6Ww--rhV_2aj4MxI/edit#gid=1121532601",
    "https://docs.google.com/spreadsheets/d/11CgvHnCOKdk-nEVfcwz8-HLK5RbMv24R5Ai2MH9fF6E/edit#gid=831492407",
    "https://docs.google.com/spreadsheets/d/1yn5Ga-3QUJ0xok5336WBn8iLzIVBljVBUpo-gYn_Cgg/edit#gid=961330869"
  )

get_sheet_name <- function(sheet_url) {
  sheet_url %>% 
    drive_get() %>% 
    select(name) %>% 
    rename(training_name = name) %>% 
    mutate(training_name = str_remove(training_name, " DB 202307")) %>%
    mutate(sheet_url = sheet_url)
}

google_sheets_name <-
  get_sheet_name(google_sheets_urls)

get_sheet_names <- function(sheet_url) {
  sheet_url %>% 
    sheet_properties() %>% 
    select(name) %>% 
    rename(sheet_name = name) %>% 
    mutate(sheet_url = sheet_url)
}

google_sheets_names <-
  map(google_sheets_urls, get_sheet_names) %>% 
  bind_rows()

google_sheets_meta <-
  left_join(google_sheets_name, google_sheets_names)

google_sheets_meta %>% 
  write_rds("data/google_sheets_meta.rds")

google_sheets_meta <-
  read_rds("data/google_sheets_meta.rds")

# General Functions to Import Data ----------------------------------------

import_cwp_data_single_sheet <- function(sheet_url, sheet_name, training_name) {
  read_sheet(
    ss = sheet_url,
    sheet = sheet_name,
    col_types = "c"
  ) %>% 
    clean_names() %>% 
    mutate(training_name = training_name)
}

import_cwp_data_multiple_sheets <- function(google_sheets_meta_filtered) {
  google_sheets_meta_filtered %>% 
    pmap(
      import_cwp_data_single_sheet
    ) %>% 
    bind_rows()
}

# Number Registered -------------------------------------------------------

number_registered <-
  google_sheets_meta %>% 
  filter(sheet_name == "registered") %>% 
  import_cwp_data_multiple_sheets() %>% 
  mutate(registered = parse_number(registered)) %>% 
  mutate(cohort = str_remove(cohort, "_1$|_2$|_3$")) %>% 
  mutate(quarter = str_replace_all(cohort, c("07$|08$|09$" = "Q1", "10$|11$|12$" = "Q2", "01$|02$|03$" = "Q3", "04$|05$|06$" = "Q4")))

number_registered %>% 
  write_rds("data/number_registered.rds")

number_registered <-
  read_rds("data/number_registered.rds")

# Overall/ID Data ---------------------------------------------------------

inclusion_data <-
  google_sheets_meta %>% 
  filter(sheet_name == "overall") %>% 
  import_cwp_data_multiple_sheets() %>% 
  mutate(
    date = mdy(date)
  ) %>% 
  # Convert some columns to numeric
  mutate(across(
    c(
      starts_with("it_"),
      id_other,
      id_trainer,
      overall,
      useful
    ),
    as.numeric
  )) %>% 
  mutate(response_id = str_glue("inclusion-{row_number()}")) %>% 
  mutate(cohort = str_remove(cohort, "_1$|_2$|_3$")) %>% 
  mutate(quarter = str_replace_all(cohort, c("07$|08$|09$" = "Q1", "10$|11$|12$" = "Q2", "01$|02$|03$" = "Q3", "04$|05$|06$" = "Q4"))) %>% 
  select(response_id, quarter, date, training_name, location, id_trainer, id_other, id_comment, id_code)

# Qualitative Codebook ----------------------------------------------------

qualitative_codebook <-
  read_sheet("https://docs.google.com/spreadsheets/d/1InMuzPS9XOacWiPCa-bGkNysKtOXZRpP1fwLrkUCEo4/edit#gid=268800523",
             col_types = "c") %>% 
  clean_names() %>% 
  select(-x5) %>% 
  pivot_longer(
    cols = c(code_positive, code_negative),
    names_to = "sentiment",
    values_to = "comment_code"
  ) %>% 
  mutate(sentiment = str_remove(sentiment, "code_"))

qualitative_codebook %>% 
  write_rds("data/qualitative_codebook.rds")

qualitative_codebook <-
  read_rds("data/qualitative_codebook.rds")

# ID Ratings --------------------------------------------------------------

id_ratings <- 
  inclusion_data %>% 
  select(response_id, quarter, training_name, id_trainer, id_other) %>% 
  pivot_longer(
    cols = c(id_trainer, id_other),
    names_to = "question",
    values_to = "rating"
  )

id_ratings %>% 
  write_rds("data/id_ratings.rds")

id_ratings <-
  read_rds("data/id_ratings.rds")

# Qualitative Data --------------------------------------------------------

id_qual_comments <-
  inclusion_data %>% 
  select(response_id, quarter, training_name, id_comment) %>%
  mutate(id_comment = na_if(id_comment, "N/A")) %>% 
  drop_na(id_comment)

id_qual_comments %>% 
  write_rds("data/id_qual_comments.rds")

id_qual_comments <-
  read_rds("data/id_qual_comments.rds")

id_qual_codes <- 
  inclusion_data %>% 
  select(response_id, quarter, training_name, id_comment, id_code) %>%
  mutate(id_comment = na_if(id_comment, "N/A")) %>% 
  drop_na(id_comment) %>% 
  separate_longer_delim(id_code,
                        delim = ", ") %>% 
  drop_na(id_code)

id_qual_codes %>% 
  write_rds("data/id_qual_codes.rds")

id_qual_codes <-
  read_rds("data/id_qual_codes.rds")