library(dplyr)
library(fuzzyjoin)
library(geosphere)
library(phonics)
library(readr)
library(readxl)
library(stringdist)
library(stringr)
library(tidyr)

# load raw datasets
source("gdrive/OLEM/olem-matching/function_clean_raw_data.R")
source("gdrive/OLEM/olem-matching/function_clean_rmp_duplicates.R")

cleaned_data <- clean_raw_data()

frp <- cleaned_data$frp
rmp <- cleaned_data$rmp
rcra <- cleaned_data$rcra

# clean RMP data
rmp_cleaned_duplicates <- 
  remove_rmp_duplicates(rmp)

rmp_cleaned <-
  rmp_cleaned_duplicates %>%
  mutate(epa_facility_id = as.character(epa_facility_id)) %>%
  glimpse()

# load in datasets
frp_rmp <-
  read_csv("gdrive/OLEM/olem-matching/output_data/matches_frp_rmp_v2.csv")
frp_rcra <-
  read_csv("gdrive/OLEM/olem-matching/output_data/matches_frp_rcra_v2.csv")
rmp_rcra <-
  read_csv("gdrive/OLEM/olem-matching/output_data/matches_rmp_rcra_v2.csv")

frp_rmp_ids <-
  frp_rmp %>%
  select(frp_id = facility_id_frp,
         rmp_id = epa_facility_id_rmp)
frp_rcra_ids <-
  frp_rcra %>%
  select(frp_id = facility_id_frp,
         rcra_id = facility_id_rcra)
rmp_rcra_ids <-
  rmp_rcra %>%
  select(rmp_id = facility_id_rmp,
         rcra_id = facility_id_rcra)

crosswalk <-
  frp_rmp_ids %>%
  full_join(frp_rcra_ids, by = "frp_id") %>%
  full_join(rmp_rcra_ids, by = "rmp_id") %>%
  mutate(
    rcra_id = coalesce(rcra_id.x, rcra_id.y)
  ) %>%
  select(frp_id, rmp_id, rcra_id) %>%
  group_by(frp_id) %>%
  summarise(
    rmp_id = paste(sort(unique(na.omit(rmp_id))), collapse = "; "),
    rcra_id = paste(sort(unique(na.omit(rcra_id))), collapse = "; "),
    .groups = "drop"
  ) %>%
  rename("frp_facility_id" = "frp_id", "rmp_epa_facility_id" = "rmp_id", "rcra_handler_id" = "rcra_id")
  
crosswalk_frp <-
  crosswalk %>%
  right_join(frp %>% select(facility_id), by = c("frp_facility_id" = "facility_id")) %>%
  arrange(frp_facility_id) %>%
  mutate(across(where(is.character), ~replace_na(., ""))) %>%
  distinct() 

crosswalk_unmatched_rmp <-
  rmp_cleaned %>%
  select(rmp_epa_facility_id = epa_facility_id) %>%
  anti_join(crosswalk_frp, by = "rmp_epa_facility_id") %>%
  arrange(rmp_epa_facility_id) %>%
  distinct()

crosswalk_unmatched_rcra <-
  rcra %>%
  select(rcra_handler_id = facility_id) %>%
  anti_join(crosswalk_frp, by = "rcra_handler_id") %>%
  arrange(rcra_handler_id) %>%
  distinct()

write.csv(crosswalk_frp,
          glue::glue("gdrive/OLEM/olem-matching/output_data/frp_crosswalk.csv"), row.names = FALSE)

write.csv(crosswalk_unmatched_rmp,
          glue::glue("gdrive/OLEM/olem-matching/output_data/rmp_unmatched.csv"), row.names = FALSE)

write.csv(crosswalk_unmatched_rcra,
          glue::glue("gdrive/OLEM/olem-matching/output_data/rcra_unmatched.csv"), row.names = FALSE)
