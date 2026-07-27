# Load libraries -----
library(dplyr)
library(fuzzyjoin)
library(geosphere)
library(phonics)
library(readr)
library(readxl)
library(stringdist)
library(stringr)
library(tidyr)

# Set version and data directory -----
version <- "v3"
save_dir <- glue::glue("gdrive/OLEM/olem-matching/output_data/", version)

# Load raw datasets -----

source("gdrive/OLEM/olem-matching/function_clean_raw_data.R")

cleaned_data <- clean_raw_data()

frp <- cleaned_data$frp
rmp_all <- cleaned_data$rmp
rcra <- cleaned_data$rcra

# select most up-to-date RMP records
rmp <-
  rmp_all %>%
  group_by(epa_facility_id) %>%
  slice_max(order_by = facility_id, n = 1, with_ties = TRUE) %>%
  ungroup()

# Load facility matches -----
frp_rmp <- read_csv(glue::glue(save_dir, "/matches_frp_rmp.csv"))
frp_rcra <- read_csv(glue::glue(save_dir, "/matches_frp_rcra.csv"))
rmp_rcra <- read_csv(glue::glue(save_dir, "/matches_rmp_rcra.csv"))

# Select facility IDs -----
frp_rmp_ids <-
  frp_rmp %>%
  select(facility_id_frp,
         epa_facility_id_rmp)

frp_rcra_ids <-
  frp_rcra %>%
  select(facility_id_frp,
         handler_id_rcra)

rmp_rcra_ids <-
  rmp_rcra %>%
  select(epa_facility_id_rmp,
         handler_id_rcra)

# Create crosswalks -----

## Join matched IDs -----

matches_all <-
  frp_rmp_ids %>%
  full_join(frp_rcra_ids, by = "facility_id_frp") %>%
  full_join(rmp_rcra_ids, by = "epa_facility_id_rmp") %>%
  mutate(handler_id_rcra = coalesce(handler_id_rcra.x, handler_id_rcra.y),
         epa_facility_id_rmp = as.character(epa_facility_id_rmp)) %>%
  select(facility_id_frp, epa_facility_id_rmp, handler_id_rcra)

# Concatenate RMP and RCRA IDs
matches_by_frp <-
  matches_all %>%
  group_by(facility_id_frp) %>%
  summarise(
    epa_facility_id_rmp = paste(sort(unique(na.omit(epa_facility_id_rmp))), collapse = "; "),
    handler_id_rcra = paste(sort(unique(na.omit(handler_id_rcra))), collapse = "; "),
    .groups = "drop"
  )
  
## Add all FRP IDs to crosswalk -----

crosswalk_frp <-
  matches_by_frp %>%
  right_join(frp %>% select(facility_id), by = c("facility_id_frp" = "facility_id")) %>%
  arrange(facility_id_frp) %>%
  mutate(across(where(is.character), ~replace_na(., ""))) %>%
  distinct() 

## Get unmatched RMP IDs ----

crosswalk_unmatched_rmp <-
  rmp %>%
  mutate(epa_facility_id_rmp = as.character(epa_facility_id)) %>%
  select(epa_facility_id_rmp) %>%
  anti_join(matches_all, by = "epa_facility_id_rmp") %>%
  arrange(epa_facility_id_rmp) %>%
  distinct()

rmp_ids <-
  matches_all %>%
  select(epa_facility_id_rmp) %>%
  distinct() %>%
  filter(!is.na(epa_facility_id_rmp))

test_rmp <- abs(nrow(rmp) - nrow(crosswalk_unmatched_rmp) - nrow(rmp_ids))
if (test_rmp > 0) {
  print("WARNING: Matched and Unmatched RMP ID counts do not add up correctly")
}

## Get unmatched RCRA IDs -----

crosswalk_unmatched_rcra <-
  rcra %>%
  select(handler_id_rcra = handler_id) %>%
  anti_join(matches_all, by = "handler_id_rcra") %>%
  arrange(handler_id_rcra) %>%
  distinct()

rcra_ids <-
  matches_all %>%
  select(handler_id_rcra) %>%
  distinct() %>%
  filter(!is.na(handler_id_rcra))

test_rcra <- abs(nrow(rcra) - nrow(crosswalk_unmatched_rcra) - nrow(rcra_ids))
if (test_rcra > 0) {
  print("WARNING: Matched and Unmatched RCRA ID counts do not add up correctly")
}

# Save outputs ----
write.csv(crosswalk_frp,
          glue::glue(save_dir, "/frp_crosswalk.csv"), row.names = FALSE)

write.csv(crosswalk_unmatched_rmp,
          glue::glue(save_dir, "/rmp_unmatched.csv"), row.names = FALSE)

write.csv(crosswalk_unmatched_rcra,
          glue::glue(save_dir, "/rcra_unmatched.csv"), row.names = FALSE)
