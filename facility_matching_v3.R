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

# Clean data -----

source("gdrive/OLEM/olem-matching/function_clean_raw_data.R")
source("gdrive/OLEM/olem-matching/function_clean_rmp_duplicates.R")

cleaned_data <- clean_raw_data()

frp <- cleaned_data$frp
rmp <- cleaned_data$rmp
rcra <- cleaned_data$rcra
gc()

# remove duplicates
rmp_cleaned_duplicates <- remove_rmp_duplicates(rmp) #11491

# DEFINE FILES FOR MATCHING - REQUIRES USER INPUT - RUN FROM HERE -----
rm(list = setdiff(ls(), c("frp", "rcra","rmp_cleaned_duplicates")))
gc()
facility_A_name <- "rmp" # frp or rmp
facility_B_name <- "rcra" #rmp or rcra

# rmp_cleaned_duplicates <-
#   rmp %>%
#   # select(-facility_id) %>%
#   rename(name = facility_name,
#          state = state_code,
#          zip = postal_code,
#          addr = street_address_1)
# rm(rmp)

# number of RMP values that still have duplicates
# rmp_duplicates <-
#  rmp_cleaned_duplicates %>%
#  count(epa_facility_id) %>%
#   glimpse()
#  filter(n > 1) %>%
#  glimpse()
#    
# # unique FRP facility ids
# frp_ids <-
#  frp %>%
#  count(facility_id) %>%
#  filter(!is.na(facility_id)) %>%
#  glimpse
# 
# rmp_ids <-
#  rmp_cleaned_duplicates %>%
#  count(epa_facility_id) %>%
#  filter(!is.na(epa_facility_id)) %>%
#  glimpse()
# 
# rcra_ids <-
#  rcra %>%
#  count(facility_id) %>%
#  filter(!is.na(facility_id)) %>%
#  glimpse()

# Match facilities -----

frp_matching <- 
  frp %>%
  mutate(has_coordinates = !is.na(latitude) & !is.na(longitude)) %>%
  rename(name = facility_name,
         state = state_code,
         zip = postal_code,
         addr = street_address_1)

rmp_matching <- 
  rmp_cleaned_duplicates %>%
  mutate(has_coordinates = !is.na(latitude) & !is.na(longitude)) %>%
  rename("facility_id" = "epa_facility_id") # set epa facility ID to main id

rcra_matching <-
  rcra %>%
  mutate(has_coordinates = !is.na(latitude) & !is.na(longitude)) %>%
  rename(name = facility_name,
         state = state_code,
         zip = postal_code,
         addr = street_address_1)

## FRP and RMP ----
facility_A <- get(paste0(facility_A_name, "_matching"))
facility_B <- get(paste0(facility_B_name, "_matching"))
rm(frp_matching, rmp_matching, rcra_matching)

suffix_A <- "_A"
suffix_B <- "_B"
final_suffix_A <- paste0("_", facility_A_name)
final_suffix_B <- paste0("_", facility_B_name)

###  Identify exact matches -----
if(facility_A_name == "rmp" & facility_B_name == "rcra") {
  exact_id_matches <-
    facility_A %>%
    inner_join(facility_B, by = c("other_epa_facility_id" = "facility_id"), suffix = c(suffix_A, suffix_B)) %>%
    filter(!is.na(facility_id)) %>%
    mutate(facility_id_B = other_epa_facility_id,
           match_type = "exact_id") %>%
    rename("facility_id{suffix_A}" := "facility_id")
}

exact_name_address_matches <- 
  facility_A %>%
  inner_join(facility_B, by = c("name", "addr"), suffix = c(suffix_A, suffix_B)) %>%
  filter(!is.na(name) & !is.na(addr)) %>%
  mutate(name_B = name,
         addr_B = addr,
         match_type = "exact_name_address") %>%
  rename("name{suffix_A}" := "name",
         "addr{suffix_A}" := "addr")

exact_name_matches_geog_verified <- 
  facility_A %>%
  inner_join(facility_B, by = "name", suffix = c(suffix_A, suffix_B), relationship = "many-to-many") %>%
  filter(!is.na(name)) %>%
  filter(zip_A == zip_B & city_A == city_B) %>%
  mutate(name_B = name,
         match_type = "exact_name_geog") %>%
  rename("name{suffix_A}" := "name") 

exact_address_matches <- 
  facility_A %>%
  inner_join(facility_B, by = "addr", suffix = c(suffix_A, suffix_B), relationship = "many-to-many") %>%
  filter(!is.na(addr)) %>%
  filter(city_A == city_B & state_A == state_B) %>%
  mutate(addr_B = addr,
         match_type = "exact_address") %>%
  rename("addr{suffix_A}" := "addr")

if(facility_A_name == "rmp" & facility_B_name == "rcra") {
  exact_matches_bind <-
    bind_rows(
      exact_id_matches %>% mutate(priority = 1),
      exact_name_address_matches %>% mutate(priority = 2),
      exact_name_matches_geog_verified %>% mutate(priority = 4),
      exact_address_matches %>% mutate(priority = 3))
} else {
  exact_matches_bind <-
    bind_rows(
      exact_name_address_matches %>% mutate(priority = 1),
      exact_name_matches_geog_verified %>% mutate(priority = 3),
      exact_address_matches %>% mutate(priority = 2))
}

exact_matches <- 
  exact_matches_bind %>%
  arrange(facility_id_A, priority) %>% 
  group_by(facility_id_A, facility_id_B) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(confidence_score = case_when(
    match_type == "exact_id" ~ 1.0,
    match_type == "exact_name_address" ~ 1.0,
    match_type == "exact_name_geog" ~0.9,
    match_type == "exact_address" ~ 0.98,
    TRUE ~ NA_real_)) %>%
  select(order(names(.)), -priority) 

rm(exact_name_address_matches, exact_address_matches, exact_name_matches_geog_verified)
gc()

### Identify fuzzy matches ------

# create blocks to reduce candidates and improve performance
fac_A_block1 <- 
  facility_A %>%
  mutate(block = paste(state, zip, substr(name, 1, 4)))

fac_A_block2 <- 
  facility_A %>%
  mutate(block = paste(state, city, substr(name, 1, 4)))

fac_A_block4 <- 
  facility_A %>%
  mutate(block = paste(state, word(name, 1)))

fac_B_block1 <- 
  facility_B %>%
  mutate(block = paste(state, zip, substr(name, 1, 4)))

fac_B_block2 <- 
  facility_B %>%
  mutate(block = paste(state, city, substr(name, 1, 4)))

fac_B_block4 <- 
  facility_B %>%
  mutate(block = paste(state, word(name, 1)))

gc()

# aggregate fuzzy match candidate
c1 <- inner_join(fac_A_block1, fac_B_block1, by = "block", suffix = c(suffix_A, suffix_B))
rm(fac_A_block1, fac_B_block1)
c2 <- inner_join(fac_A_block2, fac_B_block2, by = "block", suffix = c(suffix_A, suffix_B))
rm(fac_A_block2, fac_B_block2)
c4 <- inner_join(fac_A_block4, fac_B_block4, by = "block", suffix = c(suffix_A, suffix_B))
rm(fac_A_block4, fac_B_block4)

gc()

if (facility_B_name == "rcra") {
  
  candidates_bind <-
    bind_rows(c1, c2, c4)
  
} else {
  
  fac_A_block3 <-
    facility_A %>%
    mutate(block = paste(state, soundex(name)))
  
  fac_B_block3 <-
    facility_B %>%
    mutate(block = paste(state, soundex(name)))
  
  c3 <- inner_join(fac_A_block3, fac_B_block3, by = "block", suffix = c(suffix_A, suffix_B))
  rm(fac_A_block3, fac_B_block3)
  
  candidates_bind <-
    bind_rows(c1, c2, c3, c4)
}

candidates_scores <- 
  candidates_bind %>%
  distinct(facility_id_A, facility_id_B, .keep_all = TRUE) %>%
  # split address into street number and street
  mutate(street_num_A = str_extract(addr_A, "^\\d+"),
         street_num_B = str_extract(addr_B, "^\\d+"),
         street_name_A = str_trim(str_remove(addr_A, "^\\d+\\s*")),
         street_name_B = str_trim(str_remove(addr_B, "^\\d+\\s*")),
    has_coords = has_coordinates_A & has_coordinates_B,
    # fuzzy matching scores
    name_sim = stringsim(name_A, name_B, method = "jw"),
    street_name_sim = stringsim(street_name_A, street_name_B, method = "jw"),
    # street number difference score
    street_gate = coalesce(street_name_sim, 0)^2, # create a street name similarity threshold
    street_num_diff = abs(as.numeric(street_num_A) - as.numeric(street_num_B)),
    street_num_score = street_gate * case_when(street_num_diff == 0 ~ 1.00,
                                         street_num_diff <= 2 ~ 0.95,
                                         street_num_diff <= 5 ~ 0.80,
                                         street_num_diff <= 10 ~ 0.60,
                                         TRUE ~ 0.00),
    # direct matching on location column scores
    state_match = if_else(is.na(state_A) | is.na(state_B), 
                          NA_real_,  
                          as.integer(state_A == state_B)),
    city_match  = if_else(is.na(city_A) | is.na(city_B), 
                          NA_real_,  
                          as.integer(city_A == city_B)),
    zip_match   = if_else(is.na(zip_A) | is.na(zip_B), 
                          NA_real_,  
                          as.integer(zip_A == zip_B)),
    # latitude, longitude difference
    dist_m = case_when(has_coords ~
                         geosphere::distHaversine(cbind(longitude_A, latitude_A),
                                                  cbind(longitude_B, latitude_B)),
                       TRUE ~ NA_real_),
    dist_score = if_else(has_coords, exp(-dist_m / 5000), NA_real_),
    # data availability flags
    has_state = !is.na(state_match),
    has_city = !is.na(city_match))

rm(c1, c2, c3, c4)
gc()

fuzzy_matches <-
  candidates_scores %>%
  mutate(
    # summed street and geography scores
    street_score = 1 - (1 - street_name_sim) * (1 - street_num_score),
    geo_score = case_when(has_city | has_state ~
                            (0.6 * coalesce(city_match, 0) +
                               0.4 * coalesce(state_match, 0)) /
                            (0.6 * has_city + 0.4 * has_state),
                          TRUE ~ NA_real_),
    geo_available = !is.na(geo_score),
    # total score
    total_sum = 0.50 + 0.30 + 0.15 * has_coords + 0.05 * geo_available,
    total_raw = 0.50 * name_sim +
      0.30 * street_score +
      0.15 * if_else(has_coords, dist_score, 0) +
      0.05 * if_else(geo_available, geo_score, 0),
    confidence_score = total_raw / total_sum,
    match_type = case_when(
      confidence_score >= 0.9 ~ "fuzzy_high_confidence",
      confidence_score >= 0.85 ~ "fuzzy_review",
      TRUE ~ "unmatched")) %>%
  filter(confidence_score >= 0.85)

rm(candidates_scores)
gc()

### Combine exact matches with fuzzy matches ------

all_matches <-
  bind_rows(
    fuzzy_matches,
    exact_matches)

base_cols_clean <- c(
  "facility_id_A", 
  "facility_id_B",
  "match_type",
  "confidence_score",
  "name_A", "name_B",
  "addr_A", "addr_B",
  "city_A", "city_B",
  "state_A", "state_B",
  "zip_A", "zip_B",
  "latitude_A", "latitude_B",
  "longitude_A", "longitude_B"
)

if(facility_A_name == "rmp" | facility_B_name == "rmp") {
  base_cols_clean <- append(base_cols_clean, list("other_epa_facility_id_rmp" = "other_epa_facility_id"))
} 

if (facility_A_name == "frp") {
  base_cols_clean <- append(base_cols_clean, list("frp_id"))
}

all_matches_clean <-
  all_matches %>%
  select(all_of(unlist(base_cols_clean)))

rm(fuzzy_matches, exact_matches)

# pick exact over fuzzy match if duplicates exist based on confidence score
best_matches <-
  all_matches_clean %>%
  group_by(facility_id_A, facility_id_B) %>%
  slice_max(confidence_score, with_ties = FALSE) %>%
  arrange(facility_id_A, facility_id_B) %>%
  ungroup()

rm(all_matches)

# filter matches to those exceeding 85%
accepted_matches <-
  best_matches %>%
  mutate(match_category = case_when(
             str_detect(match_type, "exact") ~ "exact",
             str_detect(match_type, "fuzzy") ~ "fuzzy",
             TRUE ~ NA_character_))

rm(best_matches)

# count match category for ID
accepted_match_cat_facid_a <-
  accepted_matches %>%
  group_by(facility_id_A) %>%
  arrange(facility_id_A, match_category) %>%
  slice(1) %>%
  group_by(match_category) %>%
  count() %>%
  print()

write.csv(accepted_match_cat_facid_a,
          glue::glue("gdrive/OLEM/olem-matching/output_data/matches", final_suffix_A, final_suffix_B, "_facilityid", final_suffix_A, "_count_v2.csv"), row.names = FALSE)

accepted_match_cat_facid_b <-
  accepted_matches %>%
  group_by(facility_id_B) %>%
  arrange(facility_id_B, match_category) %>%
  slice(1) %>%
  group_by(match_category) %>%
  count() %>%
  print()
write.csv(accepted_match_cat_facid_b,
          glue::glue("gdrive/OLEM/olem-matching/output_data/matches", final_suffix_A, final_suffix_B, "_facilityid", final_suffix_B, "_count_v2.csv"), row.names = FALSE)

# count match type for ID
accepted_match_type_facid_a <-
  accepted_matches %>%
  group_by(facility_id_A) %>%
  arrange(facility_id_A, match_type) %>%
  slice(1) %>%
  group_by(match_type) %>%
  count() %>%
  print()
write.csv(accepted_match_type_facid_a,
          glue::glue("gdrive/OLEM/olem-matching/output_data/matches", final_suffix_A, final_suffix_B, "_facilityid", final_suffix_A, "_matchtype_v2.csv"), row.names = FALSE)

accepted_match_type_facid_b <-
  accepted_matches %>%
  group_by(facility_id_B) %>%
  arrange(facility_id_B, match_type) %>%
  slice(1) %>%
  group_by(match_type) %>%
  count() %>%
  print()
write.csv(accepted_match_type_facid_b,
          glue::glue("gdrive/OLEM/olem-matching/output_data/matches", final_suffix_A, final_suffix_B, "_facilityid", final_suffix_B, "_matchtype_v2.csv"), row.names = FALSE)

if (facility_A_name == "rmp") {
  accepted_matches_adj <-
    accepted_matches %>%
    rename("epa_facility_id_A" = "facility_id_A",
           "handler_id_B" = "facility_id_B")
} else if(facility_B_name == "rmp") {
    accepted_matches_adj <-
      accepted_matches %>%
      rename("epa_facility_id_B" = "facility_id_B")
} else if (facility_B_name == "rcra") {
  accepted_matches_adj <-
    accepted_matches %>%
    rename("handler_id_B" = "facility_id_B")
} else {
  accepted_matches_adj <-
    accepted_matches
}

# format final matching
final_matches <-
  accepted_matches_adj %>%
  mutate(confidence_score = round(confidence_score, 4)) %>%
  mutate(across(where(is.character), ~replace_na(., ""))) %>%
  relocate(match_category, .after = match_type) %>%
  rename_with(~ .x %>% 
                gsub(suffix_A, final_suffix_A, .) %>%
                gsub(suffix_B, final_suffix_B, .))

write.csv(final_matches,
          glue::glue("gdrive/OLEM/olem-matching/output_data/matches", final_suffix_A, final_suffix_B, "_v3.csv"), row.names = FALSE)
