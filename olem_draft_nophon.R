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

source("G:/OLEM/function_clean_raw_data.R")

cleaned_data <- clean_raw_data()

frp <- cleaned_data$frp
rmp <- cleaned_data$rmp
rcra <- cleaned_data$rcra
rm(cleaned_data)
gc()

# frp_ids <-
#   frp %>%
#   count(facility_id) %>%
#   filter(!is.na(facility_id)) %>%
#   glimpse
# 
# rmp_ids <-
#   rmp %>%
#   count(facility_id) %>%
#   filter(!is.na(facility_id)) %>%
#   glimpse()
# 
# rcra_ids <-
#   rcra %>%
#   count(facility_id) %>%
#   filter(!is.na(facility_id)) %>%
#   glimpse()

# Match facilities -----

frp_matching <- 
  frp %>%
  mutate(has_coordinates = !is.na(latitude) & !is.na(longitude)) %>%
  rename(name = facility_name,
         state = state_code,
         zip = postal_code,
         addr = street_address_1)

rmp_matching <- 
  rmp %>%
  mutate(has_coordinates = !is.na(latitude) & !is.na(longitude)) %>%
  rename(name = facility_name,
         state = state_code,
         zip = postal_code,
         addr = street_address_1)

rcra_matching <-
  rcra %>%
  mutate(has_coordinates = !is.na(latitude) & !is.na(longitude)) %>%
  rename(name = facility_name,
         state = state_code,
         zip = postal_code,
         addr = street_address_1)
rm(frp, rmp, rcra)

## FRP and RMP ----
facility_A <- rmp_matching
facility_B <- rcra_matching
rm(frp_matching, rmp_matching, rcra_matching)

suffix_A <- "_A"
suffix_B <- "_B"

final_suffix_A <- "_rmp"
final_suffix_B <- "_rcra"
###  Identify exact matches -----

exact_id_matches <-
  facility_A %>%
  inner_join(facility_B, by = c("other_epa_facility_id" = "facility_id"), suffix = c(suffix_A, suffix_B)) %>%
  filter(!is.na(facility_id)) %>%
  mutate(facility_id_B = other_epa_facility_id,
         match_type = "exact_id") %>%
  rename("facility_id{suffix_A}" := "facility_id")

exact_name_address_matches <- 
  facility_A %>%
  inner_join(facility_B, by = c("name", "addr"), suffix = c(suffix_A, suffix_B)) %>%
  filter(!is.na(name) & !is.na(addr)) %>%
  mutate(name_B = name,
         addr_B = addr,
         match_type = "exact_name_address") %>%
  rename("name{suffix_A}" := "name",
         "addr{suffix_A}" = "addr")

exact_name_matches_geog_verified <- 
  facility_A %>%
  inner_join(facility_B, by = "name", suffix = c(suffix_A, suffix_B)) %>%
  filter(!is.na(name)) %>%
  filter(zip_A == zip_B & city_A == city_B) %>%
  mutate(name_B = name,
         match_type = "exact_name_geog") %>%
  rename("name{suffix_A}" := "name") 

exact_address_matches <- 
  facility_A %>%
  inner_join(facility_B, by = "addr", suffix = c(suffix_A, suffix_B)) %>%
  filter(!is.na(addr)) %>%
  mutate(addr_B = addr,
         match_type = "exact_address") %>%
  rename("addr{suffix_A}" := "addr")

exact_matches <- 
  bind_rows(
    exact_id_matches %>% mutate(priority = 1),
    exact_name_address_matches %>% mutate(priority = 2),
    exact_name_matches_geog_verified %>% mutate(priority = 4),
    exact_address_matches %>% mutate(priority = 3)) %>%
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

# fac_A_block3 <- 
  # facility_A %>%
  # mutate(block = paste(state, soundex(name)))

fac_A_block4 <- 
  facility_A %>%
  mutate(block = paste(state, word(name, 1)))

fac_B_block1 <- 
  facility_B %>%
  mutate(block = paste(state, zip, substr(name, 1, 4)))

fac_B_block2 <- 
  facility_B %>%
  mutate(block = paste(state, city, substr(name, 1, 4)))

# fac_B_block3 <- 
#   facility_B %>%
#   mutate(block = paste(state, soundex(name)))

fac_B_block4 <- 
  facility_B %>%
  mutate(block = paste(state, word(name, 1)))

gc()

# aggregate fuzzy match candidate
c1 <- inner_join(fac_A_block1, fac_B_block1, by = "block", suffix = c(suffix_A, suffix_B))
rm(fac_A_block1, fac_B_block1)
c2 <- inner_join(fac_A_block2, fac_B_block2, by = "block", suffix = c(suffix_A, suffix_B))
rm(fac_A_block2, fac_B_block2)
# c3 <- inner_join(fac_A_block3, fac_B_block3, by = "block", suffix = c(suffix_A, suffix_B))
# rm(fac_A_block3, fac_B_block3)
c4 <- inner_join(fac_A_block4, fac_B_block4, by = "block", suffix = c(suffix_A, suffix_B))
rm(fac_A_block4, fac_B_block4)

gc()

candidates_scores <- 
  # bind_rows(c1, c2, c3, c4) %>%
  bind_rows(c1, c2, c4) %>%
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
    exact_matches) %>%
  select(facility_id_A, 
         facility_id_B,
         match_type,
         confidence_score,
         name_A, name_B,
         addr_A, addr_B,
         city_A, city_B,
         state_A, state_B,
         zip_A, zip_B,
         latitude_A, latitude_B,
         longitude_A, longitude_B,
         epa_facility_id,
         other_epa_facility_id,
         # frp_id
         ) 

rm(fuzzy_matches, exact_matches)

# pick exact over fuzzy match if duplicates exist based on confidence score
best_matches <-
  all_matches %>%
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

accepted_match_cat_facid_b <-
  accepted_matches %>%
  group_by(facility_id_B) %>%
  arrange(facility_id_B, match_category) %>%
  slice(1) %>%
  group_by(match_category) %>%
  count() %>%
  print()

# count match type for ID
accepted_match_type_facid_a <-
  accepted_matches %>%
  group_by(facility_id_A) %>%
  arrange(facility_id_A, match_type) %>%
  slice(1) %>%
  group_by(match_type) %>%
  count() %>%
  print()

accepted_match_type_facid_b <-
  accepted_matches %>%
  group_by(facility_id_B) %>%
  arrange(facility_id_B, match_type) %>%
  slice(1) %>%
  group_by(match_type) %>%
  count() %>%
  print()

# format final matching
final_matches <-
  accepted_matches %>%
  mutate(confidence_score = round(confidence_score, 4)) %>%
  relocate(match_category, .after = match_type) %>%
  rename_with(~ .x %>% 
                gsub(suffix_A, final_suffix_A, .) %>%
                gsub(suffix_B, final_suffix_B, .))

write.csv(final_matches, 
          glue::glue("G:/OLEM/output_data/matches", final_suffix_A, final_suffix_B, "_nophon.csv"), row.names = FALSE)
