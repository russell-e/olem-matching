library(dplyr)
library(tidyr)
library(igraph)
library(readr)
library(stringr)

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

library(dplyr)
library(tidyr)
library(stringr)

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

write.csv(crosswalk, 
          glue::glue("gdrive/OLEM/olem-matching/output_data/frp_rmp_rcra_crosswalk.csv"), row.names = FALSE)