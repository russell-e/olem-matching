remove_rmp_duplicates <- function(rmp) {
  
  print("Removing RMP duplicates....")
  
  has_conflict <- function(x) {
    length(unique(na.omit(x))) > 1
  }
  
  coords <- rmp %>%
    group_by(epa_facility_id) %>%
    summarise(
      latitude = first(latitude),
      longitude = first(longitude),
      .groups = "drop"
    )
  
  rmp_remove_exact_duplicates <-
    rmp %>%
    # clean other epa facility
    mutate(other_epa_facility_id = str_remove_all(other_epa_facility_id, " "),
           # round lat, lon
           longitude = if_else(longitude > 0, longitude * -1, longitude),
           latitude_round = round(latitude, 1),
           longitude_round = round(longitude, 1)
           ) %>%
    # remove select columns for duplicate identification (most importantly facility ID)
    select(-facility_id, -company_name, -company_name_2, -latitude_corrected, -longitude_corrected, -postal_code_ext, -street_address_2, -longitude, -latitude) %>% 
    # remove exact duplicates
    distinct() %>%
    # count non-na rows
    mutate(non_missing = rowSums(!is.na(.))) %>% 
    group_by(epa_facility_id) %>%
    # merge duplicate rows if no conflicting row information
    group_modify(~{conflict <- any(sapply(select(.x, -non_missing), has_conflict))
    .x$conflict_flag <- conflict
    if (conflict) {.x} 
    else {slice_max(.x, non_missing, n = 1, with_ties = FALSE)}}) %>%
    # create duplicate flag
    mutate(is_duplicate = n() > 1) %>%
    ungroup() %>%
    select(-non_missing, -conflict_flag, -longitude_round, -latitude_round) %>%
    left_join(coords, by = "epa_facility_id")

  rmp_rownums <-
    rmp_remove_exact_duplicates %>%
    mutate(has_coordinates = !is.na(latitude) & !is.na(longitude)) %>%
    rename(name = facility_name,
           state = state_code,
           zip = postal_code,
           addr = street_address_1) %>%
    group_by(epa_facility_id) %>%
    mutate(row_id = row_number()) %>%
    ungroup()
  
  rmp_completeness <-
    rmp_rownums %>%
    mutate(completeness = rowSums(!is.na(
      select(., name, addr, city, state, zip, latitude, longitude))))
  
  rmp_joined_candidates <-
    rmp_rownums %>%
    inner_join(
      rmp_rownums,
      by = "epa_facility_id",
      suffix = c("_A", "_B"),
      relationship = "many-to-many"
    ) %>%
    filter(row_id_A < row_id_B) %>%   # remove self matches and duplicate pairs
    ungroup()
  
  rmp_candidate_scores <-
    rmp_joined_candidates %>%
    mutate(
      exact_name = name_A == name_B,
      exact_addr = addr_A == addr_B,
      exact_name_address = exact_name & exact_addr,
      exact_name_geog = exact_name & (zip_A == zip_B) & (city_A == city_B),
      street_num_A = str_extract(addr_A, "^\\d+"),
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
  
  fuzzy_matches <-
    rmp_candidate_scores %>%
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
      confidence_score = case_when(
        exact_name_address ~ 1.0,
        exact_name_geog ~ 0.9,
        exact_addr ~ 0.98,
        TRUE ~ confidence_score),
      match_type = case_when(
        exact_name_address ~ "exact_name_address",
        exact_addr ~ "exact_address",
        exact_name_geog ~ "exact_name_geography",
        confidence_score >= 0.9 ~ "fuzzy_high_confidence",
        confidence_score >= 0.85 ~ "fuzzy_review",
        TRUE ~ "unmatched")) %>%
    select(-city_match, -county_fips_A, -county_fips_B,
           -dist_m, -dist_score, -geo_available, -geo_score, -contains("has")) %>%
    select(sort(names(.)))
  
  find_similar_clusters <- function(edges, nodes) {
    # start with every node in its own cluster
    cluster <- setNames(seq_along(nodes), nodes)
    changed <- TRUE
    
    while (changed) {
      changed <- FALSE
      for (i in seq_len(nrow(edges))) {
        a <- as.character(edges$row_id_A[i])
        b <- as.character(edges$row_id_B[i])
        m <- min(cluster[a], cluster[b])
        
        if (cluster[a] != m) {
          cluster[a] <- m
          changed <- TRUE
        }
        
        if (cluster[b] != m) {
          cluster[b] <- m
          changed <- TRUE
        }
      }
    }
    
    tibble(
      row_id = as.integer(names(cluster)),
      cluster_id = unname(cluster))
  }
  
  
  duplicate_edges <-
    fuzzy_matches %>%
    filter(confidence_score >= .80)
  
  edge_list <- split(
    duplicate_edges,
    duplicate_edges$epa_facility_id
  )
  
  clustered <-
    rmp_completeness %>%
    group_by(epa_facility_id) %>%
    group_modify(function(rows, key) {
      
      edges <- edge_list[[as.character(key$epa_facility_id)]]
      
      if (is.null(edges) || nrow(edges) == 0) {
        rows %>%
          mutate(cluster_id = row_id)
        
      } else {
        clusters <-
          find_similar_clusters(
            edges,
            rows$row_id
          )
        
        rows %>%
          left_join(clusters, by = "row_id")
        
      }
    }) %>%
    ungroup()
  
  final_df <-
    clustered %>%
    group_by(epa_facility_id, cluster_id) %>%
    slice_max(completeness,
              n = 1,
              with_ties = FALSE) %>%
    ungroup()
  
  print(glue::glue("Successfully reduced RMP records from {nrow(rmp)} to {nrow(final_df)}"))
  
  return(final_df)
}