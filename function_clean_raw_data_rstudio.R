clean_raw_data <-
  function(){
    # FRP -----
    ## Import data ----
    print("Cleaning FRP data...")
    frp_file <- "gdrive/OLEM/raw_data/SPCC and FRP data.xlsx"
    frp_sheets <- excel_sheets(frp_file)
    
    ## Load and clean R9 sheet -----
    
    state_lookup <- tibble(
      state = state.name,
      state_abb = state.abb)
    
    frp_r9 <-
      read_excel(frp_file, sheet = "R9", na = c("", "NA")) %>%
      janitor::clean_names()
    
    frp_r9_clean <-
      frp_r9 %>%
      # rename misspelled column
      rename("longitude" = "longitide",
             "address" = "facility_address") %>%
      # separate address into parts
      mutate(postal_code = str_extract(address, "[^,]+$"),
             temp = str_remove(address, ",\\s*[^,]+$"),
             state = str_extract(temp, "[^,]+$"),
             temp = str_remove(temp, ",\\s*[^,]+$"),
             city = str_extract(temp, "[^,]+$"),
             street = str_remove(temp, ",\\s*[^,]+$")) %>%
      # replace city, state, zip when street is assigned to all address parts
      mutate(across(
        c(city, state, postal_code),
        ~ if_else(. == street, NA_character_, .))) %>%
      # manage outlier
      mutate(
        street = if_else(street == "4301 W Jefferson St. Phoenix AZ 85043", "4301 W Jefferson St.", street),
        city = if_else(address == "4301 W Jefferson St. Phoenix AZ 85043", "Phoenix", city),
        state = if_else(address == "4301 W Jefferson St. Phoenix AZ 85043", "Arizona", state),
        postal_code = if_else(address == "4301 W Jefferson St. Phoenix AZ 85043", "85043", postal_code)) %>%
      # manage improper zips with state abbreviations
      mutate(has_state_zip = str_detect(postal_code, "\\b[A-Z]{2}\\s\\d{5}\\b")) %>%
      mutate(street = if_else(has_state_zip & !is.na(city), paste(street, city, sep = " "), street),
             city = if_else(has_state_zip, state, city),
             state = if_else(has_state_zip, str_extract(postal_code, "^\\s*[A-Z]{2}"), state),
             postal_code = if_else(has_state_zip, str_extract(postal_code, "\\d{5}$"), postal_code)) %>%
      # remove leading or succeeding spaces from characters
      mutate(across(where(is.character), ~ trimws(.x))) %>%
      # get state abbreviations
      left_join(state_lookup, by = "state") %>%
      mutate(state_code = if_else(!is.na(state_abb), state_abb, state),  # get state code or name if no abbreviation
             is_subject_to_frp = "YES", # assuming we want all R9 facilities
             harm_category = case_when(
               sub_or_sig_sub == "SUB" ~ "Substantial",
               sub_or_sig_sub == "SIG&SUB" ~ "Significant & Substantial",
               TRUE ~ sub_or_sig_sub)) %>% 
      select(facility_id, frp_id, latitude, longitude, facility_name, state_code,
             harm_category, street_address_1 = street, city, postal_code, is_subject_to_frp)
    
    ## Combine FRP data -----
    # combine all but R9
    frp_sheets_reg <- frp_sheets[frp_sheets != "R9"]
    frp_combined_reg <- 
      bind_rows(lapply(frp_sheets_reg, function(sheet) {
        read_excel(frp_file, sheet = sheet, na = c("", "NA"))})) %>%
      janitor::clean_names() %>% # clean column names
      janitor::remove_empty(which = "cols") %>% # remove empty columns
      select(-is_subject_to_spcc)
    
    # add R9 data
    frp_combined <-
      frp_combined_reg %>%
      bind_rows(frp_r9_clean)
    
    ## Filter for true FRP flag ----
    
    frp_flag_options <-
      frp_combined %>%
      select(is_subject_to_frp) %>%
      group_by(is_subject_to_frp) %>%
      count()
    
    frp_true <-
      frp_combined %>%
      filter(is_subject_to_frp == "YES") %>%
      select(-is_subject_to_frp) %>%
      distinct()
    
    ## Clean the character data and normalize address -----
    address_replacements <- c(
      "NORTH" = "N",
      "SOUTH" = "S",
      "EAST" = "E",
      "WEST" = "W",
      "NORTHEAST" = "NE",
      "NORTHWEST" = "NW",
      "SOUTHEAST" = "SE",
      "SOUTHWEST" = "SW",
      "STREET" = "ST",
      "AVENUE" = "AVE",
      "ROAD" = "RD",
      "DRIVE" = "DR",
      "BOULEVARD" = "BLVD",
      "LANE" = "LN",
      "COURT" = "CT",
      "CIRCLE" = "CIR",
      "PLACE" = "PL",
      "TERRACE" = "TER",
      "PARKWAY" = "PKWY",
      "HIGHWAY" = "HWY",
      "TRAIL" = "TRL",
      "APARTMENT" = "APT",
      "SUITE" = "STE",
      "BUILDING" = "BLDG",
      "FLOOR" = "FL",
      "ROOM" = "RM",
      "FIRST" = "1ST",
      "SECOND" = "2ND",
      "THIRD" = "3RD",
      "FOURTH" = "4TH",
      "FIFTH" = "5TH",
      "SIXTH" = "6TH",
      "SEVENTH" = "7TH",
      "EIGHTH" = "8TH",
      "NINTH" = "9TH",
      "TENTH" = "10TH"
    )
    
    normalize_address <- function(x) {
      x <- toupper(x)
      x <- str_replace_all(x, "[[:punct:]]", " ")
      for (i in seq_along(address_replacements)) {
        pattern <- paste0("\\b", names(address_replacements)[i], "\\b")
        x <- str_replace_all(x, pattern, address_replacements[i])
      }
      x <- str_squish(x)
      x
    }
    
    fix_coord <- function(x) {
      sapply(x, function(v) {
        if (is.na(v)) return(NA_real_)
        
        while (abs(v) > 180) {
          v <- v / 10
        }
        v
      })
    }
    
    frp_clean_strings <-
      frp_true %>%
      # remove punctuation
      mutate(across(where(is.character), ~ str_remove_all(.x, "[[:punct:]]"))) %>%
      # convert to uppercase
      mutate(across(where(is.character), str_to_upper)) %>%
      # normalize address words
      mutate(street_address_1 = normalize_address(street_address_1)) %>%
      # remove extra spaces
      mutate(across(where(is.character), ~ str_squish(.x))) %>%
      # replace 0.0 lat, lon with NA
      mutate(across(c(latitude, longitude), ~ if_else(. == 0, NA_real_, .))) %>%
      # identify incorrect lat, lon
      mutate(latitude_corrected = abs(latitude) > 90,
             longitude_corrected = abs(longitude) > 180,
             latitude = if_else(abs(latitude) > 90, fix_coord(latitude), latitude),
             longitude = if_else(abs(longitude) > 180, fix_coord(longitude), longitude))
    
    ## Remove duplicates -----
    
    frp_duplicates <-
      frp_clean_strings %>%
      group_by(facility_id) %>%
      filter(n() > 1)
    
    has_conflict <- function(x) {
      length(unique(na.omit(x))) > 1
    }
    
    if (nrow(frp_duplicates) > 1){
      frp_remove_exact_duplicates <-
        frp_clean_strings %>%
        # remove company name for duplicate identification
        select(-company_name) %>% 
        # remove exact duplicates
        distinct() %>%
        # count non-na rows
        mutate(non_missing = rowSums(!is.na(.))) %>% 
        group_by(facility_id) %>%
        # merge duplicate rows if no conflicting row information
        group_modify(~{conflict <- any(sapply(select(.x, -non_missing), has_conflict))
        .x$conflict_flag <- conflict
        if (conflict) {.x} 
        else {slice_max(.x, non_missing, n = 1, with_ties = FALSE)}}) %>%
        # create duplicate flag
        mutate(is_duplicate = n() > 1) %>%
        ungroup() %>%
        select(-non_missing, -conflict_flag)
      
      frp_for_match <-
        frp_remove_exact_duplicates
    } else {
      frp_for_match <-
        frp_clean_strings
    }
    
    print(glue::glue(nrow(frp_for_match), " cleaned FRP records"))
    
    # RMP -----
    
    print("Cleaning RMP data...")
    ## Import data -----
    rmp_file <- "gdrive/OLEM/raw_data/tblS1Facilities.txt"
    rmp_data <- read_csv(rmp_file, show_col_types = FALSE, na = c("NA", "")) %>% 
      janitor::clean_names() %>%
      janitor::remove_empty(which = "cols") # remove empty columns

    rmp_data_select <-
      rmp_data %>%
      mutate(facility_id = as.character(facility_id)) %>%
      select(facility_id, 
             epa_facility_id, 
             other_epa_facility_id, 
             latitude = facility_lat_dec_degs, 
             longitude = facility_long_dec_degs,
             facility_name, 
             state_code = facility_state, 
             street_address_1 = facility_str1,
             street_address_2 = facility_str2, 
             city = facility_city, 
             postal_code = facility_zip_code, 
             postal_code_ext = facility4digit_zip_ext,
             county_fips = facility_county_fips, 
             company_name = parent_company_name,
             company_name_2 = company2name) %>%
      mutate(latitude = as.double(latitude))
    
    ## Clean data characters -----
    rmp_clean_strings <-
      rmp_data_select %>%
      # remove punctuation
      mutate(across(where(is.character), ~ str_remove_all(.x, "[[:punct:]]"))) %>%
      # convert to uppercase
      mutate(across(where(is.character), str_to_upper)) %>%
      # normalize address words
      mutate(street_address_1 = normalize_address(street_address_1)) %>%
      # remove extra spaces
      mutate(across(where(is.character), ~ str_squish(.x))) %>%
      # replace 0.0 lat, lon with NA
      mutate(across(c(latitude, longitude), ~ if_else(. == 0, NA_real_, .))) %>%
      # identify incorrect lat, lon
      mutate(latitude_corrected = abs(latitude) > 90,
             longitude_corrected = abs(longitude) > 180,
             latitude = if_else(abs(latitude) > 90, fix_coord(latitude), latitude),
             longitude = if_else(abs(longitude) > 180, fix_coord(longitude), longitude))
    
    # Remove duplicates -----
    
    rmp_duplicates <-
      rmp_clean_strings %>%
      group_by(facility_id) %>%
      filter(n() > 1)
    
    if (nrow(rmp_duplicates) > 1){
      rmp_remove_exact_duplicates <-
        rmp_clean_strings %>%
        # remove company name for duplicate identification
        select(-company_name, -company_name_2) %>% 
        # remove exact duplicates
        distinct() %>%
        # count non-na rows
        mutate(non_missing = rowSums(!is.na(.))) %>% 
        group_by(facility_id) %>%
        # merge duplicate rows if no conflicting row information
        group_modify(~{conflict <- any(sapply(select(.x, -non_missing), has_conflict))
        .x$conflict_flag <- conflict
        if (conflict) {.x} 
        else {slice_max(.x, non_missing, n = 1, with_ties = FALSE)}}) %>%
        # create duplicate flag
        mutate(is_duplicate = n() > 1) %>%
        ungroup() %>%
        select(-non_missing, -conflict_flag)
      
      rmp_for_match <-
        rmp_remove_exact_duplicates
    } else {
      rmp_for_match <-
        rmp_clean_strings
    }
    
    print(glue::glue(nrow(rmp_for_match), " cleaned RMP records"))
    
    # RCRA -----
    print("Cleaning RCRA data...")
    
    ## Import data -----
    rcra_1 <-"gdrive/OLEM/raw_data/HD_REPORTING_0.csv"
    rcra_2 <- "gdrive/OLEM/raw_data/HD_REPORTING_1.csv"
    
    rcra_data_1 <- read_csv(rcra_1, show_col_types = FALSE, na = c("NA", ""))
    rcra_data_2 <- read_csv(rcra_2, show_col_types = FALSE, na = c("NA", ""))
    
    rcra_data <-
      bind_rows(rcra_data_1, rcra_data_2) %>%
      janitor::clean_names() %>%
      janitor::remove_empty(which = "cols") # remove empty columns

    rcra_data_select <-
      rcra_data %>%
      mutate(street = if_else(is.na(location_street_no), 
                              location_street1, 
                              paste(location_street_no, location_street1, sep = " "))) %>%
      separate(location_zip, 
               into = c("postal_code", "postal_code_ext"), 
               sep = "-",
               fill = "right", 
               remove = FALSE) %>%
      select(facility_id = handler_id, 
             latitude = location_latitude, 
             longitude = location_longitude,
             facility_name = handler_name, 
             state_code = location_state, 
             street_address_1 = street,
             street_address_2 = location_street2, 
             city = location_city, 
             postal_code,
             postal_code_ext,
             county_fips = location_county_code, 
             county = location_county_name)
    
    rcra_clean_strings <-
      rcra_data_select %>%
      # remove punctuation
      mutate(across(where(is.character), ~ str_remove_all(.x, "[[:punct:]]"))) %>%
      # convert to uppercase
      mutate(across(where(is.character), str_to_upper)) %>%
      # normalize address words
      mutate(street_address_1 = normalize_address(street_address_1)) %>%
      # remove extra spaces
      mutate(across(where(is.character), ~ str_squish(.x))) %>%
      # replace 0.0 lat, lon with NA
      mutate(across(c(latitude, longitude), ~ if_else(. == 0, NA_real_, .))) %>%
      # identify incorrect lat, lon
      mutate(latitude_corrected = abs(latitude) > 90,
             longitude_corrected = abs(longitude) > 180,
             latitude = if_else(abs(latitude) > 90, fix_coord(latitude), latitude),
             longitude = if_else(abs(longitude) > 180, fix_coord(longitude), longitude))
    
    # Remove duplicates -----
    
    rcra_duplicates <-
      rcra_clean_strings %>%
      group_by(facility_id) %>%
      filter(n() > 1)
    
    if (nrow(rcra_duplicates) > 1){
      rcra_remove_exact_duplicates <-
        rcra_clean_strings %>%
        # remove company name for duplicate identification
        select(-company_name, -company_name_2) %>% 
        # remove exact duplicates
        distinct() %>%
        # count non-na rows
        mutate(non_missing = rowSums(!is.na(.))) %>% 
        group_by(facility_id) %>%
        # merge duplicate rows if no conflicting row information
        group_modify(~{conflict <- any(sapply(select(.x, -non_missing), has_conflict))
        .x$conflict_flag <- conflict
        if (conflict) {.x} 
        else {slice_max(.x, non_missing, n = 1, with_ties = FALSE)}}) %>%
        # create duplicate flag
        mutate(is_duplicate = n() > 1) %>%
        ungroup() %>%
        select(-non_missing, -conflict_flag)
      
      rcra_for_match <-
        rcra_remove_exact_duplicates
    } else {
      rcra_for_match <-
        rcra_clean_strings
    }
    
    print(glue::glue(nrow(rcra_for_match), " cleaned RCRA records"))
    
    
    return(list(frp = frp_for_match,
                rmp = rmp_for_match,
                rcra = rcra_for_match))
    
}