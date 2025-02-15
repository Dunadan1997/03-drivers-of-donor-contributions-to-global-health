# Project Name: The Global Fund to Fight HTM GC8 Replenishment
# Author: Bruno Alves de Carvalho
# Status: ongoing


# Set up ------------------------------------------------------------------

# Load packages
library(tidyverse)
library(googlesheets4)
library(slider)
library(stringi)


# Path to Data Warehouse
path_to_data_warehouse <- 
  "/Users/brunoalvesdecarvalho/Desktop/DataWarehouse_20231015_ve01/TGF"

# Copy the URLs of each dataset
urls <- 
  list.files(path_to_data_warehouse)

# Store URLs in a table
urls_table <- tibble(
  source = str_extract_all(urls, "^[^_]+", simplify = TRUE), 
  url = paste0(path_to_data_warehouse, sep = "/", urls)
  ) %>%
  filter(!str_detect(source, "^\\d"))

# Create a function to load data
load_data <- function(url_link_index) {
    read_csv(urls_table$url[[url_link_index]])
  }

# Load all data
list_data <- tibble(
  source = urls_table$source,
  data = vector("list", length(urls_table$url))
)

for (i in seq_along(urls_table$url)) {
  list_data$data[[i]] <- load_data(i) %>% tibble()
}

# create a copy of the loaded data
copy_list_data <- 
  list_data

# Select and rename columns from OECD data
list_data$data[[which(list_data$source == "OECD")]] <- 
  list_data %>% 
  filter(source == "OECD") %>% 
  pluck(2,1) %>%
  select(
    "Donor", 
    "TIME_PERIOD", 
    "OBS_VALUE"
    ) %>% 
  rename(
    "donor_name" = "Donor", 
    "year" = "TIME_PERIOD", 
    "oda_spent" = "OBS_VALUE"
    ) %>% 
# Create 3 year running average of ODA disbursements (1yr before, 1yr after)
  arrange(donor_name, year) %>% 
  group_by(donor_name) %>% 
  mutate(
    oda_running_avg = slide_mean(
      oda_spent, before = 1, after = 1, complete = FALSE, na_rm = TRUE
      )
    ) %>% 
  ungroup()

# Gather CGD data by name of organization and replenishment year
list_data$data[[which(list_data$source == "CGD")]] <-
  list_data %>% 
  filter(source == "CGD") %>% 
  pluck(2,1) %>% 
  pivot_longer(
    cols = matches("\\d{4}$"), 
    names_to = "org_year", 
    values_to = "money") %>% 
  separate(org_year, into = c("org", "year"), sep = "_") %>% 
# Create a variable indicating under which TGF grant cycle the pledge was made
  mutate(
    year = as.numeric(year), 
    "grant_cycle" = 
      ifelse(between(year, 2000, 2002), "GC0", 
             ifelse(between(year, 2003, 2005), "GC1", 
                    ifelse(between(year, 2006, 2008), "GC2", 
                           ifelse(between(year, 2009, 2011), "GC3", 
                                  ifelse(between(year, 2012, 2014), "GC4", 
                                         ifelse(between(year, 2015, 2017), "GC5", 
                                                ifelse(between(year, 2018, 2020), "GC6", 
                                                       ifelse(between(year, 2021, 2023), "GC7", 
                                                              "GC8")
                                                )
                                         )
                                  )
                           )
                    )
             )
      )
  )

# Create table summarizing the number of replenishments per Grant Cycle
tab_n_rplnshmnt <-
  list_data %>% 
  filter(source == "CGD") %>% 
  pluck(2,1) %>%
  select(-donor_name) %>% 
  group_by(org, year, grant_cycle) %>% 
  summarise(sum = sum(money, na.rm = T)) %>% 
  group_by(grant_cycle) %>% 
  summarise(n_rplnshmnt = n())

# Spread CGD data by MDB and Health Fun
list_data$data[[which(list_data$source == "CGD")]] <-
  list_data %>% 
  filter(source == "CGD") %>% 
  pluck(2,1) %>%
  pivot_wider(names_from = org, values_from = money) %>% 
  group_by(donor_name, grant_cycle) %>% 
  summarise(
    GAVI = sum(GAVI, na.rm = T), 
    ADF = sum(ADF, na.rm = T), 
    IFAD = sum(IFAD, na.rm = T), 
    IDA = sum(IDA, na.rm = T), 
    GCF = sum(GCF, na.rm = T), 
    PF = sum(PF, na.rm = T), 
    LDF = sum(LDF, na.rm = T), 
    GEF = sum(GEF, na.rm = T), 
    GPE = sum(GPE, na.rm = T), 
    AfDf = sum(AfDf, na.rm = T), 
    CEPI = sum(CEPI, na.rm = T)
  ) %>% 
# Join the variable n_rplnshmnt into the CGD data
  left_join(tab_n_rplnshmnt, by = "grant_cycle") %>% 
  ungroup()

# Create a Grant Cycle variable in the TGF data
list_data$data[[which(list_data$source == "TGF")]] <- 
  list_data %>% 
  filter(source == "TGF") %>% 
  pluck(2,1) %>% 
  mutate(
    grant_cycle = ifelse(year == 2001, "GC0", 
                         ifelse(year == 2005, "GC1", 
                                ifelse(year == 2007, "GC2", 
                                       ifelse(year == 2010, "GC3", 
                                              ifelse(year == 2013, "GC4", 
                                                     ifelse(year == 2016, "GC5", 
                                                            ifelse(year == 2019, "GC6", 
                                                                   ifelse(year == 2022, "GC7", "GC8")
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                                )
                         )
    )

# Select relevant variables from IMF data
IMF_colnames <- 
  list_data %>% 
  filter(source == "IMF") %>% 
  pluck(2,1) %>% 
  colnames()

list_data$data[[which(list_data$source == "IMF")]] <-
  list_data %>% 
  filter(source == "IMF") %>% 
  pluck(2,1) %>% 
  select(
    donor_name = COUNTRY.Name, 
    fiscal_indicator = INDICATOR.Name, 
    starts_with("19"), 
    starts_with("20")) %>% 
  pivot_longer(
    cols = na.omit(str_extract(IMF_colnames, "\\d+")), 
    names_to = "year", values_to = "obs_value") %>% 
  mutate(
    donor_name = str_extract(donor_name, "^[^,]+")
  ) %>% 
# Filter out non-donor countries 
  filter(donor_name != "Congo") %>% 
  pivot_wider(names_from = fiscal_indicator, values_from = obs_value) %>% 
# Rename variables
  rename(
    expdtr_prctgdp = `Expenditure, Percent of GDP`, 
    revn_prctgdp = `Revenue, General government, Percent of GDP`, 
    prmryfsclblc_prctgdp = `Primary net lending (+) / net borrowing (-), Percent of GDP`,
    fsclblc_prctgdp = `Net lending (+) / net borrowing (-), Percent of GDP`, 
    adjfsclblc_prctgdp = `Cyclically adjusted balance, Percent of potential GDP`, 
    grsdbt_prctgdp = `Gross debt, Percent of GDP`, 
    ntdbt_prctgdp = `Net debt, Percent of GDP`, 
    prmryadjfsclblc_prctgdp = `Cyclically adjusted primary balance, Percent of potential GDP`
    ) %>% 
# Create rolling average of fiscal indicators
  group_by(donor_name) %>% 
  mutate(
    across(
      expdtr_prctgdp:prmryadjfsclblc_prctgdp, 
      ~ slide_mean(.x, before = 2, na_rm = TRUE),
      .names = "{.col}_rllavg" 
      )
    ) %>% 
# Rename rolling average columns
  rename_with(~ str_replace_all(.x, "_prctgdp_", "_")) %>% 
  ungroup()

# Select relevant variables from PAGED data
list_data$data[[which(list_data$source == "PAGED")]] <- 
  list_data %>% 
  filter(source == "PAGED") %>% 
  pluck(2,1) %>%  
  select(
    country_name, 
    year = year_in, 
    year_out, 
    cab_composition1,
    elecdate
    ) %>% 
# Clean abreviations of party names
  separate_wider_delim(cab_composition1, delim = ",", names = "party_name_short", too_many = "drop") %>% 
  mutate(
    party_name_short = stri_trans_general(party_name_short, "latin-ascii"),
    country_name = ifelse(country_name == "Czechia", "Czech Republic", country_name),
    elecdate = year(dmy(elecdate)),
# Create variable recording the year the Expert Survey (CHES) was conducted. This will help link data in PAGED and CHESS
    yr_rating_reported = 
      ifelse(year_out < 2000, 1999, 
             ifelse(year < 2005, 2002, 
                    ifelse(year < 2009, 2006, 
                           ifelse(year < 2013, 2010, 
                                  ifelse(year < 2017, 2014, 
                                         ifelse(year <= 2021, 2019, 
                                                2024
                                                )
                                         )
                                  )
                           )
                    )
             )
    )

# Complete PAGED data with political parties of country governments in 2024
paged_data_2024 <-
  tibble(
    country_name = c("Austria", "Belgium", "Bulgaria", "Croatia", "Denmark", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
                     "Hungary", "Iceland", "Iceland", "Ireland", "Ireland", "Italy", "Latvia", "Lithuania", "Netherlands", "Netherlands", 
                     "Norway", "Poland", "Portugal", "Portugal", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Spain", "Sweden", "United Kingdom", 
                     "United Kingdom"), 
    year = c(2024, 2024, 2024, 2024, 2019, 2022, 2023, 2023, 2024, 2021, 2023, 2022, 2021, 2024, 2020, 2024, 2022, 2022,
             2024, 2021, 2024, 2021, 2023, 2019, 2022, 2024, 2024, 2023, 2022, 2020, 2023, 2022, 2019, 2024),
    year_out = NA,
    party_name_short = c("FPO", "VLD", "GERB", "HDZ", "S", "S", "ER", "KOK", "LREM", "SPD", "ND", "Fidesz", "LG", "SDA", "FF", "FF", "FdI", "JV",
                         "LSDP", "VVD", "PVV", "A", "PO", "PS", "PS", "PSD", "PSD", "Smer-SD", "GS", "PSOE", "PSOE", "M", "Con", "Lab"),
    party_id_ches = c(1303, 107, 2010, 3101, 201, 201, 2203, 1402, 626, 302, 402, 2302, 4502, 4505, 701, 701, 844, 2412,
                 2501, 1003, 1017, 3501, 2603, 1205, 1205, 1206, 2701, 2803, 2916, 501, 501, 1605, 1101, 1102),
    elecdate = c(2024, 2024, 2024, 2024, 2019, 2022, 2023, 2023, 2024, 2021, 2023, 2022, 2021, 2024, 2020, 2024, 2022, 2022,
                 2024, 2021, 2023, 2021, 2023, 2019, 2022, 2024, 2024, 2023, 2022, 2019, 2023, 2022, 2019, 2024),
    yr_rating_reported = c(2024, 2024, 2024, 2024, 2019, 2024, 2024, 2024, 2024, 2019, 2024, 2024, 2019, 2024, 2019, 2024, 2024, 2024,
                           2024, 2019, 2024, 2019, 2024, 2019, 2024, 2024, 2024, 2024, 2024, 2019, 2024, 2024, 2019, 2024)
    )

# Add 2024 government political parties to PAGED dataset
list_data$data[[which(list_data$source == "PAGED")]] <-
  list_data %>% 
  filter(source == "PAGED") %>% 
  pluck(2,1) %>% 
  bind_rows(paged_data_2024 %>% select(-party_id_ches)) %>% 
# Correct minor issues with cabinet periods
  mutate(
    yr_rating_reported = 
      ifelse(country_name == "Czech Republic" & year == 2021, 2024, 
             ifelse(country_name == "Italy" & year == 2018, 2019, yr_rating_reported)))


# Select relevant variables
list_data$data[[which(list_data$source == "CHES24")]] <-
  list_data %>%
  filter(source == "CHES24") %>% 
  pluck(2,1) %>% 
  select(country_name = country, party_name_short = party, lrecon_ches = lrecon, party_id_ches = party_id) %>%
# Clean political party abbreviations
  mutate(yr_rating_reported = 2024,
         party_name_short = stri_trans_general(party_name_short, "latin-ascii"),
         party_name_short = ifelse(party_id_ches == 2412, "JV", party_name_short)
         ) 

# Create tibble attributing countries names to country IDs in the CHESS data
ches_country_names <- 
  tibble(
    country_id = c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 16, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 37, 38, 40),
    country_name = c("Belgium", "Denmark", "Germany", "Greece", "Spain", "France", "Ireland", "Italy", "Netherlands", "United Kingdom",
              "Portugal", "Austria", "Finland", "Sweden", "Bulgaria", "Czech Republic", "Estonia", "Hungary", "Latvia",
              "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia", "Croatia", "Malta", "Luxembourg", "Cyprus")
    )

# Select relevant variables from CHES data
list_data$data[[which(list_data$source == "CHES")]] <-
  list_data %>% 
  filter(source == "CHES") %>% 
  pluck(2,1) %>% 
  select(country_id = country, yr_rating_reported = year, party_name_short = party, lrgen_ches = lrgen, lrecon_ches = lrecon, party_id_ches = party_id) %>% 
# Add country names
  left_join(ches_country_names, by = "country_id") %>% 
# Add 2024 data
  bind_rows(
    list_data %>%
      filter(source == "CHES24") %>% 
      pluck(2,1)) %>% 
  select(country_name, everything(), -country_id)

# Remove CHESS_24 as 2024 data was combined with the rest of the CHESS data
list_data <-
  list_data %>% 
  filter(source != "CHES24")

# Create a table of all political parties in PAGED
parties_list <- 
  list_data %>%
  filter(source == "PAGED") %>% 
  pluck(2,1) %>% 
  filter(year > 1998 & !is.na(party_name_short)) %>% 
  group_by(country_name, party_name_short) %>% 
  summarise(n = n())
# Create a table of all countries in PAGED
countries_to_check <- 
  parties_list %>% 
  group_by(country_name) %>% 
  summarise(n = n()) %>% 
  pull(country_name)
# Create a table of all political parties with an ideological rating in CHES
parties_ratings <- 
  list_data %>%
  filter(source == "CHES") %>% 
  pluck(2,1) %>%  
  filter(!is.na(party_name_short)) %>% 
  group_by(country_name, party_name_short) %>% 
  summarise(n = n())

# Identify party names that are not aligned between the PAGED and CHESS dataset
results_tibble <- 
  map_dfr(
    countries_to_check, 
    function(country) {
      # Filter parties_list and parties_ratings for the current country
      filtered_parties_list <- 
        parties_list %>% 
        filter(country_name == country)
      filtered_parties_ratings <- 
        parties_ratings %>%
        filter(country_name == country) %>%
        pull(party_name_short)
      
      # Identify party names in the current country that are not the same between the PAGED and CHESS dataset
      filtered_parties_list %>%
        mutate(
          in_parties_ratings = 
            party_name_short %in% filtered_parties_ratings
        )
    }
  )

# Create a table of all party name corrections to align the data in PAGED and CHESS
list_new_party_names <- 
  results_tibble %>% 
  ungroup() %>% 
  filter(in_parties_ratings == "FALSE") %>% 
  bind_cols(
    tibble(
      new_names = 
        c(
          "SDSS", "ANO2011", "SD",
          "IL", # no change
          "ResP", # no change
          "UMP", "PS", "CDU", "ANEL",
          "IP", # Sj
          "LG", # Graen
          "PP", # F
          "SDA", # Sam
          "Independent", # no change
          "PDS",
          "LC",
          "A", # Ap
          "KRF", # KrF
          "AWS", # AWSP
          "PNTCD", # CDR 2000
          "S", # SAP
          "Con" # Cons
        )
    )
  ) %>% 
  mutate(
    adjust_dataset = ifelse(
      party_name_short == new_names, 
      "CHES", "PAGED"
    )
  )

# Create a table of the party names to change in the PAGED data
list_new_party_names_PAGED <-
  list_new_party_names %>% 
  filter(adjust_dataset == "PAGED") %>% 
  bind_rows(
    tibble(
      country_name = "Italy",
      party_name_short = "FI-PdL",
      new_names = "FI"
    )
  )

# Correct party names in the PAGED data
list_data$data[[which(list_data$source == "PAGED")]] <- reduce(
  seq_len(nrow(list_new_party_names_PAGED)),
  .init =  list_data %>%
    filter(source == "PAGED") %>% 
    pluck(2,1),
  .f = function(data, i) {
    data %>% 
      mutate(
        party_name_short = ifelse(
          country_name == list_new_party_names_PAGED[[i, 1]] & 
            party_name_short == list_new_party_names_PAGED[[i, 2]],
          list_new_party_names_PAGED[[i, 5]],
          party_name_short
        )
      )
  }
)

# Create a table of the party names to change in the CHES data
list_new_party_names_CHES <-
  list_new_party_names %>% 
  filter(adjust_dataset == "CHES") %>% 
  filter(!party_name_short %in% c("IL", "ResP", "Independent"))

list_new_party_names_CHES <-
  list_new_party_names_CHES %>% 
  bind_rows(list_new_party_names_CHES %>% slice(10)) %>% 
  mutate(
    party_name_short = c(
      "Sj", "Graen", "F", "Sam", 
      "Ap", "KrF", "AWSP", "CDR 2000", 
      "SAP", "Cons", "CONS"
    )
  ) %>% 
  bind_rows(
    tibble(
      country_name = "Italy",
      party_name_short ="PDL",
      new_names = "FI"
    )
  )

# Correct party names in the CHES data
list_data$data[[which(list_data$source == "CHES")]] <- reduce(
  seq_len(nrow(list_new_party_names_CHES)),
  .init = list_data %>%
    filter(source == "CHES") %>% 
    pluck(2,1),
  .f = function(data, i) {
    data %>% 
      mutate(
        party_name_short = ifelse(
          country_name == list_new_party_names_CHES[[i, 1]] & 
            party_name_short == list_new_party_names_CHES[[i, 2]],
          list_new_party_names_CHES[[i, 5]],
          party_name_short
        )
      )
  }
)

# Summarize the ID of each party in CHES to prepare join with PAGED
ches_party_ids <-
  list_data %>% 
    filter(source == "CHES") %>% 
    pluck(2,1) %>% 
    group_by(country_name, party_name_short, party_id_ches) %>%
    count() %>% 
    select(-n) %>% 
    ungroup()

# Join PAGED and CHES data into one set
PAGED_CHES <-
  list_data %>% 
  filter(source == "PAGED") %>% 
  pluck(2,1) %>% 
  filter(!is.na(party_name_short)) %>% 
# Join party IDs from CHES to PAGED using country and party names
  left_join(ches_party_ids, by = c("country_name", "party_name_short")) %>%
# Join ideological ratings from CHES to PAGED using party IDs and the year of Expert Survey (CHES) was conducted
  left_join(
    list_data %>%
      filter(source == "CHES") %>% 
      pluck(2,1) %>% 
      select(-country_name, -party_name_short), 
    by = c("yr_rating_reported", "party_id_ches")) %>% 
# Bring last rating forward or backward if ideological rating is missing
  group_by(party_id_ches) %>% 
  fill(lrgen_ches, .direction = "downup") %>% 
  fill(lrecon_ches, .direction = "downup") %>% 
  ungroup()
  
# Bring last rating forward or backward if ideological rating is missing; 
  # Iceland, Norway and Croatia need a special fix because the Expert Survey (CHES) was only conducted in 2024 / 2014
  # So we assign this single ideological rating regardless of the historical period
ches_ice_nor_2024 <- 
  list_data %>% 
  filter(source == "CHES") %>% 
  pluck(2,1) %>% 
  filter(country_name %in% c("Iceland", "Norway"))
ches_cro_2014 <-
  list_data %>% 
  filter(source == "CHES") %>% 
  pluck(2,1) %>% 
  filter(country_name == "Croatia" & yr_rating_reported == 2014)
PAGED_CHES <-
  bind_rows(ches_ice_nor_2024, ches_cro_2014) %>% 
  select(
    lrecon_ches_01 = lrecon_ches, 
    lrgen_ches_01 = lrgen_ches, 
    party_id_ches
    ) %>% 
  right_join(
    PAGED_CHES %>% 
      rename(lrgen_ches_02 = lrgen_ches, lrecon_ches_02 = lrecon_ches), 
    by = "party_id_ches") %>% 
  mutate(
    lrgen_ches = ifelse(is.na(lrgen_ches_02), lrgen_ches_01, lrgen_ches_02), 
    lrecon_ches = ifelse(is.na(lrecon_ches_02), lrecon_ches_01, lrecon_ches_02)) %>% 
  select(country_name, year_in = year, year_out, everything(), -ends_with(c("01", "02")))

# Store PAGED_CHES dataset with the rest of the data
list_data <-
  list_data %>% 
  add_row(
    source = "PAGED_CHES",
    data = list(PAGED_CHES)
  )

# ratings we still need to fix
PAGED_CHES %>% filter(year_in > 1999 & is.na(lrecon_ches))

# Select and rename relevant variables from MP data
list_data$data[[which(list_data$source == "MP")]] <-
  list_data %>% 
  filter(source == "MP") %>% 
  pluck(2,1) %>% 
  select(
    country_name = countryname, 
    elecdate = date, 
    party_id_mp = party, 
    party_name = partyname, 
    party_short_name = partyabbrev, 
    lrgen_mp = rile) %>% 
  mutate(
    elecdate = year(ym(elecdate)), 
# Re-scale the Left-Right scale from -100-100 to 0-1 
    lrgen_mp = (lrgen_mp + 100)/20
    )

# Select and rename relevant variables from ParlGov data
list_data$data[[which(list_data$source == "PARLGOV1")]] <-
  list_data %>% 
  filter(source == "PARLGOV1") %>% 
  pluck(2,1) %>% 
  select(
    country_name, 
    elecdate = election_date, 
    year_in = start_date, 
    party_id_parlgov = party_id, 
    party_name_short, 
    party_name = party_name_english,
    cabinet_party,
    prime_minister) %>% 
  mutate(
    elecdate = year(elecdate), 
    year_in = year(year_in)
    )

# Keep only cabinet parties for Switzerland
parlgov_data_ch <- 
  list_data %>%
  filter(source == "PARLGOV1") %>% 
  pluck(2,1) %>% 
  filter(country_name == "Switzerland" & cabinet_party == 1) 

# Keep only the prime minister party for all other countries
list_data$data[[which(list_data$source == "PARLGOV1")]] <-
  list_data %>%
  filter(source == "PARLGOV1") %>% 
  pluck(2,1) %>% 
  filter(prime_minister == 1 & country_name != "Switzerland") %>% 
# Join Switzerland back with the rest of the countries
  bind_rows(parlgov_data_ch) %>% 
  select(-cabinet_party, -prime_minister) %>% 
# Join the party ID from the MP data
  inner_join(
   list_data %>% 
     filter(source == "PARLGOV2") %>% 
     pluck(2,1) %>% 
     select(party_id_parlgov = party_id, cmp, chess), 
    by = "party_id_parlgov"
    ) %>% 
  rename(
    party_id_mp = cmp, 
    party_id_ches = chess
    ) 

# Join the Left-Right assessment from the MP data to the ParlGov data
PARLGOV_MP <-
  list_data %>% 
  filter(source == "PARLGOV1") %>% 
  pluck(2,1) %>% 
  left_join(
    list_data %>%
      filter(source == "MP") %>% 
      pluck(2,1) %>% 
      select(elecdate, party_id_mp, lrgen_mp), 
    by = c("elecdate", "party_id_mp"), 
    relationship = "many-to-many")

# Store PARLGOV_MP dataset with the rest of the data
list_data <-
  list_data %>% 
  add_row(
    source = "PARLGOV_MP",
    data = list(PARLGOV_MP)
  )

# Create table of United States governments since 1999
Untd_States_data <-
  tibble(
    country_name = c("United States"),
    year_in = c(2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2023, 2025),
    year_out = c(2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2023, 2025, 2027),
    party_name_short = c("Rep_divided", "Rep_trifecta", "Rep_trifecta", "Rep_divided", 
                         "Dem_trifecta", "Dem_divided", "Dem_divided", "Dem_divided",
                         "Rep_trifecta", "Rep_divided", "Dem_trifecta", "Dem_divided",
                         "Rep_trifecta"
    ),
    party_id_mp = c(61620, 61620, 61620, 61620, 61320, 61320, 61320, 61320, 61620, 61620, 61320, 61320, 61620),
    elecdate = c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024),
    yr_rating_reported_mp = c(2000, 2000, 2004, 2004, 2008, 2008, 2012, 2012, 2016, 2016, 2020, 2020, 2020)
  )

# Create table of South Korean governments since 1999
sth_korea_data <-
  tibble(
    country_name = c("South Korea"),
    year_in = c(1997, 2002, 2007, 2012, 2017, 2022),
    year_out = c(2002, 2007, 2012, 2017, 2022, 2027),
    party_name_short = c("NCNP", "MDP", "GNP", "NFP", "DPK", "PP"),
    party_id_mp = c(113421, 113430, 113630, 113630, 113441, 113450),
    elecdate = c(1997, 2002, 2007, 2012, 2017, 2022),
    yr_rating_reported_mp = c(1996, 2000, 2004, 2012, 2016, 2020)
  )

# Join data from US and South Korea
sthkor_untdstates_data <- 
  bind_rows(sth_korea_data, Untd_States_data) %>% 
  left_join(
    list_data %>% 
      filter(source == "MP") %>% 
      pluck(2,1) %>% 
      select(party_id_mp, yr_rating_reported_mp = elecdate, lrgen_mp),
    by = c("party_id_mp", "yr_rating_reported_mp")
  )

# Store US and South Korea with the rest of the PARLGOV_MP data
list_data$data[[which(list_data$source == "PARLGOV_MP")]] <-
  bind_rows(
    list_data %>% 
      filter(source == "PARLGOV_MP") %>% 
      pluck(2,1),
    sthkor_untdstates_data
  )

# Create table of European Commission governments since 1999
eurocomm_data <- 
  tibble(
    country_name = c("European Commission"),
    year_in = c(1999, 2004, 2009, 2014, 2019, 2024),
    year_out = c(2004, 2009, 2014, 2019, 2024, 2029),
    party_name_short = c("DEM, DL", "PPD, PSD", "PPD, PSD", "CSV", "CDU", "CDU"),
    party_id_ches = c(819, 1206, 1206, 3801, 301, 301),
    elecdate = c(1999, 2004, 2009, 2014, 2019, 2024),
    yr_rating_reported = c(1999, 2002, 2010, 2014, 2019, 2024)
  ) %>% 
  left_join(
    list_data %>% 
      filter(source == "CHES") %>% 
      pluck(2,1) %>% 
      select(party_id_ches, lrgen_ches, lrecon_ches, yr_rating_reported),
    by = c("party_id_ches", "yr_rating_reported")
  )

# Store EU data with the rest of the PAGED_CHES data
list_data$data[[which(list_data$source == "PAGED_CHES")]] <-
  bind_rows(
    list_data %>% 
      filter(source == "PAGED_CHES") %>% 
      pluck(2,1),
    eurocomm_data
  )

# Gather ideological placement of governments per year


# Design function to create continuous time scale for each country
countries_ches <- 
  list_data$data[[which(list_data$source == "PAGED_CHES")]] %>% 
  distinct(country_name) %>% 
  pull()
countries_mp <- 
  list_data$data[[which(list_data$source == "PARLGOV_MP")]] %>% 
  distinct(country_name) %>% 
  pull()

create_continuous_time_scale <- function(country_names, dataset_name) {
  countries <- 
    list_data$data[[which(list_data$source == dataset_name)]] %>% 
    distinct(country_name) %>% 
    pull()
  
  index <- 
    which(list_data$data[[which(list_data$source == dataset_name)]]$country_name == countries[[country_names]])
  
  table_store <-
    list_data$data[[which(list_data$source == dataset_name)]] %>% 
    slice(index) %>% 
    mutate(year = year_in) %>% 
    full_join(tibble(year = min(.$year):2025)) %>% 
    arrange(year) %>% 
    fill(country_name, .direction = "down")
}

# Create continuous time scale for each country in the Paged_Ches data
table_paged_ches <- 
  map(seq_along(countries_ches), create_continuous_time_scale, dataset_name = "PAGED_CHES") %>% 
# Join the list of countries back into one table
  map_dfr(., bind_rows)

# Create continuous time scale for each country in the Parlgov_Mp data
table_parlgov_mp <- 
  map(seq_along(countries_mp), create_continuous_time_scale, dataset_name = "PARLGOV_MP") %>% 
# Join the list of countries back into one table
  map_dfr(., bind_rows)

# Join the new tables with the rest of the data sets
list_data <-
  list_data %>% 
  add_row(
    source = "POLITICAL_FACTORS",
    data = list(
      full_join(
        table_paged_ches, 
        table_parlgov_mp %>% 
          select(
            country_name, 
            year, 
            year_in,
            year_out,
            elecdate,
            party_name, 
            lrgen_mp, 
            party_id_ches, 
            party_id_mp, 
            party_id_parlgov), 
        by = c("country_name", "year", "party_id_ches"), 
        relationship = "many-to-many") %>% 
        mutate(
          year_in = ifelse(is.na(year_in.x), year_in.y, year_in.x), 
          year_out = ifelse(is.na(year_out.x), year_out.y, year_out.x),
          elecdate = ifelse(is.na(elecdate.x), elecdate.y, elecdate.x)
        ) %>% 
# Group by year and summarize mean of political ideology to create constant continuous scale
# Context: Individual years can have multiple governments with different parties and ideologies, and governments can have multiple parties in government. Example: Switzerland has a federal government.
        group_by(country_name, year) %>% 
        mutate(across(contains("_id_"), ~ as.character(.))) %>% 
        summarise(
          across(where(is.numeric), ~ mean(., na.rm = TRUE)), 
          across(where(is.character), ~ str_c(unique(.), collapse = ", "))) %>% 
        fill(party_name, party_name_short, lrgen_ches, lrecon_ches, lrgen_mp, contains("_id_")) %>% 
        ungroup() %>% 
        mutate(across(everything(), ~ ifelse(is.nan(.), NA, .))) %>% 
        select(country_name, year, elecdate, party_name, party_name_short, year_in, year_out, lrgen_ches, lrecon_ches, lrgen_mp, contains("_id_"),
               -year_in.x, -year_out.x, -year_in.y, -year_out.y) %>% 
# Consolidation of Swiss data
        mutate(
          year_in = replace(year_in, is.na(year_in) & year == 2023, 2023), 
          elecdate = replace(elecdate, is.na(elecdate) & year == 2023, 2023)
        ) %>% 
# Removing unnecessary text from some date values
        mutate(
          elecdate = str_sub(elecdate, start = 1, end = 4) %>% as.numeric(), 
          year_out = str_sub(year_out, start = 1,4) %>% as.numeric()
        )
    )
  )

# Calculate number of elections per year from Donor Countries

  




