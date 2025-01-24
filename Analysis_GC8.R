# Project Name: The Global Fund to Fight HTM GC8 Replenishment
# Author: Bruno Alves de Carvalho
# Status: ongoing


# Set up ------------------------------------------------------------------

# Load packages
library(tidyverse)
library(googlesheets4)
library(slider)
library(stringi)

# Link to Google Sheets
sheet_url <- 
  "https://docs.google.com/spreadsheets/d/1tc4cgr_uA36VCEW33GbZVTaPehKALThdjSd2QzBSYgM/edit?gid=406461073#gid=406461073"

# Create functions to load data
my_sheet_names <- 
  c(
    "OECD" = "ODA Disbursements 1997-2023", 
    "CGD" ="Other Replenishments", 
    "TGF" = "0-7 Replenishments" 
    )

load_data <- 
  function(x) {
    read_sheet(
      sheet_url, sheet = my_sheet_names[[x]]
    )
  }

load_data <- 
  function(location, source_name = NULL, url_link = NULL, sheet_name = NULL) {
    if (location == "Google Drive") {
      read_sheet(
        sheet_url, sheet = my_sheet_names[[sheet_name]]
      ) 
    } else if (location == "Desktop") {
      list_data %>% 
        add_row(
          source = source_name,
          data = list(as_tibble(read_csv(url_link)))
          )
    }
  }


# Load OEDC, CGD, and TGF data
list_data <- tibble(
  source = names(my_sheet_names),
  data = vector("list", length(my_sheet_names))
)

for (i in seq_along(my_sheet_names)) {
  list_data$data[[i]] <- load_data("Google Drive", sheet_name = i) 
}

# create a copy of the loaded data
copy_list_data <- 
  list_data

# Select and rename columns from OECD data
list_data$data[[which(list_data$source == "OECD")]] <- 
  list_data %>% 
  filter(source == "OECD") %>% 
  pluck(2) %>%
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
list_data$data[[which(list_data$source == "CDG")]] <-
  list_data %>% 
  filter(source == "CDG") %>% 
  pluck(2) %>% 
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
  filter(source == "CDG") %>% 
  pluck(2) %>%
  select(-donor_name) %>% 
  group_by(org, year, grant_cycle) %>% 
  summarise(sum = sum(money, na.rm = T)) %>% 
  group_by(grant_cycle) %>% 
  summarise(n_rplnshmnt = n())

# Spread CDG data by MDB and Health Fun
list_data$data[[which(list_data$source == "CDG")]] <-
  list_data %>% 
  filter(source == "CDG") %>% 
  pluck(2) %>%
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
  pluck(2) %>% 
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


# Load IMF data
list_data <- load_data(
  location = "Desktop", 
  source_name = "IMF", 
  url_link = "/Users/brunoalvesdecarvalho/Desktop/Research/IMF/dataset_2024-12-22T16_56_47.354302444Z_DEFAULT_INTEGRATION_IMF.FAD_FM_2.0.0.csv"
  )

# Select relevant variables from IMF data
list_data$data[[which(list_data$source == "IMF")]] <-
  list_data %>% 
  filter(source == "IMF") %>% 
  pluck(2) %>% 
  select(
    donor_name = COUNTRY.Name, 
    fiscal_indicator = INDICATOR.Name, 
    starts_with("19"), 
    starts_with("20")) %>% 
  pivot_longer(
    cols = na.omit(str_extract(colnames(IMF_data), "\\d+")), 
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

# Load PAGED data
list_data <- load_data(
  location = "Desktop", 
  source_name = "PAGED", 
  url_link = "/Users/brunoalvesdecarvalho/Desktop/Research/Party_Government/PAGED-WECEE.csv"
)

# Select relevant variables from PAGED data
list_data$data[[which(list_data$source == "PAGED")]] <- 
  list_data %>% 
  filter(source == "PAGED") %>% 
  pluck(2) %>%  
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
# Create variable recording the year the left-right placement was made to link data in PAGED and CHESS
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
    party_id_2 = c(1303, 107, 2010, 3101, 201, 201, 2203, 1402, 626, 302, 402, 2302, 4502, 4505, 701, 701, 844, 2412,
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
  pluck(2) %>% 
  bind_rows(paged_data_2024) %>% 
# Correct minor issues with cabinet periods
  mutate(
    yr_rating_reported = 
      ifelse(country_name == "Czech Republic" & year == 2021, 2024, 
             ifelse(country_name == "Italy" & year == 2018, 2019, yr_rating_reported)))

# Load CHESS data from 1999 to 2019
list_data <- 
  load_data(
    location = "Desktop", 
    source_name = "CHESS", 
    url_link = "/Users/brunoalvesdecarvalho/Desktop/Research/Party_Orientation/chess/1999-2019_CHES_dataset_means(v3).csv"
  )

# Load CHESS data from 2024
list_data <- 
  load_data(
    location = "Desktop", 
    source_name = "CHESS_24", 
    url_link = "/Users/brunoalvesdecarvalho/Desktop/Research/Party_Orientation/chess/CHES_Ukraine_March_2024.csv"
  )

# Select relevant variables
list_data$data[[which(list_data$source == "CHESS_24")]] <-
  list_data %>%
  filter(source == "CHESS_24") %>% 
  pluck(2) %>% 
  select(country_name = country, party_name_short = party, lrecon, party_id) %>%
# Clean political party abbreviations
  mutate(yr_rating_reported = 2024,
         party_name_short = stri_trans_general(party_name_short, "latin-ascii"),
         party_name_short = ifelse(party_id == 2412, "JV", party_name_short)
         ) 

# Create tibble attributing countries names to country IDs in the CHESS data
chess_country_names <- 
  tibble(
    country_id = c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 16, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 37, 38, 40),
    country_name = c("Belgium", "Denmark", "Germany", "Greece", "Spain", "France", "Ireland", "Italy", "Netherlands", "United Kingdom",
              "Portugal", "Austria", "Finland", "Sweden", "Bulgaria", "Czech Republic", "Estonia", "Hungary", "Latvia",
              "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia", "Croatia", "Malta", "Luxembourg", "Cyprus")
    )

# Select relevant variables from CHESS data
list_data$data[[which(list_data$source == "CHESS")]] <-
  list_data %>% 
  filter(source == "CHESS")
  pluck(2) %>% 
  select(country_id = country, yr_rating_reported = year, party_name_short = party, lrgen, lrecon, party_id) %>% 
# Add country names
  left_join(chess_country_names, by = "country_id") %>% 
# Add 2024 data
  bind_rows(
    list_data %>%
      filter(source == "CHESS_24") %>% 
      pluck(2)) %>% 
  select(country_name, everything(), -country_id)

# Remove CHESS_24 as 2024 data was combined with the rest of the CHESS data
list_data <-
  list_data %>% 
  filter(source != "CHESS_24")

# Create a table of all political parties in PAGED
parties_list <- 
  paged_data_test %>% 
  filter(year > 1998 & !is.na(party_name_short)) %>% 
  group_by(country_name, party_name_short) %>% 
  summarise(n = n())
# Create a table of all countries in PAGED
countries_to_check <- 
  parties_list %>% 
  group_by(country_name) %>% 
  summarise(n = n()) %>% 
  pull(country_name)
# Create a table of all ideological ratings in CHESS
parties_ratings <- 
  chess_data_test %>% 
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
      "CHESS", "PAGED"
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
paged_data_test <- reduce(
  seq_len(nrow(list_new_party_names_PAGED)),
  .init = paged_data_test,
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

# Create a table of the party names to change in the CHESS data
list_new_party_names_CHESS <-
  list_new_party_names %>% 
  filter(adjust_dataset == "CHESS") %>% 
  filter(!party_name_short %in% c("IL", "ResP", "Independent"))

list_new_party_names_CHESS <-
  list_new_party_names_CHESS %>% 
  bind_rows(list_new_party_names_CHESS %>% slice(10)) %>% 
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

# Correct party names in the CHESS data
chess_data_test <- reduce(
  seq_len(nrow(list_new_party_names_CHESS)),
  .init = chess_data_test,
  .f = function(data, i) {
    data %>% 
      mutate(
        party_name_short = ifelse(
          country_name == list_new_party_names_CHESS[[i, 1]] & 
            party_name_short == list_new_party_names_CHESS[[i, 2]],
          list_new_party_names_CHESS[[i, 5]],
          party_name_short
        )
      )
  }
)

paged_data_test2 <-
  paged_data_test %>% 
  filter(
    !is.na(party_name_short)
  ) %>% 
  left_join(
    chess_data_test %>% 
      group_by(country_name, party_name_short, party_id) %>%
      summarise(n = n()) %>% 
      select(-n) %>% 
      ungroup(), 
    by = c("country_name", "party_name_short")) %>% 
  left_join(
    chess_data_test %>% 
      select(-country_name, -party_name_short), 
    by = c("yr_rating_reported", "party_id")) %>% 
  left_join(
    paged_data_test %>% 
      group_by(elecdate) %>% 
      summarise(n_elections = n()), 
    by = "elecdate") %>% 
# Bring last observation forward (and then backward) if value is missing
  group_by(party_id) %>% 
  fill(lrgen, .direction = "downup") %>% 
  fill(lrecon, .direction = "downup") %>% 
  ungroup()

# Bring last observation backward if value is missing
chess_ice_nor_2024 <- 
  chess_data_test %>% 
  filter(country_name %in% c("Iceland", "Norway"))
chess_cro_2014 <-
  chess_data_test %>% 
  filter(country_name == "Croatia" & yr_rating_reported == 2014)
paged_data_test2 <-
  bind_rows(chess_ice_nor_2024, chess_cro_2014) %>% 
  select(lrecon_01 = lrecon, lrgen_01 = lrgen, party_id) %>% 
  right_join(paged_data_test2 %>% rename(lrgen_02 = lrgen, lrecon_02 = lrecon), by = "party_id") %>% 
  mutate(lrgen = ifelse(is.na(lrgen_02), lrgen_01, lrgen_02), lrecon = ifelse(is.na(lrecon_02), lrecon_01, lrecon_02)) %>% 
  select(country_name, year_in = year, year_out, everything(), -ends_with(c("01", "02")), -party_id_2)

# ratings we still need to fix
paged_data_test2 %>% filter(year_in > 1999 & is.na(lrecon))

# Load MPD data
mpd_data_long <- 
  read_csv("/Users/brunoalvesdecarvalho/Desktop/Research/Party_Orientation/ManifestoPD/MPDataset_MPDS2024a.csv")

# Select and rename relevant variables from MPD data
mpd_data_short <-
  mpd_data_long %>% 
  select(
    country_name = countryname, 
    elecdate = date, 
    mpd_party_id = party, 
    party_name = partyname, 
    party_short_name = partyabbrev, 
    rile) %>% 
  mutate(
    elecdate = year(ym(elecdate)), 
# Re-scale the Left-Right scale from -100-100 to 0-1 
    lrgen = (rile + 100)/20
    )

# Load ParlGov data
parlgov_data_long <- 
  read_csv("/Users/brunoalvesdecarvalho/Desktop/Research/Party_Government/parlgov/view_cabinet.csv")

# Select and rename relevant variables from ParlGov data
parlgov_data_short <-
  parlgov_data_long %>% 
  select(
    country_name, 
    elecdate = election_date, 
    year_in = start_date, 
    party_id, 
    party_name_short, 
    party_name = party_name_english,
    cabinet_party,
    prime_minister) %>% 
  mutate(
    elecdate = year(elecdate), 
    year_in = year(year_in)
    )

# Keep only cabinet parties for Switzerland
parlgov_data_short_ch <- 
  parlgov_data_short %>% 
  filter(country_name == "Switzerland" & cabinet_party == 1) 

# Keep only the prime minister party for all other countries
parlgov_data_short <-
  parlgov_data_short %>% 
  filter(prime_minister == 1 & country_name != "Switzerland") %>% 
# Join Switzerland back with the rest of the countries
  bind_rows(parlgov_data_short_ch) %>% 
  select(-cabinet_party, -prime_minister) %>% 
# Join the party id from the MPD data
  inner_join(
    read_csv("/Users/brunoalvesdecarvalho/Desktop/Research/Party_Government/parlgov/view_party.csv") %>% 
      select(party_id, cmp, chess), 
    by= "party_id"
    ) %>% 
  rename(
    mpd_party_id = cmp, 
    parlgov_party_id = party_id,
    chess_party_id = chess
    ) %>% 
# Join the Left-Right assessment from the MPD data to the ParlGov data
  left_join(
    mpd_data_short %>% 
      select(elecdate, mpd_party_id, lrgen), 
    by = c("elecdate", "mpd_party_id"), 
    relationship = "many-to-many")

donor_govrt_lrplc <- 
  paged_data_test2 %>% 
  rename(
    chess_party_id = party_id
    ) %>% 
  left_join(
    parlgov_data_short %>% 
      rename(mpd_lrgen = lrgen) %>% 
      select(elecdate, chess_party_id, mpd_lrgen), 
    by = c("elecdate", "chess_party_id"), 
    multiple = "first") %>% 
  bind_rows(
    parlgov_data_short %>%
      mutate(check = country_name %in% pull(paged_data_test2, country_name)) %>% 
      filter(check == "FALSE"))

tgf_public_donors <- 
  list_data$data %>% 
  pluck(3) %>% 
  filter(donor_type == "public") %>% 
  group_by(donor_name) %>%
  count() %>% 
  select(-n) %>% 
  mutate(check = donor_name %in% pull(donor_govrt_lrplc, country_name))

tibble(
  country_name = c("United States"),
  year = c(2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2023, 2025),
  year_out = c(2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2023, 2025, 2027),
  party_name_short = c("Rep_divided", "Rep_trifecta", "Rep_trifecta", "Rep_divided", 
                       "Dem_trifecta", "Dem_divided", "Dem_divided", "Dem_divided",
                       "Rep_trifecta", "Rep_divided", "Dem_trifecta", "Dem_divided",
                       "Rep_trifecta"
                       ),
  party_id_mpd = c(),
  elecdate = c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024),
  yr_rile_reported_mpd = c(2000, 2000, 2004, 2004, 2008, 2008, 2012, 2012, 2016, 2016, 2020, 2020, 2020)
)

# South Korean, from MPD
skorea_data <-
  tibble(
    country_name = c("South Korea"),
    year = c(1997, 2002, 2007, 2012, 2017, 2022),
    year_out = c(2002, 2007, 2012, 2017, 2022, 2027),
    party_name_short = c("NCNP", "MDP", "GNP", "NFP", "DPK", "PP"),
    party_id_mpd = c(113421, 113430, 113630, 113630, 113441, 113450),
    elecdate = c(1997, 2002, 2007, 2012, 2017, 2022),
    yr_rile_reported_mpd = c(1996, 2000, 2004, 2012, 2016, 2020)
  )

# European Commission, from CHESS
eurocomm_data <- 
  tibble(
    country_name = c("European Commission"),
    year = c(1999, 2004, 2009, 2014, 2019, 2024),
    year_out = c(2004, 2009, 2014, 2019, 2024, 2029),
    party_name_short = c("DEM, DL", "PPD, PSD", "PPD, PSD", "CSV", "CDU", "CDU"),
    party_id = c(819, 1206, 1206, 3801, 301, 301),
    elecdate = c(1999, 2004, 2009, 2014, 2019, 2024),
    yr_rating_reported = c(1999, 2002, 2010, 2014, 2019, 2024)
  ) %>% 
    left_join(
      chess_data_test %>% select(party_id, yr_rating_reported, lrecon, lrgen), 
      by = c("yr_rating_reported", "party_id")
      ) %>% 
  rowwise() %>% 
  mutate(avg_rating = mean(c(lrgen, lrecon), na.rm = T))


# to-do 1: manually complete governments for the United States, South Korea, and the European Commission (for the EC, construct the L-R based on EU member parties)
# to-do 2: assign ratings to grant cycles based on proximity

# research longitudinal data on ideological placement for other regions (non-EU)

# need to find the left-right info for Iceland: until 2024 = LG, after 2024 = Sam (or SDA)

# Assumptions: 3yr running average, 1 left-right ratings for every 4 years from 2000 (grouping)



