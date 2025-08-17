# Project Name: The Global Fund to Fight HTM GC8 Replenishment
# Author: Bruno Alves de Carvalho
# Status: ongoing

# Set up ------------------------------------------------------------------

# Load packages
library(tidyverse)
library(slider)
library(stringi)
library(patchwork)
library(boot)
library(leaps)
library(glmnet)
library(Matrix)

# Load Functions
source("Functions_GC8.R")

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


# Data Wrangling ----------------------------------------------------------

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
      ),
    donor_name = ifelse(donor_name == "EU Institutions", "European Commission", donor_name)
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

# Select relevant variables from IMF fiscal data
IMF_colnames <- 
  list_data %>% 
  filter(source == "IMF1") %>% 
  pluck(2,1) %>% 
  colnames()

list_data$data[[which(list_data$source == "IMF1")]] <-
  list_data %>% 
  filter(source == "IMF1") %>% 
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
      .names = "{.col}_rllavg01" 
      ),
    year = as.numeric(year)
    ) %>% 
# Rename rolling average columns
  rename_with(~ str_replace_all(.x, "_prctgdp_", "_")) %>% 
  ungroup()

# Select relevant variables from IMF economic outlook data
list_data$data[[which(list_data$source == "IMF2")]] <-
  list_data %>% 
  filter(source == "IMF2") %>% 
  pluck(2,1) %>% 
  filter(`Subject Descriptor` %in% c(
    "Gross domestic product per capita, constant prices", 
    "Gross domestic product, constant prices",
    "Inflation, end of period consumer prices",
    "Unemployment rate",
    "Total investment",
    "Volume of exports of goods and services",
    "Volume of imports of goods and services"
    ),
  Units %in% c(
    "Percent change",
    "Purchasing power parity; 2021 international dollar",
    "Percent of GDP",
    "Percent of total labor force"
    )
  ) %>%
  select(-Scale, -Units, -`Country/Series-specific Notes`, -`Estimates Start After`) %>% 
# Transform table into a long format by turning years into rows
  pivot_longer(
    str_extract(colnames(.), "^\\d\\d\\d\\d") %>% 
      discard(is.na), 
    names_to = "year",
    names_transform = list(year = as.numeric),
    values_to = "obs_values") %>% 
# Transform missing values to NA
  mutate(
    obs_values = case_when(
      obs_values %in% c("n/a", "--") ~ NA, 
      TRUE ~ obs_values
    ),
    obs_values = as.numeric(obs_values)
  ) %>% 
# Transform table into a wide format by turning economic indicators into columns
  pivot_wider(
    names_from = "Subject Descriptor", 
    values_from = "obs_values") %>%
# Clean and rename column names
  rename_with(~ str_replace_all(.x, " \\(|, | |/|-|\\) ", "_"), everything()) %>% 
  rename(donor_name = Country, 
         gdp_cp = Gross_domestic_product_constant_prices,
         gdp_per_cap_cp = Gross_domestic_product_per_capita_constant_prices,
         inflation_rt = Inflation_end_of_period_consumer_prices,
         imports_vl = Volume_of_imports_of_goods_and_services,
         exports_vl = Volume_of_exports_of_goods_and_services,
         unemployment_rt = Unemployment_rate) %>% 
# Create rolling average of economic outlook indicators
  group_by(donor_name) %>% 
  mutate(across(
   gdp_cp:unemployment_rt, 
    ~ slide_mean(.x, before = 2, na_rm = TRUE),
    .names = "{.col}_rllavg02" 
  ))

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

# Join the new tables with the rest of the data
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
            party_name_short,
            lrgen_mp, 
            party_id_ches, 
            party_id_mp, 
            party_id_parlgov), 
        by = c("country_name", "year", "party_id_ches"), 
        relationship = "many-to-many") %>%
# Complete data from both tables
        mutate(
          year_in = ifelse(is.na(year_in.x), year_in.y, year_in.x), 
          year_out = ifelse(is.na(year_out.x), year_out.y, year_out.x),
          elecdate = ifelse(is.na(elecdate.x), elecdate.y, elecdate.x),
          party_name_short = ifelse(is.na(party_name_short.x), party_name_short.y, party_name_short.x)
        ) %>% 
# Group by year and summarize mean of political ideology to create constant continuous scale
# Context: Individual years can have multiple governments with different parties and ideologies, and governments can have multiple parties in government. Example: Switzerland has a federal government.
        group_by(country_name, year) %>% 
        mutate(across(contains("_id_"), ~ as.character(.))) %>% 
        summarise(
          across(where(is.numeric), ~ mean(., na.rm = TRUE)), 
          across(where(is.character), ~ str_c(unique(.), collapse = ", "))) %>% 
        fill(party_name, party_name_short, lrgen_ches, lrecon_ches, lrgen_mp, contains("_id_"), elecdate) %>% 
        ungroup() %>% 
        mutate(across(everything(), ~ ifelse(is.nan(.), NA, .))) %>% 
        select(country_name, year, elecdate, party_name, party_name_short, year_in, year_out, lrgen_ches, lrecon_ches, lrgen_mp, contains("_id_"),
               -year_in.x, -year_out.x, -year_in.y, -year_out.y) %>% 
# Consolidation of Swiss data
        mutate(
          year_in = replace(year_in, country_name == "Swtizerland" & year == 2023, 2023), 
          elecdate = replace(elecdate, country_name == "Swtizerland" & year == 2023, 2023)
        ) %>% 
# Removing unnecessary text from some date values
        mutate(
          elecdate = str_sub(elecdate, start = 1, end = 4) %>% as.numeric(), 
          year_out = str_sub(year_out, start = 1,4) %>% as.numeric()
        )
    )
  )

# Join all data with the TGF table  
list_data <-
  list_data %>% 
  add_row(
    source = "FULL_FACTORS",
    data = list(
      list_data %>% 
        filter(source == "TGF") %>% 
        pluck(2, 1) %>% 
        left_join(
          list_data %>% 
            filter(source == "POLITICAL_FACTORS") %>% 
            pluck(2,1) %>% 
            rename(donor_name = country_name), 
          by = c("donor_name", "year")
          ) %>% 
        left_join(
          list_data %>% 
            filter(source == "IMF1") %>%
            pluck(2,1), 
          by = c("donor_name", "year")
          ) %>% 
        left_join(
          list_data %>% 
            filter(source == "IMF2") %>%
            pluck(2,1), 
          by = c("donor_name", "year")
        ) %>%
        left_join(
          list_data %>% 
            filter(source == "OECD") %>% 
            pluck(2,1), 
          by = c("donor_name", "year")
          ) %>% 
        left_join(
          list_data %>% 
            filter(source == "CGD") %>%
            pluck(2,1), 
          by = c("donor_name", "grant_cycle")
          )
      )
    )

# Calculate if election took place during replenishment year
list_data$data[[which(list_data$source == "FULL_FACTORS")]] <-
  list_data %>% 
  filter(source == "FULL_FACTORS") %>% 
  pluck(2,1) %>% 
  mutate(yes_elec = ifelse(year == elecdate, 1, 0))

# Calculate a variable combining left-right ideological placements from CHES and MP
list_data$data[[which(list_data$source == "FULL_FACTORS")]] <-
  list_data %>% 
  filter(source == "FULL_FACTORS") %>% 
  pluck(2,1) %>% 
  mutate(
    lrgen_all = 
      ifelse(
        is.na(lrgen_ches), lrgen_mp, ifelse(
          is.na(lrgen_mp), lrgen_ches, (lrgen_ches + lrgen_mp) / 2
          )
        )
    ) 

# Create grouping variables
list_data$data[[which(list_data$source == "FULL_FACTORS")]] <-
  list_data %>% 
  filter(source == "FULL_FACTORS") %>% 
  pluck(2,1) %>% 
  mutate(
    continent = case_when(
      donor_name %in% c("United States") ~ "United States",
      donor_name %in% c("United States", "Canada", "Mexico", "Barbados", "Brazil", "Paraguay") ~ "Americas (w/o US)",
      donor_name %in% c("European Commission", "France", "Italy", "United Kingdom", "Germany", "Netherlands", 
                        "Sweden", "Norway", "Denmark", "Spain", "Ireland", "Belgium", "Russia", "Switzerland", 
                        "Luxembourg", "Portugal", "Austria", "Iceland", "Greece", "Liechtenstein", "Monaco", 
                        "Andorra", "Poland", "Hungary", "Slovenia", "Finland", "Malta", "Ukraine", "Cyprus") ~ "Europe",
      donor_name %in% c("Nigeria", "South Africa", "Uganda", "Zimbabwe", "Burkina Faso", "Zambia", "Kenya", 
                        "Tunisia", "Namibia", "Côte d'Ivoire", "Malawi", "Benin", "Senegal", "Togo", "Congo", 
                        "Eswatini", "Cameroon", "Rwanda", "Equatorial Guinea", "Burundi", "Central African Republic", 
                        "Chad", "Madagascar", "Niger", "Mali", "Ghana", "Morocco", "Tanzania", "Guinea") ~ "Africa",
      donor_name %in% c("Japan", "Saudi Arabia", "China", "Thailand", "South Korea", "Singapore", "India", 
                        "Kuwait", "Qatar", "United Arab Emirates", "Azerbaijan", "Armenia", "Indonesia", "Australia", "New Zealand") ~ "Asia & Oceania",
      TRUE ~ "Other"
      ),
      oecd_status = case_when(
        donor_name %in% c("United States") ~ "United States",
        donor_name %in% c("Canada", "Mexico", "United Kingdom", "Germany", "France", "Italy", 
                          "Netherlands", "Sweden", "Norway", "Denmark", "Spain", "Ireland", "Belgium", "Switzerland", 
                          "Luxembourg", "Portugal", "Austria", "Iceland", "Greece", "Poland", "Hungary", "Slovenia", 
                          "Finland", "Malta", "Japan", "South Korea", "Australia", "New Zealand") ~ "OECD (w/o US)",
        TRUE ~ "Non-OECD"
      )
    )


# Descriptive Data Analysis -----------------------------------------------------------

# Set up visuals
blue <- "#2E4DF9"
red <- "#EE0C3D"
yellow <- "#F6DE00"
grey <- "#696969"

# Define blue shades
blue_shades <- c(
  "blue_light" = "#9AA7FB",
  "blue_med_light" = "#5D77FA",
  "blue_base" = "#2E4DF9",
  "blue_dark" = "#1E30B3"
)

# Define red shades
red_shades <- c(
  "red_light" = "#F89CAB",
  "red_med_light" = "#F05070",
  "red_base" = "#EE0C3D",
  "red_dark" = "#9E062A"
)

# Define yellow shades
yellow_shades <- c(
  "yellow_light" = "#FFF4A3",
  "yellow_med_light" = "#FBE84F",
  "yellow_base" = "#F6DE00",
  "yellow_dark" = "#BBAA00"
)

# Define purple shades
purple_shades <- c(
  "purple_light" = "#D7A6F2",
  "purple_med_light" = "#B865E8",
  "purple_base" = "#8C29D3",
  "purple_dark" = "#5D0D91"
)

# Define green shades
green_shades <- c(
  "green_light" = "#B9F5B1",
  "green_med_light" = "#73DD63",
  "green_base" = "#44CC36",
  "green_dark" = "#2D8F25"
)

# Define orange shades
orange_shades <- c(
  "orange_light" = "#FFD69A",
  "orange_med_light" = "#FFB847",
  "orange_base" = "#FC9B00",
  "orange_dark" = "#B76B00"
)

source_TGF <- 
  "Source: The Global Fund (TGF), author's calculation\nAuthor: Bruno Alves de Carvalho (balvesdecarvalho1906@gmail.com)"

source_CGD <-
  "Source: Center for Global Development (CGD), author's calculation\nAuthor: Bruno Alves de Carvalho (balvesdecarvalho1906@gmail.com)"

source_OECD <-
  "Source: Organisation for Economic Co-operation and Development (OECD), author's calculation\nAuthor: Bruno Alves de Carvalho (balvesdecarvalho1906@gmail.com)"

source_IMF <-
  "Source: International Monetary Fund (IMF), author's calculation\nAuthor: Bruno Alves de Carvalho (balvesdecarvalho1906@gmail.com)"

source_POL <-
  "Source: Chapel Hill Expert Survey (CHES), Party Government in Europe Database (PAGED), ParlGov project, and\nthe Manifesto Project database (MP), author's calculation\nAuthor: Bruno Alves de Carvalho (balvesdecarvalho1906@gmail.com)"

source_TGFPOL <- 
  "Party Government in Europe Database (PAGED), ParlGov project, and The Global Fund (TGF), author's calculation\nAuthor: Bruno Alves de Carvalho (balvesdecarvalho1906@gmail.com)"

plot_frame_bar <- 
  theme_minimal() + 
  theme( 
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, margin = margin(t = 15, r = 0, b = 0, l = 0), size = 9),
    panel.grid = element_blank(),
    text = element_text(size = 12.5, family = "Arial", color = "black")
  ) 

plot_frame <- 
  theme_minimal() + 
  theme( 
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, margin = margin(t = 15, r = 0, b = 0, l = 0), size = 9),
    panel.grid = element_blank(),
    axis.line = element_line(linewidth = 0.15),
    axis.ticks = element_line(linewidth = 0.15),
    text = element_text(size = 12.5, family = "Arial", color = "black")
  ) 

# Test Hypothesis 1
hypo_01 <-
  list_data %>% 
  filter(source == "FULL_FACTORS") %>% 
  pluck(2,1) %>%
  filter(donor_type == "public") %>% 
  select(pledge_USD, GAVI, ADF, IFAD, IDA, GCF, GEF, GPE, AfDf, CEPI) %>% 
  mutate(across(
    c(pledge_USD, GAVI, ADF, IFAD, IDA, GCF, GEF, GPE, AfDf, CEPI),
    ~ ifelse(. > 0, log(.), NA)
    ))

hypo_01_plot <- 
  hypo_01 %>% 
  pivot_longer(
    cols = c("GAVI", "ADF", "IFAD", "IDA", "GCF", "GEF", "GPE", "AfDf", "CEPI"), 
    names_to = "orgs", values_to = "pledge_orgs"
    ) %>% 
  mutate(org_type = 
           ifelse(orgs %in% c("AfDf", "ADF", "IDA", "IFAD"), "MDB Concessional funds", 
                  ifelse(orgs %in% c("GAVI", "CEPI", "GEF"), "Health funds", "Climate funds"))
         ) %>% 
  filter(pledge_orgs > 0) %>% 
  mutate(orgs = factor(orgs, levels = c("GCF", "GPE", "CEPI", "GAVI", "GEF", "ADF", "AfDf", "IDA", "IFAD"))) %>% 
  ggplot(aes(pledge_orgs, pledge_USD, color = orgs)) + 
  geom_point(alpha = 0.15) + 
  geom_smooth(method = "lm", se = F) + 
  facet_wrap(~ org_type)

hypo_01_corr_matrix <-
  cor(hypo_01, use = "complete.obs")

hypo_01_corr_plot <-
  ggcorrplot::ggcorrplot(
    hypo_01_corr_matrix, 
    lab = TRUE, 
    colors = c(red, "white", blue)
    )
  
# Test Hypothesis 2
hypo_02 <-
  list_data %>% 
  filter(source == "FULL_FACTORS") %>%
  pluck(2,1) %>% 
  filter(donor_type == "public" & !is.na(oda_spent)) %>% 
  select(pledge_USD, oda_spent) %>% 
  mutate(pledge_USD = log(pledge_USD), oda_spent = log(oda_spent))

hypo_02_plot <-
  hypo_02 %>% 
  ggplot(aes(oda_spent, pledge_USD)) + 
  geom_point(alpha = 0.25, color = purple_shades[[3]]) + 
  geom_smooth(method = "lm", se = FALSE, color = purple_shades[[3]])

hypo_02_corr <-
  cor(hypo_02$oda_spent, hypo_02$pledge_USD, use = "complete.obs")

# Test Hypothesis 3
hypo_03 <-
  list_data %>% 
  filter(source == "FULL_FACTORS") %>% 
  pluck(2,1) %>% 
  filter(donor_type == "public") %>% 
  select(pledge_USD, ends_with("rllavg01")) %>% 
  mutate(pledge_USD = log(pledge_USD)) %>% 
  drop_na()

hypo_03_plot <-
  hypo_03 %>% 
  pivot_longer(
    cols = c("expdtr_rllavg01", "revn_rllavg01", "prmryfsclblc_rllavg01", 
             "fsclblc_rllavg01", "adjfsclblc_rllavg01", "grsdbt_rllavg01", 
             "ntdbt_rllavg01", "prmryadjfsclblc_rllavg01"), 
    names_to = "fiscal_indicators", values_to = "obs_value") %>% 
  mutate(
    fiscal_indicators = factor(
      fiscal_indicators, 
      levels = c("prmryfsclblc_rllavg01", "prmryadjfsclblc_rllavg01", "fsclblc_rllavg01", 
                 "adjfsclblc_rllavg01", "grsdbt_rllavg01", "ntdbt_rllavg01", 
                 "expdtr_rllavg01", "revn_rllavg01"),
      labels =c(
        "Primary Fiscal Balance\n(corr -0.14)", "Primary Adjusted Fiscal\nBalance (corr -0.23)", "Fiscal Balance (corr -0.06)", 
        "Adjusted Fiscal Balance\n(corr -0.17)", "Gross Government Debt\n(corr 0.35)", "Net Government Debt\n(corr 0.19)", 
        "Government Expenditure\n(corr 0.30)", "Government Revenue\n(corr 0.24)"
      )
      )
    ) %>%
  ggplot(aes(obs_value, pledge_USD, color = fiscal_indicators)) + 
  geom_point(show.legend = FALSE, alpha = 0.25) + 
  geom_smooth(method = "lm", se = F, show.legend = FALSE) + 
  facet_wrap(~ fiscal_indicators, scales = "free_x")

hypo_03_corr_matrix <-
  cor(hypo_03, use = "complete.obs", method = "pearson")

hypo_03_corr_plot <-
  ggcorrplot::ggcorrplot(
    hypo_03_corr_matrix, 
    lab = TRUE, 
    colors = c(red, "white", blue)
  )
  
# Test Hypothesis 4
hypo_04 <-
  list_data %>% 
  filter(source == "FULL_FACTORS") %>% 
  pluck(2,1) %>% 
  filter(donor_type == "public") %>% 
  select(pledge_USD, ends_with("rllavg02")) %>% 
  mutate(
    pledge_USD = log(pledge_USD),
    gdp_per_cap_cp_rllavg02 = log(gdp_per_cap_cp_rllavg02)
    ) %>% 
  drop_na()

hypo_04_plot_tab <-
  hypo_04 %>% 
  pivot_longer(
    cols = c(c(hypo_04 %>% select(-pledge_USD) %>% colnames)), 
    names_to = "growth_indicators", values_to = "obs_value")

hypo_04_plot <-
  hypo_04_plot_tab %>% 
  mutate(
    growth_indicators = factor(
      growth_indicators, 
      levels = c("gdp_cp_rllavg02",  "gdp_per_cap_cp_rllavg02", "inflation_rt_rllavg02",
                 "unemployment_rt_rllavg02", "exports_vl_rllavg02", "imports_vl_rllavg02",
                 "Total_investment_rllavg02"),
      labels =c(
        "GDP (corr -0.32)", "Log of GDP per Capita\n(corr 0.29)", "Inflation (corr -0.28)", 
        "Unemployment (corr -0.17)", "Exports (corr -0.30)", "Imports (corr -0.21)", 
        "Total Investment (corr -0.02)"
      )
    )
  ) %>%
  ggplot(aes(obs_value, pledge_USD, color = growth_indicators)) + 
  geom_point(alpha = 0.25, show.legend = FALSE) + 
  geom_smooth(method = "lm", se = F, show.legend = FALSE) + 
  facet_wrap(~ growth_indicators, scales = "free_x")

hypo_04_corr_matrix <-
  cor(hypo_04, use = "complete.obs")

hypo_04_corr_plot <-
  ggcorrplot::ggcorrplot(
    hypo_04_corr_matrix, 
    lab = TRUE, 
    colors = c(red, "white", blue)
  )

# Test Hypothesis 5
hypo_05 <-
  list_data %>% 
  filter(source == "FULL_FACTORS") %>% 
  pluck(2,1) %>% 
  filter(donor_type == "public") %>% 
  select(pledge_USD, starts_with("lr")) %>% 
  mutate(pledge_USD = log(pledge_USD))

hypo_05_plot_tab <-
  hypo_05 %>% 
  pivot_longer(
    cols = c(c(hypo_05 %>% select(-pledge_USD) %>% colnames)), 
    names_to = "lr_scale", values_to = "obs_value") %>% 
  filter(lr_scale %in% c("lrgen_all", "lrecon_ches")) %>% 
  mutate(
    lr_scale = factor(
      lr_scale, 
      levels = c("lrgen_all", "lrecon_ches", "lrgen_ches", "lrgen_mp"),
      labels = c("Overall\nIdeological Position", "Economic\nIdeological Position", "lrgen_ches", "lrgen_mp"), 
      )
  )

hypo_05_plot <-
  hypo_05_plot_tab %>% 
  ggplot(aes(obs_value, pledge_USD, color = lr_scale)) + 
  geom_point(alpha = 0.15) + 
  geom_smooth(method = "lm", se = F) + 
  facet_wrap(~ lr_scale)

hypo_05_corr_matrix <-
  cor(hypo_05, use = "complete.obs")

hypo_05_corr_plot <-
  ggcorrplot::ggcorrplot(
    hypo_05_corr_matrix, 
    lab = TRUE, 
    colors = c(red, "white", blue)
  )

# Test Hypothesis 6
hypo_06 <-
  list_data %>% 
  filter(source == "FULL_FACTORS") %>% 
  pluck(2,1) %>% 
  filter(donor_type == "public") %>% 
  select(pledge_USD, yes_elec) %>%
  mutate(pledge_USD_log = log(pledge_USD)) %>% 
  drop_na()

alpha.fn <- function(data, index) {
  df <- data$pledge_USD[index]
  median(df, na.rm = TRUE)
}

set.seed(221)
hypo_06_plot_tab <- 
  hypo_06 %>%
  select(-pledge_USD_log) %>% 
  group_by(yes_elec) %>%  
  summarise(
    median = median(pledge_USD), 
    se = {
      group <- pick(.$yes_elec)
      b <- boot(group, alpha.fn, R = 1000)
      sd(b$t)
    },
    .groups = "drop"
  )

hypo_06_plot <-
  hypo_06_plot_tab %>% 
  ggplot(aes(x = as.factor(yes_elec), y = median)) +
  geom_bar(
    aes(fill = as.factor(yes_elec)), 
    stat = "identity", show.legend = FALSE, width = 0.5
    ) +
  geom_errorbar(
    data = hypo_06_plot_tab %>% filter(yes_elec == 1),
    aes(ymin = median - se, ymax = median + se),
    width = 0.05, linewidth = 0.25
  ) +
  geom_errorbar(
    data = hypo_06_plot_tab %>% filter(yes_elec == 0),
    aes(ymin = median - se, ymax = median + se),
    width = 0.05, linewidth = 0.25
  ) 

hypo_06_corr_matrix <-
  cor(hypo_06 %>% select(-pledge_USD), use = "complete.obs", method = "spearman")

hypo_06_corr_plot <-
  ggcorrplot::ggcorrplot(
    hypo_06_corr_matrix, 
    lab = TRUE, 
    colors = c(red, "white", blue)
  )


# Predictive Data Analysis ------------------------------------------------

# Load price deflator
list_data$data[[which(list_data$source == "FRED")]] <-
  list_data %>% 
  filter(source == "FRED") %>% 
  pluck(2,1) %>% 
  mutate(observation_date = year(observation_date)) %>% 
  rename(year = observation_date, price_deflator = GDPDEF_NBD20220101)

# Transform dollar denominated variables into constant prices
list_data$data[[which(list_data$source == "FULL_FACTORS")]] <-
  list_data %>% 
  filter(source == "FULL_FACTORS") %>% 
  pluck(2,1) %>% 
  left_join(
    list_data %>% 
      filter(source == "FRED") %>%
      pluck(2,1), 
    by = "year") %>% 
  mutate(
    pledge_USD_cp = (pledge_USD / price_deflator) * 100,
    across(GAVI:CEPI, 
    ~ (.x / price_deflator) * 100,
    .names = "{.col}_cp"
    )
  )

# Create dummy variables for each country and check data type for all variables, try to fill in missing values where possible
test <- 
  list_data %>% 
  filter(source == "FULL_FACTORS") %>% 
  pluck(2,1) %>% 
  filter(donor_type == "public") %>% 
  group_by(year, donor_name) %>% 
  mutate(
    other_orgs_cp = sum(GAVI_cp, ADF_cp, IFAD_cp, IDA_cp, GCF_cp, GEF_cp, GPE_cp, AfDf_cp, CEPI_cp, na.rm = TRUE) / 9,
    other_orgs_cp_log = ifelse(is.infinite(other_orgs_cp), 0, log(other_orgs_cp)),
    other_orgs_cp_log = ifelse(other_orgs_cp_log == -Inf, 0, other_orgs_cp_log), # addressing -Inf error with Malta 2022
    pledge_USD_cp_log = log(pledge_USD_cp),
    oda_spent_log = log(oda_spent),
    gdp_per_cap_cp_log_rllavg02 = log(gdp_per_cap_cp_rllavg02),
    lr_all = mean(sum(lrgen_ches, lrgen_mp, lrecon_ches, na.rm = TRUE) / 3),
    year_std = year-2000
    ) %>% 
  ungroup() %>% 
  filter(lr_all != 0) %>% 
# Select relevant variables for modelling
  select(
    pledge_USD_cp_log, # outcome variable
    year_std, "donor_name", # control variables
    other_orgs_cp_log, oda_spent_log, # aid financing variables
    ends_with("rllavg01"), # fiscal variables
    ends_with("rllavg02"), # macroeconomic variables
    lr_all, yes_elec, # political variables
    -gdp_per_cap_cp_rllavg02 # Remove unnecessary variables
    ) %>%
# Remove rows that contain missing data
  na.omit() %>% 
  arrange(donor_name, year_std)

test$donor_name <- 
  relevel(as.factor(test$donor_name), ref = "Malta")

# Set up cross-validation parameters
k <- 5
n <- nrow(test)
x <- model.matrix(pledge_USD_cp_log ~ ., test %>% select(-year_std, -donor_name))[, -1]
x_controls <- model.matrix(pledge_USD_cp_log ~ ., test,contrasts.arg = list(donor_name = "contr.treatment"))[, -1]
y <- test$pledge_USD_cp_log
penalty <- !startsWith(colnames(x_controls), "donor_name") & !startsWith(colnames(x_controls), "year")

# OLS: Model selection and Assessment
set.seed(345)
# Create fold indices 
folds <- 
  sample(rep(1:k, length = n))
# Create vectors to store loop results
cv.errors <- 
  matrix(NA, nrow = k, ncol = sum(penalty))
adjr2_vals <- 
  matrix(NA, nrow = k, ncol = sum(penalty))
adjr2_best_vars <- 
  as_tibble(matrix(NA, nrow = k, ncol = sum(penalty), dimnames = list(c(NULL), c(colnames(x)))))
mse_best_vars <-
  as_tibble(matrix(NA, nrow = k, ncol = sum(penalty), dimnames = list(c(NULL), c(colnames(x)))))
# Loop over folds
for (j in 1:k) {

  # Subset training and test data
  train_data <- 
    test[ , !(names(test) %in% c("year_std", "donor_name"))][folds != j, ]
  test_data <- 
    test[ , !(names(test) %in% c("year_std", "donor_name"))][folds == j, ]
  
  # Fit regsubsets
  best.fit <- regsubsets(
    pledge_USD_cp_log ~ ., 
    data = train_data, 
    nvmax = sum(penalty), 
    method = "forward"
  )
  
  # Extract model sizes and Adjusted R2
  model_sizes <- 
    as.numeric(rownames(summary(best.fit)$which))
  adj_r2_all <- 
    summary(best.fit)$adjr2
  
  for (i in seq_along(model_sizes)) {
    
    id_val <- 
      model_sizes[i]
    
    # Predictions on validation fold
    pred <- 
      predict(best.fit, test_data, id = id_val)
    
    # Store CV error
    cv.errors[j, i] <- 
      mean((test$pledge_USD_cp_log[folds == j] - pred)^2)
    
    # Store Adjusted R2
    adjr2_vals[j, i] <- adj_r2_all[id_val]
  }
  
  # Store variables in the best model based on adjr2
  adjr2_best_vars[j,colnames(adjr2_best_vars) %in% names(coef(best.fit, which.max(adjr2_vals[j, ])))] <- 
    as_tibble_row(coef(best.fit, which.max(adjr2_vals[j, ])))[,colnames(as_tibble_row(coef(best.fit, which.max(adjr2_vals[j, ])))) %in% colnames(adjr2_best_vars)]
  
  # Store variables in the best model based on mse
  mse_best_vars[j,colnames(mse_best_vars) %in% names(coef(best.fit, which.min(cv.errors[j, ])))] <- 
    as_tibble_row(coef(best.fit, which.min(cv.errors[j, ])))[,colnames(as_tibble_row(coef(best.fit, which.min(cv.errors[j, ])))) %in% colnames(mse_best_vars)]
  
}

# Calculate mean MSE
mean.cv.errors <-
  apply(cv.errors, 2, mean)

# Table with variables included in the best model based on adjr2
cv_adjr2_results_ols <- 
  pivot_longer(
  adjr2_best_vars, 
  cols = colnames(adjr2_best_vars), 
  names_to = "Variable", 
  values_to = "Coef") %>% 
  filter(!is.na(Coef)) %>% 
  group_by(Variable) %>% 
  summarise(Included = n(), Mean_Coef = mean(Coef)) %>% 
  arrange(desc(Included), desc(abs(Mean_Coef)))

# Table with variables included in the best model based on mse
cv_mse_results_ols <-
  pivot_longer(
    mse_best_vars, 
    cols = colnames(mse_best_vars), 
    names_to = "Variable", 
    values_to = "Coef") %>% 
  filter(!is.na(Coef)) %>% 
  group_by(Variable) %>% 
  summarise(Included = n(), Mean_Coef = mean(Coef)) %>% 
  arrange(desc(Included), desc(abs(Mean_Coef)))

# Ridge: Model selection and Assessment (select best tuning parameter first on training data)

# Loop over folds
cv_results_ridge <- 
  reg_model(0)
cv_mse_ridge <-
  cv_results_ridge$mse
cv_results_ridge_tab <-
  pivot_longer(
    cv_results_ridge$vars, 
    cols = colnames(cv_results_ridge$vars), 
    names_to = "Variable", 
    values_to = "Coef") %>% 
  filter(!is.na(Coef)) %>% 
  group_by(Variable) %>% 
  summarise(Mean_Coef = mean(Coef)) %>% 
  arrange(desc(abs(Mean_Coef)))

# Compute best lambda value
cv.ridge <- 
  cv.glmnet(x, y, alpha = 0)

# Store best lambda value
ridge_bestlam <- 
  cv.ridge$lambda.min

# Plot lambda path and selection
plot(cv.ridge)

# Plot coefficient path, WITHOUT control variables
coef_path_plot(0, ridge_bestlam, 0.049)


# Lasso: Model selection and Assessment (select best tuning parameter first on training data)

# Loop over training data, WITHOUT control variables
cv_results_lasso <- 
  reg_model(1)
cv_mse_lasso <-
  cv_results_lasso$mse
cv_results_lasso_tab <-
  pivot_longer(
    cv_results_lasso$vars, 
    cols = colnames(cv_results_lasso$vars), 
    names_to = "Variable", 
    values_to = "Coef") %>% 
  filter(Coef != 0) %>% 
  group_by(Variable) %>% 
  summarise(Included = n(), Mean_Coef = mean(Coef)) %>% 
  arrange(desc(Included), desc(abs(Mean_Coef)))

# Plot coefficient path on full date, WITHOUT control variables
bestlam_lasso_nocontrols <-
  cv.glmnet(x, y, alpha = 1)$lambda.min
coef_path_plot(1, bestlam_lasso_nocontrols, 0) +
  theme(
    legend.position = c(0.8, 0.65),  
    legend.title = element_blank()
  )

# Fit lasso model on full data, WITH control variables
cv_lasso <- 
  cv.glmnet(x_controls, y, alpha = 1, penalty.factor = penalty)
plot(cv_lasso)
bestlam_lasso <- 
  cv_lasso$lambda.min
lasso_mod <- 
  glmnet(x_controls, y, alpha = 1, lambda = bestlam_lasso, penalty.factor = penalty)
lasso_coef <- 
  as.vector(coef(lasso_mod))
names(lasso_coef) <- 
  rownames(coef(lasso_mod))
lasso_coef <-
  enframe(lasso_coef, name = "Variable", value = "Coef") %>% 
  filter(Coef != 0 & !Variable %in% c("year_std", "(Intercept)") & !str_starts(Variable, "donor_name")) %>% 
  arrange(desc(abs(Coef)))

# Plot coefficient path, WITH control variables
coef_path_plot(1, bestlam_lasso, 0, control_vars = TRUE) + xlim(-10, 0)


# Fit post-lasso OLS for interpretation
lm.formula.ve01 <-
  as.formula(
    paste0(
      "pledge_USD_cp_log ~ donor_name + year_std + ", # outcome and control variables
      paste0(cv_mse_results_ols %>% pluck(1), collapse = "+") # predictor variables
      )
    )
lm.mod.ve01 <-
  lm(
    lm.formula.ve01,
    data = test
    )
summary(lm.mod.ve01)
ols_coef <-
  lm.mod.ve01$coefficients %>% 
  enframe(name = "Variables", value = "Coef") %>% 
  filter(!startsWith(Variables, "donor_name") & !Variables %in% c("year_std", "(Intercept)"))

lm.formula.ve02 <-
  as.formula(
    paste0(
      "pledge_USD_cp_log ~ donor_name + year_std + ", # outcome and control variables
      paste0(lasso_coef %>% pluck(1), collapse = "+") # predictor variables
    )
  )
lm.mod.ve02 <-
  lm(
    lm.formula.ve02,
    data = test
  )
summary(lm.mod.ve02)


library(modelr)
# Examine residuals from both models
test %>% select(pledge_USD_cp_log, donor_name, year_std, lasso_coef %>% pluck(1), cv_mse_results_ols %>% pluck(1)) %>% gather_predictions(lm.mod.ve01, lm.mod.ve02) %>% select(pred, pledge_USD_cp_log, model, donor_name, year_std) %>% mutate(resid = pledge_USD_cp_log - pred) %>% filter(abs(resid) > 1)
test %>% select(pledge_USD_cp_log, donor_name, year_std, lasso_coef %>% pluck(1), cv_mse_results_ols %>% pluck(1)) %>% add_predictions(lm.mod.ve02) %>% add_residuals(lm.mod.ve02) %>% ggplot(aes(year_std)) + geom_point(aes(y = pledge_USD_cp_log)) + geom_point(aes(y = pred, size = abs(resid)), color = "red", alpha = 0.5)  
test %>% select(pledge_USD_cp_log, donor_name, year_std, lasso_coef %>% pluck(1), cv_mse_results_ols %>% pluck(1)) %>% mutate(donor_level = ifelse(donor_name %in% c("United States", "France", "United Kingdom", "Germany", "Japan"), "top5", "other")) %>% add_predictions(lm.mod.ve02) %>% add_residuals(lm.mod.ve02) %>% ggplot(aes(year_std, resid)) + geom_line(aes(color = donor_name), alpha = 1/3) + facet_wrap(~ donor_level) + geom_hline(yintercept = 0, color = "white", linewidth = 2) + scale_color_manual(values = c("United States" = red_shades[[2]])) + ylim(-2.5,2.5)

df_models <- test[colnames(test) %in% c("pledge_USD_cp_log", "donor_name", "year_std", lasso_coef %>% pluck(1), cv_mse_results_ols %>% pluck(1))]
boot_size <- 1000
list01 <- rep(list(1:n), n)
list02 <- rep(list(1:n), n)
list03 <- rep(list(1:n), n)
list04 <- rep(list(1:n), n)
list05 <- rep(list(1:n), n)
vec01 <- vector("double", length = n)
vec02 <- vector("double", length = n)
vec03 <- vector("double", length = n)

set.seed(456)
for (i in seq_along(list01)) {
  sample_ols <- test[-i, ]
  list01[[i]] <- sample_ols
  
  model_ve02 <- lm(lm.formula.ve02, data = list01[[i]])
  list02[[i]] <- model_ve02
  
  vec02[i] <- tryCatch({
    as_tibble_col(
      predict(model_ve02, newdata = test[i,]),
      column_name = "y_pred"
    ) %>%
      bind_cols(as_tibble_col(test$pledge_USD_cp_log[i], column_name = "y_actual")) %>%
      mutate(mse = (y_actual - y_pred)^2) %>%
      pluck(3)
  }, error = function(e) {
    NA  # return NA if prediction fails
  })
  
  model_ve01 <- lm(lm.formula.ve01, data = list01[[i]])
  list03[[i]] <- model_ve01
  
  vec01[i] <- tryCatch({
    as_tibble_col(
      predict(model_ve01, newdata = test[i,]),
      column_name = "y_pred"
    ) %>%
      bind_cols(as_tibble_col(test$pledge_USD_cp_log[i], column_name = "y_actual")) %>%
      mutate(mse = (y_actual - y_pred)^2) %>%
      pluck(3)
  }, error = function(e) {
    NA  # return NA if prediction fails
  })
  
  sample_lasso <- x_controls[-i, ]
  list04[[i]] <- sample_lasso
  
  cv_model <- 
    cv.glmnet(list04[[i]], y[-i], alpha = 1, penalty.factor = penalty)
  
  model_ve03 <- glmnet(list04[[i]], y[-i], alpha = 1, lambda = cv_model$lambda.min, penalty.factor = penalty)
  list05[[i]] <- model_ve03
  
  vec03[i] <- tryCatch({
    as_tibble_col(
      predict(model_ve03, newx = x_controls[i, ]),
      column_name = "y_pred"
    ) %>%
      bind_cols(as_tibble_col(y[i], column_name = "y_actual")) %>%
      mutate(mse = (y_actual - y_pred)^2) %>%
      pluck(3)
  }, error = function(e) {
    NA  # return NA if prediction fails
  })
  
} 

boot_ve01 <-
  as_tibble_col(list03, column_name = "model") %>% 
  mutate(fit = map(model, broom::glance), mse = vec01) %>% 
  unnest(fit) %>% 
  mutate(model = "ve01")
boot_ve01 %>% pluck(3) %>% mean()
boot_ve01 %>% pluck(14) %>% mean(na.rm = TRUE)

boot_ve02 <- 
  as_tibble_col(list02, column_name = "model") %>% 
  mutate(fit = map(model, broom::glance), mse = vec02) %>% 
  unnest(fit) %>% 
  mutate(model = "ve02")
boot_ve02 %>% pluck(3) %>% mean()
boot_ve02 %>% pluck(14) %>% mean(na.rm = TRUE)

boot_ve03 <- 
  as_tibble_col(list05, column_name = "model") %>% 
  mutate(fit = map(model, broom::glance), mse = vec03) %>% 
  unnest(fit) %>% 
  mutate(model = "ve03")
boot_ve03 %>% pluck(5) %>% mean(na.rm = TRUE)

ggplot(
  data = bind_rows(boot_ve01, boot_ve02, boot_ve03) %>%
    select(model, adj.r.squared, p.value, AIC, BIC, deviance, mse) %>%
    pivot_longer(cols = c("adj.r.squared", "p.value", "AIC", "BIC", "deviance"), names_to = "metric", values_to = "values"), 
  aes(y = values, group = model)
  ) + 
  geom_boxplot() +
  facet_wrap(~ metric, scales= "free")

# Post-estimation
library(sandwich)
library(lmtest)

# Post-Estimation ---------------------------------------------------------

# Non-linearity of the response-predictor relationships: SUCCESS
plot(lm.mod.ve02, which = 1)
car::crPlots(lm.mod.ve02, layout = c(4, 3))

# Correlation of error terms: PARTIAL SUCCESS, but MITIGATED with Robust SEs
dev.off()
plot(residuals(lm.mod.ve02), type = "l",
     main = paste0("Residuals over Observations\n(", "corr ", round(cor(1:length(resids), resids), 3),")"),
     xlab = "Observation index",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)
acf(residuals(lm.mod.ve02), main = "ACF of Residuals")
resids <- residuals(lm.mod.ve02)
cor(1:length(resids), resids)
lmtest::dwtest(lm.mod.ve02)

# Zero Conditional Mean
test$year_c <- 
  scale(test$year_std, center = TRUE, scale = FALSE)
lm.formula.ve03 <-
  as.formula(
    paste0(
      "pledge_USD_cp_log ~ donor_name + year_c + I(year_c^2) +", # outcome and control variables
      paste0(lasso_coef %>% pluck(1), collapse = "+") # predictor variables
    )
  )
lm.mod.ve03 <-
  lm(
    lm.formula.ve03,
    data = test
  )
summary(lm.mod.ve03)
lmtest::resettest(lm.mod.ve03)

# Non-constant variance of error terms: SUCCESS
plot(lm.mod.ve03, which = 3)
lmtest::bptest(lm.mod.ve03)

# Outliers: FAIL (3 outliers) but NEGLIGEABLE impact
plot(lm.mod.ve03, which = 5)
test %>% slice(c(36, 111, 132))

# High-leverage points: FAIL (9 high leverage points) but NEGLIGEABLE impact
leverage <- hatvalues(lm.mod.ve03)
high_leverage <- which(leverage > (2 * mean(leverage)))
test[high_leverage, ]

# Collinearity: FAIL (2 problematic) but JUSTIFIED
vif_values_ve02 <- car::vif(lm.mod.ve02)
vif_values_ve03 <- car::vif(lm.mod.ve03)

# Normality: (robust SEs if necessary)
library(moments)
plot(lm.mod.ve03, 2)
skewness(residuals(lm.mod.ve03))
kurtosis(residuals(lm.mod.ve03))



# Calculate robust SEs to account for auto-correlation of error terms
lm.mod.lasso.robust <- 
  lmtest::coeftest(lm.mod.ve02, vcov = sandwich::vcovCL(lm.mod.ve02, cluster = ~ donor_name))
lm.mod.lasso.robust

# Compare full model with model without high leverage points and robust model with minimized outliers and leverage points
model_robust <- MASS::rlm(pledge_USD_cp_log ~ ., data = test %>% select(pledge_USD_cp_log, other_orgs_cp_log, oda_spent_log, lr_all, yes_elec, unemployment_rt_rllavg02, inflation_rt_rllavg02, Total_investment_rllavg02, gdp_cp_rllavg02, ntdbt_rllavg01, adjfsclblc_rllavg01, starts_with("donor_name"), year_std))
model_clean <- lm(pledge_USD_cp_log ~ ., data = test[-high_leverage,] %>% select(pledge_USD_cp_log, other_orgs_cp_log, oda_spent_log, lr_all, yes_elec, unemployment_rt_rllavg02, inflation_rt_rllavg02, Total_investment_rllavg02, gdp_cp_rllavg02, ntdbt_rllavg01, adjfsclblc_rllavg01, starts_with("donor_name"), year_std))

rmse <- function(model) {
  sqrt(mean(residuals(model)^2))
}
rmse(lm.mod.ve02)
rmse(model_robust)
rmse(model_clean)

# Get predicted values
test$pred_full <- predict(lm.mod.ve02)
test_clean <- test[-high_leverage, ]  # remove high leverage points
test_clean$pred_clean <- predict(model_clean)
test_robust <- test
test_robust$pred_robust <- predict(model_robust)

# Add actual values
test$actual <- test$pledge_USD_cp_log
test_clean$actual <- test_clean$pledge_USD_cp_log
test_robust$actual <- test$pledge_USD_cp_log

# Combine both into a single data-frame for ggplot
test$Model <- "Full Model"
test_clean$Model <- "Clean Model"
test_robust$Model <- "Robust Model"

combined <- bind_rows(
  test %>% select(actual, predicted = pred_full, Model),
  test_clean %>% select(actual, predicted = pred_clean, Model),
  test_robust %>% select(actual, predicted = pred_robust, Model)
)

ggplot(combined, aes(x = actual, y = predicted, color = Model)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dotted") +  # ideal line
  labs(title = "Predicted vs Actual Values",
       x = "Actual (log pledge)",
       y = "Predicted (log pledge)") +
  theme_minimal() +
  scale_color_manual(values = c("Full Model" = blue_shades[[3]], "Clean Model" = red_shades[[3]], "Robust Model" = yellow_shades[[3]]))






