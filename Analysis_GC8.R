# Project Name: The Global Fund to Fight HTM GC8 Replenishment
# Author: Bruno Alves de Carvalho
# Status: ongoing


# Set up ------------------------------------------------------------------

# Load packages
library(tidyverse)
library(googlesheets4)
library(slider)

# Link to Google Sheets
sheet_url <- 
  "https://docs.google.com/spreadsheets/d/1tc4cgr_uA36VCEW33GbZVTaPehKALThdjSd2QzBSYgM/edit?gid=406461073#gid=406461073"

# Create function to load data
my_sheet_names <- 
  c(
    "OECD" = "ODA Disbursements 1997-2023", 
    "CGD" ="Other Replenishments", 
    "TGF" = "0-7 Replenishments", 
    "IMF" = "Macroeconomics 2000-2024"
    )

load_data <- 
  function(x) {
    read_sheet(
      sheet_url, sheet = my_sheet_names[[x]]
    )
  }

# Load data
list_data <- tibble(
  source = names(my_sheet_names),
  data = vector("list", length(my_sheet_names))
)

for (i in seq_along(my_sheet_names)) {
  list_data$data[[i]] <- load_data(i) 
}

# create a copy of the loaded data
copy_list_data <- list_data

# Select and rename columns from OECD data
list_data$data[[1]] <- 
  list_data %>% 
  pluck(2, 1) %>% 
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
    )

# Gather CGD data by name of organization and replenishment year
list_data$data[[2]] <-
  list_data$data[[2]] %>% 
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
  list_data$data[[2]] %>%
  select(-donor_name) %>% 
  group_by(org, year, grant_cycle) %>% 
  summarise(sum = sum(money, na.rm = T)) %>% 
  group_by(grant_cycle) %>% 
  summarise(n_rplnshmnt = n())

# Spread CDG data by MDB and Health Fun
list_data$data[[2]] <-
  list_data$data[[2]] %>%
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
  left_join(tab_n_rplnshmnt, by = "grant_cycle")

# Create a Grant Cycle variable in the TGF data
list_data$data[[3]] <- 
  list_data %>% 
  pluck(2,3) %>% 
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
IMF_data <- 
  read_csv("/Users/brunoalvesdecarvalho/Desktop/Research/IMF/dataset_2024-12-22T16_56_47.354302444Z_DEFAULT_INTEGRATION_IMF.FAD_FM_2.0.0.csv")

# Organize IMF data into appropriate format
IMF_test<-
  IMF_data %>% 
  select(
    donor_name = COUNTRY.Name, 
    fiscal_indicator = INDICATOR.Name, 
    starts_with("19"), 
    starts_with("20")) %>% 
  pivot_longer(
    cols = na.omit(str_extract(colnames(IMF), "\\d+")), 
    names_to = "year", values_to = "obs_value") %>% 
  mutate(
    donor_name = str_extract(donor_name, "^[^,]+")
  ) %>% 
  filter(donor_name != "Congo") %>% 
  pivot_wider(names_from = fiscal_indicator, values_from = obs_value)

IMF_test <- IMF_test %>% group_by(donor_name)

for (i in seq_along(IMF_test[1:8])+2) {
  
  IMF_test %>% 
    mutate(
      rolling_mean = slide_mean(
        `Revenue, General government, Percent of GDP`, before = 2, na_rm = T
      )
    )
  
}


IMF_test <- IMF_test %>% ungroup()

## to-do: link each year to a Grant Cycle; look for pledge realization by year


