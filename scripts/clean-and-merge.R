## Libraries and Source Files
library(tidyverse)

options(scipen = 999)

# Load Data
options <- read_csv("data/options.csv",
                    col_types = cols(
                      Percentile = col_character(),
                      Age = col_integer(),
                      cohort = col_character(),
                      value = col_double(),
                      data_source = col_character(),
                      Asset = col_character()
                    )
)

validation <- read_csv("data/validation.csv",
                       col_types = cols(
                         Age = col_integer(),
                         Percentile = col_character(),
                         cohort = col_character(),
                         value = col_double(),
                         data_source = col_character(),
                         Asset = col_character()
                       )
)

ntiles <- bind_rows(options, validation)
rm(options, validation)

# spread assets into columns
ntiles <- ntiles %>%
  spread(Percentile, value) %>%
  rename(`5th percentile` = `5`,
         `10th percentile` = `10`,
         `20th percentile` = `20`,
         `30th percentile` = `30`,
         `40th percentile` = `40`,
         `50th percentile` = `50`,
         `60th percentile` = `60`,
         `70th percentile` = `70`,
         `80th percentile` = `80`,
         `90th percentile` = `90`,
         `95th percentile` = `95`,
         `98th percentile` = `98`)

# break ntiles down by asset asset
ntiles %>%
  filter(Asset == "Financial assets") %>%
  select(-Asset) %>%
  write_csv("data/financial-assets.csv")

ntiles %>%
  filter(Asset == "Home equity") %>%
  select(-Asset) %>%  
  write_csv("data/home-equity.csv")

ntiles %>%
  filter(Asset == "Retirement account assets") %>%
  select(-Asset) %>%  
  write_csv("data/retirement-account-assets.csv")

ntiles %>%
  filter(Asset == "Total assets") %>%
  select(-Asset) %>%  
  write_csv("data/total-assets.csv")

# remove old data files
file.remove("data/options.csv")
file.remove("data/validation.csv")