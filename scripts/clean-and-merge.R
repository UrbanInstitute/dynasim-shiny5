## Libraries and Source Files
library(tidyverse)

options(scipen = 999)

# Load Data
options <- read_csv("data/options.csv")

,
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
         `10th Percentile` = `10`,
         `20th Percentile` = `20`,
         `30th Percentile` = `30`,
         `40th Percentile` = `40`,
         `50th Percentile` = `50`,
         `60th Percentile` = `60`,
         `70th Percentile` = `70`,
         `80th Percentile` = `80`,
         `90th Percentile` = `90`,
         `95th Percentile` = `95`,
         `98th Percentile` = `98`)

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