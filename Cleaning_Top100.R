library(tidyverse)


data_ba <- read_csv('data/2023_Top100_BA.csv')

data_ba <- data_ba[-c(which(data_ba$rank == 'rank')),]

write_csv(data_ba, 'data/2023_Top100_BA.csv')


for (year in 2014:2024){
  print(year)
  # Baseball America
  data_ba <- read_csv(glue::glue('data/{year}_Top100_BA.csv'), show_col_types = FALSE)
  #safeguard in case csv is already cleaned
  if (length(which(data_ba$rank == 'rank')) > 0) {data_ba <- data_ba[-c(which(data_ba$rank == 'rank')),]}
  data_ba$year <- year
  data_ba <- janitor::clean_names(data_ba)
  data_ba <- data_ba %>% 
    mutate(height_in = as.integer(str_split_i(ht, pattern = '-', 1))*12 + as.integer(str_split_i(ht, pattern = '-', 2))) %>% 
    select(-ht) %>% 
    relocate(height_in, .before = 'wt')
  write_csv(data_ba, glue::glue('data/{year}_Top100_BA.csv'))
  #only have mlb pipline top 100 after 2016
  if (year >= 2016){
  # MLB Pipeline
  data_mlb <- read_csv(glue::glue('data/{year}_Top100_MLB.csv'), show_col_types = FALSE)
  #safeguard in case csv is already cleaned
  if (length(which(data_mlb$rank == 'rank')) > 0) {data_mlb <- data_mlb[-c(which(data_mlb$rank == 'rank')),]}
  data_mlb$year <- year
  data_mlb <- janitor::clean_names(data_mlb)
  data_mlb <- data_mlb %>% 
    mutate(height_in = as.integer(str_split_i(ht, pattern = '-', 1))*12 + as.integer(str_split_i(ht, pattern = '-', 2))) %>% 
    select(-ht) %>% 
    relocate(height_in, .before = 'wt')
  write_csv(data_mlb, glue::glue('data/{year}_Top100_MLB.csv'))
  }
  print(nrow(data_ba)); print(nrow(data_mlb))
}

## final top 100 BA rankings
combine_ba_rankings <- function(year){
  data_ba <- read_csv(glue::glue('data/{year}_Top100_BA.csv'))
  return(data_ba)
}

ba_rankings <- map_df(2014:2024, combine_ba_rankings)


## final top 100 MLB rankings
combine_mlb_rankings <- function(year){
  data_mlb <- read_csv(glue::glue('data/{year}_Top100_MLB.csv'))
  return(data_mlb)
}

mlb_rankings <- map_df(2016:2024, combine_mlb_rankings)

# renaming ranks for each ranking
ba_rankings <- ba_rankings %>% 
  rename('rank_ba' = 'rank')

mlb_rankings <- mlb_rankings %>% 
  rename('rank_mlb' = 'rank')
