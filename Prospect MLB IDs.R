library(baseballr)
library(tidyverse)


glimpse(playerid_lookup(last_name = 'Zhuang'))

player <- 'Nacho Alvarez Jr.'

str_remove(player, ' Jr.| jr.')
str_split(player, ' ')

tryCatch({
  df <- playerid_lookup(last_name = 'Alvarez', first_name = 'Nacho')
  if (nrow(df) == 0){
    df <- playerid_lookup(last_name = 'Ginn') %>% 
      filter(birth_year %in% c(1999,2000))
    print(df)
  }
}, error = function(e){
  print('No MLBID')
})
        