library(tidyverse)

team_rankings <- read_csv('Data/prospect_data.csv') %>% 
  filter(Year >= 2014, prospectType == 'Team')

#filtering for Baseball America and MLB Pipeline Rankings
team_rankings <- team_rankings %>% 
  filter(Prospect_Source %in% c("Baseball America",'MLB Pipeline'))

# getting player name
team_rankings <- team_rankings %>% 
  mutate(Name = str_c(firstname, lastname, sep = ' '))

#selecting necessary columns
team_rankings <- team_rankings %>% 
  select(Year, Prospect_Source, Name, Rank, TeamAbbr)

# fixing Team Abbrevs so that they match with fangraphs

sort(unique(team_rankings$TeamAbbr))

sort(unique(minor_league_hitting$Team))

team_rankings <- team_rankings %>% 
  mutate(TeamAbbr = case_when(
    TeamAbbr == 'ANA' ~ 'LAA',
    TeamAbbr =='CHN' ~ 'CHC',
    TeamAbbr =='CHA' ~ 'CHW',
    TeamAbbr =='KCA' ~ 'KCR',
    TeamAbbr == 'LAN' ~ 'LAD',
    TeamAbbr == 'NYA' ~ 'NYY',
    TeamAbbr == 'NYN' ~ 'NYM',
    TeamAbbr == 'SDN' ~ 'SDP',
    TeamAbbr == 'SFN' ~ 'SFG',
    TeamAbbr == 'SLN' ~ 'STL',
    TeamAbbr == 'TBA' ~ 'TBR',
    TeamAbbr == 'WAS' ~ 'WSN',
    .default = TeamAbbr
  ))

# separating Baseball America and MLB Pipeline Rankings
team_rankings <- team_rankings %>% 
  group_by(Name, Year, Prospect_Source) %>% 
  mutate(Rank = mean(Rank, na.rm = TRUE)) %>% 
  ungroup() %>% 
  distinct() %>% 
  pivot_wider(names_from = Prospect_Source, values_from = 'Rank', names_prefix = 'Rank_')

team_rankings <- team_rankings %>% 
  rename('team_rank_ba' = 'Rank_Baseball America',
         'team_rank_mlb' = 'Rank_MLB Pipeline') %>% 
  mutate(across(c('team_rank_ba','team_rank_mlb'), ~replace_na(., 35)))
