library(tidyverse)
library(gridExtra)

## fangraphs pitching minors
pitching_standard_df <- read_csv('Data/pitchers_thesis_data_standard.csv')
pitching_advanced_df <- read_csv('Data/pitchers_thesis_data_advanced.csv')
pitching_battedball_df <- read_csv('Data/pitchers_thesis_data_batted_ball.csv')

## joining on PlayerId, Season, Level, & Team
minor_league_pitching <- pitching_standard_df %>% 
  filter(Level %in% c('A', 'A+' , 'AA', 'AAA')) %>% 
  left_join(pitching_advanced_df, by = c('Season', 'PlayerId', 'Level', 'Team', 'Name',
                                        'Age')) %>% 
  left_join(pitching_battedball_df, by = c('Season', 'PlayerId', 'Level', 'Team', 'Name',
                                          'Age')) 

glimpse(minor_league_pitching)

# adding BF weights
minor_league_pitching <- minor_league_pitching %>% 
  group_by(PlayerId,Level) %>% 
  mutate(bf_weights = TBF/sum(TBF)) %>% 
  relocate(bf_weights, .after = 'Level') %>% 
  arrange(Name, Age, Level) %>%
  ungroup()

# finding the last season they were at that level
minor_league_pitching <- minor_league_pitching %>% 
  group_by(PlayerId, Level) %>% 
  mutate(lag_year = Season - lag(Season)) %>% 
  mutate(lag_year = ifelse(is.na(lag_year), 0, lag_year)) %>% 
  ungroup() %>% 
  relocate(lag_year, .after = 'bf_weights')

# removing seasons that have been more than 2 years since he was last promoted
minor_league_pitching <- minor_league_pitching %>% 
  filter(lag_year <= 2)  %>% 
  group_by(PlayerId,Level) %>% 
  mutate(bf_weights = TBF/sum(TBF)) %>% #recalculating pa_weights
  ungroup()


# renaming columns
minor_league_pitching <- minor_league_pitching %>% 
  select(-ERA.y, -IP.x, -IP.y, -BABIP.y) %>% 
  rename('BB_pct' = 'BB%', 'K_pct' = 'K%', 'GB_per_FB' = 'GB/FB',
         'LD_pct' = 'LD%', 'GB_pct' = 'GB%', 'FB_pct' = 'FB%',
         'IFFB_pct' = 'IFFB%', 'HR_per_FB' = 'HR/FB', 'Pull_pct' = 'Pull%',
         'Cent_pct' = 'Cent%', 'Oppo_pct' = 'Oppo%', 'SwStr_pct' = 'SwStr%',
          'K_9' = 'K/9', 'BB_9' = 'BB/9','HR_9' = 'HR/9', 
         'K_minus_BB_pct' = 'K-BB%', 'ERA' = 'ERA.x','BABIP' = 'BABIP.x',
         'ERA_minus_FIP' = 'E-F', 'K_BB' = 'K/BB', 'LOB_pct' = 'LOB%')

glimpse(minor_league_pitching)

# Joining on Baseball America Rankings
minor_league_pitching <- minor_league_pitching %>% 
  left_join(ba_rankings, by = c('Name' = 'player', 'Season' = 'year'), relationship = 'many-to-many') %>% 
  relocate(rank_ba, .after = bf_weights)

minor_league_pitching <- minor_league_pitching %>% 
  mutate(rank_ba = ifelse(is.na(rank_ba), 110, rank_ba)) %>% 
  select(-c(pos, ba, th, born, place, mlb_years, stat_years, draft_info))

#joining MLB Pipeline Rankings
minor_league_pitching <- minor_league_pitching %>% 
  left_join(mlb_rankings, by = c('Name' = 'player', 'Season' = 'year'), relationship = 'many-to-many') %>% 
  relocate(rank_mlb, .after = rank_ba)

# filling in NA values with 110
minor_league_pitching <- minor_league_pitching %>% 
  mutate(rank_mlb = ifelse(is.na(rank_mlb), 110, rank_mlb)) %>% 
  select(-c(pos, ba, th, born, place, mlb_years, stat_years, draft_info))

# dropping duplicate weight and height columns
minor_league_pitching <- minor_league_pitching %>% 
  select(-height_in.y, -wt.y,-hilvl.y) %>% 
  rename('height_in' = 'height_in.x', 'wt' = 'wt.x', 'hilvl' = 'hilvl.x')



#joining team rankings
minor_league_pitching <- minor_league_pitching %>% 
  left_join(team_rankings, by = c('Season' = 'Year', 'Name' = 'Name', 'Team' = 'TeamAbbr')) %>% 
  mutate(across(c('team_rank_ba','team_rank_mlb'), ~replace_na(., 35)))


for (level in c('A', 'A+', 'AA', 'AAA')){
  print(level)
  
  df <- minor_league_pitching %>% filter(Level == level)
  
  df <- df %>% 
    group_by(PlayerId, Name, Level) %>% 
    reframe(
      rank_ba = mean(rank_ba, na.rm = TRUE),
      rank_mlb = mean(rank_mlb, na.rm =TRUE),
      team_rank_ba = mean(team_rank_ba, na.rm = TRUE),
      team_rank_mlb =  mean(team_rank_mlb, na.rm = TRUE),
      Age = mean(Age, na.rm = TRUE),
      G = sum(G, na.rm = TRUE),
      GS = sum(GS, na.rm = TRUE),
      CG = sum(CG, na.rm = TRUE),
      ShO = sum(ShO, na.rm = TRUE),
      W = sum(W, na.rm = TRUE),
      L = sum(L, na.rm = TRUE),
      SV = sum(SV, na.rm = TRUE),
      BS = sum(BS, na.rm = TRUE),
      ERA = sum(bf_weights*ERA, na.rm = TRUE),
      TBF = sum(TBF, na.rm = TRUE),
      H = sum(H, na.rm = TRUE),
      R = sum(R, na.rm = TRUE),
      ER = sum(ER, na.rm = TRUE),
      HR = sum(HR, na.rm = TRUE),
      BB = sum(BB, IBB, HBP, na.rm = TRUE), # combining BB, IBB, and HBP for simplicity/less models for similar result
      SO = sum(SO, na.rm = TRUE),
      WP = sum(WP, na.rm = TRUE),
      BK = sum(BK, na.rm = TRUE),
      K_9 = sum(bf_weights*K_9, na.rm = TRUE),
      BB_9 = sum(bf_weights*BB_9, na.rm = TRUE),
      K_BB = sum(bf_weights*K_BB, na.rm = TRUE),
      HR_9 = sum(bf_weights * HR_9, na.rm = TRUE),
      K_pct = sum(bf_weights * K_pct, na.rm = TRUE),
      BB_pct = sum(bf_weights * BB_pct, na.rm = TRUE),
      K_minus_BB_pct = sum(bf_weights * K_minus_BB_pct, na.rm = TRUE),
      AVG = sum(bf_weights * AVG, na.rm = TRUE),
      WHIP = sum(bf_weights * WHIP, na.rm = TRUE),
      BABIP = sum(bf_weights * BABIP, na.rm = TRUE),
      LOB_pct = sum(bf_weights * LOB_pct, na.rm = TRUE),
      FIP = sum(bf_weights * FIP, na.rm = TRUE),
      ERA_minus_FIP = sum(bf_weights * ERA_minus_FIP, na.rm = TRUE),
      xFIP = sum(bf_weights * xFIP, na.rm = TRUE),
      IP = sum(IP, na.rm = TRUE),
      GB_per_FB = sum(bf_weights*GB_per_FB, na.rm = TRUE),
      LD_pct = sum(bf_weights * LD_pct, na.rm = TRUE),
      GB_pct = sum(bf_weights * GB_pct, na.rm = TRUE),
      FB_pct = sum(bf_weights * FB_pct, na.rm = TRUE),
      IFFB_pct = sum(bf_weights * IFFB_pct, na.rm = TRUE),
      IFFB_pct = sum(bf_weights * IFFB_pct, na.rm = TRUE),
      HR_per_FB = sum(bf_weights * HR_per_FB, na.rm = TRUE),
      Pull_pct = sum(bf_weights * Pull_pct, na.rm = TRUE),
      Cent_pct = sum(bf_weights * Cent_pct, na.rm = TRUE),
      Oppo_pct = sum(bf_weights * Oppo_pct, na.rm = TRUE),
      SwStr_pct = sum(bf_weights * SwStr_pct, na.rm = TRUE),
      height_in = ifelse(is.na(height_in), NA, mean(height_in)),
      wt = ifelse(is.na(wt), NA, mean(wt)),
      bf_weights = sum(bf_weights, na.rm = TRUE)
    ) %>% distinct() %>% relocate('bf_weights', .after = 'Level')
  
  if(level == 'A+'){
    df_name <- 'minor_league_pitching_Aplus'
  }else{
    df_name <- glue::glue('minor_league_pitching_{level}')
  }
  
  assign(df_name,df)
}

### Reading SP in MLB Data
sp_pitching_standard_df_mlb <- read_csv('Data/sp_pitchers_thesis_data_standard_mlb.csv')
sp_pitching_advanced_df_mlb <- read_csv('Data/sp_pitchers_thesis_data_advanced_mlb.csv')
sp_pitching_battedball_df_mlb <- read_csv('Data/sp_pitchers_thesis_data_batted_ball_mlb.csv')


mlb_sp_pitching <- sp_pitching_standard_df_mlb %>% 
  left_join(sp_pitching_advanced_df_mlb, by = c('Season', 'PlayerId', 'Team', 'Name',
                                            'Age')) %>% 
  left_join(sp_pitching_battedball_df_mlb, by = c('Season', 'PlayerId', 'Team', 'Name',
                                              'Age')) 

glimpse(mlb_sp_pitching)

# removing duplicate columns & fixing messy column names
mlb_sp_pitching <- mlb_sp_pitching %>% 
  select(-ERA.y, -NameASCII.x, -NameASCII.y, -MLBAMID.x, -MLBAMID.y,-BABIP.y) %>% 
  rename('BABIP' = 'BABIP.x', 'ERA' = 'ERA.x',
         'BB_pct' = 'BB%','K_pct' = 'K%','K_BB'= 'K/BB','GB_per_FB' = 'GB/FB','LD_pct' = 'LD%',
         'GB_pct' = 'GB%', 'FB_pct' = 'FB%','IFFB_pct' = 'IFFB%',
         'HR_per_FB' = 'HR/FB', 'RS_9' = 'RS/9', 'ERA_minus_FIP' = 'E-F',
         'xFIP_minus' = 'xFIP-', 'FIP_minus' = 'FIP-', 'ERA_minus' = 'ERA-',
         'LOB_pct' = 'LOB%','K_minus_BB_pct' = 'K-BB%', 'HR_9' = 'HR/9',
         'BB_9' = 'BB/9', 'K_9' = 'K/9','Pull_pct' = 'Pull%','Cent_pct' = 'Cent%',
         'Oppo_pct' = 'Oppo%', 'Soft_pct' = 'Soft%', 'Med_pct' = 'Med%','Hard_pct' = 'Hard%',
         'SwStr_pct' = 'SwStr%')

#making PlayerId a character vector
mlb_sp_pitching <- mlb_sp_pitching %>% 
  mutate(PlayerId = as.character(PlayerId))

# adding bf weights
mlb_sp_pitching <- mlb_sp_pitching %>% 
  group_by(PlayerId) %>% 
  mutate(bf_weights = TBF/sum(TBF)) %>% 
  ungroup()

# accounting for players whose rookie seasons occur over 2 seasons
mlb_sp_pitching <- mlb_sp_pitching %>% 
  group_by(PlayerId, Name) %>% 
  reframe(
  Age = mean(Age, na.rm = TRUE),
  G = sum(G, na.rm = TRUE),
  GS = sum(GS, na.rm = TRUE),
  CG = sum(CG, na.rm = TRUE),
  ShO = sum(ShO, na.rm = TRUE),
  W = sum(W, na.rm = TRUE),
  L = sum(L, na.rm = TRUE),
  SV = sum(SV, na.rm = TRUE),
  BS = sum(BS, na.rm = TRUE),
  ERA = sum(bf_weights*ERA, na.rm = TRUE),
  TBF = sum(TBF, na.rm = TRUE),
  H = sum(H, na.rm = TRUE),
  R = sum(R, na.rm = TRUE),
  ER = sum(ER, na.rm = TRUE),
  HR = sum(HR, na.rm = TRUE),
  BB = sum(BB, IBB, HBP, na.rm = TRUE), # combining BB, IBB, and HBP for simplicity/less models for similar result
  SO = sum(SO, na.rm = TRUE),
  WP = sum(WP, na.rm = TRUE),
  BK = sum(BK, na.rm = TRUE),
  K_9 = sum(bf_weights*K_9, na.rm = TRUE),
  BB_9 = sum(bf_weights*BB_9, na.rm = TRUE),
  K_BB = sum(bf_weights*K_BB, na.rm = TRUE),
  HR_9 = sum(bf_weights * HR_9, na.rm = TRUE),
  K_pct = sum(bf_weights * K_pct, na.rm = TRUE),
  BB_pct = sum(bf_weights * BB_pct, na.rm = TRUE),
  K_minus_BB_pct = sum(bf_weights * K_minus_BB_pct, na.rm = TRUE),
  AVG = sum(bf_weights * AVG, na.rm = TRUE),
  WHIP = sum(bf_weights * WHIP, na.rm = TRUE),
  BABIP = sum(bf_weights * BABIP, na.rm = TRUE),
  LOB_pct = sum(bf_weights * LOB_pct, na.rm = TRUE),
  FIP = sum(bf_weights * FIP, na.rm = TRUE),
  ERA_minus_FIP = sum(bf_weights * ERA_minus_FIP, na.rm = TRUE),
  xFIP = sum(bf_weights * xFIP, na.rm = TRUE),
  IP = sum(IP, na.rm = TRUE),
  GB_per_FB = sum(bf_weights*GB_per_FB, na.rm = TRUE),
  LD_pct = sum(bf_weights * LD_pct, na.rm = TRUE),
  GB_pct = sum(bf_weights* GB_pct, na.rm = TRUE),
  FB_pct = sum(bf_weights * FB_pct, na.rm = TRUE),
  IFFB_pct = sum(bf_weights * IFFB_pct, na.rm = TRUE),
  IFFB_pct = sum(bf_weights * IFFB_pct, na.rm = TRUE),
  HR_per_FB = sum(bf_weights * HR_per_FB, na.rm = TRUE),
  Pull_pct = sum(bf_weights * Pull_pct, na.rm = TRUE),
  Cent_pct = sum(bf_weights * Cent_pct, na.rm = TRUE),
  Oppo_pct = sum(bf_weights * Oppo_pct, na.rm = TRUE),
  SwStr_pct = sum(bf_weights * SwStr_pct),
  bf_weights = sum(bf_weights, na.rm = TRUE)
) %>% relocate('bf_weights', .after = 'Name')



# joining previous level to minor league stats
mlb_sp_pitching <- mlb_sp_pitching %>% 
  left_join(minor_league_pitching_AAA, by = 'PlayerId', suffix = c('_MLB', '_AAA'))


minor_league_pitching_AAA <- minor_league_pitching_AAA %>% 
  left_join(minor_league_pitching_AA, by = c('PlayerId'), suffix = c('_AAA','_AA'))

minor_league_pitching_AA <- minor_league_pitching_AA %>% 
  left_join(minor_league_pitching_Aplus, by = c('PlayerId'), suffix = c('_AA','_Aplus'))

minor_league_pitching_Aplus <- minor_league_pitching_Aplus %>% 
  left_join(minor_league_pitching_A, by = c('PlayerId'), suffix = c('_Aplus','_A'))

#finding whether the hitter made the mlb or not
minor_league_pitching_AAA <- minor_league_pitching_AAA %>% 
  mutate(made_mlb_sp = ifelse(PlayerId %in% mlb_sp_pitching$PlayerId, 1,0))

minor_league_pitching_AAA <- minor_league_pitching_AAA %>% 
  left_join(mlb_sp_pitching %>% select(PlayerId, Age_MLB) %>% rename('Age_MLB_sp' = 'Age_MLB'), by = 'PlayerId')

minor_league_pitching_AAA <- minor_league_pitching_AAA %>% 
  mutate(years_to_mlb = ifelse(made_mlb_sp == 1, Age_MLB_sp - Age_AAA, 25))


minor_league_pitching_AA <- minor_league_pitching_AA %>% 
  mutate(made_mlb_sp = ifelse(PlayerId %in% mlb_sp_pitching$PlayerId, 1,0))

minor_league_pitching_AA <- minor_league_pitching_AA %>% 
  left_join(mlb_sp_pitching %>% select(PlayerId, Age_MLB) %>% rename('Age_MLB_sp' = 'Age_MLB'), by = 'PlayerId')

minor_league_pitching_AA <- minor_league_pitching_AA %>% 
  mutate(years_to_mlb = ifelse(made_mlb_sp == 1, Age_MLB_sp - Age_AA, 25))


minor_league_pitching_Aplus <- minor_league_pitching_Aplus %>% 
  mutate(made_mlb_sp = ifelse(PlayerId %in% mlb_sp_pitching$PlayerId, 1,0))

minor_league_pitching_Aplus <- minor_league_pitching_Aplus %>% 
  left_join(mlb_sp_pitching %>% select(PlayerId, Age_MLB) %>% rename('Age_MLB_sp' = 'Age_MLB'), by = 'PlayerId')

minor_league_pitching_Aplus <- minor_league_pitching_Aplus %>% 
  mutate(years_to_mlb = ifelse(made_mlb_sp == 1, Age_MLB_sp - Age_Aplus, 25))

minor_league_pitching_A <- minor_league_pitching_A %>% 
  mutate(made_mlb_sp = ifelse(PlayerId %in% mlb_sp_pitching$PlayerId, 1,0))

minor_league_pitching_A <- minor_league_pitching_A %>% 
  left_join(mlb_sp_pitching %>% select(PlayerId, Age_MLB) %>% rename('Age_MLB_sp' = 'Age_MLB'), by = 'PlayerId')

minor_league_pitching_A <- minor_league_pitching_A %>% 
  mutate(years_to_mlb = ifelse(made_mlb_sp == 1, Age_MLB_sp - Age, 25))


### Reading RP data into R ###
rp_pitching_standard_df_mlb <- read_csv('Data/rp_pitchers_thesis_data_standard_mlb.csv')
rp_pitching_advanced_df_mlb <- read_csv('Data/rp_pitchers_thesis_data_advanced_mlb.csv')
rp_pitching_battedball_df_mlb <- read_csv('Data/rp_pitchers_thesis_data_batted_ball_mlb.csv')

mlb_rp_pitching <- rp_pitching_standard_df_mlb %>% 
  left_join(rp_pitching_advanced_df_mlb, by = c('Season', 'PlayerId', 'Team', 'Name',
                                                'Age')) %>% 
  left_join(rp_pitching_battedball_df_mlb, by = c('Season', 'PlayerId', 'Team', 'Name',
                                                  'Age')) 

glimpse(mlb_rp_pitching)

# removing duplicate columns & fixing messy column names
mlb_rp_pitching <- mlb_rp_pitching %>% 
  select(-ERA.y, -NameASCII.x, -NameASCII.y, -MLBAMID.x, -MLBAMID.y,-BABIP.y) %>% 
  rename('BABIP' = 'BABIP.x', 'ERA' = 'ERA.x',
         'BB_pct' = 'BB%','K_pct' = 'K%','K_BB'= 'K/BB','GB_per_FB' = 'GB/FB','LD_pct' = 'LD%',
         'GB_pct' = 'GB%', 'FB_pct' = 'FB%','IFFB_pct' = 'IFFB%',
         'HR_per_FB' = 'HR/FB', 'RS_9' = 'RS/9', 'ERA_minus_FIP' = 'E-F',
         'xFIP_minus' = 'xFIP-', 'FIP_minus' = 'FIP-', 'ERA_minus' = 'ERA-',
         'LOB_pct' = 'LOB%','K_minus_BB_pct' = 'K-BB%', 'HR_9' = 'HR/9',
         'BB_9' = 'BB/9', 'K_9' = 'K/9','Pull_pct' = 'Pull%','Cent_pct' = 'Cent%',
         'Oppo_pct' = 'Oppo%', 'Soft_pct' = 'Soft%', 'Med_pct' = 'Med%','Hard_pct' = 'Hard%',
         'SwStr_pct' = 'SwStr%')

#making PlayerId a character vector
mlb_rp_pitching <- mlb_rp_pitching %>% 
  mutate(PlayerId = as.character(PlayerId))

# adding bf weights
mlb_rp_pitching <- mlb_rp_pitching %>% 
  group_by(PlayerId) %>% 
  mutate(bf_weights = TBF/sum(TBF)) %>% 
  ungroup()

# accounting for players whose rookie seasons occur over 2 seasons
mlb_rp_pitching <- mlb_rp_pitching %>% 
  group_by(PlayerId, Name) %>% 
  reframe(
    Age = mean(Age, na.rm = TRUE),
    G = sum(G, na.rm = TRUE),
    GS = sum(GS, na.rm = TRUE),
    CG = sum(CG, na.rm = TRUE),
    ShO = sum(ShO, na.rm = TRUE),
    W = sum(W, na.rm = TRUE),
    L = sum(L, na.rm = TRUE),
    SV = sum(SV, na.rm = TRUE),
    BS = sum(BS, na.rm = TRUE),
    ERA = sum(bf_weights*ERA, na.rm = TRUE),
    TBF = sum(TBF, na.rm = TRUE),
    H = sum(H, na.rm = TRUE),
    R = sum(R, na.rm = TRUE),
    ER = sum(ER, na.rm = TRUE),
    HR = sum(HR, na.rm = TRUE),
    BB = sum(BB, IBB, HBP, na.rm = TRUE), # combining BB, IBB, and HBP for simplicity/less models for similar result
    SO = sum(SO, na.rm = TRUE),
    WP = sum(WP, na.rm = TRUE),
    BK = sum(BK, na.rm = TRUE),
    K_9 = sum(bf_weights*K_9, na.rm = TRUE),
    BB_9 = sum(bf_weights*BB_9, na.rm = TRUE),
    K_BB = sum(bf_weights*K_BB, na.rm = TRUE),
    HR_9 = sum(bf_weights * HR_9, na.rm = TRUE),
    K_pct = sum(bf_weights * K_pct, na.rm = TRUE),
    BB_pct = sum(bf_weights * BB_pct, na.rm = TRUE),
    K_minus_BB_pct = sum(bf_weights * K_minus_BB_pct, na.rm = TRUE),
    AVG = sum(bf_weights * AVG, na.rm = TRUE),
    WHIP = sum(bf_weights * WHIP, na.rm = TRUE),
    BABIP = sum(bf_weights * BABIP, na.rm = TRUE),
    LOB_pct = sum(bf_weights * LOB_pct, na.rm = TRUE),
    FIP = sum(bf_weights * FIP, na.rm = TRUE),
    ERA_minus_FIP = sum(bf_weights * ERA_minus_FIP, na.rm = TRUE),
    xFIP = sum(bf_weights * xFIP, na.rm = TRUE),
    IP = sum(IP, na.rm = TRUE),
    GB_per_FB = sum(bf_weights*GB_per_FB, na.rm = TRUE),
    LD_pct = sum(bf_weights * LD_pct, na.rm = TRUE),
    GB_pct = sum(bf_weights * GB_pct, na.rm = TRUE),
    FB_pct = sum(bf_weights * FB_pct, na.rm = TRUE),
    IFFB_pct = sum(bf_weights * IFFB_pct, na.rm = TRUE),
    IFFB_pct = sum(bf_weights * IFFB_pct, na.rm = TRUE),
    HR_per_FB = sum(bf_weights * HR_per_FB, na.rm = TRUE),
    Pull_pct = sum(bf_weights * Pull_pct, na.rm = TRUE),
    Cent_pct = sum(bf_weights * Cent_pct, na.rm = TRUE),
    Oppo_pct = sum(bf_weights * Oppo_pct, na.rm = TRUE),
    SwStr_pct = sum(bf_weights * SwStr_pct, na.rm = TRUE),
    bf_weights = sum(bf_weights, na.rm = TRUE)
  ) %>% relocate('bf_weights', .after = 'Name')

# joining previous level to minor league stats
mlb_rp_pitching <- mlb_rp_pitching %>% 
  left_join(minor_league_pitching_AAA, by = 'PlayerId', suffix = c('_MLB', '_AAA')) %>% 
  select(-ends_with('_AA')) # removes AA stats that were added in the previous joining process above


#finding whether the hitter made the mlb or not
minor_league_pitching_AAA <- minor_league_pitching_AAA %>% 
  mutate(made_mlb_rp = ifelse(PlayerId %in% mlb_rp_pitching$PlayerId, 1,0))

minor_league_pitching_AAA <- minor_league_pitching_AAA %>% 
  left_join(mlb_rp_pitching %>% select(PlayerId, Age) %>% rename('Age_MLB_rp' = 'Age'), by = 'PlayerId')

minor_league_pitching_AAA <- minor_league_pitching_AAA %>% 
  mutate(years_to_mlb_rp = ifelse(made_mlb_rp == 1, Age_MLB_rp - Age_AAA, 25))


minor_league_pitching_AA <- minor_league_pitching_AA %>% 
  mutate(made_mlb_rp = ifelse(PlayerId %in% mlb_rp_pitching$PlayerId, 1,0))

minor_league_pitching_AA <- minor_league_pitching_AA %>% 
  left_join(mlb_rp_pitching %>% select(PlayerId, Age) %>% rename('Age_MLB_rp' = 'Age'), by = 'PlayerId')

minor_league_pitching_AA <- minor_league_pitching_AA %>% 
  mutate(years_to_mlb_rp = ifelse(made_mlb_rp == 1, Age_MLB_rp - Age_AA, 25))


minor_league_pitching_Aplus <- minor_league_pitching_Aplus %>% 
  mutate(made_mlb_rp = ifelse(PlayerId %in% mlb_rp_pitching$PlayerId, 1,0))

minor_league_pitching_Aplus <- minor_league_pitching_Aplus %>% 
  left_join(mlb_rp_pitching %>% select(PlayerId, Age) %>% rename('Age_MLB_rp' = 'Age'), by = 'PlayerId')

minor_league_pitching_Aplus <- minor_league_pitching_Aplus %>% 
  mutate(years_to_mlb_rp = ifelse(made_mlb_rp == 1, Age_MLB_rp - Age_Aplus, 25))

minor_league_pitching_A <- minor_league_pitching_A %>% 
  mutate(made_mlb_rp = ifelse(PlayerId %in% mlb_rp_pitching$PlayerId, 1,0))

minor_league_pitching_A <- minor_league_pitching_A %>% 
  left_join(mlb_rp_pitching %>% select(PlayerId, Age) %>% rename('Age_MLB_rp' = 'Age'), by = 'PlayerId')

minor_league_pitching_A <- minor_league_pitching_A %>% 
  mutate(years_to_mlb_rp = ifelse(made_mlb_rp == 1, Age_MLB_rp - Age, 25))


# removing players who have already made the mlb
minor_league_pitching_AAA <- minor_league_pitching_AAA %>% 
  filter(years_to_mlb >= 0 & years_to_mlb_rp >= 0)

minor_league_pitching_AA <- minor_league_pitching_AA %>% 
  filter(years_to_mlb >= 0 & years_to_mlb_rp >= 0)

minor_league_pitching_Aplus <- minor_league_pitching_Aplus %>% 
  filter(years_to_mlb >= 0 & years_to_mlb_rp >= 0)

minor_league_pitching_A <- minor_league_pitching_A %>% 
  filter(years_to_mlb >= 0 & years_to_mlb_rp >= 0)

#reading in sp and rp who played in 2004-2013
mlb_sp_2004_2013 <- read_csv('Data/mlb_sp_2004_2013.csv')
mlb_rp_2004_2013 <- read_csv('Data/mlb_rp_2004_2013.csv')

minor_league_pitching_AAA <- minor_league_pitching_AAA %>% 
  filter(!(PlayerId %in% unique(mlb_sp_2004_2013$PlayerId)) & !(PlayerId %in% unique(mlb_rp_2004_2013$PlayerId)))

minor_league_pitching_AA <- minor_league_pitching_AA %>% 
  filter(!(PlayerId %in% unique(mlb_sp_2004_2013$PlayerId)) & !(PlayerId %in% unique(mlb_rp_2004_2013$PlayerId)))

minor_league_pitching_Aplus <- minor_league_pitching_Aplus %>% 
  filter(!(PlayerId %in% unique(mlb_sp_2004_2013$PlayerId)) & !(PlayerId %in% unique(mlb_rp_2004_2013$PlayerId)))

minor_league_pitching_A <- minor_league_pitching_A %>% 
  filter(!(PlayerId %in% unique(mlb_sp_2004_2013$PlayerId)) & !(PlayerId %in% unique(mlb_rp_2004_2013$PlayerId)))

minor_league_pitching_A <- minor_league_pitching_A %>% 
  distinct(PlayerId, .keep_all = TRUE)

minor_league_pitching_Aplus <- minor_league_pitching_Aplus %>% 
  distinct(PlayerId, .keep_all = TRUE)

minor_league_pitching_AA <- minor_league_pitching_AA %>% 
  distinct(PlayerId, .keep_all = TRUE)

minor_league_pitching_AAA <- minor_league_pitching_AAA %>% 
  distinct(PlayerId, .keep_all = TRUE)

mlb_rp_pitching <- mlb_rp_pitching %>% 
  distinct(PlayerId, .keep_all = TRUE)

mlb_sp_pitching <- mlb_sp_pitching %>% 
  distinct(PlayerId, .keep_all = TRUE)

## graphics
mlb_sp_pitching %>% ggplot(aes(Age_MLB)) +
  geom_density(fill = 'royalblue', alpha = 0.5, adjust = 2) +
  geom_density(data = mlb_rp_pitching, aes(Age), fill = 'tomato', alpha = 0.5, adjust = 2) +
  geom_vline(xintercept = mean(mlb_sp_pitching$Age_MLB), linetype = 'dashed', color = 'royalblue') +
  geom_vline(xintercept = mean(mlb_rp_pitching$Age), linetype = 'dashed', color = 'tomato')+
  theme_bw() +
  labs(x = 'Age When Promoted',
       title = 'Density Plot Showing the Ages that SP (Blue) &\nRP (Red) Are Called Up to the MLB')


(era_mlbsp <- mlb_sp_pitching %>% ggplot(aes(ERA_AAA, ERA_MLB)) +
  geom_point(color = 'goldenrod') +
  theme_bw() +
  scale_x_continuous(limits = c(0,12),
                     breaks = seq(0,12,by = 1.5)) +
  scale_y_continuous(limits = c(0,12),
                     breaks = seq(0,12,by = 1.5)) +
  geom_abline(slope = 1, intercept = 0) +
  labs(
    title = 'ERA as a SP in MLB vs AAA',
    y = 'ERA (MLB)',
    x = 'ERA (AAA)'
  ))

(era_mlbrp <- mlb_rp_pitching %>% ggplot(aes(ERA_AAA, ERA)) +
  geom_point(color = 'goldenrod') +
  theme_bw() +
  scale_x_continuous(limits = c(0,12),
                     breaks = seq(0,12,by = 1.5)) +
  scale_y_continuous(limits = c(0,12),
                     breaks = seq(0,12,by = 1.5)) +
  geom_abline(slope = 1, intercept = 0) +
  ylab('ERA_MLB') +
    labs(
      title = 'ERA as a RP in MLB vs AAA',
      y = 'ERA (MLB)',
      x = 'ERA (AAA)'
    ))

(era_aaa <- minor_league_pitching_AAA %>% ggplot(aes(ERA_AA, ERA_AAA)) +
  geom_point(color = 'goldenrod')+
  theme_bw() +
  scale_x_continuous(limits = c(0,12),
                     breaks = seq(0,12,by = 1.5)) +
  scale_y_continuous(limits = c(0,12),
                     breaks = seq(0,12,by = 1.5)) +
  geom_abline(slope = 1, intercept = 0) +
  labs(title = 'ERA in AAA vs AA',
       y = 'ERA (AAA)', 
       x = 'ERA (AA)'))


(era_aa <- minor_league_pitching_AA %>% ggplot(aes(ERA_Aplus, ERA_AA)) +
  geom_point(color = 'goldenrod') +
  theme_bw() +
  scale_x_continuous(limits = c(0,12),
                     breaks = seq(0,12,by = 1.5)) +
  scale_y_continuous(limits = c(0,12),
                     breaks = seq(0,12,by = 1.5)) +
  geom_abline(slope = 1, intercept = 0)+
  labs(title = 'ERA in AA vs A+',
       y = 'ERA (AA)', 
       x = 'ERA (AA+)'))


(era_aplus <- minor_league_pitching_Aplus %>% ggplot(aes(ERA_A, ERA_Aplus)) +
  geom_point(color = 'goldenrod') +
  theme_bw() +
  scale_x_continuous(limits = c(0,12),
                     breaks = seq(0,12,by = 1.5)) +
  scale_y_continuous(limits = c(0,12),
                     breaks = seq(0,12,by = 1.5)) +
  geom_abline(slope = 1, intercept = 0)+
    labs(title = 'ERA in A+ vs A',
         y = 'ERA (A+)', 
         x = 'ERA (A)'))

grid.arrange(era_mlbsp, era_mlbrp, era_aaa, era_aa, era_aplus, nrow = 2)

## K%
mlb_sp_pitching %>% ggplot(aes(K_pct_AAA, K_pct_MLB)) +
  geom_point(color = 'forestgreen') +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5,by = 0.1)) +
  scale_y_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5,by = 0.1)) +
  geom_abline(slope = 1, intercept = 0) +
  labs(title = 'K% as a SP in MLB vs AAA',
       y = 'K% (MLB)',
       x = 'K% (AAA)')

mlb_rp_pitching %>% ggplot(aes(K_pct_AAA, K_pct)) +
  geom_point(color = 'forestgreen') +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5,by = 0.1)) +
  scale_y_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5,by = 0.1)) +
  geom_abline(slope = 1, intercept = 0) +
  labs(title = 'K% as a RP in MLB vs AAA',
       y = 'K% (MLB)',
       x = 'K% (AAA)')


minor_league_pitching_AAA %>% ggplot(aes(K_pct_AA, K_pct_AAA)) +
  geom_point(color = 'forestgreen') +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5,by = 0.1)) +
  scale_y_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5,by = 0.1)) +
  geom_abline(slope = 1, intercept = 0)

minor_league_pitching_AA %>% ggplot(aes(K_pct_Aplus, K_pct_AA)) +
  geom_point(color = 'forestgreen') +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5,by = 0.1)) +
  scale_y_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5,by = 0.1)) +
  geom_abline(slope = 1, intercept = 0)

minor_league_pitching_Aplus %>% ggplot(aes(K_pct_A, K_pct_Aplus)) +
  geom_point(color = 'forestgreen') +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5,by = 0.1)) +
  scale_y_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5,by = 0.1)) +
  geom_abline(slope = 1, intercept = 0)

## BB%
mlb_sp_pitching %>% ggplot(aes(BB_pct_AAA, BB_pct_MLB)) +
  geom_point(color = 'coral') +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3,by = 0.1)) +
  scale_y_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3,by = 0.1)) +
  geom_abline(slope = 1, intercept = 0) +
  ggtitle('SP')

mlb_rp_pitching %>% ggplot(aes(BB_pct_AAA, BB_pct)) +
  geom_point(color = 'coral') +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3,by = 0.1)) +
  scale_y_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3,by = 0.1)) +
  geom_abline(slope = 1, intercept = 0) +
  ylab('BB% MLB') +
  ggtitle('RP')

minor_league_pitching_AAA %>% ggplot(aes(BB_pct_AA, BB_pct_AAA)) +
  geom_point(color = 'coral') +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3,by = 0.1)) +
  scale_y_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3,by = 0.1)) +
  geom_abline(slope = 1, intercept = 0) 

minor_league_pitching_AA %>% ggplot(aes(BB_pct_Aplus, BB_pct_AA)) +
  geom_point(color = 'coral') +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3,by = 0.1)) +
  scale_y_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3,by = 0.1)) +
  geom_abline(slope = 1, intercept = 0) 

minor_league_pitching_Aplus %>% ggplot(aes(BB_pct_A, BB_pct_Aplus)) +
  geom_point(color = 'coral') +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3,by = 0.1)) +
  scale_y_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3,by = 0.1)) +
  geom_abline(slope = 1, intercept = 0) 


### FIP
(fip_mlbsp <- mlb_sp_pitching %>% ggplot(aes(FIP_AAA, FIP_MLB)) +
  geom_point(color = 'firebrick2') +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(1.5,7.5),
                     breaks = seq(1.5,7.5, by = 1.5)) +
  scale_y_continuous(limits = c(1.5,7.5),
                     breaks = seq(1.5,7.5, by = 1.5))+
  labs(title = 'FIP as a SP in MLB vs AAA',
       y = 'FIP (MLB)',
       x = 'FIP (AAA)'))
  
(fip_mlbrp <- mlb_rp_pitching %>% ggplot(aes(FIP_AAA, FIP)) +
    geom_point(color = 'firebrick2') +
    theme_bw() +
    geom_abline(slope = 1, intercept = 0) +
    scale_x_continuous(limits = c(1.5,7.5),
                       breaks = seq(1.5,7.5, by = 1.5)) +
    scale_y_continuous(limits = c(1.5,7.5),
                       breaks = seq(1.5,7.5, by = 1.5)) +
  labs(title = 'FIP as a RP in MLB vs AAA',
       y = 'FIP (MLB)',
       x = 'FIP (AAA)'))

(fip_aaa <- minor_league_pitching_AAA %>% ggplot(aes(FIP_AA, FIP_AAA)) +
  geom_point(color = 'firebrick2') +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(1.5,7.5),
                     breaks = seq(1.5,7.5, by = 1.5)) +
  scale_y_continuous(limits = c(1.5,7.5),
                     breaks = seq(1.5,7.5, by = 1.5)) +
  labs(title = 'FIP in AAA vs AA',
       y = 'FIP (AAA)', x = 'FIP (AA)'))

(fip_aa <- minor_league_pitching_AA %>% ggplot(aes(FIP_Aplus, FIP_AA)) +
  geom_point(color = 'firebrick2') +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(1.5,7.5),
                     breaks = seq(1.5,7.5, by = 1.5)) +
  scale_y_continuous(limits = c(1.5,7.5),
                     breaks = seq(1.5,7.5, by = 1.5))+
    labs(title = 'FIP in AA vs A+',
         y = 'FIP (AA)', 
         x = 'FIP (A+)'))

(fip_aplus <- minor_league_pitching_Aplus %>% ggplot(aes(FIP_A, FIP_Aplus)) +
  geom_point(color = 'firebrick2') +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(1.5,7.5),
                     breaks = seq(1.5,7.5, by = 1.5)) +
  scale_y_continuous(limits = c(1.5,7.5),
                     breaks = seq(1.5,7.5, by = 1.5))+
    labs(title = 'FIP in A+ vs A',
         y = 'FIP (A+)', x = 'FIP (A)'))

grid.arrange(fip_mlbsp, fip_mlbrp, fip_aaa, fip_aa, fip_aplus, nrow = 2)

#whiff%
mlb_sp_pitching %>% ggplot(aes(SwStr_pct_AAA, SwStr_pct_MLB)) +
  geom_point(color = 'burlywood') +
  theme_bw() + 
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(0,0.25),
                     breaks = seq(0,0.25, by = 0.05)) +
  scale_y_continuous(limits = c(0,0.25),
                     breaks = seq(0,0.25, by = 0.05)) +
  ggtitle('SP')

mlb_rp_pitching %>% ggplot(aes(SwStr_pct_AAA, SwStr_pct)) +
  geom_point(color = 'burlywood') +
  theme_bw() + 
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(0,0.25),
                     breaks = seq(0,0.25, by = 0.05)) +
  scale_y_continuous(limits = c(0,0.25),
                     breaks = seq(0,0.25, by = 0.05)) +
  ggtitle('RP') +
  ylab('SwStr% MLB')

minor_league_hitting_AAA %>% ggplot(aes(SwStr_pct_AA, SwStr_pct_AAA)) +
  geom_point(color = 'burlywood') +
  theme_bw() + 
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(0,0.25),
                     breaks = seq(0,0.25, by = 0.05)) +
  scale_y_continuous(limits = c(0,0.25),
                     breaks = seq(0,0.25, by = 0.05))


minor_league_hitting_AA %>% ggplot(aes(SwStr_pct_Aplus, SwStr_pct_AA)) +
  geom_point(color = 'burlywood') +
  theme_bw() + 
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(0,0.25),
                     breaks = seq(0,0.25, by = 0.05)) +
  scale_y_continuous(limits = c(0,0.25),
                     breaks = seq(0,0.25, by = 0.05))

minor_league_hitting_AA %>% ggplot(aes(SwStr_pct_Aplus, SwStr_pct_AA, color = Age_Aplus)) +
  geom_point() +
  scale_color_continuous(low= 'lightblue2', high = 'tomato2') +
  theme_bw() + 
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(0,0.25),
                     breaks = seq(0,0.25, by = 0.05)) +
  scale_y_continuous(limits = c(0,0.25),
                     breaks = seq(0,0.25, by = 0.05))

minor_league_hitting_Aplus %>% ggplot(aes(SwStr_pct_A, SwStr_pct_Aplus)) +
  geom_point(color = 'burlywood') +
  theme_bw() + 
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(0,0.25),
                     breaks = seq(0,0.25, by = 0.05)) +
  scale_y_continuous(limits = c(0,0.25),
                     breaks = seq(0,0.25, by = 0.05))

# finding correlation between age and making mlb
minor_league_pitching_AAA %>% ggplot(aes(Age_AAA, made_mlb_sp)) +
  geom_jitter(position = position_jitter(height = 0.01, width = 0.01, seed = 101)) +
  geom_smooth(method = 'loess') +
  ylim(c(0,1))+
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw() +
  labs(y = 'Made MLB (SP)',
       x = 'Age (AAA)',
       title = 'Ages at Which SP Make The MLB at AAA')

minor_league_pitching_AAA %>% ggplot(aes(Age_AAA, made_mlb_rp)) +
  geom_jitter(position = position_jitter(height = 0.01, width = 0.01, seed = 101)) +
  geom_smooth(method = 'loess') +
  ylim(c(0,1))+
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()



minor_league_pitching_AA %>% ggplot(aes(Age_AA, made_mlb_sp)) +
  geom_jitter(position = position_jitter(height = 0.01, width = 0.01, seed = 101)) +
  geom_smooth() +
  ylim(c(0,1))+
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()

minor_league_pitching_AA %>% ggplot(aes(Age_AA, made_mlb_rp)) +
  geom_jitter(position = position_jitter(height = 0.01, width = 0.01, seed = 101)) +
  geom_smooth(method = 'loess') +
  ylim(c(0,1))+
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()

minor_league_pitching_Aplus %>% ggplot(aes(Age_Aplus, made_mlb_sp)) +
  geom_jitter(position = position_jitter(height = 0.01, width = 0.01, seed = 101)) +
  geom_smooth(method = 'loess') +
  ylim(c(0,1)) +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()

minor_league_pitching_Aplus %>% ggplot(aes(Age_Aplus, made_mlb_rp)) +
  geom_jitter(position = position_jitter(height = 0.01, width = 0.01, seed = 101)) +
  geom_smooth(method = 'loess') +
  ylim(c(0,1)) +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()

minor_league_pitching_A %>% ggplot(aes(Age, made_mlb_sp)) +
  geom_jitter(position = position_jitter(height = 0.01, width = 0.01, seed = 101)) +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  geom_smooth(method = 'loess') +
  ylim(c(0,1)) +
  xlab('Age_A') +
  theme_bw()

minor_league_pitching_A %>% ggplot(aes(Age, made_mlb_rp)) +
  geom_jitter(position = position_jitter(height = 0.01, width = 0.01, seed = 101)) +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  geom_smooth(method = 'loess') +
  ylim(c(0,1)) +
  xlab('Age_A') +
  theme_bw()


minor_league_pitching_AAA %>%
  filter(years_to_mlb != 25) %>% 
  ggplot(aes(Age_AAA, years_to_mlb)) +
  geom_jitter(position = position_jitter(height = 0.05, width = 0.05, seed = 101)) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()

minor_league_pitching_AAA %>% 
  filter(years_to_mlb_rp != 25) %>% 
  ggplot(aes(Age_AAA, years_to_mlb_rp)) +
  geom_jitter(position = position_jitter(height = 0.05, width = 0.05, seed = 101)) +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()


minor_league_pitching_AA %>%
  filter(years_to_mlb != 25) %>% 
  ggplot(aes(Age_AA, years_to_mlb)) +
  geom_jitter(position = position_jitter(height = 0.05, width = 0.05, seed = 101)) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()

minor_league_pitching_AA %>% 
  filter(years_to_mlb_rp != 25) %>% 
  ggplot(aes(Age_AA, years_to_mlb_rp)) +
  geom_jitter(position = position_jitter(height = 0.05, width = 0.05, seed = 101)) +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()

minor_league_pitching_Aplus %>%
  filter(years_to_mlb != 25) %>% 
  ggplot(aes(Age_Aplus, years_to_mlb)) +
  geom_jitter(position = position_jitter(height = 0.05, width = 0.05, seed = 101)) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()

minor_league_pitching_Aplus %>% 
  filter(years_to_mlb_rp != 25) %>% 
  ggplot(aes(Age_Aplus, years_to_mlb_rp)) +
  geom_jitter(position = position_jitter(height = 0.05, width = 0.05, seed = 101)) +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()

minor_league_pitching_A %>%
  filter(years_to_mlb != 25) %>% 
  ggplot(aes(Age, years_to_mlb)) +
  geom_jitter(position = position_jitter(height = 0.05, width = 0.05, seed = 101)) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  ylim(c(0,max(minor_league_pitching_A %>% filter(years_to_mlb != 25) %>% pull(years_to_mlb))+0.1)) +
  theme_bw()

minor_league_pitching_A %>% 
  filter(years_to_mlb_rp != 25) %>% 
  ggplot(aes(Age, years_to_mlb_rp)) +
  geom_jitter(position = position_jitter(height = 0.05, width = 0.05, seed = 101)) +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()

