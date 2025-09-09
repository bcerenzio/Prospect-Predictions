library(tidyverse)
library(gridExtra)


## fangraphs hitting minors
hitting_standard_df <- read_csv('Data/hitters_thesis_data_standard.csv')
hitting_advanced_df <- read_csv('Data/hitters_thesis_data_advanced.csv')
hitting_battedball_df <- read_csv('Data/hitters_thesis_data_batted_ball.csv')

## joining on PlayerId, Season, Level, & Team
minor_league_hitting <- hitting_standard_df %>% 
  filter(Level %in% c('A', 'A+' , 'AA', 'AAA')) %>% 
  left_join(hitting_advanced_df, by = c('Season', 'PlayerId', 'Level', 'Team', 'Name',
                                        'Age','AVG', 'PA')) %>% 
  left_join(hitting_battedball_df, by = c('Season', 'PlayerId', 'Level', 'Team', 'Name',
                                          'Age', 'PA','BABIP')) 
            
# adding PA weights
minor_league_hitting <- minor_league_hitting %>% 
  group_by(PlayerId,Level) %>% 
  mutate(pa_weights = PA/sum(PA)) %>% 
  relocate(pa_weights, .after = 'Level') %>% 
  arrange(Name, Age, Level) %>%
  ungroup()

# finding the last season they were at that level
minor_league_hitting <- minor_league_hitting %>% 
  group_by(PlayerId, Level) %>% 
  mutate(lag_year = Season - lag(Season)) %>% 
  mutate(lag_year = ifelse(is.na(lag_year), 0, lag_year)) %>% 
  ungroup() %>% 
  relocate(lag_year, .after = 'pa_weights')

# removing seasons that have been more than 2 years since he was last promoted
minor_league_hitting <- minor_league_hitting %>% 
  filter(lag_year <= 2)  %>% 
  group_by(PlayerId,Level) %>% 
  mutate(pa_weights = PA/sum(PA)) %>% #recalculating pa_weights
  ungroup()


# renaming columns
minor_league_hitting <- minor_league_hitting %>% 
  rename('BB_pct' = 'BB%', 'K_pct' = 'K%', 'GB_per_FB' = 'GB/FB',
         'LD_pct' = 'LD%', 'GB_pct' = 'GB%', 'FB_pct' = 'FB%',
         'IFFB_pct' = 'IFFB%', 'HR_per_FB' = 'HR/FB', 'Pull_pct' = 'Pull%',
         'Cent_pct' = 'Cent%', 'Oppo_pct' = 'Oppo%', 'SwStr_pct' = 'SwStr%',
         'wRC_plus' = 'wRC+', 'BB_K' = 'BB/K', 'x1B' = '1B', 'x2B' = '2B', 'x3B' = '3B')

# Joining Baseball America top 100 Rankings
minor_league_hitting <- minor_league_hitting %>% 
  left_join(ba_rankings, by = c('Name' = 'player', 'Season' = 'year'), relationship = 'many-to-many') %>% 
  relocate(rank_ba, .after = pa_weights)

minor_league_hitting <- minor_league_hitting %>% 
  mutate(rank_ba = ifelse(is.na(rank_ba), 110, rank_ba)) %>% 
  select(-c(pos, ba, th, born, place, mlb_years, stat_years, draft_info))

#joining MLB Pipeline Top 100 Rankings
minor_league_hitting <- minor_league_hitting %>% 
  left_join(mlb_rankings, by = c('Name' = 'player', 'Season' = 'year'), relationship = 'many-to-many') %>% 
  relocate(rank_mlb, .after = rank_ba)

# filling in NA values with 110
minor_league_hitting <- minor_league_hitting %>% 
  mutate(rank_mlb = ifelse(is.na(rank_mlb), 110, rank_mlb)) %>% 
  select(-c(pos, ba, th, born, place, mlb_years, stat_years, draft_info))

# dropping duplicate weight and height columns
minor_league_hitting <- minor_league_hitting %>% 
  select(-height_in.y, -wt.y,-hilvl.y) %>% 
  rename('height_in' = 'height_in.x', 'wt' = 'wt.x', 'hilvl' = 'hilvl.x')
  
#joining team rankings
minor_league_hitting <- minor_league_hitting %>% 
  left_join(team_rankings, by = c('Season' = 'Year', 'Name' = 'Name', 'Team' = 'TeamAbbr')) %>% 
  mutate(across(c('team_rank_ba','team_rank_mlb'), ~replace_na(., 35)))

for (level in c('A', 'A+', 'AA', 'AAA')){
  print(level)
  
  df <- minor_league_hitting %>% filter(Level == level)
  
  df <- df %>% 
    group_by(PlayerId, Name, Level) %>% 
    reframe(
      rank_ba = mean(rank_ba, na.rm = TRUE),
      rank_mlb = mean(rank_mlb, na.rm =TRUE),
      team_rank_ba = mean(team_rank_ba, na.rm = TRUE),
      team_rank_mlb = mean(team_rank_mlb, na.rm = TRUE),
      Age = mean(Age, na.rm = TRUE),
      G = sum(G, na.rm = TRUE),
      AB = sum(AB, na.rm = TRUE),
      PA = sum(PA, na.rm = TRUE),
      H = sum(H, na.rm = TRUE),
      x1B = sum(x1B, na.rm = TRUE),
      x2B = sum(x2B, na.rm = TRUE),
      x3B = sum(x3B, na.rm = TRUE),
      HR = sum(HR, na.rm = TRUE),
      R = sum(R, na.rm = TRUE),
      RBI = sum(RBI, na.rm = TRUE),
      BB = sum(BB, IBB, HBP, na.rm = TRUE), # combining BB, IBB, and HBP for simplicity/less models for similar result
      SO = sum(SO, na.rm = TRUE),
      SF = sum(SF, na.rm = TRUE),
      SH = sum(SH, na.rm = TRUE),
      GDP = sum(GDP, na.rm = TRUE),
      SB = sum(SB, na.rm = TRUE),
      CS = sum(CS, na.rm = TRUE),
      AVG = sum(pa_weights * AVG, na.rm = TRUE),
      BB_pct = sum(pa_weights * BB_pct, na.rm = TRUE),
      K_pct = sum(pa_weights*K_pct, na.rm = TRUE),
      BB_K = sum(pa_weights*BB_K, na.rm = TRUE),
      OBP = sum(pa_weights * OBP, na.rm = TRUE),
      SLG = sum(pa_weights * SLG, na.rm = TRUE),
      OPS = sum(pa_weights * OPS, na.rm = TRUE),
      ISO = sum(pa_weights * ISO, na.rm = TRUE),
      Spd = sum(pa_weights * Spd, na.rm = TRUE), # maybe could be useful for doubles, triples, and SB
      BABIP = sum(pa_weights * BABIP, na.rm = TRUE),
      wSB = sum(pa_weights * wSB, na.rm = TRUE),
      wRC = sum(wRC, na.rm = TRUE),
      wRAA = sum(wRAA, na.rm = TRUE),
      wOBA = sum(pa_weights * wOBA, na.rm = TRUE),
      wRC_plus = sum(pa_weights*wRC_plus, na.rm = TRUE),
      GB_per_FB = sum(pa_weights * GB_per_FB, na.rm = TRUE),
      GB_pct = sum(pa_weights * GB_pct, na.rm = TRUE),
      LD_pct = sum(pa_weights * LD_pct, na.rm = TRUE),
      FB_pct = sum(pa_weights * FB_pct, na.rm = TRUE),
      IFFB_pct = sum(pa_weights * IFFB_pct, na.rm = TRUE),
      HR_per_FB = sum(pa_weights * HR_per_FB, na.rm = TRUE),
      Pull_pct = sum(pa_weights * Pull_pct, na.rm = TRUE),
      Cent_pct = sum(pa_weights * Cent_pct, na.rm = TRUE),
      Oppo_pct = sum(pa_weights * Oppo_pct, na.rm = TRUE),
      SwStr_pct = sum(pa_weights * SwStr_pct, na.rm = TRUE),
      height_in = ifelse(is.na(height_in), NA, mean(height_in)),
      wt = ifelse(is.na(wt), NA, mean(wt)),
      pa_weights = sum(pa_weights, na.rm = TRUE)
    ) %>% distinct() %>% relocate(pa_weights, .after = 'Name')
  
  if(level == 'A+'){
    df_name <- 'minor_league_hitting_Aplus'
  }else{
    df_name <- glue::glue('minor_league_hitting_{level}')
  }
  
  assign(df_name,df)
}

### Reading in MLB Data
hitting_standard_df_mlb <- read_csv('Data/hitters_thesis_data_standard_mlb.csv')
hitting_advanced_df_mlb <- read_csv('Data/hitters_thesis_data_advanced_mlb.csv')
hitting_battedball_df_mlb <- read_csv('Data/hitters_thesis_data_batted_ball_mlb.csv')


mlb_hitting <- hitting_standard_df_mlb %>% 
  left_join(hitting_advanced_df_mlb, by = c('Season', 'PlayerId', 'Team', 'Name',
                                        'Age')) %>% 
  left_join(hitting_battedball_df_mlb, by = c('Season', 'PlayerId', 'Team', 'Name',
                                          'Age')) 

glimpse(mlb_hitting)

# removing duplicate columns & fixing messy column names
mlb_hitting <- mlb_hitting %>% 
  select(-PA.y, -AVG.y, -NameASCII.x, -NameASCII.y, -MLBAMID.x, -MLBAMID.y,
         -BABIP.y) %>% 
  rename('PA' = 'PA.x', 'AVG' = 'AVG.x', 'BABIP' = 'BABIP.x',
         'x1B' = '1B', 'x2B' = '2B', 'x3B' = '3B',
         'BB_pct' = 'BB%','K_pct' = 'K%','BB_K'= 'BB/K',
         'wRC_plus' = 'wRC+','GB_per_FB' = 'GB/FB','LD_pct' = 'LD%',
         'GB_pct' = 'GB%', 'FB_pct' = 'FB%','IFFB_pct' = 'IFFB%',
         'HR_per_FB' = 'HR/FB','IFH_pct' = 'IFH%', 'BUH_pct' = 'BUH%',
         'Pull_pct' = 'Pull%','Cent_pct' = 'Cent%','Oppo_pct' = 'Oppo%',
         'Soft_pct' = 'Soft%', 'Med_pct' = 'Med%','Hard_pct' = 'Hard%',
         'SwStr_pct' = 'SwStr%')

#making PlayerId a character vector
mlb_hitting <- mlb_hitting %>% 
  mutate(PlayerId = as.character(PlayerId))

# adding PA weights
mlb_hitting <- mlb_hitting %>% 
  group_by(PlayerId) %>% 
  mutate(pa_weights = PA/sum(PA)) %>% 
  ungroup()

# accounting for players whose rookie seasons occur over 2 seasons
mlb_hitting <- mlb_hitting %>% 
  group_by(PlayerId, Name) %>% 
  reframe(
    Age = mean(Age, na.rm = TRUE),
    G = sum(G, na.rm = TRUE),
    AB = sum(AB, na.rm = TRUE),
    PA = sum(PA, na.rm = TRUE),
    H = sum(H, na.rm = TRUE),
    x1B = sum(x1B, na.rm = TRUE),
    x2B = sum(x2B, na.rm = TRUE),
    x3B = sum(x3B, na.rm = TRUE),
    HR = sum(HR, na.rm = TRUE),
    R = sum(R, na.rm = TRUE),
    RBI = sum(RBI, na.rm = TRUE),
    BB = sum(BB, IBB, HBP, na.rm = TRUE), # combining BB, IBB, and HBP for simplicity/less models for similar result
    SO = sum(SO, na.rm = TRUE),
    SF = sum(SF, na.rm = TRUE),
    SH = sum(SH, na.rm = TRUE),
    GDP = sum(GDP, na.rm = TRUE),
    SB = sum(SB, na.rm = TRUE),
    CS = sum(CS, na.rm = TRUE),
    AVG = sum(pa_weights * AVG, na.rm = TRUE),
    BB_pct = sum(pa_weights * BB_pct, na.rm = TRUE),
    K_pct = sum(pa_weights*K_pct, na.rm = TRUE),
    BB_K = sum(pa_weights*BB_K, na.rm = TRUE),
    OBP = sum(pa_weights * OBP, na.rm = TRUE),
    SLG = sum(pa_weights * SLG, na.rm = TRUE),
    OPS = sum(pa_weights * OPS, na.rm = TRUE),
    ISO = sum(pa_weights * ISO, na.rm = TRUE),
    Spd = sum(pa_weights * Spd, na.rm = TRUE), # maybe could be useful for doubles, triples, and SB
    BABIP = sum(pa_weights * BABIP, na.rm = TRUE),
    wSB = sum(pa_weights * wSB, na.rm = TRUE),
    wRC = sum(wRC, na.rm = TRUE),
    wRAA = sum(wRAA, na.rm = TRUE),
    wOBA = sum(pa_weights * wOBA, na.rm = TRUE),
    wRC_plus = sum(pa_weights*wRC_plus, na.rm = TRUE),
    GB_per_FB = sum(pa_weights * GB_per_FB, na.rm = TRUE),
    GB_pct = sum(pa_weights * GB_pct, na.rm = TRUE),
    LD_pct = sum(pa_weights * LD_pct, na.rm = TRUE),
    FB_pct = sum(pa_weights * FB_pct, na.rm = TRUE),
    IFFB_pct = sum(pa_weights * IFFB_pct, na.rm = TRUE),
    HR_per_FB = sum(pa_weights * HR_per_FB, na.rm = TRUE),
    Pull_pct = sum(pa_weights * Pull_pct, na.rm = TRUE),
    Cent_pct = sum(pa_weights * Cent_pct, na.rm = TRUE),
    Oppo_pct = sum(pa_weights * Oppo_pct, na.rm = TRUE),
    Soft_pct = sum(pa_weights * Soft_pct, na.rm = TRUE),
    Med_pct = sum(pa_weights*Med_pct, na.rm = TRUE),
    Hard_pct = sum(pa_weights * Hard_pct, na.rm = TRUE),
    SwStr_pct = sum(pa_weights * SwStr_pct, na.rm = TRUE),
    MLBAMID,
    pa_weights = sum(pa_weights, na.rm = TRUE)
  ) %>% distinct()



# joining previous level to minor league stats
mlb_hitting <- mlb_hitting %>% 
  left_join(minor_league_hitting_AAA, by = 'PlayerId', suffix = c('_MLB', '_AAA'))


minor_league_hitting_AAA <- minor_league_hitting_AAA %>% 
  left_join(minor_league_hitting_AA, by = c('PlayerId'), suffix = c('_AAA','_AA'))

minor_league_hitting_AA <- minor_league_hitting_AA %>% 
  left_join(minor_league_hitting_Aplus, by = c('PlayerId'), suffix = c('_AA','_Aplus'))

minor_league_hitting_Aplus <- minor_league_hitting_Aplus %>% 
  left_join(minor_league_hitting_A, by = c('PlayerId'), suffix = c('_Aplus','_A'))

#finding whether the hitter made the mlb or not
minor_league_hitting_AAA <- minor_league_hitting_AAA %>% 
    mutate(made_mlb = ifelse(PlayerId %in% mlb_hitting$PlayerId, 1,0))

minor_league_hitting_AAA <- minor_league_hitting_AAA %>% 
  left_join(mlb_hitting %>% select(PlayerId, Age_MLB), by = 'PlayerId')

minor_league_hitting_AAA <- minor_league_hitting_AAA %>% 
    mutate(years_to_mlb = ifelse(made_mlb == 1, Age_MLB - Age_AAA, 25))


minor_league_hitting_AA <- minor_league_hitting_AA %>% 
  mutate(made_mlb = ifelse(PlayerId %in% mlb_hitting$PlayerId, 1,0))

minor_league_hitting_AA <- minor_league_hitting_AA %>% 
  left_join(mlb_hitting %>% select(PlayerId, Age_MLB), by = 'PlayerId')

minor_league_hitting_AA <- minor_league_hitting_AA %>% 
  mutate(years_to_mlb = ifelse(made_mlb == 1, Age_MLB - Age_AA, 25))


minor_league_hitting_Aplus <- minor_league_hitting_Aplus %>% 
  mutate(made_mlb = ifelse(PlayerId %in% mlb_hitting$PlayerId, 1,0))

minor_league_hitting_Aplus <- minor_league_hitting_Aplus %>% 
  left_join(mlb_hitting %>% select(PlayerId, Age_MLB), by = 'PlayerId')

minor_league_hitting_Aplus <- minor_league_hitting_Aplus %>% 
  mutate(years_to_mlb = ifelse(made_mlb == 1, Age_MLB - Age_Aplus, 25))

minor_league_hitting_A <- minor_league_hitting_A %>% 
  mutate(made_mlb = ifelse(PlayerId %in% mlb_hitting$PlayerId, 1,0))

minor_league_hitting_A <- minor_league_hitting_A %>% 
  left_join(mlb_hitting %>% select(PlayerId, Age_MLB), by = 'PlayerId')

minor_league_hitting_A <- minor_league_hitting_A %>% 
  mutate(years_to_mlb = ifelse(made_mlb == 1, Age_MLB - Age, 25))

# removing players who have already made the MLB
minor_league_hitting_AAA <- minor_league_hitting_AAA %>% 
  filter(years_to_mlb >= 0)

minor_league_hitting_AA <- minor_league_hitting_AA %>% 
  filter(years_to_mlb >= 0)

minor_league_hitting_Aplus <- minor_league_hitting_Aplus %>% 
  filter(years_to_mlb >= 0)

minor_league_hitting_A <- minor_league_hitting_A %>% 
  filter(years_to_mlb >= 0)

# reading in players who played between 2004-2013
mlb_2004_2013 <- read_csv('Data/mlb_data_2004_2013.csv')

# removing plays from minor league datasets that share a playerid
minor_league_hitting_AAA <- minor_league_hitting_AAA %>% 
  filter(!(PlayerId %in% unique(mlb_2004_2013$PlayerId)))

minor_league_hitting_AA <- minor_league_hitting_AA %>% 
  filter(!(PlayerId %in% unique(mlb_2004_2013$PlayerId)))

minor_league_hitting_Aplus <- minor_league_hitting_Aplus %>% 
  filter(!(PlayerId %in% unique(mlb_2004_2013$PlayerId)))

minor_league_hitting_A <- minor_league_hitting_A %>% 
  filter(!(PlayerId %in% unique(mlb_2004_2013$PlayerId)))

### Graphics
mlb_hitting %>% ggplot(aes(Age_MLB))+ 
  geom_density(fill = 'royalblue', alpha = 0.7) +
  geom_vline(xintercept = mean(mlb_hitting$Age_MLB), linetype = 'dashed') +
  theme_bw() +
  labs(title = 'Density Plot Showing the Ages That Batters Are\nCalled Up to the MLB',
       x = 'Age When Promoted')

(ba_mlb <- mlb_hitting %>% ggplot(aes(AVG_AAA, AVG_MLB)) +
  geom_point(color = 'deepskyblue') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0.15,0.4),
                     breaks = seq(0.15,0.4, by = 0.05)) +
  scale_y_continuous(limits = c(0.15,0.4),
                     breaks = seq(0.15,0.4, by = 0.05)) +
  labs(title = 'Batting Average (BA) in MLB vs AAA',
       y = 'BA (MLB)',
       x = 'BA (AAA)'))


(ba_AAA <- minor_league_hitting_AAA %>% ggplot(aes(AVG_AA, AVG_AAA)) +
  geom_point(color = 'deepskyblue') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0.15,0.4),
                     breaks = seq(0.15,0.4, by = 0.05)) +
  scale_y_continuous(limits = c(0.15,0.4),
                     breaks = seq(0.15,0.4, by = 0.05)) +
  labs(title = 'Batting Average (BA) in AAA vs AA',
       y = 'BA (AAA)',
       x = 'BA (AA)'))


(ba_AA <- minor_league_hitting_AA %>% ggplot(aes(AVG_Aplus, AVG_AA)) +
  geom_point(color = 'deepskyblue') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0.15,0.4),
                     breaks = seq(0.15,0.4, by = 0.05)) +
  scale_y_continuous(limits = c(0.15,0.4),
                     breaks = seq(0.15,0.4, by = 0.05))+
  labs(title = 'Batting Average (BA) in AA vs A+',
       y = 'BA (AA)',
       x = 'BA (A+)'))

(ba_Aplus <- minor_league_hitting_Aplus %>% ggplot(aes(AVG_A, AVG_Aplus)) +
  geom_point(color = 'deepskyblue') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0.15,0.4),
                     breaks = seq(0.15,0.4, by = 0.05)) +
  scale_y_continuous(limits = c(0.15,0.4),
                     breaks = seq(0.15,0.4, by = 0.05))+
  labs(title = 'Batting Average (BA) in A+ vs A',
       y = 'BA (A+)',
       x = 'BA (A)'))

grid.arrange(ba_mlb, ba_AAA, ba_AA, ba_Aplus, ncol = 2)

## Kpct
(kpct_mlb <- mlb_hitting %>% ggplot(aes(K_pct_AAA, K_pct_MLB)) +
  geom_point(color = 'slategray3') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5, by = 0.1)) +
  scale_y_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5, by = 0.1)) +
  labs(title = 'K% in MLB vs AAA',
       y = 'K% (MLB)',
       x = 'K% (AAA)'))

(kpct_AAA <- minor_league_hitting_AAA %>% ggplot(aes(K_pct_AA, K_pct_AAA)) +
  geom_point(color = 'slategray3') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5, by = 0.1)) +
  scale_y_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5, by = 0.1))+
    labs(title = 'K% in AAA vs AA',
         y = 'K% (AAA)',
         x = 'K% (AA)'))

(kpct_AA <- minor_league_hitting_AA %>% ggplot(aes(K_pct_Aplus, K_pct_AA)) +
  geom_point(color = 'slategray3') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5, by = 0.1)) +
  scale_y_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5, by = 0.1))+
    labs(title = 'K% in AA vs A+',
         y = 'K% (AA)',
         x = 'K% (A+)'))


(kpct_Aplus <- minor_league_hitting_Aplus %>% ggplot(aes(K_pct_A, K_pct_Aplus)) +
  geom_point(color = 'slategray3') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5, by = 0.1)) +
  scale_y_continuous(limits = c(0,0.5),
                     breaks = seq(0,0.5, by = 0.1)) +
    labs(title = 'K% in A+ vs A',
         y = 'K% (A+)',
         x = 'K% (A)'))

grid.arrange(kpct_mlb, kpct_AAA, kpct_AA, kpct_Aplus, ncol = 2)

# wRC+
mlb_hitting %>% ggplot(aes(wRC_plus_AAA, wRC_plus_MLB)) +
  geom_point(color = 'royalblue')+
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0,200),
                     breaks = seq(0,200, by = 50)) +
  scale_y_continuous(limits = c(0,200),
                     breaks = seq(0,200,by = 50))

minor_league_hitting_AAA %>% ggplot(aes(wRC_plus_AA, wRC_plus_AAA)) +
  geom_point(color = 'royalblue')+
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0,300),
                     breaks = seq(0,300, by = 50)) +
  scale_y_continuous(limits = c(0,300),
                     breaks = seq(0,300,by = 50))

minor_league_hitting_AA %>% ggplot(aes(wRC_plus_Aplus, wRC_plus_AA)) +
  geom_point(color = 'royalblue')+
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0,300),
                     breaks = seq(0,300, by = 50)) +
  scale_y_continuous(limits = c(0,300),
                     breaks = seq(0,300,by = 50))

minor_league_hitting_Aplus %>% ggplot(aes(wRC_plus_A, wRC_plus_Aplus)) +
  geom_point(color = 'royalblue')+
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0,300),
                     breaks = seq(0,300, by = 50)) +
  scale_y_continuous(limits = c(0,300),
                     breaks = seq(0,300,by = 50))

# gb%
mlb_hitting %>% ggplot(aes(y = GB_pct_MLB, x = GB_pct_AAA)) +
  geom_point(color = 'tomato2') +
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw() +
  scale_x_continuous(limits = c(0.2,0.7),
                     breaks = seq(0.2,0.7, by = 0.1)) +
  scale_y_continuous(limits = c(0.2,0.7),
                     breaks = seq(0.2,0.7, by = 0.1))

mlb_hitting %>% ggplot(aes(x = GB_pct_MLB, y = wRC_plus_MLB)) +
  geom_point(color = 'lightpink2') +
  theme_bw() +
  geom_smooth()

minor_league_hitting_AAA %>% ggplot(aes(y = GB_pct_AAA, x = GB_pct_AA)) +
  geom_point(color = 'tomato2') +
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw() +
  scale_x_continuous(limits = c(0.2,0.7),
                     breaks = seq(0.2,0.7, by = 0.1)) +
  scale_y_continuous(limits = c(0.2,0.7),
                     breaks = seq(0.2,0.7, by = 0.1))

minor_league_hitting_AA %>% ggplot(aes(y = GB_pct_AA, x = GB_pct_Aplus)) +
  geom_point(color = 'tomato2') +
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw() +
  scale_x_continuous(limits = c(0.2,0.7),
                     breaks = seq(0.2,0.7, by = 0.1)) +
  scale_y_continuous(limits = c(0.2,0.7),
                     breaks = seq(0.2,0.7, by = 0.1))

minor_league_hitting_Aplus %>% ggplot(aes(y = GB_pct_Aplus, x = GB_pct_A)) +
  geom_point(color = 'tomato2') +
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw() +
  scale_x_continuous(limits = c(0.2,0.7),
                     breaks = seq(0.2,0.7, by = 0.1)) +
  scale_y_continuous(limits = c(0.2,0.7),
                     breaks = seq(0.2,0.7, by = 0.1)) 

## BB%
mlb_hitting %>% ggplot(aes(BB_pct_AAA, BB_pct_MLB)) +
  geom_point(color = 'limegreen') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3, by = 0.1)) +
  scale_y_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3, by = 0.1))


minor_league_hitting_AAA %>% ggplot(aes(BB_pct_AA, BB_pct_AAA)) +
  geom_point(color = 'limegreen') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3, by = 0.1)) +
  scale_y_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3, by = 0.1))

minor_league_hitting_AA %>% ggplot(aes(BB_pct_Aplus, BB_pct_AA)) +
  geom_point(color = 'limegreen') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3, by = 0.1)) +
  scale_y_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3, by = 0.1))

minor_league_hitting_Aplus %>% ggplot(aes(BB_pct_A, BB_pct_Aplus)) +
  geom_point(color = 'limegreen') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3, by = 0.1)) +
  scale_y_continuous(limits = c(0,0.3),
                     breaks = seq(0,0.3, by = 0.1))

## Pull%
mlb_hitting %>% ggplot(aes(Pull_pct_AAA, Pull_pct_MLB)) +
  geom_point(color = 'mediumaquamarine') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0.2,0.7),
                     breaks = seq(0.2,0.7, by = 0.1)) +
  scale_y_continuous(limits = c(0.2,0.7),
                     breaks = seq(0.2,0.7, by = 0.1))

minor_league_hitting_AAA %>% ggplot(aes(Pull_pct_AA, Pull_pct_AAA)) +
  geom_point(color = 'mediumaquamarine') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0.2,0.7),
                     breaks = seq(0.2,0.7, by = 0.1)) +
  scale_y_continuous(limits = c(0.2,0.7),
                     breaks = seq(0.2,0.7, by = 0.1))

minor_league_hitting_AA %>% ggplot(aes(Pull_pct_Aplus, Pull_pct_AA)) +
  geom_point(color = 'mediumaquamarine') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0.2,0.7),
                     breaks = seq(0.2,0.7, by = 0.1)) +
  scale_y_continuous(limits = c(0.2,0.7),
                     breaks = seq(0.2,0.7, by = 0.1))

minor_league_hitting_Aplus %>% ggplot(aes(Pull_pct_A, Pull_pct_Aplus)) +
  geom_point(color = 'mediumaquamarine') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  scale_x_continuous(limits = c(0.2,0.7),
                     breaks = seq(0.2,0.7, by = 0.1)) +
  scale_y_continuous(limits = c(0.2,0.7),
                     breaks = seq(0.2,0.7, by = 0.1))

# whiff%
mlb_hitting %>% ggplot(aes(SwStr_pct_AAA, SwStr_pct_MLB)) +
  geom_point(color = 'darkorchid') + 
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()

minor_league_hitting_AAA %>% ggplot(aes(SwStr_pct_AA, SwStr_pct_AAA)) +
  geom_point(color = 'darkorchid') + 
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()


minor_league_hitting_AA %>% ggplot(aes(SwStr_pct_Aplus, SwStr_pct_AA)) +
  geom_point(color = 'darkorchid') + 
  scale_color_continuous() +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()

minor_league_hitting_AA %>% ggplot(aes(SwStr_pct_Aplus, SwStr_pct_AA, color = PA_Aplus)) +
  geom_point() + 
  scale_color_continuous(low = 'lightblue3', high = 'tomato') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  geom_smooth(method = 'lm')

minor_league_hitting_Aplus %>% ggplot(aes(SwStr_pct_A, SwStr_pct_Aplus)) +
  geom_point(color = 'darkorchid') + 
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()


minor_league_hitting_AAA %>% 
  ggplot(aes(Age_AAA, made_mlb)) +
  geom_jitter(position = position_jitter(height = 0.01, width = .01, seed = 101)) +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw() +
  ylim(c(0,1)) +
  labs(y = 'Made MLB',
       x = 'Age (AAA)',
       title = 'Ages at Which Batters Make the Majors at AAA')

minor_league_hitting_AA %>% 
  ggplot(aes(Age_AA, made_mlb)) +
  geom_jitter(position = position_jitter(height = 0.01, width = 0.01, seed = 101)) +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw() +
  ylim(c(0,1))

minor_league_hitting_Aplus %>% 
  ggplot(aes(Age_Aplus, made_mlb)) +
  geom_jitter(position = position_jitter(height = 0.01, width = 0.01, seed = 101)) +
  geom_smooth(method = 'loess') +
  theme_bw() +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  ylim(c(0,1))


minor_league_hitting_A %>% 
  ggplot(aes(Age, made_mlb)) +
  geom_jitter(position = position_jitter(height = 0.01, width = 0.01, seed = 101)) +
  geom_smooth(method = 'loess') +
  theme_bw() +
  xlab('Age_A') +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  ylim(c(0,1))

# where Years = 25 when they didn't make MLB
minor_league_hitting_AAA %>% 
  ggplot(aes(Age_AAA, years_to_mlb)) +
  geom_jitter(position = position_jitter(height = 0.01, width = 0.01, seed = 101)) +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()

# removing players who didn't make the MLB
minor_league_hitting_AAA %>% 
  filter(years_to_mlb != 25) %>% 
  ggplot(aes(Age_AAA, years_to_mlb)) +
  geom_jitter(position = position_jitter(height = 0.01, width = 0.01, seed = 101)) +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()


# removing players who didn't make the MLB
minor_league_hitting_AA %>% 
  filter(years_to_mlb != 25) %>% 
  ggplot(aes(Age_AA, years_to_mlb)) +
  geom_jitter(position = position_jitter(height = 0.01, width = 0.01, seed = 101)) +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()

# removing players who didn't make the MLB
minor_league_hitting_Aplus %>% 
  filter(years_to_mlb != 25) %>% 
  ggplot(aes(Age_Aplus, years_to_mlb)) +
  geom_jitter(position = position_jitter(height = 0.01, width = 0.01, seed = 101)) +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()


# removing players who didn't make the MLB
minor_league_hitting_A %>% 
  filter(years_to_mlb != 25) %>% 
  ggplot(aes(Age, years_to_mlb)) +
  geom_jitter(position = position_jitter(height = 0.01, width = 0.01, seed = 101)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'tp')) +
  xlab('Age_A')+
  scale_x_continuous(breaks = seq(16,36, by = 2),
                     labels = seq(16,36,by = 2)) +
  theme_bw()
