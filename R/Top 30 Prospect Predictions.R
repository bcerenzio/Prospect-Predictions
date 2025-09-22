library(readxl)
library(tidyverse)
library(dbarts)

source('fangraphs_cleaning_hitting.R')
source('fangraphs_cleaning_pitchers.R')
source('Final BART Models.R')

top_100_prospects <- read_excel('Prospect-Predictions//MLB 2025 Prospect Rankings.xlsx', sheet = 'Top 100') %>% 
  rename('top_100_rank' = 'Rank')
top_30_prospects <- read_excel('Prospect-Predictions//MLB 2025 Prospect Rankings.xlsx', sheet = 'Top 30 Rankings') %>% 
  mutate(Position = case_when(
    Position == 'RHp' ~ 'RHP', 
    Position == '3N' ~ '3B',
    .default = Position))

unique(top_30_prospects$Position)


top_30_A <- top_30_prospects %>% filter(Level == 'A')
summary(top_30_A$Age)
top_30_Aplus <- top_30_prospects %>% filter(Level == 'A+')
top_30_AA <- top_30_prospects %>% filter(Level == 'AA')
top_30_AAA <- top_30_prospects %>% filter(Level %in% c('MLB', 'AAA') & Name != 'Roki Sasaki')

# Single A ####
top_30_A_hitting <- top_30_A %>% 
  filter(!(Position %in% c('RHP', 'LHP'))) %>% 
  left_join(minor_league_hitting_A %>% select(-Age), by = c('Name', 'Level'))

top_30_A_hitting <- top_30_A_hitting %>% 
  drop_na(PlayerId) %>% # not enough games at A
  filter(str_detect(PlayerId, 'sa'))
  
nrow(top_30_A_hitting) == length(unique(top_30_A_hitting$Name)) # FALSE -> Jared Thomas -> fangraphs id = sa3025252

top_30_A_hitting <- top_30_A_hitting %>% 
  mutate(PlayerId = ifelse(Name == 'Jared Thomas' & PlayerId != 'sa3025252', NA, PlayerId)) %>% 
  filter(!is.na(PlayerId))

nrow(top_30_A_hitting) == length(unique(top_30_A_hitting$Name)) #TRUE

top_30_A_pitching <- top_30_A %>% 
  filter(Position %in% c('RHP', 'LHP')) %>% 
  left_join(minor_league_pitching_A %>% select(-Age), by = c('Name', 'Level'))

top_30_A_pitching <- top_30_A_pitching %>% 
  drop_na(PlayerId) %>% # not enough games at A
  filter(str_detect(PlayerId, 'sa')) # filters out players who made the MLB with the same name

nrow(top_30_A_pitching) == length(unique(top_30_A_pitching$Name)) # FALSE -> Manuel Rodriguezn -> fangraphs id = sa3019291

top_30_A_pitching <- top_30_A_pitching %>% 
  mutate(PlayerId = ifelse(Name == 'Manuel Rodriguez' & PlayerId != 'sa3019291', NA, PlayerId)) %>% 
  filter(!is.na(PlayerId))

nrow(top_30_A_pitching) == length(unique(top_30_A_pitching$Name)) # TRUE


# A+ ####
top_30_Aplus_hitting <- top_30_Aplus %>% 
  filter(!(Position %in% c('RHP', 'LHP'))) %>% 
  left_join(minor_league_hitting_Aplus, by = c('Name' = 'Name_Aplus', 'Level' = 'Level_Aplus'))


top_30_Aplus_hitting <- top_30_Aplus_hitting %>% 
  drop_na(PlayerId) %>% # not enough games at A+
  filter(str_detect(PlayerId, 'sa'))

nrow(top_30_Aplus_hitting) == length(unique(top_30_Aplus_hitting$Name)) # TRUE
  
top_30_Aplus_pitching <- top_30_Aplus %>% 
  filter(Position %in% c('RHP', 'LHP')) %>% 
  left_join(minor_league_pitching_Aplus, by = c('Name' = 'Name_Aplus', 'Level' = 'Level_Aplus'))

top_30_Aplus_pitching <- top_30_Aplus_pitching %>% 
  drop_na(PlayerId) %>% # not enough games at A+
  filter(str_detect(PlayerId, 'sa'))

nrow(top_30_Aplus_pitching) == length(unique(top_30_Aplus_pitching$Name)) # FALSE -> Jake Miller & Juan Nunez
                                                                          # Jake Miller fangraphs id = sa3020408
                                                                          # Juan Nunez fangraphs id = sa3015464
top_30_Aplus_pitching <- top_30_Aplus_pitching %>% 
  mutate(PlayerId = ifelse(Name == 'Jake Miller' & PlayerId != 'sa3020408', NA, PlayerId)) %>%
  mutate(PlayerId = ifelse(Name == 'Juan Nunez' & PlayerId != 'sa3015464', NA, PlayerId)) %>% 
  filter(!is.na(PlayerId))

nrow(top_30_Aplus_pitching) == length(unique(top_30_Aplus_pitching$Name)) # TRUE

# AA ####
top_30_AA_hitting <- top_30_AA %>% 
  filter(!(Position %in% c('RHP', 'LHP'))) %>% 
  left_join(minor_league_hitting_AA, by = c('Name' = 'Name_AA', 'Level' = 'Level_AA'))

top_30_AA_hitting <- top_30_AA_hitting %>% 
  drop_na(PlayerId) # not enough games at AA


nrow(top_30_AA_hitting) == length(unique(top_30_AA_hitting$Name)) #FALSE -> Jacob Gonzalez -> fangraphs id = sa3022498


top_30_AA_hitting <- top_30_AA_hitting %>% 
  mutate(PlayerId = ifelse(Name == 'Jacob Gonzalez' & PlayerId != 'sa3022498', NA, PlayerId)) %>% 
  filter(!is.na(PlayerId))

nrow(top_30_AA_hitting) == length(unique(top_30_AA_hitting$Name)) # TRUE

top_30_AA_pitching <- top_30_AA %>% 
  filter(Position %in% c('RHP', 'LHP')) %>% 
  left_join(minor_league_pitching_AA, by = c('Name' = 'Name_AA', 'Level' = 'Level_AA'))

top_30_AA_pitching <- top_30_AA_pitching %>% 
  drop_na(PlayerId)# not enough games at AA

nrow(top_30_AA_pitching) == length(unique(top_30_AA_pitching$Name)) # FALSE -> Sean Sullivan & Jack Perkins
                                                                    # Jack Perkins fangraphs id = sa3020076
                                                                    # Sean Sullivan fangraphs id = sa3023162

top_30_AA_pitching <- top_30_AA_pitching %>% 
  mutate(PlayerId = ifelse(Name == 'Jack Perkins' & PlayerId != 'sa3020076', NA, PlayerId)) %>% 
  mutate(PlayerId = ifelse(Name == 'Sean Sullivan' & PlayerId != 'sa3023162', NA, PlayerId)) %>% 
  filter(!is.na(PlayerId))

nrow(top_30_AA_pitching) == length(unique(top_30_AA_pitching$Name)) # TRUE

# AAA ####
top_30_AAA_hitting <- top_30_AAA %>% 
  mutate(Level = 'AAA') %>% 
  filter(!(Position %in% c('RHP', 'LHP'))) %>% 
  left_join(minor_league_hitting_AAA, by = c('Name' = 'Name_AAA', 'Level' = 'Level_AAA'))

top_30_AAA_hitting <- top_30_AAA_hitting %>% 
  drop_na(PlayerId) # not enough games at AA

nrow(top_30_AAA_hitting) == length(unique(top_30_AAA_hitting$Name)) # FALSE -> Jacob Wilson -> fangraphs id = 33266

top_30_AAA_hitting <- top_30_AAA_hitting %>% 
  mutate(PlayerId = ifelse(Name == 'Jacob Wilson' & PlayerId != '33266', NA, PlayerId)) %>% 
  filter(!is.na(PlayerId))

nrow(top_30_AAA_hitting) == length(unique(top_30_AAA_hitting$Name)) # TRUE

top_30_AAA_pitching <- top_30_AAA %>% 
  mutate(Level = 'AAA') %>% 
  filter(Position %in% c('RHP', 'LHP')) %>% 
  left_join(minor_league_pitching_AAA, by = c('Name' = 'Name_AAA', 'Level' = 'Level_AAA'))

top_30_AAA_pitching <- top_30_AAA_pitching %>% 
  drop_na(PlayerId)# not enough games at AA

nrow(top_30_AAA_pitching) == length(unique(top_30_AAA_pitching$Name)) # TRUE

sum(nrow(top_30_A_hitting), nrow(top_30_A_pitching), nrow(top_30_AA_hitting), nrow(top_30_AA_pitching),
         nrow(top_30_AAA_hitting), nrow(top_30_AAA_pitching), nrow(top_30_Aplus_hitting), nrow(top_30_Aplus_pitching)) # 675 players remaining


### Hitter Predictions ####
# Level A ####

glimpse(top_30_A_hitting)

top_30_A_hitting <- top_30_A_hitting %>% 
  left_join(top_100_prospects %>% 
              select(-Team, -Position, -Level), by = c('Name', 'Age')) 

preds1 <- predict(avg_aplus_mod, top_30_A_hitting %>%
                        mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                               Age_A = Age,
                               team_rank = Rank,
                               H_A_adj = H/(AB^0.8),
                               LD_pct_A = LD_pct,
                               GB_per_FB_A = GB_per_FB,
                               wRC_plus_A = wRC_plus,
                               BB_minus_K_pct_A = BB_pct - K_pct,
                               SwStr_pct_A = SwStr_pct,
                               Pull_pct_A = Pull_pct))

predictions <- numeric();for (pred in 1:nrow(top_30_A_hitting)){
    col <- paste0('V', pred)
    predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_A_hitting$pred_AVG <- round(predictions,3)

preds1 <- predict(obp_aplus_mod, top_30_A_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_A = Age,
                           team_rank = Rank,
                           on_base_A_adj = (H + BB)/(PA^0.8),
                           wRC_plus_A = wRC_plus,
                           BB_minus_K_pct_A = BB_pct - K_pct,
                           SwStr_pct_A = SwStr_pct))

predictions <- numeric();for (pred in 1:nrow(top_30_A_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_A_hitting$pred_OBP <- round(predictions,3)


preds1 <- predict(iso_aplus_mod, top_30_A_hitting %>%
                    mutate(TB = x1B + 2*x2B + 3*x3B + 4*HR) %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_A = Age,
                           team_rank = Rank,
                           iso_A_adj = (TB-H)/(AB^0.8),
                           wRC_plus_A = wRC_plus,
                           BB_minus_K_pct_A = BB_pct - K_pct,
                           SwStr_pct_A = SwStr_pct,
                           HR_per_FB_A = HR_per_FB))

predictions <- numeric();for (pred in 1:nrow(top_30_A_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_A_hitting$pred_ISO <- round(predictions,3)

#Adding SLG
top_30_A_hitting <- top_30_A_hitting %>% 
  mutate(pred_SLG = pred_AVG + pred_ISO) %>% 
  relocate(pred_SLG, .before = 'pred_ISO')

preds1 <- predict(wrcplus_aplus_mod, top_30_A_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_A = Age,
                           team_rank = Rank,
                           wRAA_A_adj = (wRAA)/(PA^0.8),
                           BB_minus_K_pct_A = BB_pct - K_pct,
                           Pull_pct_A = Pull_pct,
                           Oppo_pct_A = Oppo_pct,
                           GB_pct_A = GB_pct,
                           LD_pct_A = LD_pct,
                           BABIP_A = BABIP,
                           SwStr_pct_A = SwStr_pct))

predictions <- numeric();for (pred in 1:nrow(top_30_A_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_A_hitting$pred_wrcplus <- round(predictions,0)


preds1 <- predict(kpct_aplus_mod, top_30_A_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_A = Age,
                           team_rank = Rank,
                           kpct_A_adj = (SO)/(PA^0.6),
                           wRC_plus_A = wRC_plus,
                           K_pct_A = K_pct,
                           SwStr_pct_A = SwStr_pct))

predictions <- numeric();for (pred in 1:nrow(top_30_A_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_A_hitting$pred_Kpct <- round(predictions,3)*100


preds1 <- predict(bbpct_aplus_mod, top_30_A_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_A = Age,
                           team_rank = Rank,
                           bbpct_A_adj = (BB)/(PA^0.8),
                           wRC_plus_A = wRC_plus,
                           SwStr_pct_A = SwStr_pct))

predictions <- numeric();for (pred in 1:nrow(top_30_A_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_A_hitting$pred_BBpct <- round(predictions,3)*100


preds1 <- predict(swstrpct_aplus_mod, top_30_A_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_A = Age,
                           team_rank = Rank,
                           wRC_plus_A = wRC_plus,
                           BB_minus_K_pct = BB_pct - K_pct,
                           SwStr_pct_A = SwStr_pct))

predictions <- numeric();for (pred in 1:nrow(top_30_A_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_A_hitting$pred_SwStrpct <- round(predictions,3)*100

top_30_A_hitting$mlb_prob <- round(
  predict(made_mlb_hitA, as.matrix(top_30_A_hitting %>% 
                                     reframe(Age, PA,
                                            team_rank_ba = Rank,
                                            avg_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                                            wRC_plus,
                                            BB_minus_K_pct = BB_pct - K_pct,
                                            Pull_pct,
                                            GB_per_FB,
                                            SwStr_pct))
),3
)*100


### A+ ####
glimpse(top_30_Aplus_hitting)

top_30_Aplus_hitting <- top_30_Aplus_hitting %>% 
  left_join(top_100_prospects %>% 
              select(-Team, -Position, -Level), by = c('Name', 'Age')) 

preds1 <- predict(avg_aa_mod, top_30_Aplus_hitting %>%
                        mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                               Age_Aplus = Age,
                               team_rank = Rank,
                               H_Aplus_adj = H_Aplus/(AB_Aplus^0.8),
                               BB_minus_K_pct_Aplus = BB_pct_Aplus - K_pct_Aplus))

predictions <- numeric();for (pred in 1:nrow(top_30_Aplus_hitting)){
    col <- paste0('V', pred)
    predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_Aplus_hitting$pred_AVG <- round(predictions,3)

preds1 <- predict(obp_AA_mod, top_30_Aplus_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_Aplus = Age,
                           team_rank = Rank,
                           on_base_Aplus_adj = (H_Aplus + BB_Aplus)/(PA_Aplus^0.8),
                           BB_minus_K_pct_Aplus = BB_pct_Aplus - K_pct_Aplus))

predictions <- numeric();for (pred in 1:nrow(top_30_Aplus_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_Aplus_hitting$pred_OBP <- round(predictions,3)


preds1 <- predict(iso_AA_mod, top_30_Aplus_hitting %>%
                    mutate(TB = x1B_Aplus + 2*x2B_Aplus + 3*x3B_Aplus + 4*HR_Aplus) %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_Aplus = Age,
                           team_rank = Rank,
                           iso_Aplus_adj = (TB-H_Aplus)/(AB_Aplus^0.8),
                           BB_minus_K_pct_Aplus = BB_pct_Aplus - K_pct_Aplus))

predictions <- numeric();for (pred in 1:nrow(top_30_Aplus_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_Aplus_hitting$pred_ISO <- round(predictions,3)

#Adding SLG
top_30_Aplus_hitting <- top_30_Aplus_hitting %>% 
  mutate(pred_SLG = pred_AVG + pred_ISO) %>% 
  relocate(pred_SLG, .before = 'pred_ISO')

preds1 <- predict(wrcplus_AA_mod, top_30_Aplus_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_Aplus = Age,
                           team_rank = Rank,
                           wRAA_Aplus_adj = (wRAA_Aplus)/(PA_Aplus^0.8),
                           BB_minus_K_pct_Aplus = BB_pct_Aplus - K_pct_Aplus))

predictions <- numeric();for (pred in 1:nrow(top_30_Aplus_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_Aplus_hitting$pred_wrcplus <- round(predictions,0)


preds1 <- predict(kpct_AA_mod, top_30_Aplus_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_Aplus = Age,
                           team_rank = Rank,
                           kpct_Aplus_adj = (SO_Aplus)/(PA_Aplus^0.6)))

predictions <- numeric();for (pred in 1:nrow(top_30_Aplus_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_Aplus_hitting$pred_Kpct <- round(predictions,3)*100


preds1 <- predict(bbpct_AA_mod, top_30_Aplus_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_Aplus = Age,
                           team_rank = Rank,
                           bbpct_Aplus_adj = (BB_Aplus)/(PA_Aplus^0.8)))

predictions <- numeric();for (pred in 1:nrow(top_30_Aplus_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_Aplus_hitting$pred_BBpct <- round(predictions,3)*100


preds1 <- predict(swstrpct_AA_mod, top_30_Aplus_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_Aplus = Age,
                           team_rank = Rank,
                           BB_minus_K_pct_Aplus = BB_pct_Aplus - K_pct_Aplus))

predictions <- numeric();for (pred in 1:nrow(top_30_Aplus_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_Aplus_hitting$pred_SwStrpct <- round(predictions,3)*100

top_30_Aplus_hitting$mlb_prob <- round(
  predict(made_mlb_hitAplus_mod, as.matrix(top_30_Aplus_hitting %>% 
                                     reframe(Age_Aplus, PA_Aplus,
                                             team_rank_ba_Aplus = Rank,
                                             avg_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                                             wRC_plus_Aplus,
                                             BB_minus_K_pct = BB_pct_Aplus - K_pct_Aplus,
                                             Pull_pct_Aplus,
                                             SwStr_pct_Aplus,
                                             GB_per_FB_Aplus
                                             ))
  ),3
)*100

### AA ####
glimpse(top_30_AA_hitting)

top_30_AA_hitting <- top_30_AA_hitting %>% 
  left_join(top_100_prospects %>% 
              select(-Team, -Position, -Level), by = c('Name', 'Age')) 

preds1 <- predict(avg_AAA_mod, top_30_AA_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AA = Age,
                           team_rank = Rank,
                           H_AA_adj = H_AA/(AB_AA^0.8),
                           BB_minus_K_pct_AA = BB_pct_AA - K_pct_AA))

predictions <- numeric();for (pred in 1:nrow(top_30_AA_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AA_hitting$pred_AVG <- round(predictions,3)

preds1 <- predict(obp_AAA_mod, top_30_AA_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AA = Age,
                           team_rank = Rank,
                           on_base_AA = (H_AA + BB_AA)/(PA_AA^0.8),
                           BB_minus_K_pct_AA = BB_pct_AA - K_pct_AA))

predictions <- numeric();for (pred in 1:nrow(top_30_AA_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AA_hitting$pred_OBP <- round(predictions,3)


preds1 <- predict(iso_AAA_mod, top_30_AA_hitting %>%
                    mutate(TB = x1B_AA + 2*x2B_AA + 3*x3B_AA + 4*HR_AA) %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AA = Age,
                           team_rank = Rank,
                           iso_base_AA = (TB-H_AA)/(AB_AA^0.8),
                           BB_minus_K_pct_AA = BB_pct_AA - K_pct_AA))

predictions <- numeric();for (pred in 1:nrow(top_30_AA_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AA_hitting$pred_ISO <- round(predictions,3)

#Adding SLG
top_30_AA_hitting <- top_30_AA_hitting %>% 
  mutate(pred_SLG = pred_AVG + pred_ISO) %>% 
  relocate(pred_SLG, .before = 'pred_ISO')

preds1 <- predict(wrcplus_AAA_mod, top_30_AA_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AA = Age,
                           team_rank = Rank,
                           wRAA_AA_adj = (wRAA_AA)/(PA_AA^0.8),
                           BB_minus_K_pct_AA = BB_pct_AA - K_pct_AA))

predictions <- numeric();for (pred in 1:nrow(top_30_AA_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AA_hitting$pred_wrcplus <- round(predictions,0)


preds1 <- predict(kpct_AAA_mod, top_30_AA_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AA = Age,
                           team_rank = Rank,
                           kpct_AA_adj = (SO_AA)/(PA_AA^0.6)))

predictions <- numeric();for (pred in 1:nrow(top_30_AA_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AA_hitting$pred_Kpct <- round(predictions,3)*100


preds1 <- predict(bbpct_AAA_mod, top_30_AA_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AA = Age,
                           team_rank = Rank,
                           bbpct_AA_adj = (BB_AA)/(PA_AA^0.8)))

predictions <- numeric();for (pred in 1:nrow(top_30_AA_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AA_hitting$pred_BBpct <- round(predictions,3)*100


preds1 <- predict(swstrpct_AAA_mod, top_30_AA_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AA = Age,
                           team_rank = Rank,
                           BB_minus_K_pct_AA = BB_pct_AA - K_pct_AA))

predictions <- numeric();for (pred in 1:nrow(top_30_AA_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AA_hitting$pred_SwStrpct <- round(predictions,3)*100

top_30_AA_hitting$mlb_prob <- round(
  predict(made_mlb_hitAA_mod, as.matrix(top_30_AA_hitting %>% 
                                             reframe(Age_AA, PA_AA,
                                                     team_rank_ba_AA = Rank,
                                                     avg_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                                                     wRC_plus_AA,
                                                     BB_minus_K_pct = BB_pct_AA - K_pct_AA,
                                                     Pull_pct_AA,
                                                     SwStr_pct_AA,
                                                     GB_per_FB_AA
                                             ))
  ),3
)*100


### AAA ####
glimpse(top_30_AAA_hitting)

top_30_AAA_hitting <- top_30_AAA_hitting %>% 
  left_join(top_100_prospects %>% 
              select(-Team, -Position, -Level), by = c('Name', 'Age')) 

preds1 <- predict(avg_mlb_mod, top_30_AAA_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AAA = Age,
                           team_rank = Rank,
                           AVG_AAA_adj = H_AAA/(AB_AAA^0.8),
                           BB_minus_K_pct_AAA = BB_pct_AAA - K_pct_AAA))

predictions <- numeric();for (pred in 1:nrow(top_30_AAA_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AAA_hitting$pred_AVG <- round(predictions,3)

preds1 <- predict(obp_mlb_mod, top_30_AAA_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AAA = Age,
                           team_rank = Rank,
                           on_bases_AAA_adj = (H_AAA + BB_AAA)/(PA_AAA^0.8),
                           BB_minus_K_pct_AAA = BB_pct_AAA - K_pct_AAA))

predictions <- numeric();for (pred in 1:nrow(top_30_AAA_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AAA_hitting$pred_OBP <- round(predictions,3)


preds1 <- predict(iso_mlb_mod, top_30_AAA_hitting %>%
                    mutate(TB = x1B_AAA + 2*x2B_AAA + 3*x3B_AAA + 4*HR_AAA) %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AAA = Age,
                           team_rank = Rank,
                           iso_AAA_adj = (TB-H_AAA)/(AB_AAA^0.8),
                           BB_minus_K_pct_AAA = BB_pct_AAA - K_pct_AAA))

predictions <- numeric();for (pred in 1:nrow(top_30_AAA_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AAA_hitting$pred_ISO <- round(predictions,3)

#Adding SLG
top_30_AAA_hitting <- top_30_AAA_hitting %>% 
  mutate(pred_SLG = pred_AVG + pred_ISO) %>% 
  relocate(pred_SLG, .before = 'pred_ISO')

preds1 <- predict(wrcplus_mlb_mod, top_30_AAA_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AAA = Age,
                           team_rank = Rank,
                           wRAA_AAA_adj = (wRAA_AAA)/(PA_AAA^0.8),
                           BB_minus_K_pct_AAA = BB_pct_AAA - K_pct_AAA))

predictions <- numeric();for (pred in 1:nrow(top_30_AAA_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AAA_hitting$pred_wrcplus <- round(predictions,0)


preds1 <- predict(kpct_mlb_mod, top_30_AAA_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AAAA = Age,
                           team_rank = Rank,
                           kpct_AAA_adj = (SO_AAA)/(PA_AAA^0.6)))

predictions <- numeric();for (pred in 1:nrow(top_30_AAA_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AAA_hitting$pred_Kpct <- round(predictions,3)*100


preds1 <- predict(bbpct_mlb_mod, top_30_AAA_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AAA = Age,
                           team_rank = Rank,
                           bbpct_AAA_adj = (BB_AAA)/(PA_AAA^0.8)))

predictions <- numeric();for (pred in 1:nrow(top_30_AAA_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AAA_hitting$pred_BBpct <- round(predictions,3)*100


preds1 <- predict(swstrpct_mlb_mod, top_30_AAA_hitting %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AAA = Age,
                           team_rank = Rank,
                           BB_minus_K_pct = BB_pct_AAA - K_pct_AAA))

predictions <- numeric();for (pred in 1:nrow(top_30_AAA_hitting)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AAA_hitting$pred_SwStrpct <- round(predictions,3)*100

top_30_AAA_hitting$mlb_prob <- round(
  predict(made_mlb_hitAAA_mod, as.matrix(top_30_AAA_hitting %>% 
                                             reframe(Age_AAA, PA_AAA,
                                                     team_rank_ba_AAA = Rank,
                                                     avg_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                                                     wRC_plus_AAA,
                                                     BB_minus_K_pct = BB_pct_AAA - K_pct_AAA,
                                                     Pull_pct_AAA,
                                                     SwStr_pct_AAA,
                                                     GB_per_FB_AAA
                                             ))
  ),3
)*100


### Pitcher Predictions ####
# Level A ####
glimpse(top_30_A_pitching)

top_30_A_pitching <- top_30_A_pitching %>% 
  left_join(top_100_prospects %>% 
              select(-Team, -Position, -Level), by = c('Name', 'Age')) 

preds1 <- predict(era_aplus_mod, top_30_A_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_A = Age,
                           team_rank = Rank,
                           ER_A_adj = ER/(IP^0.6),
                           ERA_A = ERA,
                           WHIP_A = WHIP,
                           HR_per_FB_A = HR_per_FB,
                           Pull_pct_A = Pull_pct,
                           GB_pct_A = GB_pct,
                           SwStr_pct_A = SwStr_pct,
                           FIP_A = FIP
                           ))

predictions <- numeric();for (pred in 1:nrow(top_30_A_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_A_pitching$pred_ERA <- round(predictions,2)


preds1 <- predict(fip_aplus_mod, top_30_A_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_A = Age,
                           team_rank = Rank,
                           FIP_A_adj = (13*HR + 3*BB - 2*SO)/(IP^0.6),
                           ERA_A = ERA,
                           WHIP_A = WHIP,
                           HR_per_FB_A = HR_per_FB,
                           SwStr_pct_A = SwStr_pct,
                           FIP_A = FIP
                    ))

predictions <- numeric();for (pred in 1:nrow(top_30_A_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_A_pitching$pred_FIP <- round(predictions,2)

preds1 <- predict(kpctpit_aplus_mod, top_30_A_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_A = Age,
                           team_rank = Rank,
                           K_pct_A_adj = (SO)/(TBF^0.8),
                           ERA_A = ERA,
                           SwStr_pct_A = SwStr_pct))

predictions <- numeric();for (pred in 1:nrow(top_30_A_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_A_pitching$pred_Kpct <- round(predictions,3)*100


preds1 <- predict(bbpctpit_aplus_mod, top_30_A_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_A = Age,
                           team_rank = Rank,
                           BB_pct_A_adj = (BB)/(TBF^0.6),
                           ERA_A = ERA,
                           BB_pct_A = BB_pct,
                           SwStr_pct_A = SwStr_pct))

predictions <- numeric();for (pred in 1:nrow(top_30_A_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_A_pitching$pred_BBpct <- round(predictions,3)*100


preds1 <- predict(swstrpctpit_aplus_mod, top_30_A_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_A = Age,
                           team_rank = Rank,
                           K_pct_A = K_pct,
                           ERA_A = ERA,
                           SwStr_pct_A = SwStr_pct))

predictions <- numeric();for (pred in 1:nrow(top_30_A_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_A_pitching$pred_SwStrpct <- round(predictions,3)*100


top30_pit_A <- round(
  predict(made_mlb_A_pitchers, 
          as.matrix(top_30_A_pitching %>% 
                      reframe(
                        avg_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                        team_rank_ba = Rank,
                        Age,
                        GS,
                        ERA,
                        FIP,
                        GB_pct,
                        K_minus_BB_pct = K_pct - BB_pct,
                        SwStr_pct
                      )), reshape = TRUE),3) %>% 
  as_tibble()


colnames(top30_pit_A) <- c('no_mlb', 'mlb_rp','mlb_sp')

top_30_A_pitching$mlb_rp_prob <- top30_pit_A$mlb_rp*100
top_30_A_pitching$mlb_sp_prob <- top30_pit_A$mlb_sp*100

### A+ ####
glimpse(top_30_Aplus_pitching)

top_30_Aplus_pitching <- top_30_Aplus_pitching %>% 
  left_join(top_100_prospects %>% 
              select(-Team, -Position, -Level), by = c('Name', 'Age')) 

preds1 <- predict(era_AA_mod, top_30_Aplus_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_Aplus = Age,
                           team_rank = Rank,
                           ER_Aplus_adj = ER_Aplus/(IP_Aplus^0.6),
                    ))

predictions <- numeric();for (pred in 1:nrow(top_30_Aplus_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_Aplus_pitching$pred_ERA <- round(predictions,2)


preds1 <- predict(fip_AA_mod, top_30_Aplus_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_Aplus = Age,
                           team_rank = Rank,
                           FIP_Aplus_adj = (13*HR_Aplus + 3*BB_Aplus - 2*SO_Aplus)/(IP_Aplus^0.6)
                    ))

predictions <- numeric();for (pred in 1:nrow(top_30_Aplus_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_Aplus_pitching$pred_FIP <- round(predictions,2)

preds1 <- predict(kpctpit_AA_mod, top_30_Aplus_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_Aplus = Age,
                           team_rank = Rank,
                           K_pct_Aplus_adj = (SO_Aplus)/(TBF_Aplus^0.8),))

predictions <- numeric();for (pred in 1:nrow(top_30_Aplus_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_Aplus_pitching$pred_Kpct <- round(predictions,3)*100


preds1 <- predict(bbpctpit_AA_mod, top_30_Aplus_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_Aplus = Age,
                           team_rank = Rank,
                           BB_pct_Aplus_adj = (BB_Aplus)/(TBF_Aplus^0.6)))

predictions <- numeric();for (pred in 1:nrow(top_30_Aplus_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_Aplus_pitching$pred_BBpct <- round(predictions,3)*100


preds1 <- predict(swstrpctpit_AA_mod, top_30_Aplus_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_Aplus = Age,
                           team_rank = Rank))

predictions <- numeric();for (pred in 1:nrow(top_30_Aplus_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_Aplus_pitching$pred_SwStrpct <- round(predictions,3)*100

top30_pit_Aplus <- round(
  predict(made_mlb_Aplus_pitchers, 
          as.matrix(top_30_Aplus_pitching %>% 
                      reframe(
                        avg_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                        team_rank_ba_Aplus = Rank,
                        Age_Aplus,
                        GS_Aplus,
                        ERA_Aplus,
                        FIP_Aplus,
                        GB_pct_Aplus,
                        K_minus_BB_pct = K_pct_Aplus - BB_pct_Aplus,
                        SwStr_pct_Aplus
                      )), reshape = TRUE),3) %>% 
  as_tibble()


colnames(top30_pit_Aplus) <- c('no_mlb', 'mlb_rp','mlb_sp')

top_30_Aplus_pitching$mlb_rp_prob <- top30_pit_Aplus$mlb_rp*100
top_30_Aplus_pitching$mlb_sp_prob <- top30_pit_Aplus$mlb_sp*100


### AA ####
glimpse(top_30_AA_pitching)

top_30_AA_pitching <- top_30_AA_pitching %>% 
  left_join(top_100_prospects %>% 
              select(-Team, -Position, -Level), by = c('Name', 'Age')) 

preds1 <- predict(era_AAA_mod, top_30_AA_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AA = Age,
                           team_rank = Rank,
                           ER_AA_adj = ER_AA/(IP_AA^0.6),
                    ))

predictions <- numeric();for (pred in 1:nrow(top_30_AA_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AA_pitching$pred_ERA <- round(predictions,2)


preds1 <- predict(fip_AAA_mod, top_30_AA_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AA = Age,
                           team_rank = Rank,
                           FIP_AA_adj = (13*HR_AA + 3*BB_AA - 2*SO_AA)/(IP_AA^0.6)
                    ))

predictions <- numeric();for (pred in 1:nrow(top_30_AA_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AA_pitching$pred_FIP <- round(predictions,2)

preds1 <- predict(kpctpit_AAA_mod, top_30_AA_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AA = Age,
                           team_rank = Rank,
                           K_pct_AA_adj = (SO_AA)/(TBF_AA^0.8),))

predictions <- numeric();for (pred in 1:nrow(top_30_AA_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AA_pitching$pred_Kpct <- round(predictions,3)*100


preds1 <- predict(bbpctpit_AAA_mod, top_30_AA_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AA = Age,
                           team_rank = Rank,
                           BB_pct_AA_adj = (BB_AA)/(TBF_AA^0.6)))

predictions <- numeric();for (pred in 1:nrow(top_30_AA_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AA_pitching$pred_BBpct <- round(predictions,3)*100


preds1 <- predict(swstrpctpit_AAA_mod, top_30_AA_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AA = Age,
                           team_rank = Rank))

predictions <- numeric();for (pred in 1:nrow(top_30_AA_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AA_pitching$pred_SwStrpct <- round(predictions,3)*100

top30_pit_AA <- round(
  predict(made_mlb_AA_pitchers, 
          as.matrix(top_30_AA_pitching %>% 
                      reframe(
                        avg_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                        team_rank_ba_AA = Rank,
                        Age_AA,
                        GS_AA,
                        ERA_AA,
                        FIP_AA,
                        GB_pct_AA,
                        K_minus_BB_pct = K_pct_AA - BB_pct_AA,
                        SwStr_pct_AA
                      )), reshape = TRUE),3) %>% 
  as_tibble()


colnames(top30_pit_AA) <- c('no_mlb', 'mlb_rp','mlb_sp')

top_30_AA_pitching$mlb_rp_prob <- top30_pit_AA$mlb_rp*100
top_30_AA_pitching$mlb_sp_prob <- top30_pit_AA$mlb_sp*100

### AAA ####
glimpse(top_30_AAA_pitching)

top_30_AAA_pitching <- top_30_AAA_pitching %>% 
  left_join(top_100_prospects %>% 
              select(-Team, -Position, -Level), by = c('Name', 'Age')) 

preds1 <- predict(era_mlb_mod, top_30_AAA_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AAA = Age,
                           team_rank = Rank,
                           ER_AAA_adj = ER_AAA/(IP_AAA^0.6),
                    ))

predictions <- numeric();for (pred in 1:nrow(top_30_AAA_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AAA_pitching$pred_ERA <- round(predictions,2)


preds1 <- predict(fip_mlb_mod, top_30_AAA_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AAA = Age,
                           team_rank = Rank,
                           FIP_AAA_adj = (13*HR_AAA + 3*BB_AAA - 2*SO_AAA)/(IP_AAA^0.6)
                    ))

predictions <- numeric();for (pred in 1:nrow(top_30_AAA_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AAA_pitching$pred_FIP <- round(predictions,2)

preds1 <- predict(kpctpit_mlb_mod, top_30_AAA_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AAA = Age,
                           team_rank = Rank,
                           K_pct_AAA_adj = (SO_AAA)/(TBF_AAA^0.8),))

predictions <- numeric();for (pred in 1:nrow(top_30_AAA_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AAA_pitching$pred_Kpct <- round(predictions,3)*100


preds1 <- predict(bbpctpit_mlb_mod, top_30_AAA_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AAA = Age,
                           team_rank = Rank,
                           BB_pct_AAA_adj = (BB_AAA)/(TBF_AAA^0.6)))

predictions <- numeric();for (pred in 1:nrow(top_30_AAA_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AAA_pitching$pred_BBpct <- round(predictions,3)*100


preds1 <- predict(swstrpctpit_mlb_mod, top_30_AAA_pitching %>%
                    mutate(top_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                           Age_AAA = Age,
                           team_rank = Rank))

predictions <- numeric();for (pred in 1:nrow(top_30_AAA_pitching)){
  col <- paste0('V', pred)
  predictions <- c(predictions, mean(as_tibble(preds1) %>% pull(!!col), names = FALSE))
}  
top_30_AAA_pitching$pred_SwStrpct <- round(predictions,3)*100

top30_pit_AAA <- round(
  predict(made_mlb_AAA_pitchers, 
          as.matrix(top_30_AAA_pitching %>% 
                      reframe(
                        avg_100_rank = ifelse(is.na(top_100_rank), 110, top_100_rank),
                        team_rank_ba_AAA = Rank,
                        Age_AAA,
                        GS_AAA,
                        ERA_AAA,
                        FIP_AAA,
                        GB_pct_AAA,
                        K_minus_BB_pct = K_pct_AAA - BB_pct_AAA,
                        SwStr_pct_AAA
                      )), reshape = TRUE),3) %>% 
  as_tibble()


colnames(top30_pit_AAA) <- c('no_mlb', 'mlb_rp','mlb_sp')

top_30_AAA_pitching$mlb_rp_prob <- top30_pit_AAA$mlb_rp*100
top_30_AAA_pitching$mlb_sp_prob <- top30_pit_AAA$mlb_sp*100




### Final Table for Shiny ####
shiny_hitting_top_30 <- bind_rows(
  top_30_A_hitting %>% 
  select(Name, Position, Team, Level, Age, Rank,
         starts_with('pred_'),mlb_prob),
  top_30_Aplus_hitting %>% 
    select(Name, Position, Team, Level, Age, Rank,
           starts_with('pred_'),mlb_prob),
  top_30_AA_hitting %>% 
    select(Name, Position, Team, Level, Age, Rank,
           starts_with('pred_'),mlb_prob),
  top_30_AAA_hitting %>% 
    select(Name, Position, Team, Level, Age, Rank,
           starts_with('pred_'),mlb_prob)) %>% 
  arrange(Team, Rank)

# Sample Table
DT::datatable(shiny_hitting_top_30 %>% 
   mutate(mlb_prob = mlb_prob/100,
          pred_Kpct = pred_Kpct/100,
          pred_BBpct = pred_BBpct/100,
          pred_SwStrpct = pred_SwStrpct/100) %>% 
   rename(
  'AVG' = 'pred_AVG',
  'OBP' = 'pred_OBP',
  'SLG' = 'pred_SLG',
  'ISO' = 'pred_ISO',
  'wRC+' = 'pred_wrcplus',
  'K%' = 'pred_Kpct',
  'BB%' = 'pred_BBpct',
  'SwStr%' = 'pred_SwStrpct',
  'MLB Likelihood' = 'mlb_prob'
)) %>% 
 DT::formatRound(columns = c('AVG', 'OBP', 'SLG', 'ISO'), digits = 3) %>% 
 DT::formatPercentage(columns = c('K%','BB%','SwStr%'), digits = 1) %>% 
 DT::formatPercentage(columns = c('MLB Likelihood'), digits = 1)
  
write_csv(shiny_hitting_top_30, 'Prospect-Predictions/shiny_hitting_top_30.csv')

shiny_pitching_top_30 <- bind_rows(
  top_30_A_pitching %>% 
    select(Name, Position, Team, Level, Age, Rank,
           starts_with('pred_'),mlb_rp_prob, mlb_sp_prob),
  top_30_Aplus_pitching %>% 
    select(Name, Position, Team, Level, Age, Rank,
           starts_with('pred_'),mlb_rp_prob, mlb_sp_prob),
  top_30_AA_pitching %>% 
    select(Name, Position, Team, Level, Age, Rank,
           starts_with('pred_'),mlb_rp_prob, mlb_sp_prob),
  top_30_AAA_pitching %>% 
    select(Name, Position, Team, Level, Age, Rank,
           starts_with('pred_'),mlb_rp_prob, mlb_sp_prob)) %>% 
  arrange(Team, Rank)


DT::datatable(shiny_pitching_top_30 %>% 
                mutate(mlb_rp_prob = mlb_rp_prob/100,
                       mlb_sp_prob = mlb_sp_prob/100,
                       pred_Kpct = pred_Kpct/100,
                       pred_BBpct = pred_BBpct/100,
                       pred_SwStrpct = pred_SwStrpct/100) %>% 
                rename(
                  'ERA' = 'pred_ERA',
                  'FIP' = 'pred_FIP',
                  'K%' = 'pred_Kpct',
                  'BB%' = 'pred_BBpct',
                  'SwStr%' = 'pred_SwStrpct',
                  'MLB RP Likelihood' = 'mlb_rp_prob',
                  'MLB SP Likelihood' = 'mlb_sp_prob'
                )) %>% 
  DT::formatRound(columns = c('ERA', 'FIP'), digits = 2) %>% 
  DT::formatPercentage(columns = c('K%','BB%','SwStr%'), digits = 1) %>% 
  DT::formatPercentage(columns = c('MLB RP Likelihood', 'MLB SP Likelihood'), digits = 1)

write_csv(shiny_pitching_top_30, 'Prospect-Predictions/shiny_pitching_top_30.csv')

# creating top 100 table
top_100_hitting_shiny <- top_100_prospects %>%
  select(-Level, -Team) %>% 
  left_join(shiny_hitting_top_30 %>% select(-Position, -Rank), by = c('Name', 'Age')) %>% 
  drop_na()


write_csv(top_100_hitting_shiny, 'Prospect-Predictions/top_100_hitting_shiny.csv')

DT::datatable(top_100_hitting_shiny %>% 
                mutate(mlb_prob = mlb_prob/100,
                       pred_Kpct = pred_Kpct/100,
                       pred_BBpct = pred_BBpct/100,
                       pred_SwStrpct = pred_SwStrpct/100) %>% 
                rename(
                  'Top 100 Rank' = 'top_100_rank',
                  'AVG' = 'pred_AVG',
                  'OBP' = 'pred_OBP',
                  'SLG' = 'pred_SLG',
                  'ISO' = 'pred_ISO',
                  'wRC+' = 'pred_wrcplus',
                  'K%' = 'pred_Kpct',
                  'BB%' = 'pred_BBpct',
                  'SwStr%' = 'pred_SwStrpct',
                  'MLB Likelihood' = 'mlb_prob'
                )) %>% 
  DT::formatRound(columns = c('AVG', 'OBP', 'SLG', 'ISO'), digits = 3) %>% 
  DT::formatPercentage(columns = c('K%','BB%','SwStr%'), digits = 1) %>% 
  DT::formatPercentage(columns = c('MLB Likelihood'), digits = 1)


top_100_pitching_shiny <- top_100_prospects %>%
  select(-Level, -Team) %>% 
  left_join(shiny_pitching_top_30 %>% select(-Position, -Rank), by = c('Name', 'Age')) %>% 
  drop_na()

write_csv(top_100_pitching_shiny, 'Prospect-Predictions/top_100_pitching_shiny.csv')

DT::datatable(top_100_pitching_shiny %>% 
                mutate(mlb_rp_prob = mlb_rp_prob/100,
                       mlb_sp_prob = mlb_sp_prob/100,
                       pred_Kpct = pred_Kpct/100,
                       pred_BBpct = pred_BBpct/100,
                       pred_SwStrpct = pred_SwStrpct/100) %>% 
                rename(
                  'Top 100 Rank' = 'top_100_rank',
                  'ERA' = 'pred_ERA',
                  'FIP' = 'pred_FIP',
                  'K%' = 'pred_Kpct',
                  'BB%' = 'pred_BBpct',
                  'SwStr%' = 'pred_SwStrpct',
                  'MLB RP Likelihood' = 'mlb_rp_prob',
                  'MLB SP Likelihood' = 'mlb_sp_prob'
                )) %>% 
  DT::formatRound(columns = c('ERA', 'FIP'), digits = 2) %>% 
  DT::formatPercentage(columns = c('K%','BB%','SwStr%'), digits = 1) %>% 
  DT::formatPercentage(columns = c('MLB RP Likelihood', 'MLB SP Likelihood'), digits = 1)
