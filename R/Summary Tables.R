library(summarytools)
library(tidyverse)
library(gtsummary)

combined_df <- minor_league_hitting_A %>%
  select(PlayerId, Name, Level, made_mlb) %>% 
  bind_rows(minor_league_hitting_Aplus %>% 
              select(PlayerId, Name_Aplus, Level_Aplus, made_mlb) %>% 
              rename('Name' = 'Name_Aplus',
                     'Level' = 'Level_Aplus'),
            minor_league_hitting_AA %>% 
              select(PlayerId, Name_AA, Level_AA, made_mlb) %>% 
              rename('Name' = 'Name_AA',
                     'Level' = 'Level_AA'),
            minor_league_hitting_AAA %>% 
              select(PlayerId, Name_AAA, Level_AAA, made_mlb) %>% 
              rename('Name' = 'Name_AAA',
                     'Level' = 'Level_AAA'))




minor_league_hitting_summary_stats <- minor_league_hitting %>%
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
  ) %>% distinct() %>% relocate(pa_weights, .after = 'Name') %>% 
  right_join(combined_df, by = c('PlayerId','Name','Level')) %>% 
  mutate(
    wRC_plus = round(wRC_plus, 0),
    rank_ba = ifelse(rank_ba > 100, NA, rank_ba),
    rank_mlb = ifelse(rank_mlb > 100, NA, rank_mlb),
    team_rank_ba = ifelse(team_rank_ba > 30, NA, team_rank_ba)
  ) %>%
  select(Level, Age, PA, AVG, OBP, SLG, K_pct, BB_pct, SwStr_pct, 
         wRC_plus, rank_ba, rank_mlb, team_rank_ba, made_mlb)

hitting_summary <- stby(
  data = minor_league_hitting_summary_stats %>% select(-Level),
  INDICES = minor_league_hitting_summary_stats$Level,
  FUN = descr,
  stats = c('min', 'q1', 'med', 'mean', 'q3', 'sd', 'max', 'n.valid'),
  order = "p",
  round.digits = 3
)

# writes to html file
print(hitting_summary, file = 'hitting_summary_table.html')


combined_df_pit <- minor_league_pitching_A %>%
  select(PlayerId, Name, Level, made_mlb_sp, made_mlb_rp, years_to_mlb) %>% 
  bind_rows(minor_league_pitching_Aplus %>% 
              select(PlayerId, Name_Aplus, Level_Aplus, made_mlb_sp, made_mlb_rp) %>% 
              rename('Name' = 'Name_Aplus',
                     'Level' = 'Level_Aplus'),
            minor_league_pitching_AA %>% 
              select(PlayerId, Name_AA, Level_AA, made_mlb_sp, made_mlb_rp) %>% 
              rename('Name' = 'Name_AA',
                     'Level' = 'Level_AA'),
            minor_league_pitching_AAA %>% 
              select(PlayerId, Name_AAA, Level_AAA, made_mlb_sp, made_mlb_rp) %>% 
              rename('Name' = 'Name_AAA',
                     'Level' = 'Level_AAA'))


minor_league_pitching_summary_stats <- minor_league_pitching %>%
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
  ) %>% distinct() %>% relocate('bf_weights', .after = 'Level') %>% 
  right_join(combined_df_pit, by = c('PlayerId','Name','Level')) %>% 
  mutate(
    rank_ba = ifelse(rank_ba > 100, NA, rank_ba),
    rank_mlb = ifelse(rank_mlb > 100, NA, rank_mlb),
    team_rank_ba = ifelse(team_rank_ba > 30, NA, team_rank_ba)
  ) %>%
  select(Level, Age, TBF, ERA, FIP, BB_pct, K_pct, SwStr_pct, GB_pct, rank_ba, rank_mlb, team_rank_ba, made_mlb_sp, made_mlb_rp)

pitching_summary <- stby(
  data = minor_league_pitching_summary_stats %>% select(-Level),
  INDICES = minor_league_pitching_summary_stats$Level,
  FUN = descr,
  stats = c('min', 'q1', 'med', 'mean', 'q3', 'sd', 'max', 'n.valid'),
  order = "p",
  round.digits = 3
)

# writes to html file
print(pitching_summary, file = 'pitching_summary_table.html')


# MLB summary tables

mlb_hitting_summary_input <- mlb_hitting %>% rename('Level_AAA' = 'Level',
                                                    'rank_ba_AAA' = 'rank_ba',
                                                    'rank_mlb_AAA' = 'rank_mlb',
                                                    'team_rank_ba_AAA' = 'team_rank_ba') %>% 
  rename_with(~paste0('', str_remove(., '_MLB$'), recycle0 = TRUE), ends_with('_MLB')) %>% 
  select(Age, PA, AVG, OBP, SLG, K_pct, BB_pct, SwStr_pct, 
         wRC_plus) %>% 
  mutate(Level = 'MLB')

(mlb_hitting_summary <- descr(mlb_hitting %>% rename('Level_AAA' = 'Level',
                             'rank_ba_AAA' = 'rank_ba',
                             'rank_mlb_AAA' = 'rank_mlb',
                             'team_rank_ba_AAA' = 'team_rank_ba') %>% 
        rename_with(~paste0('', str_remove(., '_MLB$'), recycle0 = TRUE), ends_with('_MLB')) %>% 
        select(Age, PA, AVG, OBP, SLG, K_pct, BB_pct, SwStr_pct, 
               wRC_plus),
      round.digits = 3,
      order = 'p',
      stats = c('min', 'q1', 'med', 'mean', 'q3', 'sd', 'max', 'n.valid'),
      
))

mlb_hitting_summary <- stby(
  data = mlb_hitting_summary_input %>% select(-Level),
  INDICES = mlb_hitting_summary_input$Level,
  FUN = descr,
  stats = c('min', 'q1', 'med', 'mean', 'q3', 'sd', 'max', 'n.valid'),
  order = "p",
  round.digits = 3
)

print(mlb_hitting_summary, 
      file = 'mlb_hitting_summary.html')

mlb_sp_summary_input <- mlb_sp_pitching %>% rename('Level_AAA' = 'Level',
                                                   'rank_ba_AAA' = 'rank_ba',
                                                   'rank_mlb_AAA' = 'rank_mlb',
                                                   'team_rank_ba_AAA' = 'team_rank_ba') %>% 
  rename_with(~paste0('', str_remove(., '_MLB$'), recycle0 = TRUE), ends_with('_MLB')) %>% 
  select(Age, TBF, ERA, FIP, BB_pct, K_pct, SwStr_pct, GB_pct) %>% 
  mutate(Level = "MLB")


mlb_sp_summary <- stby(
  data = mlb_sp_summary_input %>% select(-Level),
  INDICES = mlb_sp_summary_input$Level,
  FUN = descr,
  stats = c('min', 'q1', 'med', 'mean', 'q3', 'sd', 'max', 'n.valid'),
  order = "p",
  round.digits = 3
)

# (mlb_sp_summary <- descr(mlb_sp_pitching %>% rename('Level_AAA' = 'Level',
#                                                      'rank_ba_AAA' = 'rank_ba',
#                                                      'rank_mlb_AAA' = 'rank_mlb',
#                                                      'team_rank_ba_AAA' = 'team_rank_ba') %>% 
#                                 rename_with(~paste0('', str_remove(., '_MLB$'), recycle0 = TRUE), ends_with('_MLB')) %>% 
#                                 select(Age, TBF, ERA, FIP, BB_pct, K_pct, SwStr_pct, GB_pct),
#                               round.digits = 3,
#                               order = 'p',
#                               stats = c('min', 'q1', 'med', 'mean', 'q3', 'sd', 'max', 'n.valid')
# ))


print(mlb_sp_summary, file = 'mlb_sp_summary.html')

mlb_rp_summary_input <- mlb_rp_pitching %>% 
  select(Age, TBF, ERA, FIP, BB_pct, K_pct, SwStr_pct, GB_pct) %>% 
  mutate(Level = 'MLB')


# (mlb_rp_summary <- descr(mlb_rp_pitching %>% 
#                            select(Age, TBF, ERA, FIP, BB_pct, K_pct, SwStr_pct, GB_pct, Level),
#                          round.digits = 3,
#                          order = 'p',
#                          stats = c('min', 'q1', 'med', 'mean', 'q3', 'sd', 'max', 'n.valid')
# ))

mlb_rp_summary <- stby(
  data = mlb_rp_summary_input %>% select(-Level),
  INDICES = mlb_rp_summary_input$Level,
  FUN = descr,
  stats = c('min', 'q1', 'med', 'mean', 'q3', 'sd', 'max', 'n.valid'),
  order = "p",
  round.digits = 3
)


print(mlb_rp_summary, file = 'mlb_rp_summary.html')
