library(tidyverse)
library(gt)


normalized_rmse_table <- tribble(
  ~Stat, ~Category, ~normalized_rmse, ~sd_,
  'BA', 'Batting', mean(avg_aplus_error_ratio, avg_aa_error_ratio,
                        avg_aaa_error_ratio, avg_mlb_error_ratio), mean(sd(avg_aplus$AVG_Aplus),
                                                                        sd(avg_AA$AVG_AA),
                                                                        sd(avg_AAA$AVG_AAA),
                                                                        sd(avg_mlb$AVG_MLB)),
  'OBP', 'Batting', mean(obp_aplus_error_ratio, obp_AA_error_ratio,
                         obp_aaa_error_ratio, obp_mlb_error_ratio), mean(sd(obp_aplus$OBP_Aplus),
                                                                         sd(obp_AA$OBP_AA),
                                                                         sd(obp_AAA$OBP_AAA),
                                                                         sd(obp_mlb$OBP_MLB)),
  'ISO', 'Batting', mean(iso_aplus_error_ratio, iso_aa_error_ratio,
                         iso_aaa_error_ratio, iso_mlb_error_ratio), mean(sd(iso_aplus$ISO_Aplus),
                                                                         sd(iso_AA$ISO_AA),
                                                                          sd(iso_AAA$ISO_AAA),
                                                                         sd(iso_mlb$ISO_MLB)),
  'wRC+', 'Batting', mean(0.892,0.865,0.864,0.944), mean(sd(wrcplus_aplus$wRC_plus_Aplus),
                                                         sd(wrcplus_AA$wRC_plus_AA),
                                                         sd(wrcplus_AAA$wRC_plus_AAA),
                                                         sd(wrcplus_mlb$wRC_plus_MLB)),
  'K%', 'Batting', mean(kpct_aplus_error_ratio, kpct_AA_error_ratio,
                        kpct_AAA_error_ratio, kpct_mlb_error_ratio), mean(sd(kpct_aplus$K_pct_Aplus),
                                                                          sd(kpct_AA$K_pct_AA),
                                                                            sd(kpct_AAA$K_pct_AAA),
                                                                          sd(kpct_mlb$K_pct_MLB)),
  'BB%', 'Batting', mean(bbpct_aplus_error_ratio, bbpct_AA_error_ratio,
                         bbpct_AAA_error_ratio, bbpct_mlb_error_ratio), mean(sd(bbpct_aplus$BB_pct_Aplus),
                                                                             sd(bbpct_AA$BB_pct_AA),
                                                                             sd(bbpct_AAA$BB_pct_AAA),
                                                                             sd(bbpct_mlb$BB_pct_MLB)),
  'SwStr%','Batting', mean(swstrpct_aplus_error_ratio, swstrpct_AA_error_ratio,
                           swstrpct_AAA_error_ratio, swstrpct_mlb_error_ratio), mean(sd(swstrpct_aplus$SwStr_pct_A),
                                                                                     sd(swstrpct_AA$SwStr_pct_AA),
                                                                                     sd(swstrpct_AAA$SwStr_pct_AAA),
                                                                                     sd(swstrpct_mlb$SwStr_pct_MLB)),
  'ERA', 'Pitching', mean(era_aplus_error_ratio, era_AA_error_ratio, 
                          era_AAA_error_ratio, era_mlb_error_ratio), mean(sd(era_aplus$ERA_Aplus),
                                                                          sd(era_AA$ERA_AA),
                                                                          sd(era_AAA$ERA_AAA),
                                                                          sd(era_mlb$ERA_MLB)),
  'FIP', 'Pitching', mean(fip_aplus_error_ratio, fip_AA_error_ratio,
                          fip_AAA_error_ratio, fip_mlb_error_ratio), mean(sd(fip_aplus$FIP_Aplus),
                                                                          sd(fip_AA$FIP_AA),
                                                                          sd(fip_AAA$FIP_AAA),
                                                                          sd(fip_mlb$FIP_MLB)),
  'K%', 'Pitching', mean(kpctpit_aplus_error_ratio, kpctpit_AA_error_ratio,
                         kpctpit_AAA_error_ratio, kpctpit_mlb_error_ratio), mean(sd(kpctpit_aplus$K_pct_Aplus),
                                                                                 sd(kpctpit_AA$K_pct_AA),
                                                                                 sd(kpctpit_AAA$K_pct_AAA),
                                                                                 sd(kpctpit_mlb$K_pct_MLB)),
  'BB%', 'Pitching', mean(bbpctpit_aplus_error_ratio, bbpctpit_AA_error_ratio,
                          bbpctpit_AAA_error_ratio, bbpctpit_mlb_error_ratio), mean(sd(bbpctpit_aplus$BB_pct_Aplus),
                                                                                    sd(bbpctpit_AA$BB_pct_AA),
                                                                                    sd(bbpctpit_AAA$BB_pct_AAA),
                                                                                    sd(bbpctpit_mlb$BB_pct_MLB)),
  'SwStr%', 'Pitching', mean(swstrpctpit_aplus_error_ratio, swstrpctpit_AA_error_ratio,
                             swstrpctpit_AAA_error_ratio, swstrpctpit_mlb_error_ratio), mean(sd(swstrpctpit_aplus$SwStr_pct_A),
                                                                                             sd(swstrpctpit_AA$SwStr_pct_AA),
                                                                                             sd(swstrpctpit_AAA$SwStr_pct_AAA),
                                                                                             sd(swstrpctpit_mlb$SwStr_pct_MLB))
) %>% 
  mutate(rmse = round(normalized_rmse * sd_,3),
         normalized_rmse = round(normalized_rmse, 3),
         sd_ = round(sd_, 3)) %>% 
  relocate('rmse', .after = Category) %>% 
  relocate('normalized_rmse', .after = sd_)

gt(normalized_rmse_table) %>% 
  cols_label(
    rmse = "RMSE",
    sd_ = "Standard Deviation (SD)",
    normalized_rmse = "Normalized RMSE (RMSE/SD)"
  ) %>%
  tab_header(
    title = "Model Accuracy Comparison",
    subtitle = 'Average of All 4 Levels'
  )
