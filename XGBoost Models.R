library(xgboost)
library(tidyverse)
library(progress)
library(SHAPforxgboost)

slice <- dplyr::slice


### AVG A to A+ ####
avg_aplus <- minor_league_hitting_Aplus %>% 
  drop_na(Name_A) %>% 
  rowwise() %>% 
  reframe(
    AVG_Aplus,
    top_100_rank = mean(c(rank_ba_A, rank_mlb_A)),
    team_rank = team_rank_ba_A,
    Age_A,
    H_A_adj = H_A/(AB_A^0.8),
    GB_per_FB_A,
    LD_pct_A,
    wRC_plus_A,
    BB_minus_K_pct_A = BB_pct_A - K_pct_A,
    SwStr_pct_A,
    Pull_pct_A,
  ) %>% 
  ungroup()


dtrain_avg_aplus <- xgb.DMatrix(as.matrix(avg_aplus %>% select(-AVG_Aplus)), label = avg_aplus$AVG_Aplus)


hyperparam_avg_aplus_tuning_reg <- function(max_depth_avg_aplus, weight_avg_aplus,subsample_avg_aplus, row_num_avg_aplus){
  
  print(paste('Max Depth: ', max_depth_avg_aplus))
  print(paste('Weight: ', weight_avg_aplus))
  print(paste('Subsample: ', subsample_avg_aplus))
  print(paste('Row Number: ', row_num_avg_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_avg_aplus,
      min_child_weight = weight_avg_aplus,
      subsample = subsample_avg_aplus
    ),
    data = dtrain_avg_aplus,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_avg_aplus_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_avg_aplus_df$rmse <- pmap_dbl(list(reg_tuning_avg_aplus_df$max_depth, reg_tuning_avg_aplus_df$weight,
                                              reg_tuning_avg_aplus_df$subsample, reg_tuning_avg_aplus_df$row_num),
                                         hyperparam_avg_aplus_tuning_reg, .progress = TRUE)

reg_tuning_avg_aplus_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_avg_aplus_best <- reg_tuning_avg_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_avg_aplus <- reg_tuning_avg_aplus_best$max_depth # 4
weight_avg_aplus <- reg_tuning_avg_aplus_best$weight # 10
subsample_avg_aplus <- reg_tuning_avg_aplus_best$subsample # 0.8

reg_tuning_avg_aplus_best$rmse/sd(avg_aplus$AVG_Aplus) #0.922

hyperparam_avg_aplus_tuning_reg(4,10,0.8,1) # 4296 trees

# for Importance Plot for BART comparison
set.seed(101);avg_aplus_mod_xg <- xgboost(
  params = list(
    eta = 0.01,
    objective = 'reg:squarederror',
    eval_metric = 'rmse',
    alpha = 5,
    max_depth = max_depth_avg_aplus,
    min_child_weight = weight_avg_aplus,
    subsample = subsample_avg_aplus
  ),
  data = dtrain_avg_aplus,
  nrounds = 4296,
  print_every_n = 100,
  nthread = 7
)

avg_aplus_mod_xgimportance <- xgb.importance(model = avg_aplus_mod_xg) %>% 
  arrange(desc(Frequency))

avg_aplus_mod_xgimportance$Feature <- factor(avg_aplus_mod_xgimportance$Feature, levels = rev(avg_aplus_mod_xgimportance$Feature))

avg_aplus_mod_xgimportance %>% 
  ggplot(aes(Frequency, Feature)) +
  geom_col(fill = 'red') +
  theme_classic() +
  labs(x = 'Importance (Frequency)',
       title = 'XGBoost AVG (A to A+) Importance Plot')
  

shap.plot.summary.wrap1(avg_aplus_mod_xg, as.matrix(avg_aplus %>% select(-AVG_Aplus)))

### Avg A+ to AA ####
avg_AA <- minor_league_hitting_AA %>% 
  drop_na(Name_Aplus) %>% 
  rowwise() %>% 
  reframe(
    AVG_AA,
    top_100_rank = mean(c(rank_ba_Aplus, rank_mlb_Aplus)),
    team_rank = team_rank_ba_Aplus,
    Age_Aplus,
    H_Aplus_adj = H_Aplus/(AB_Aplus^0.8),
    GB_per_FB_Aplus,
    LD_pct_Aplus,
    wRC_plus_Aplus,
    BB_minus_K_pct_Aplus = BB_pct_Aplus - K_pct_Aplus,
    SwStr_pct_Aplus,
    Pull_pct_Aplus,
  ) %>% 
  ungroup()

dtrain_avg_aa <- xgb.DMatrix(as.matrix(avg_AA %>% select(-AVG_AA)), label = avg_AA$AVG_AA)


hyperparam_avg_aa_tuning_reg <- function(max_depth_avg_aplus, weight_avg_aplus,subsample_avg_aplus, row_num_avg_aplus){
  
  print(paste('Max Depth: ', max_depth_avg_aplus))
  print(paste('Weight: ', weight_avg_aplus))
  print(paste('Subsample: ', subsample_avg_aplus))
  print(paste('Row Number: ', row_num_avg_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_avg_aplus,
      min_child_weight = weight_avg_aplus,
      subsample = subsample_avg_aplus
    ),
    data = dtrain_avg_aa,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_avg_aa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_avg_aa_df$rmse <- pmap_dbl(list(reg_tuning_avg_aa_df$max_depth, reg_tuning_avg_aa_df$weight,
                                              reg_tuning_avg_aa_df$subsample, reg_tuning_avg_aa_df$row_num),
                                         hyperparam_avg_aa_tuning_reg, .progress = TRUE)

reg_tuning_avg_aa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_avg_aa_best <- reg_tuning_avg_aa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_avg_aa <- reg_tuning_avg_aa_best$max_depth # 4
weight_avg_aa <- reg_tuning_avg_aa_best$weight # 10
subsample_avg_aa <- reg_tuning_avg_aa_best$subsample # 0.8

reg_tuning_avg_aa_best$rmse/sd(avg_AA$AVG_AA) #0.914

### AVG AA to AAA ####
avg_AAA <- minor_league_hitting_AAA %>% 
  drop_na(Name_AA) %>% 
  rowwise() %>% 
  reframe(
    AVG_AAA,
    top_100_rank = mean(c(rank_ba_AA, rank_mlb_AA)),
    team_rank = team_rank_ba_AA,
    Age_AA,
    H_AA_adj = H_AA/(AB_AA^0.8),
    GB_per_FB_AA,
    LD_pct_AA,
    wRC_plus_AA,
    BB_minus_K_pct_AA = BB_pct_AA - K_pct_AA,
    SwStr_pct_AA,
    Pull_pct_AA
  ) %>% 
  ungroup()

dtrain_avg_aaa <- xgb.DMatrix(as.matrix(avg_AAA %>% select(-AVG_AAA)), label = avg_AAA$AVG_AAA)


hyperparam_avg_aaa_tuning_reg <- function(max_depth_avg_aplus, weight_avg_aplus,subsample_avg_aplus, row_num_avg_aplus){
  
  print(paste('Max Depth: ', max_depth_avg_aplus))
  print(paste('Weight: ', weight_avg_aplus))
  print(paste('Subsample: ', subsample_avg_aplus))
  print(paste('Row Number: ', row_num_avg_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_avg_aplus,
      min_child_weight = weight_avg_aplus,
      subsample = subsample_avg_aplus
    ),
    data = dtrain_avg_aaa,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_avg_aaa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_avg_aaa_df$rmse <- pmap_dbl(list(reg_tuning_avg_aaa_df$max_depth, reg_tuning_avg_aaa_df$weight,
                                           reg_tuning_avg_aaa_df$subsample, reg_tuning_avg_aaa_df$row_num),
                                      hyperparam_avg_aaa_tuning_reg, .progress = TRUE)

reg_tuning_avg_aaa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_avg_aaa_best <- reg_tuning_avg_aaa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_avg_aaa <- reg_tuning_avg_aaa_best$max_depth # 4
weight_avg_aaa <- reg_tuning_avg_aaa_best$weight # 10
subsample_avg_aaa <- reg_tuning_avg_aaa_best$subsample # 0.8

reg_tuning_avg_aaa_best$rmse/sd(avg_AAA$AVG_AAA) #0.928


### AVG AAA to MLB ####
avg_mlb <- mlb_hitting %>% 
  drop_na(Name_AAA) %>% 
  rowwise() %>% 
  reframe(
    AVG_MLB,
    top_100_rank = mean(c(rank_ba, rank_mlb)),
    team_rank = team_rank_ba,
    Age_AAA,
    AVG_AAA_adj = H_AAA/(AB_AAA^0.8),
    GB_per_FB_AAA,
    LD_pct_AAA,
    wRC_plus_AAA,
    BB_minus_K_pct_AAA = K_pct_AAA - BB_pct_AAA,
    SwStr_pct_AAA,
    Pull_pct_AAA
  ) %>% 
  ungroup()


dtrain_avg_mlb <- xgb.DMatrix(as.matrix(avg_mlb %>% select(-AVG_MLB)), label = avg_mlb$AVG_MLB)


hyperparam_avg_mlb_tuning_reg <- function(max_depth_avg_aplus, weight_avg_aplus,subsample_avg_aplus, row_num_avg_aplus){
  
  print(paste('Max Depth: ', max_depth_avg_aplus))
  print(paste('Weight: ', weight_avg_aplus))
  print(paste('Subsample: ', subsample_avg_aplus))
  print(paste('Row Number: ', row_num_avg_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_avg_aplus,
      min_child_weight = weight_avg_aplus,
      subsample = subsample_avg_aplus
    ),
    data = dtrain_avg_mlb,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_avg_mlb_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_avg_mlb_df$rmse <- pmap_dbl(list(reg_tuning_avg_mlb_df$max_depth, reg_tuning_avg_mlb_df$weight,
                                            reg_tuning_avg_mlb_df$subsample, reg_tuning_avg_mlb_df$row_num),
                                       hyperparam_avg_mlb_tuning_reg, .progress = TRUE)

reg_tuning_avg_mlb_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_avg_mlb_best <- reg_tuning_avg_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_avg_mlb <- reg_tuning_avg_mlb_best$max_depth # 6
weight_avg_mlb <- reg_tuning_avg_mlb_best$weight # 1
subsample_avg_mlb <- reg_tuning_avg_mlb_best$subsample # 1

reg_tuning_avg_mlb_best$rmse/sd(avg_mlb$AVG_MLB) #0.928 #XGBoost Better


### OBP A to A+ ####
obp_aplus <- minor_league_hitting_Aplus %>% 
  drop_na(Name_A) %>% 
  rowwise() %>% 
  reframe(
    OBP_Aplus,
    top_100_rank = mean(c(rank_ba_A, rank_mlb_A)),
    team_rank = team_rank_ba_A,
    Age_A,
    on_base_A_adj = (H_A + BB_A)/(PA_A^0.8),
    wRC_plus_A,
    BB_minus_K_pct_A = BB_pct_A - K_pct_A,
    SwStr_pct_A
  ) %>% 
  ungroup()

dtrain_obp_aplus <- xgb.DMatrix(as.matrix(obp_aplus %>% select(-OBP_Aplus)), label = obp_aplus$OBP_Aplus)


hyperparam_obp_aplus_tuning_reg <- function(max_depth_obp_aplus, weight_obp_aplus,subsample_obp_aplus, row_num_obp_aplus){
  
  print(paste('Max Depth: ', max_depth_obp_aplus))
  print(paste('Weight: ', weight_obp_aplus))
  print(paste('Subsample: ', subsample_obp_aplus))
  print(paste('Row Number: ', row_num_obp_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_obp_aplus,
      min_child_weight = weight_obp_aplus,
      subsample = subsample_obp_aplus
    ),
    data = dtrain_obp_aplus,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_obp_aplus_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_obp_aplus_df$rmse <- pmap_dbl(list(reg_tuning_obp_aplus_df$max_depth, reg_tuning_obp_aplus_df$weight,
                                              reg_tuning_obp_aplus_df$subsample, reg_tuning_obp_aplus_df$row_num),
                                         hyperparam_obp_aplus_tuning_reg, .progress = TRUE)

reg_tuning_obp_aplus_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_obp_aplus_best <- reg_tuning_obp_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_obp_aplus <- reg_tuning_obp_aplus_best$max_depth # 4
weight_obp_aplus <- reg_tuning_obp_aplus_best$weight # 10
subsample_obp_aplus <- reg_tuning_obp_aplus_best$subsample # 0.8

reg_tuning_obp_aplus_best$rmse/sd(obp_aplus$OBP_Aplus) #0.906




### OBP A+ to AA ####
obp_AA <- minor_league_hitting_AA %>% 
  drop_na(Name_Aplus) %>% 
  rowwise() %>% 
  reframe(
    OBP_AA,
    top_100_rank = mean(c(rank_ba_Aplus, rank_mlb_Aplus)),
    team_rank = team_rank_ba_Aplus,
    Age_Aplus,
    on_base_Aplus_adj = (H_Aplus + BB_Aplus)/(PA_Aplus^0.8),
    GB_per_FB_Aplus,
    LD_pct_Aplus,
    wRC_plus_Aplus,
    BB_minus_K_pct_Aplus = BB_pct_Aplus - K_pct_Aplus,
    SwStr_pct_Aplus
  ) %>% 
  ungroup()

dtrain_obp_aa <- xgb.DMatrix(as.matrix(obp_AA %>% select(-OBP_AA)), label = obp_AA$OBP_AA)


hyperparam_obp_aa_tuning_reg <- function(max_depth_obp_aplus, weight_obp_aplus,subsample_obp_aplus, row_num_obp_aplus){
  
  print(paste('Max Depth: ', max_depth_obp_aplus))
  print(paste('Weight: ', weight_obp_aplus))
  print(paste('Subsample: ', subsample_obp_aplus))
  print(paste('Row Number: ', row_num_obp_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_obp_aplus,
      min_child_weight = weight_obp_aplus,
      subsample = subsample_obp_aplus
    ),
    data = dtrain_obp_aa,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_obp_aa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_obp_aa_df$rmse <- pmap_dbl(list(reg_tuning_obp_aa_df$max_depth, reg_tuning_obp_aa_df$weight,
                                           reg_tuning_obp_aa_df$subsample, reg_tuning_obp_aa_df$row_num),
                                      hyperparam_obp_aa_tuning_reg, .progress = TRUE)

reg_tuning_obp_aa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_obp_aa_best <- reg_tuning_obp_aa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_obp_aa <- reg_tuning_obp_aa_best$max_depth # 4
weight_obp_aa <- reg_tuning_obp_aa_best$weight # 10
subsample_obp_aa <- reg_tuning_obp_aa_best$subsample # 1

reg_tuning_obp_aa_best$rmse/sd(obp_AA$OBP_AA) #0.900

### OBP AA to AAA ####
obp_AAA <- minor_league_hitting_AAA %>% 
  drop_na(Name_AA) %>% 
  rowwise() %>% 
  reframe(
    OBP_AAA,
    top_100_rank = mean(c(rank_ba_AA, rank_mlb_AA)),
    team_rank = team_rank_ba_AA,
    Age_AA,
    on_base_AA = (H_AA + BB_AA)/(PA_AA^0.8),
    GB_per_FB_AA,
    LD_pct_AA,
    wRC_plus_AA,
    BB_minus_K_pct_AA = BB_pct_AA - K_pct_AA,
    SwStr_pct_AA
  ) %>% 
  ungroup()
dtrain_obp_aaa <- xgb.DMatrix(as.matrix(obp_AAA %>% select(-OBP_AAA)), label = obp_AAA$OBP_AAA)


hyperparam_obp_aaa_tuning_reg <- function(max_depth_obp_aplus, weight_obp_aplus,subsample_obp_aplus, row_num_obp_aplus){
  
  print(paste('Max Depth: ', max_depth_obp_aplus))
  print(paste('Weight: ', weight_obp_aplus))
  print(paste('Subsample: ', subsample_obp_aplus))
  print(paste('Row Number: ', row_num_obp_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_obp_aplus,
      min_child_weight = weight_obp_aplus,
      subsample = subsample_obp_aplus
    ),
    data = dtrain_obp_aaa,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_obp_aaa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_obp_aaa_df$rmse <- pmap_dbl(list(reg_tuning_obp_aaa_df$max_depth, reg_tuning_obp_aaa_df$weight,
                                            reg_tuning_obp_aaa_df$subsample, reg_tuning_obp_aaa_df$row_num),
                                       hyperparam_obp_aaa_tuning_reg, .progress = TRUE)

reg_tuning_obp_aaa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_obp_aaa_best <- reg_tuning_obp_aaa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_obp_aaa <- reg_tuning_obp_aaa_best$max_depth # 4
weight_obp_aaa <- reg_tuning_obp_aaa_best$weight # 10
subsample_obp_aaa <- reg_tuning_obp_aaa_best$subsample # 1

reg_tuning_obp_aaa_best$rmse/sd(obp_AAA$OBP_AAA) #0.915


### OBP AAA to MLB ####
obp_mlb <- mlb_hitting %>% 
  drop_na(Name_AAA) %>% 
  rowwise() %>% 
  reframe(
    OBP_MLB,
    top_100_rank = mean(c(rank_ba, rank_mlb)),
    team_rank = team_rank_ba,
    on_bases_AAA_adj = (H_AAA + BB_AAA)/(PA_AAA^0.8),
    GB_per_FB_AAA,
    LD_pct_AAA,
    wRC_plus_AAA,
    BB_minus_K_pct_AAA = BB_pct_AAA - K_pct_AAA,
    SwStr_pct_AAA
  ) %>% 
  ungroup()


dtrain_obp_mlb <- xgb.DMatrix(as.matrix(obp_mlb %>% select(-OBP_MLB)), label = obp_mlb$OBP_MLB)


hyperparam_obp_mlb_tuning_reg <- function(max_depth_obp_aplus, weight_obp_aplus,subsample_obp_aplus, row_num_obp_aplus){
  
  print(paste('Max Depth: ', max_depth_obp_aplus))
  print(paste('Weight: ', weight_obp_aplus))
  print(paste('Subsample: ', subsample_obp_aplus))
  print(paste('Row Number: ', row_num_obp_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_obp_aplus,
      min_child_weight = weight_obp_aplus,
      subsample = subsample_obp_aplus
    ),
    data = dtrain_obp_mlb,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_obp_mlb_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_obp_mlb_df$rmse <- pmap_dbl(list(reg_tuning_obp_mlb_df$max_depth, reg_tuning_obp_mlb_df$weight,
                                            reg_tuning_obp_mlb_df$subsample, reg_tuning_obp_mlb_df$row_num),
                                       hyperparam_obp_mlb_tuning_reg, .progress = TRUE)

reg_tuning_obp_mlb_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_obp_mlb_best <- reg_tuning_obp_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_obp_mlb <- reg_tuning_obp_mlb_best$max_depth # 8
weight_obp_mlb <- reg_tuning_obp_mlb_best$weight # 10
subsample_obp_mlb <- reg_tuning_obp_mlb_best$subsample # 1

reg_tuning_obp_mlb_best$rmse/sd(obp_mlb$OBP_MLB) #0.984



### ISO A to A+ ####
iso_aplus <- minor_league_hitting_Aplus %>% 
  drop_na(Name_A) %>% 
  rowwise() %>% 
  mutate(TB_A = x1B_A + 2*x2B_A + 3*x3B_A + 4*HR_A) %>% 
  reframe(
    ISO_Aplus,
    top_100_rank = mean(c(rank_ba_A, rank_mlb_A)),
    team_rank = team_rank_ba_A,
    Age_A,
    iso_A_adj = (TB_A-H_A)/(AB_A^0.8),
    wRC_plus_A,
    BB_minus_K_pct_A = BB_pct_A - K_pct_A,
    SwStr_pct_A,
    HR_per_FB_A
  ) %>% 
  ungroup()


dtrain_iso_aplus <- xgb.DMatrix(as.matrix(iso_aplus %>% select(-ISO_Aplus)), label = iso_aplus$ISO_Aplus)


hyperparam_iso_aplus_tuning_reg <- function(max_depth_iso_aplus, weight_iso_aplus,subsample_iso_aplus, row_num_iso_aplus){
  
  print(paste('Max Depth: ', max_depth_iso_aplus))
  print(paste('Weight: ', weight_iso_aplus))
  print(paste('Subsample: ', subsample_iso_aplus))
  print(paste('Row Number: ', row_num_iso_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_iso_aplus,
      min_child_weight = weight_iso_aplus,
      subsample = subsample_iso_aplus
    ),
    data = dtrain_iso_aplus,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_iso_aplus_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_iso_aplus_df$rmse <- pmap_dbl(list(reg_tuning_iso_aplus_df$max_depth, reg_tuning_iso_aplus_df$weight,
                                              reg_tuning_iso_aplus_df$subsample, reg_tuning_iso_aplus_df$row_num),
                                         hyperparam_iso_aplus_tuning_reg, .progress = TRUE)

reg_tuning_iso_aplus_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_iso_aplus_best <- reg_tuning_iso_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_iso_aplus <- reg_tuning_iso_aplus_best$max_depth # 4
weight_iso_aplus <- reg_tuning_iso_aplus_best$weight # 10
subsample_iso_aplus <- reg_tuning_iso_aplus_best$subsample # 0.8

reg_tuning_iso_aplus_best$rmse/sd(iso_aplus$ISO_Aplus) #0.876




### ISO A+ to AA ####
iso_AA <- minor_league_hitting_AA %>% 
  drop_na(Name_Aplus) %>%
  mutate(TB_Aplus = x1B_Aplus + 2*x2B_Aplus + 3*x3B_Aplus + 4*HR_Aplus) %>% 
  rowwise() %>% 
  reframe(
    ISO_AA,
    top_100_rank = mean(c(rank_ba_Aplus, rank_mlb_Aplus)),
    team_rank = team_rank_ba_Aplus,
    Age_Aplus,
    iso_Aplus_adj = (TB_Aplus - H_Aplus)/(AB_Aplus^0.8),
    GB_per_FB_Aplus,
    LD_pct_Aplus,
    wRC_plus_Aplus,
    BB_minus_K_pct_Aplus = BB_pct_Aplus - K_pct_Aplus,
    SwStr_pct_Aplus
  ) %>% 
  ungroup()

dtrain_iso_aa <- xgb.DMatrix(as.matrix(iso_AA %>% select(-ISO_AA)), label = iso_AA$ISO_AA)


hyperparam_iso_aa_tuning_reg <- function(max_depth_iso_aplus, weight_iso_aplus,subsample_iso_aplus, row_num_iso_aplus){
  
  print(paste('Max Depth: ', max_depth_iso_aplus))
  print(paste('Weight: ', weight_iso_aplus))
  print(paste('Subsample: ', subsample_iso_aplus))
  print(paste('Row Number: ', row_num_iso_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_iso_aplus,
      min_child_weight = weight_iso_aplus,
      subsample = subsample_iso_aplus
    ),
    data = dtrain_iso_aa,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_iso_aa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_iso_aa_df$rmse <- pmap_dbl(list(reg_tuning_iso_aa_df$max_depth, reg_tuning_iso_aa_df$weight,
                                           reg_tuning_iso_aa_df$subsample, reg_tuning_iso_aa_df$row_num),
                                      hyperparam_iso_aa_tuning_reg, .progress = TRUE)

reg_tuning_iso_aa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_iso_aa_best <- reg_tuning_iso_aa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_iso_aa <- reg_tuning_iso_aa_best$max_depth # 6
weight_iso_aa <- reg_tuning_iso_aa_best$weight # 10
subsample_iso_aa <- reg_tuning_iso_aa_best$subsample # 1

reg_tuning_iso_aa_best$rmse/sd(iso_AA$ISO_AA) #0.840

### ISO AA to AAA ####
iso_AAA <- minor_league_hitting_AAA %>% 
  drop_na(Name_AA) %>%
  mutate(TB_AA = x1B_AA + 2*x2B_AA + 3*x3B_AA + 4*HR_AA) %>% 
  rowwise() %>% 
  reframe(
    ISO_AAA,
    top_100_rank = mean(c(rank_ba_AA, rank_mlb_AA)),
    team_rank = team_rank_ba_AA,
    Age_AA,
    iso_base_AA = (TB_AA - H_AA)/(AB_AA^0.8),
    GB_per_FB_AA,
    LD_pct_AA,
    wRC_plus_AA,
    BB_minus_K_pct_AA = BB_pct_AA - K_pct_AA,
    SwStr_pct_AA
  ) %>% 
  ungroup()

dtrain_iso_aaa <- xgb.DMatrix(as.matrix(iso_AAA %>% select(-ISO_AAA)), label = iso_AAA$ISO_AAA)


hyperparam_iso_aaa_tuning_reg <- function(max_depth_iso_aplus, weight_iso_aplus,subsample_iso_aplus, row_num_iso_aplus){
  
  print(paste('Max Depth: ', max_depth_iso_aplus))
  print(paste('Weight: ', weight_iso_aplus))
  print(paste('Subsample: ', subsample_iso_aplus))
  print(paste('Row Number: ', row_num_iso_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_iso_aplus,
      min_child_weight = weight_iso_aplus,
      subsample = subsample_iso_aplus
    ),
    data = dtrain_iso_aaa,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_iso_aaa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_iso_aaa_df$rmse <- pmap_dbl(list(reg_tuning_iso_aaa_df$max_depth, reg_tuning_iso_aaa_df$weight,
                                            reg_tuning_iso_aaa_df$subsample, reg_tuning_iso_aaa_df$row_num),
                                       hyperparam_iso_aaa_tuning_reg, .progress = TRUE)

reg_tuning_iso_aaa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_iso_aaa_best <- reg_tuning_iso_aaa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_iso_aaa <- reg_tuning_iso_aaa_best$max_depth # 4
weight_iso_aaa <- reg_tuning_iso_aaa_best$weight # 10
subsample_iso_aaa <- reg_tuning_iso_aaa_best$subsample # 1

reg_tuning_iso_aaa_best$rmse/sd(iso_AAA$ISO_AAA) #0.818


### ISO AAA to MLB ####
iso_mlb <- mlb_hitting %>% 
  drop_na(Name_AAA) %>% 
  rowwise() %>% 
  mutate(TB_AAA = x1B_AAA + 2*x2B_AAA + 3*x3B_AAA + 4*HR_AAA) %>% 
  reframe(
    ISO_MLB,
    top_100_rank = mean(c(rank_ba, rank_mlb)),
    team_rank = team_rank_ba,
    on_bases_AAA_adj = (TB_AAA - H_AAA)/(AB_AAA^0.8),
    GB_per_FB_AAA,
    LD_pct_AAA,
    wRC_plus_AAA,
    BB_minus_K_pct_AAA = BB_pct_AAA - K_pct_AAA,
    SwStr_pct_AAA
  ) %>% 
  ungroup()


dtrain_iso_mlb <- xgb.DMatrix(as.matrix(iso_mlb %>% select(-ISO_MLB)), label = iso_mlb$ISO_MLB)


hyperparam_iso_mlb_tuning_reg <- function(max_depth_iso_aplus, weight_iso_aplus,subsample_iso_aplus, row_num_iso_aplus){
  
  print(paste('Max Depth: ', max_depth_iso_aplus))
  print(paste('Weight: ', weight_iso_aplus))
  print(paste('Subsample: ', subsample_iso_aplus))
  print(paste('Row Number: ', row_num_iso_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_iso_aplus,
      min_child_weight = weight_iso_aplus,
      subsample = subsample_iso_aplus
    ),
    data = dtrain_iso_mlb,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_iso_mlb_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_iso_mlb_df$rmse <- pmap_dbl(list(reg_tuning_iso_mlb_df$max_depth, reg_tuning_iso_mlb_df$weight,
                                            reg_tuning_iso_mlb_df$subsample, reg_tuning_iso_mlb_df$row_num),
                                       hyperparam_iso_mlb_tuning_reg, .progress = TRUE)

reg_tuning_iso_mlb_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_iso_mlb_best <- reg_tuning_iso_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_iso_mlb <- reg_tuning_iso_mlb_best$max_depth # 8
weight_iso_mlb <- reg_tuning_iso_mlb_best$weight # 10
subsample_iso_mlb <- reg_tuning_iso_mlb_best$subsample # 1

reg_tuning_iso_mlb_best$rmse/sd(iso_mlb$ISO_MLB) #0.910



### wRC+ A to A+ ####
wrcplus_aplus <- minor_league_hitting_Aplus %>% 
  drop_na(Name_A) %>% 
  rowwise() %>% 
  reframe(
    wRC_plus_Aplus,
    top_100_rank = mean(c(rank_ba_A, rank_mlb_A)),
    team_rank = team_rank_ba_A,
    Age_A,
    wRAA_A_adj = wRAA_A/(PA_A^0.8),
    BB_minus_K_pct_A = BB_pct_A - K_pct_A,
    Pull_pct_A,
    Oppo_pct_A,
    GB_pct_A,
    LD_pct_A,
    BABIP_A,
    SwStr_pct_A
  ) %>% 
  ungroup()

dtrain_wrcplus_aplus <- xgb.DMatrix(as.matrix(wrcplus_aplus %>% select(-wRC_plus_Aplus)), label = wrcplus_aplus$wRC_plus_Aplus)


hyperparam_wrcplus_aplus_tuning_reg <- function(max_depth_wrcplus_aplus, weight_wrcplus_aplus,subsample_wrcplus_aplus, row_num_wrcplus_aplus){
  
  print(paste('Max Depth: ', max_depth_wrcplus_aplus))
  print(paste('Weight: ', weight_wrcplus_aplus))
  print(paste('Subsample: ', subsample_wrcplus_aplus))
  print(paste('Row Number: ', row_num_wrcplus_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 10,
      gamma = 10,
      lambda = 10,
      max_depth = max_depth_wrcplus_aplus,
      min_child_weight = weight_wrcplus_aplus,
      subsample = subsample_wrcplus_aplus
    ),
    data = dtrain_wrcplus_aplus,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_wrcplus_aplus_df <- expand_grid(
  max_depth = c(4,6,8),
  weight = c(10,20,30),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_wrcplus_aplus_df$rmse <- pmap_dbl(list(reg_tuning_wrcplus_aplus_df$max_depth, reg_tuning_wrcplus_aplus_df$weight,
                                              reg_tuning_wrcplus_aplus_df$subsample, reg_tuning_wrcplus_aplus_df$row_num),
                                         hyperparam_wrcplus_aplus_tuning_reg, .progress = TRUE)

reg_tuning_wrcplus_aplus_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_wrcplus_aplus_best <- reg_tuning_wrcplus_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_wrcplus_aplus <- reg_tuning_wrcplus_aplus_best$max_depth # 4
weight_wrcplus_aplus <- reg_tuning_wrcplus_aplus_best$weight # 30
subsample_wrcplus_aplus <- reg_tuning_wrcplus_aplus_best$subsample # 0.6

reg_tuning_wrcplus_aplus_best$rmse/sd(wrcplus_aplus$wRC_plus_Aplus) #0.898




### wRC+ A+ to AA ####
wrcplus_AA <- minor_league_hitting_AA %>% 
  drop_na(Name_Aplus) %>%  
  rowwise() %>% 
  reframe(
    wRC_plus_AA,
    top_100_rank = mean(c(rank_ba_Aplus, rank_mlb_Aplus)),
    team_rank = team_rank_ba_Aplus,
    Age_Aplus,
    wRAA_Aplus_adj = wRAA_Aplus/(PA_Aplus^0.8),
    BB_minus_K_pct_Aplus = BB_pct_Aplus - K_pct_Aplus,
    Pull_pct_Aplus,
    Oppo_pct_Aplus,
    GB_pct_Aplus,
    LD_pct_Aplus,
    BABIP_Aplus,
    SwStr_pct_Aplus
  ) %>% 
  ungroup()

dtrain_wrcplus_aa <- xgb.DMatrix(as.matrix(wrcplus_AA %>% select(-wRC_plus_AA)), label = wrcplus_AA$wRC_plus_AA)


hyperparam_wrcplus_aa_tuning_reg <- function(max_depth_wrcplus_aplus, weight_wrcplus_aplus,subsample_wrcplus_aplus, row_num_wrcplus_aplus){
  
  print(paste('Max Depth: ', max_depth_wrcplus_aplus))
  print(paste('Weight: ', weight_wrcplus_aplus))
  print(paste('Subsample: ', subsample_wrcplus_aplus))
  print(paste('Row Number: ', row_num_wrcplus_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 10,
      gamma = 10,
      lambda = 10,
      max_depth = max_depth_wrcplus_aplus,
      min_child_weight = weight_wrcplus_aplus,
      subsample = subsample_wrcplus_aplus
    ),
    data = dtrain_wrcplus_aa,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_wrcplus_aa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(10,20,30),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_wrcplus_aa_df$rmse <- pmap_dbl(list(reg_tuning_wrcplus_aa_df$max_depth, reg_tuning_wrcplus_aa_df$weight,
                                           reg_tuning_wrcplus_aa_df$subsample, reg_tuning_wrcplus_aa_df$row_num),
                                      hyperparam_wrcplus_aa_tuning_reg, .progress = TRUE)

reg_tuning_wrcplus_aa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_wrcplus_aa_best <- reg_tuning_wrcplus_aa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_wrcplus_aa <- reg_tuning_wrcplus_aa_best$max_depth # 4
weight_wrcplus_aa <- reg_tuning_wrcplus_aa_best$weight # 30
subsample_wrcplus_aa <- reg_tuning_wrcplus_aa_best$subsample # 0.6

reg_tuning_wrcplus_aa_best$rmse/sd(wrcplus_AA$wRC_plus_AA) #0.873

### wRC+ AA to AAA ####
wrcplus_AAA <- minor_league_hitting_AAA %>% 
  drop_na(Name_AA) %>%
  rowwise() %>% 
  reframe(
    wRC_plus_AAA,
    top_100_rank = mean(c(rank_ba_AA, rank_mlb_AA)),
    team_rank = team_rank_ba_AA,
    Age_AA,
    wRAA_AA_adj = wRAA_AA/(PA_AA^0.8),
    BB_minus_K_pct_AA = BB_pct_AA - K_pct_AA,
    Pull_pct_AA,
    Oppo_pct_AA,
    GB_pct_AA,
    LD_pct_AA,
    BABIP_AA,
    SwStr_pct_AA
  ) %>% 
  ungroup()


dtrain_wrcplus_aaa <- xgb.DMatrix(as.matrix(wrcplus_AAA %>% select(-wRC_plus_AAA)), label = wrcplus_AAA$wRC_plus_AAA)


hyperparam_wrcplus_aaa_tuning_reg <- function(max_depth_wrcplus_aplus, weight_wrcplus_aplus,subsample_wrcplus_aplus, row_num_wrcplus_aplus){
  
  print(paste('Max Depth: ', max_depth_wrcplus_aplus))
  print(paste('Weight: ', weight_wrcplus_aplus))
  print(paste('Subsample: ', subsample_wrcplus_aplus))
  print(paste('Row Number: ', row_num_wrcplus_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 10,
      gamma = 10,
      lambda = 10,
      max_depth = max_depth_wrcplus_aplus,
      min_child_weight = weight_wrcplus_aplus,
      subsample = subsample_wrcplus_aplus
    ),
    data = dtrain_wrcplus_aaa,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_wrcplus_aaa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(10,20,30),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_wrcplus_aaa_df$rmse <- pmap_dbl(list(reg_tuning_wrcplus_aaa_df$max_depth, reg_tuning_wrcplus_aaa_df$weight,
                                            reg_tuning_wrcplus_aaa_df$subsample, reg_tuning_wrcplus_aaa_df$row_num),
                                       hyperparam_wrcplus_aaa_tuning_reg, .progress = TRUE)

reg_tuning_wrcplus_aaa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_wrcplus_aaa_best <- reg_tuning_wrcplus_aaa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_wrcplus_aaa <- reg_tuning_wrcplus_aaa_best$max_depth # 4
weight_wrcplus_aaa <- reg_tuning_wrcplus_aaa_best$weight # 30
subsample_wrcplus_aaa <- reg_tuning_wrcplus_aaa_best$subsample # 0.6

reg_tuning_wrcplus_aaa_best$rmse/sd(wrcplus_AAA$wRC_plus_AAA) #0.875


### wRC+ AAA to MLB ####
wrcplus_mlb <- mlb_hitting %>% 
  drop_na(Name_AAA) %>% 
  rowwise() %>% 
  reframe(
    wRC_plus_MLB,
    top_100_rank = mean(c(rank_ba, rank_mlb)),
    team_rank = team_rank_ba,
    Age_AAA,
    wRAA_AAA_adj = wRAA_AAA/(PA_AAA^0.8),
    BB_minus_K_pct_AAA = BB_pct_AAA - K_pct_AAA,
    Pull_pct_AAA,
    Oppo_pct_AAA,
    GB_pct_AAA,
    LD_pct_AAA,
    BABIP_AAA,
    SwStr_pct_AAA
  ) %>% 
  ungroup()


dtrain_wrcplus_mlb <- xgb.DMatrix(as.matrix(wrcplus_mlb %>% select(-wRC_plus_MLB)), label = wrcplus_mlb$wRC_plus_MLB)


hyperparam_wrcplus_mlb_tuning_reg <- function(max_depth_wrcplus_aplus, weight_wrcplus_aplus,subsample_wrcplus_aplus, row_num_wrcplus_aplus){
  
  print(paste('Max Depth: ', max_depth_wrcplus_aplus))
  print(paste('Weight: ', weight_wrcplus_aplus))
  print(paste('Subsample: ', subsample_wrcplus_aplus))
  print(paste('Row Number: ', row_num_wrcplus_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 10,
      lambda = 10,
      gamma = 10,
      max_depth = max_depth_wrcplus_aplus,
      min_child_weight = weight_wrcplus_aplus,
      subsample = subsample_wrcplus_aplus
    ),
    data = dtrain_wrcplus_mlb,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_wrcplus_mlb_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(10,20,30),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_wrcplus_mlb_df$rmse <- pmap_dbl(list(reg_tuning_wrcplus_mlb_df$max_depth, reg_tuning_wrcplus_mlb_df$weight,
                                            reg_tuning_wrcplus_mlb_df$subsample, reg_tuning_wrcplus_mlb_df$row_num),
                                       hyperparam_wrcplus_mlb_tuning_reg, .progress = TRUE)

reg_tuning_wrcplus_mlb_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_wrcplus_mlb_best <- reg_tuning_wrcplus_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_wrcplus_mlb <- reg_tuning_wrcplus_mlb_best$max_depth # 4
weight_wrcplus_mlb <- reg_tuning_wrcplus_mlb_best$weight # 30
subsample_wrcplus_mlb <- reg_tuning_wrcplus_mlb_best$subsample # 0.6

reg_tuning_wrcplus_mlb_best$rmse/sd(wrcplus_mlb$wRC_plus_MLB) #0.954



### K% A to A+ ####
kpct_aplus <- minor_league_hitting_Aplus %>% 
  drop_na(Name_A) %>% 
  rowwise() %>% 
  reframe(
    K_pct_Aplus,
    top_100_rank = mean(c(rank_ba_A, rank_mlb_A)),
    team_rank = team_rank_ba_A,
    Age_A,
    kpct_A_adj = SO_A/(PA_A^0.6),
    K_pct_A,
    wRC_plus_A,
    SwStr_pct_A
  ) %>% 
  ungroup()


dtrain_kpct_aplus <- xgb.DMatrix(as.matrix(kpct_aplus %>% select(-K_pct_Aplus)), label = kpct_aplus$K_pct_Aplus)


hyperparam_kpct_aplus_tuning_reg <- function(max_depth_kpct_aplus, weight_kpct_aplus,subsample_kpct_aplus, row_num_kpct_aplus){
  
  print(paste('Max Depth: ', max_depth_kpct_aplus))
  print(paste('Weight: ', weight_kpct_aplus))
  print(paste('Subsample: ', subsample_kpct_aplus))
  print(paste('Row Number: ', row_num_kpct_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_kpct_aplus,
      min_child_weight = weight_kpct_aplus,
      subsample = subsample_kpct_aplus
    ),
    data = dtrain_kpct_aplus,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_kpct_aplus_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_kpct_aplus_df$rmse <- pmap_dbl(list(reg_tuning_kpct_aplus_df$max_depth, reg_tuning_kpct_aplus_df$weight,
                                              reg_tuning_kpct_aplus_df$subsample, reg_tuning_kpct_aplus_df$row_num),
                                         hyperparam_kpct_aplus_tuning_reg, .progress = TRUE)

reg_tuning_kpct_aplus_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_kpct_aplus_best <- reg_tuning_kpct_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_kpct_aplus <- reg_tuning_kpct_aplus_best$max_depth # 4
weight_kpct_aplus <- reg_tuning_kpct_aplus_best$weight # 10
subsample_kpct_aplus <- reg_tuning_kpct_aplus_best$subsample # 0.8

reg_tuning_kpct_aplus_best$rmse/sd(kpct_aplus$K_pct_Aplus) #0.695




### K% A+ to AA ####
kpct_AA <- minor_league_hitting_AA %>% 
  drop_na(Name_Aplus) %>%  
  rowwise() %>% 
  reframe(
    K_pct_AA,
    top_100_rank = mean(c(rank_ba_Aplus, rank_mlb_Aplus)),
    team_rank = team_rank_ba_Aplus,
    Age_Aplus,
    kpct_Aplus_adj = SO_Aplus/(PA_Aplus^0.6),
    K_pct_Aplus,
    wRC_plus_Aplus,
    SwStr_pct_Aplus
  ) %>% 
  ungroup()

dtrain_kpct_aa <- xgb.DMatrix(as.matrix(kpct_AA %>% select(-K_pct_AA)), label = kpct_AA$K_pct_AA)


hyperparam_kpct_aa_tuning_reg <- function(max_depth_kpct_aplus, weight_kpct_aplus,subsample_kpct_aplus, row_num_kpct_aplus){
  
  print(paste('Max Depth: ', max_depth_kpct_aplus))
  print(paste('Weight: ', weight_kpct_aplus))
  print(paste('Subsample: ', subsample_kpct_aplus))
  print(paste('Row Number: ', row_num_kpct_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_kpct_aplus,
      min_child_weight = weight_kpct_aplus,
      subsample = subsample_kpct_aplus
    ),
    data = dtrain_kpct_aa,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_kpct_aa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_kpct_aa_df$rmse <- pmap_dbl(list(reg_tuning_kpct_aa_df$max_depth, reg_tuning_kpct_aa_df$weight,
                                           reg_tuning_kpct_aa_df$subsample, reg_tuning_kpct_aa_df$row_num),
                                      hyperparam_kpct_aa_tuning_reg, .progress = TRUE)

reg_tuning_kpct_aa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_kpct_aa_best <- reg_tuning_kpct_aa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_kpct_aa <- reg_tuning_kpct_aa_best$max_depth # 4
weight_kpct_aa <- reg_tuning_kpct_aa_best$weight # 1
subsample_kpct_aa <- reg_tuning_kpct_aa_best$subsample # 1

reg_tuning_kpct_aa_best$rmse/sd(kpct_AA$K_pct_AA) #0.701

### K% AA to AAA ####
kpct_AAA <- minor_league_hitting_AAA %>% 
  drop_na(Name_AA) %>%
  rowwise() %>% 
  reframe(
    K_pct_AAA,
    top_100_rank = mean(c(rank_ba_AA, rank_mlb_AA)),
    team_rank = team_rank_ba_AA,
    Age_AA,
    kpct_AA_adj = SO_AA/(PA_AA^0.6),
    K_pct_AA,
    wRC_plus_AA,
    SwStr_pct_AA
  ) %>% 
  ungroup()

dtrain_kpct_aaa <- xgb.DMatrix(as.matrix(kpct_AAA %>% select(-K_pct_AAA)), label = kpct_AAA$K_pct_AAA)


hyperparam_kpct_aaa_tuning_reg <- function(max_depth_kpct_aplus, weight_kpct_aplus,subsample_kpct_aplus, row_num_kpct_aplus){
  
  print(paste('Max Depth: ', max_depth_kpct_aplus))
  print(paste('Weight: ', weight_kpct_aplus))
  print(paste('Subsample: ', subsample_kpct_aplus))
  print(paste('Row Number: ', row_num_kpct_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_kpct_aplus,
      min_child_weight = weight_kpct_aplus,
      subsample = subsample_kpct_aplus
    ),
    data = dtrain_kpct_aaa,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_kpct_aaa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_kpct_aaa_df$rmse <- pmap_dbl(list(reg_tuning_kpct_aaa_df$max_depth, reg_tuning_kpct_aaa_df$weight,
                                            reg_tuning_kpct_aaa_df$subsample, reg_tuning_kpct_aaa_df$row_num),
                                       hyperparam_kpct_aaa_tuning_reg, .progress = TRUE)

reg_tuning_kpct_aaa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_kpct_aaa_best <- reg_tuning_kpct_aaa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_kpct_aaa <- reg_tuning_kpct_aaa_best$max_depth # 4
weight_kpct_aaa <- reg_tuning_kpct_aaa_best$weight # 10
subsample_kpct_aaa <- reg_tuning_kpct_aaa_best$subsample # 1

reg_tuning_kpct_aaa_best$rmse/sd(kpct_AAA$K_pct_AAA) #0.725


### K% AAA to MLB ####
kpct_mlb <- mlb_hitting %>% 
  drop_na(Name_AAA) %>% 
  rowwise() %>% 
  reframe(
    K_pct_MLB,
    top_100_rank = mean(c(rank_ba, rank_mlb)),
    team_rank = team_rank_ba,
    Age_AAA,
    kpct_AAA_adj = SO_AAA/(PA_AAA^0.6),
    K_pct_AAA,
    wRC_plus_AAA,
    SwStr_pct_AAA
  ) %>% 
  ungroup()


dtrain_kpct_mlb <- xgb.DMatrix(as.matrix(kpct_mlb %>% select(-K_pct_MLB)), label = kpct_mlb$K_pct_MLB)


hyperparam_kpct_mlb_tuning_reg <- function(max_depth_kpct_aplus, weight_kpct_aplus,subsample_kpct_aplus, row_num_kpct_aplus){
  
  print(paste('Max Depth: ', max_depth_kpct_aplus))
  print(paste('Weight: ', weight_kpct_aplus))
  print(paste('Subsample: ', subsample_kpct_aplus))
  print(paste('Row Number: ', row_num_kpct_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_kpct_aplus,
      min_child_weight = weight_kpct_aplus,
      subsample = subsample_kpct_aplus
    ),
    data = dtrain_kpct_mlb,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_kpct_mlb_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_kpct_mlb_df$rmse <- pmap_dbl(list(reg_tuning_kpct_mlb_df$max_depth, reg_tuning_kpct_mlb_df$weight,
                                            reg_tuning_kpct_mlb_df$subsample, reg_tuning_kpct_mlb_df$row_num),
                                       hyperparam_kpct_mlb_tuning_reg, .progress = TRUE)

reg_tuning_kpct_mlb_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_kpct_mlb_best <- reg_tuning_kpct_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_kpct_mlb <- reg_tuning_kpct_mlb_best$max_depth # 4
weight_kpct_mlb <- reg_tuning_kpct_mlb_best$weight # 10
subsample_kpct_mlb <- reg_tuning_kpct_mlb_best$subsample # 1

reg_tuning_kpct_mlb_best$rmse/sd(kpct_mlb$K_pct_MLB) #0.743



### BB% A to A+ ####
bbpct_aplus <- minor_league_hitting_Aplus %>% 
  drop_na(Name_A) %>% 
  rowwise() %>% 
  reframe(
    BB_pct_Aplus,
    top_100_rank = mean(c(rank_ba_A, rank_mlb_A)),
    team_rank = team_rank_ba_A,
    Age_A,
    bbpct_A_adj = BB_Aplus/(PA_A^0.8),
    wRC_plus_A,
    SwStr_pct_A
  ) %>% 
  ungroup()


dtrain_bbpct_aplus <- xgb.DMatrix(as.matrix(bbpct_aplus %>% select(-BB_pct_Aplus)), label = bbpct_aplus$BB_pct_Aplus)


hyperparam_bbpct_aplus_tuning_reg <- function(max_depth_bbpct_aplus, weight_bbpct_aplus,subsample_bbpct_aplus, row_num_bbpct_aplus){
  
  print(paste('Max Depth: ', max_depth_bbpct_aplus))
  print(paste('Weight: ', weight_bbpct_aplus))
  print(paste('Subsample: ', subsample_bbpct_aplus))
  print(paste('Row Number: ', row_num_bbpct_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_bbpct_aplus,
      min_child_weight = weight_bbpct_aplus,
      subsample = subsample_bbpct_aplus
    ),
    data = dtrain_bbpct_aplus,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_bbpct_aplus_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_bbpct_aplus_df$rmse <- pmap_dbl(list(reg_tuning_bbpct_aplus_df$max_depth, reg_tuning_bbpct_aplus_df$weight,
                                              reg_tuning_bbpct_aplus_df$subsample, reg_tuning_bbpct_aplus_df$row_num),
                                         hyperparam_bbpct_aplus_tuning_reg, .progress = TRUE)

reg_tuning_bbpct_aplus_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_bbpct_aplus_best <- reg_tuning_bbpct_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_bbpct_aplus <- reg_tuning_bbpct_aplus_best$max_depth # 4
weight_bbpct_aplus <- reg_tuning_bbpct_aplus_best$weight # 10
subsample_bbpct_aplus <- reg_tuning_bbpct_aplus_best$subsample # 1

reg_tuning_bbpct_aplus_best$rmse/sd(bbpct_aplus$BB_pct_Aplus) #0.918




### BB% A+ to AA ####
bbpct_AA <- minor_league_hitting_AA %>% 
  drop_na(Name_Aplus) %>%  
  rowwise() %>% 
  reframe(
    BB_pct_AA,
    top_100_rank = mean(c(rank_ba_Aplus, rank_mlb_Aplus)),
    team_rank = team_rank_ba_Aplus,
    Age_Aplus,
    bbpct_Aplus_adj = BB_Aplus/(PA_Aplus^0.8),
    wRC_plus_Aplus,
    SwStr_pct_Aplus
  ) %>% 
  ungroup()

dtrain_bbpct_aa <- xgb.DMatrix(as.matrix(bbpct_AA %>% select(-BB_pct_AA)), label = bbpct_AA$BB_pct_AA)


hyperparam_bbpct_aa_tuning_reg <- function(max_depth_bbpct_aplus, weight_bbpct_aplus,subsample_bbpct_aplus, row_num_bbpct_aplus){
  
  print(paste('Max Depth: ', max_depth_bbpct_aplus))
  print(paste('Weight: ', weight_bbpct_aplus))
  print(paste('Subsample: ', subsample_bbpct_aplus))
  print(paste('Row Number: ', row_num_bbpct_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_bbpct_aplus,
      min_child_weight = weight_bbpct_aplus,
      subsample = subsample_bbpct_aplus
    ),
    data = dtrain_bbpct_aa,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_bbpct_aa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_bbpct_aa_df$rmse <- pmap_dbl(list(reg_tuning_bbpct_aa_df$max_depth, reg_tuning_bbpct_aa_df$weight,
                                           reg_tuning_bbpct_aa_df$subsample, reg_tuning_bbpct_aa_df$row_num),
                                      hyperparam_bbpct_aa_tuning_reg, .progress = TRUE)

reg_tuning_bbpct_aa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_bbpct_aa_best <- reg_tuning_bbpct_aa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_bbpct_aa <- reg_tuning_bbpct_aa_best$max_depth # 8
weight_bbpct_aa <- reg_tuning_bbpct_aa_best$weight # 10
subsample_bbpct_aa <- reg_tuning_bbpct_aa_best$subsample # 1

reg_tuning_bbpct_aa_best$rmse/sd(bbpct_AA$BB_pct_AA) #0.881

### BB% AA to AAA ####
bbpct_AAA <- minor_league_hitting_AAA %>% 
  drop_na(Name_AA) %>%
  rowwise() %>% 
  reframe(
    BB_pct_AAA,
    top_100_rank = mean(c(rank_ba_AA, rank_mlb_AA)),
    team_rank = team_rank_ba_AA,
    Age_AA,
    bbpct_AA_adj = BB_AA/(PA_AA^0.8),
    wRC_plus_AA,
    SwStr_pct_AA
  ) %>% 
  ungroup()

dtrain_bbpct_aaa <- xgb.DMatrix(as.matrix(bbpct_AAA %>% select(-BB_pct_AAA)), label = bbpct_AAA$BB_pct_AAA)


hyperparam_bbpct_aaa_tuning_reg <- function(max_depth_bbpct_aplus, weight_bbpct_aplus,subsample_bbpct_aplus, row_num_bbpct_aplus){
  
  print(paste('Max Depth: ', max_depth_bbpct_aplus))
  print(paste('Weight: ', weight_bbpct_aplus))
  print(paste('Subsample: ', subsample_bbpct_aplus))
  print(paste('Row Number: ', row_num_bbpct_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_bbpct_aplus,
      min_child_weight = weight_bbpct_aplus,
      subsample = subsample_bbpct_aplus
    ),
    data = dtrain_bbpct_aaa,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_bbpct_aaa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_bbpct_aaa_df$rmse <- pmap_dbl(list(reg_tuning_bbpct_aaa_df$max_depth, reg_tuning_bbpct_aaa_df$weight,
                                            reg_tuning_bbpct_aaa_df$subsample, reg_tuning_bbpct_aaa_df$row_num),
                                       hyperparam_bbpct_aaa_tuning_reg, .progress = TRUE)

reg_tuning_bbpct_aaa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_bbpct_aaa_best <- reg_tuning_bbpct_aaa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_bbpct_aaa <- reg_tuning_bbpct_aaa_best$max_depth # 4
weight_bbpct_aaa <- reg_tuning_bbpct_aaa_best$weight # 4
subsample_bbpct_aaa <- reg_tuning_bbpct_aaa_best$subsample # 1

reg_tuning_bbpct_aaa_best$rmse/sd(bbpct_AAA$BB_pct_AAA) #0.883


### BB% AAA to MLB ####
bbpct_mlb <- mlb_hitting %>% 
  drop_na(Name_AAA) %>% 
  rowwise() %>% 
  reframe(
    BB_pct_MLB,
    top_100_rank = mean(c(rank_ba, rank_mlb)),
    team_rank = team_rank_ba,
    Age_AAA,
    bbpct_AAA_adj = BB_AAA/(PA_AAA^0.8),
    wRC_plus_AAA,
    SwStr_pct_AAA
  ) %>% 
  ungroup()


dtrain_bbpct_mlb <- xgb.DMatrix(as.matrix(bbpct_mlb %>% select(-BB_pct_MLB)), label = bbpct_mlb$BB_pct_MLB)


hyperparam_bbpct_mlb_tuning_reg <- function(max_depth_bbpct_aplus, weight_bbpct_aplus,subsample_bbpct_aplus, row_num_bbpct_aplus){
  
  print(paste('Max Depth: ', max_depth_bbpct_aplus))
  print(paste('Weight: ', weight_bbpct_aplus))
  print(paste('Subsample: ', subsample_bbpct_aplus))
  print(paste('Row Number: ', row_num_bbpct_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_bbpct_aplus,
      min_child_weight = weight_bbpct_aplus,
      subsample = subsample_bbpct_aplus
    ),
    data = dtrain_bbpct_mlb,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_bbpct_mlb_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_bbpct_mlb_df$rmse <- pmap_dbl(list(reg_tuning_bbpct_mlb_df$max_depth, reg_tuning_bbpct_mlb_df$weight,
                                            reg_tuning_bbpct_mlb_df$subsample, reg_tuning_bbpct_mlb_df$row_num),
                                       hyperparam_bbpct_mlb_tuning_reg, .progress = TRUE)

reg_tuning_bbpct_mlb_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_bbpct_mlb_best <- reg_tuning_bbpct_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_bbpct_mlb <- reg_tuning_bbpct_mlb_best$max_depth # 8
weight_bbpct_mlb <- reg_tuning_bbpct_mlb_best$weight # 10
subsample_bbpct_mlb <- reg_tuning_bbpct_mlb_best$subsample # 1

reg_tuning_bbpct_mlb_best$rmse/sd(bbpct_mlb$BB_pct_MLB) #0.962



### SwStr% A to A+ ####
swstrpct_aplus <- minor_league_hitting_Aplus %>% 
  drop_na(Name_A) %>% 
  rowwise() %>% 
  reframe(
    SwStr_pct_Aplus,
    top_100_rank = mean(c(rank_ba_A, rank_mlb_A)),
    team_rank = team_rank_ba_A,
    Age_A,
    BB_minus_K_pct = BB_pct_A - K_pct_A,
    wRC_plus_A,
    SwStr_pct_A
  ) %>% 
  ungroup()



dtrain_swstrpct_aplus <- xgb.DMatrix(as.matrix(swstrpct_aplus %>% select(-SwStr_pct_Aplus)), label = swstrpct_aplus$SwStr_pct_Aplus)


hyperparam_swstrpct_aplus_tuning_reg <- function(max_depth_swstrpct_aplus, weight_swstrpct_aplus,subsample_swstrpct_aplus, row_num_swstrpct_aplus){
  
  print(paste('Max Depth: ', max_depth_swstrpct_aplus))
  print(paste('Weight: ', weight_swstrpct_aplus))
  print(paste('Subsample: ', subsample_swstrpct_aplus))
  print(paste('Row Number: ', row_num_swstrpct_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_swstrpct_aplus,
      min_child_weight = weight_swstrpct_aplus,
      subsample = subsample_swstrpct_aplus
    ),
    data = dtrain_swstrpct_aplus,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_swstrpct_aplus_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_swstrpct_aplus_df$rmse <- pmap_dbl(list(reg_tuning_swstrpct_aplus_df$max_depth, reg_tuning_swstrpct_aplus_df$weight,
                                              reg_tuning_swstrpct_aplus_df$subsample, reg_tuning_swstrpct_aplus_df$row_num),
                                         hyperparam_swstrpct_aplus_tuning_reg, .progress = TRUE)

reg_tuning_swstrpct_aplus_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_swstrpct_aplus_best <- reg_tuning_swstrpct_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_swstrpct_aplus <- reg_tuning_swstrpct_aplus_best$max_depth # 4
weight_swstrpct_aplus <- reg_tuning_swstrpct_aplus_best$weight # 10
subsample_swstrpct_aplus <- reg_tuning_swstrpct_aplus_best$subsample # 1

reg_tuning_swstrpct_aplus_best$rmse/sd(swstrpct_aplus$SwStr_pct_Aplus) #0.768




### SwStr% A+ to AA ####
swstrpct_AA <- minor_league_hitting_AA %>% 
  drop_na(Name_Aplus) %>%  
  rowwise() %>% 
  reframe(
    SwStr_pct_AA,
    top_100_rank = mean(c(rank_ba_Aplus, rank_mlb_Aplus)),
    team_rank = team_rank_ba_Aplus,
    Age_Aplus,
    BB_minus_K_pct_Aplus = BB_pct_Aplus - K_pct_Aplus,
    wRC_plus_Aplus,
    SwStr_pct_Aplus
  ) %>% 
  ungroup()

dtrain_swstrpct_aa <- xgb.DMatrix(as.matrix(swstrpct_AA %>% select(-SwStr_pct_AA)), label = swstrpct_AA$SwStr_pct_AA)


hyperparam_swstrpct_aa_tuning_reg <- function(max_depth_swstrpct_aplus, weight_swstrpct_aplus,subsample_swstrpct_aplus, row_num_swstrpct_aplus){
  
  print(paste('Max Depth: ', max_depth_swstrpct_aplus))
  print(paste('Weight: ', weight_swstrpct_aplus))
  print(paste('Subsample: ', subsample_swstrpct_aplus))
  print(paste('Row Number: ', row_num_swstrpct_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_swstrpct_aplus,
      min_child_weight = weight_swstrpct_aplus,
      subsample = subsample_swstrpct_aplus
    ),
    data = dtrain_swstrpct_aa,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_swstrpct_aa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_swstrpct_aa_df$rmse <- pmap_dbl(list(reg_tuning_swstrpct_aa_df$max_depth, reg_tuning_swstrpct_aa_df$weight,
                                           reg_tuning_swstrpct_aa_df$subsample, reg_tuning_swstrpct_aa_df$row_num),
                                      hyperparam_swstrpct_aa_tuning_reg, .progress = TRUE)

reg_tuning_swstrpct_aa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_swstrpct_aa_best <- reg_tuning_swstrpct_aa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_swstrpct_aa <- reg_tuning_swstrpct_aa_best$max_depth # 4
weight_swstrpct_aa <- reg_tuning_swstrpct_aa_best$weight # 10
subsample_swstrpct_aa <- reg_tuning_swstrpct_aa_best$subsample # 1

reg_tuning_swstrpct_aa_best$rmse/sd(swstrpct_AA$SwStr_pct_AA) #0.746

### SwStr% AA to AAA ####
swstrpct_AAA <- minor_league_hitting_AAA %>% 
  drop_na(Name_AA) %>%
  rowwise() %>% 
  reframe(
    SwStr_pct_AAA,
    top_100_rank = mean(c(rank_ba_AA, rank_mlb_AA)),
    team_rank = team_rank_ba_AA,
    Age_AA,
    BB_minus_K_pct_AA = BB_pct_AA - K_pct_AA,
    wRC_plus_AA,
    SwStr_pct_AA
  ) %>% 
  ungroup()

dtrain_swstrpct_aaa <- xgb.DMatrix(as.matrix(swstrpct_AAA %>% select(-SwStr_pct_AAA)), label = swstrpct_AAA$SwStr_pct_AAA)


hyperparam_swstrpct_aaa_tuning_reg <- function(max_depth_swstrpct_aplus, weight_swstrpct_aplus,subsample_swstrpct_aplus, row_num_swstrpct_aplus){
  
  print(paste('Max Depth: ', max_depth_swstrpct_aplus))
  print(paste('Weight: ', weight_swstrpct_aplus))
  print(paste('Subsample: ', subsample_swstrpct_aplus))
  print(paste('Row Number: ', row_num_swstrpct_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_swstrpct_aplus,
      min_child_weight = weight_swstrpct_aplus,
      subsample = subsample_swstrpct_aplus
    ),
    data = dtrain_swstrpct_aaa,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_swstrpct_aaa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_swstrpct_aaa_df$rmse <- pmap_dbl(list(reg_tuning_swstrpct_aaa_df$max_depth, reg_tuning_swstrpct_aaa_df$weight,
                                            reg_tuning_swstrpct_aaa_df$subsample, reg_tuning_swstrpct_aaa_df$row_num),
                                       hyperparam_swstrpct_aaa_tuning_reg, .progress = TRUE)

reg_tuning_swstrpct_aaa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_swstrpct_aaa_best <- reg_tuning_swstrpct_aaa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_swstrpct_aaa <- reg_tuning_swstrpct_aaa_best$max_depth # 4
weight_swstrpct_aaa <- reg_tuning_swstrpct_aaa_best$weight # 10
subsample_swstrpct_aaa <- reg_tuning_swstrpct_aaa_best$subsample # 1

reg_tuning_swstrpct_aaa_best$rmse/sd(swstrpct_AAA$SwStr_pct_AAA) #0.669


### SwStr% AAA to MLB ####
swstrpct_mlb <- mlb_hitting %>% 
  drop_na(Name_AAA) %>% 
  rowwise() %>% 
  reframe(
    SwStr_pct_MLB,
    top_100_rank = mean(c(rank_ba, rank_mlb)),
    team_rank = team_rank_ba,
    Age_AAA,
    BB_minus_K_pct = BB_pct_AAA - K_pct_AAA,
    wRC_plus_AAA,
    SwStr_pct_AAA
  ) %>% 
  ungroup()


dtrain_swstrpct_mlb <- xgb.DMatrix(as.matrix(swstrpct_mlb %>% select(-SwStr_pct_MLB)), label = swstrpct_mlb$SwStr_pct_MLB)


hyperparam_swstrpct_mlb_tuning_reg <- function(max_depth_swstrpct_aplus, weight_swstrpct_aplus,subsample_swstrpct_aplus, row_num_swstrpct_aplus){
  
  print(paste('Max Depth: ', max_depth_swstrpct_aplus))
  print(paste('Weight: ', weight_swstrpct_aplus))
  print(paste('Subsample: ', subsample_swstrpct_aplus))
  print(paste('Row Number: ', row_num_swstrpct_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_swstrpct_aplus,
      min_child_weight = weight_swstrpct_aplus,
      subsample = subsample_swstrpct_aplus
    ),
    data = dtrain_swstrpct_mlb,
    nrounds = 200000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_swstrpct_mlb_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_swstrpct_mlb_df$rmse <- pmap_dbl(list(reg_tuning_swstrpct_mlb_df$max_depth, reg_tuning_swstrpct_mlb_df$weight,
                                            reg_tuning_swstrpct_mlb_df$subsample, reg_tuning_swstrpct_mlb_df$row_num),
                                       hyperparam_swstrpct_mlb_tuning_reg, .progress = TRUE)

reg_tuning_swstrpct_mlb_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_swstrpct_mlb_best <- reg_tuning_swstrpct_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_swstrpct_mlb <- reg_tuning_swstrpct_mlb_best$max_depth # 4
weight_swstrpct_mlb <- reg_tuning_swstrpct_mlb_best$weight # 4
subsample_swstrpct_mlb <- reg_tuning_swstrpct_mlb_best$subsample # 1

reg_tuning_swstrpct_mlb_best$rmse/sd(swstrpct_mlb$SwStr_pct_MLB) #0.714



### ERA A to A+ ####
era_aplus <- minor_league_pitching_Aplus %>% 
  drop_na(Name_A) %>% 
  rowwise() %>% 
  reframe(
    ERA_Aplus,
    top_100_rank = mean(c(rank_ba_A, rank_mlb_A)),
    team_rank = team_rank_ba_A,
    Age_A,
    ER_A_adj = ER_A/(IP_A^0.6),
    ERA_A,
    WHIP_A,
    HR_per_FB_A,
    Pull_pct_A,
    GB_pct_A,
    SwStr_pct_A,
    FIP_A
  ) %>% 
  ungroup()


dtrain_era_aplus <- xgb.DMatrix(as.matrix(era_aplus %>% select(-ERA_Aplus)), label = era_aplus$ERA_Aplus)


hyperparam_era_aplus_tuning_reg <- function(max_depth_era_aplus, weight_era_aplus,subsample_era_aplus, row_num_era_aplus){
  
  print(paste('Max Depth: ', max_depth_era_aplus))
  print(paste('Weight: ', weight_era_aplus))
  print(paste('Subsample: ', subsample_era_aplus))
  print(paste('Row Number: ', row_num_era_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.005,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 10,
      max_depth = max_depth_era_aplus,
      min_child_weight = weight_era_aplus,
      subsample = subsample_era_aplus
    ),
    data = dtrain_era_aplus,
    nrounds = 20000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_era_aplus_df <- expand_grid(
  max_depth = c(6,4,2),
  weight = c(10, 20 ,30),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_era_aplus_df$rmse <- pmap_dbl(list(reg_tuning_era_aplus_df$max_depth, reg_tuning_era_aplus_df$weight,
                                              reg_tuning_era_aplus_df$subsample, reg_tuning_era_aplus_df$row_num),
                                         hyperparam_era_aplus_tuning_reg, .progress = TRUE)

reg_tuning_era_aplus_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_era_aplus_best <- reg_tuning_era_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_era_aplus <- reg_tuning_era_aplus_best$max_depth # 2
weight_era_aplus <- reg_tuning_era_aplus_best$weight # 20
subsample_era_aplus <- reg_tuning_era_aplus_best$subsample # 0.6

reg_tuning_era_aplus_best$rmse/sd(era_aplus$ERA_Aplus) #0.964




### ERA A+ to AA ####
era_AA <- minor_league_pitching_AA %>% 
  drop_na(Name_Aplus) %>%  
  rowwise() %>% 
  reframe(
    ERA_AA,
    top_100_rank = mean(c(rank_ba_Aplus, rank_mlb_Aplus)),
    team_rank = team_rank_ba_Aplus,
    Age_Aplus,
    ER_Aplus_adj = ER_Aplus/(IP_Aplus^0.6),
    ERA_Aplus,
    WHIP_Aplus,
    HR_per_FB_Aplus,
    Pull_pct_Aplus,
    GB_pct_Aplus,
    SwStr_pct_Aplus,
    FIP_Aplus
  ) %>% 
  ungroup()

dtrain_era_aa <- xgb.DMatrix(as.matrix(era_AA %>% select(-ERA_AA)), label = era_AA$ERA_AA)


hyperparam_era_aa_tuning_reg <- function(max_depth_era_aplus, weight_era_aplus,subsample_era_aplus, row_num_era_aplus){
  
  print(paste('Max Depth: ', max_depth_era_aplus))
  print(paste('Weight: ', weight_era_aplus))
  print(paste('Subsample: ', subsample_era_aplus))
  print(paste('Row Number: ', row_num_era_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.005,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 10,
      max_depth = max_depth_era_aplus,
      min_child_weight = weight_era_aplus,
      subsample = subsample_era_aplus
    ),
    data = dtrain_era_aa,
    nrounds = 20000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_era_aa_df <- expand_grid(
  max_depth = c(6,4,2),
  weight = c(10, 20 ,30),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_era_aa_df$rmse <- pmap_dbl(list(reg_tuning_era_aa_df$max_depth, reg_tuning_era_aa_df$weight,
                                           reg_tuning_era_aa_df$subsample, reg_tuning_era_aa_df$row_num),
                                      hyperparam_era_aa_tuning_reg, .progress = TRUE)

reg_tuning_era_aa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_era_aa_best <- reg_tuning_era_aa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_era_aa <- reg_tuning_era_aa_best$max_depth # 2
weight_era_aa <- reg_tuning_era_aa_best$weight # 30
subsample_era_aa <- reg_tuning_era_aa_best$subsample # 0.6

reg_tuning_era_aa_best$rmse/sd(era_AA$ERA_AA) #0.973

### ERA AA to AAA ####
era_AAA <- minor_league_pitching_AAA %>% 
  drop_na(Name_AA) %>%
  rowwise() %>% 
  reframe(
    ERA_AAA,
    top_100_rank = mean(c(rank_ba_AA, rank_mlb_AA)),
    team_rank = team_rank_ba_AA,
    Age_AA,
    ER_AA_adj = ER_AA/(IP_AA^0.6),
    ERA_AA,
    WHIP_AA,
    HR_per_FB_AA,
    Pull_pct_AA,
    GB_pct_AA,
    SwStr_pct_AA,
    FIP_AA
  ) %>% 
  ungroup()


dtrain_era_aaa <- xgb.DMatrix(as.matrix(era_AAA %>% select(-ERA_AAA)), label = era_AAA$ERA_AAA)


hyperparam_era_aaa_tuning_reg <- function(max_depth_era_aplus, weight_era_aplus,subsample_era_aplus, row_num_era_aplus){
  
  print(paste('Max Depth: ', max_depth_era_aplus))
  print(paste('Weight: ', weight_era_aplus))
  print(paste('Subsample: ', subsample_era_aplus))
  print(paste('Row Number: ', row_num_era_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.005,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 10,
      max_depth = max_depth_era_aplus,
      min_child_weight = weight_era_aplus,
      subsample = subsample_era_aplus
    ),
    data = dtrain_era_aaa,
    nrounds = 20000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_era_aaa_df <- expand_grid(
  max_depth = c(6,4,2),
  weight = c(10, 20 ,30),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_era_aaa_df$rmse <- pmap_dbl(list(reg_tuning_era_aaa_df$max_depth, reg_tuning_era_aaa_df$weight,
                                            reg_tuning_era_aaa_df$subsample, reg_tuning_era_aaa_df$row_num),
                                       hyperparam_era_aaa_tuning_reg, .progress = TRUE)

reg_tuning_era_aaa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_era_aaa_best <- reg_tuning_era_aaa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_era_aaa <- reg_tuning_era_aaa_best$max_depth # 2
weight_era_aaa <- reg_tuning_era_aaa_best$weight # 10
subsample_era_aaa <- reg_tuning_era_aaa_best$subsample # 0.6

reg_tuning_era_aaa_best$rmse/sd(era_AAA$ERA_AAA) #0.973 


### ERA AAA to MLB ####
era_mlb <- mlb_sp_pitching %>%
  bind_rows(mlb_rp_pitching %>% 
              rename('ERA_MLB' = 'ERA',
                     'rank_ba' = 'rank_ba_AAA', 
                     'rank_mlb' = 'rank_mlb_AAA',
                     'team_rank_ba' = 'team_rank_ba_AAA')) %>% 
  drop_na(Name_AAA) %>% 
  rowwise() %>% 
  reframe(
    ERA_MLB,
    top_100_rank = mean(c(rank_ba, rank_mlb)),
    team_rank = team_rank_ba,
    Age_AAA,
    ER_AAA_adj = ER_AAA/(IP_AAA^0.6),
    ERA_AAA,
    WHIP_AAA,
    HR_per_FB_AAA,
    Pull_pct_AAA,
    GB_pct_AAA,
    SwStr_pct_AAA,
    FIP_AAA
  ) %>% 
  ungroup()


dtrain_era_mlb <- xgb.DMatrix(as.matrix(era_mlb %>% select(-ERA_MLB)), label = era_mlb$ERA_MLB)


hyperparam_era_mlb_tuning_reg <- function(max_depth_era_aplus, weight_era_aplus,subsample_era_aplus, row_num_era_aplus){
  
  print(paste('Max Depth: ', max_depth_era_aplus))
  print(paste('Weight: ', weight_era_aplus))
  print(paste('Subsample: ', subsample_era_aplus))
  print(paste('Row Number: ', row_num_era_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.005,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 10,
      max_depth = max_depth_era_aplus,
      min_child_weight = weight_era_aplus,
      subsample = subsample_era_aplus
    ),
    data = dtrain_era_mlb,
    nrounds = 20000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_era_mlb_df <- expand_grid(
  max_depth = c(6,4,2),
  weight = c(10, 20 ,30),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_era_mlb_df$rmse <- pmap_dbl(list(reg_tuning_era_mlb_df$max_depth, reg_tuning_era_mlb_df$weight,
                                            reg_tuning_era_mlb_df$subsample, reg_tuning_era_mlb_df$row_num),
                                       hyperparam_era_mlb_tuning_reg, .progress = TRUE)

reg_tuning_era_mlb_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_era_mlb_best <- reg_tuning_era_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_era_mlb <- reg_tuning_era_mlb_best$max_depth # 2
weight_era_mlb <- reg_tuning_era_mlb_best$weight # 20
subsample_era_mlb <- reg_tuning_era_mlb_best$subsample # 0.6

reg_tuning_era_mlb_best$rmse/sd(era_mlb$ERA_MLB) #0.989 




### FIP A to A+ ####
fip_aplus <- minor_league_pitching_Aplus %>% 
  drop_na(Name_A) %>% 
  rowwise() %>% 
  reframe(
    FIP_Aplus,
    top_100_rank = mean(c(rank_ba_A, rank_mlb_A)),
    team_rank = team_rank_ba_A,
    Age_A,
    FIP_A_adj = (13*HR_A + 3*BB_A - 2*SO_A)/(IP_A^0.6),
    FIP_A, 
    HR_per_FB_A,
    SwStr_pct_A
  ) %>% 
  ungroup()


dtrain_fip_aplus <- xgb.DMatrix(as.matrix(fip_aplus %>% select(-FIP_Aplus)), label = fip_aplus$FIP_Aplus)


hyperparam_fip_aplus_tuning_reg <- function(max_depth_fip_aplus, weight_fip_aplus,subsample_fip_aplus, row_num_fip_aplus){
  
  print(paste('Max Depth: ', max_depth_fip_aplus))
  print(paste('Weight: ', weight_fip_aplus))
  print(paste('Subsample: ', subsample_fip_aplus))
  print(paste('Row Number: ', row_num_fip_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_fip_aplus,
      min_child_weight = weight_fip_aplus,
      subsample = subsample_fip_aplus
    ),
    data = dtrain_fip_aplus,
    nrounds = 20000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_fip_aplus_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_fip_aplus_df$rmse <- pmap_dbl(list(reg_tuning_fip_aplus_df$max_depth, reg_tuning_fip_aplus_df$weight,
                                              reg_tuning_fip_aplus_df$subsample, reg_tuning_fip_aplus_df$row_num),
                                         hyperparam_fip_aplus_tuning_reg, .progress = TRUE)

reg_tuning_fip_aplus_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_fip_aplus_best <- reg_tuning_fip_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_fip_aplus <- reg_tuning_fip_aplus_best$max_depth # 8
weight_fip_aplus <- reg_tuning_fip_aplus_best$weight # 4
subsample_fip_aplus <- reg_tuning_fip_aplus_best$subsample # 0.8

reg_tuning_fip_aplus_best$rmse/sd(fip_aplus$FIP_Aplus) #0.857 #XGBoost Better

hyperparam_fip_aplus_tuning_reg(8,4,0.8,1) #436

set.seed(101);fip_aplus_mod <- xgboost(
  params = list(
  eta = 0.01,
  objective = 'reg:squarederror',
  eval_metric = 'rmse',
  alpha = 5,
  max_depth = max_depth_fip_aplus,
  min_child_weight = weight_fip_aplus,
  subsample = subsample_fip_aplus
),
data = dtrain_fip_aplus,
nrounds = 436,
print_every_n = 100,
nthread = 7)

shap.plot.summary.wrap1(fip_aplus_mod, as.matrix(fip_aplus %>% select(-FIP_Aplus)))

### FIP A+ to AA ####
fip_AA <- minor_league_pitching_AA %>% 
  drop_na(Name_Aplus) %>%  
  rowwise() %>% 
  reframe(
    FIP_AA,
    top_100_rank = mean(c(rank_ba_Aplus, rank_mlb_Aplus)),
    team_rank = team_rank_ba_Aplus,
    Age_Aplus,
    FIP_Aplus_adj = (13*HR_Aplus + 3*BB_Aplus - 2*SO_Aplus)/(IP_Aplus^0.6),
    FIP_Aplus, 
    HR_per_FB_Aplus,
    SwStr_pct_Aplus
  ) %>% 
  ungroup()

dtrain_fip_aa <- xgb.DMatrix(as.matrix(fip_AA %>% select(-FIP_AA)), label = fip_AA$FIP_AA)


hyperparam_fip_aa_tuning_reg <- function(max_depth_fip_aplus, weight_fip_aplus,subsample_fip_aplus, row_num_fip_aplus){
  
  print(paste('Max Depth: ', max_depth_fip_aplus))
  print(paste('Weight: ', weight_fip_aplus))
  print(paste('Subsample: ', subsample_fip_aplus))
  print(paste('Row Number: ', row_num_fip_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_fip_aplus,
      min_child_weight = weight_fip_aplus,
      subsample = subsample_fip_aplus
    ),
    data = dtrain_fip_aa,
    nrounds = 20000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_fip_aa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_fip_aa_df$rmse <- pmap_dbl(list(reg_tuning_fip_aa_df$max_depth, reg_tuning_fip_aa_df$weight,
                                           reg_tuning_fip_aa_df$subsample, reg_tuning_fip_aa_df$row_num),
                                      hyperparam_fip_aa_tuning_reg, .progress = TRUE)

reg_tuning_fip_aa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_fip_aa_best <- reg_tuning_fip_aa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_fip_aa <- reg_tuning_fip_aa_best$max_depth # 8
weight_fip_aa <- reg_tuning_fip_aa_best$weight # 1
subsample_fip_aa <- reg_tuning_fip_aa_best$subsample # 1

reg_tuning_fip_aa_best$rmse/sd(fip_AA$FIP_AA) #0.821 #XGBoost Better

### FIP AA to AAA ####
fip_AAA <- minor_league_pitching_AAA %>% 
  drop_na(Name_AA) %>%
  rowwise() %>% 
  reframe(
    FIP_AAA,
    top_100_rank = mean(c(rank_ba_AA, rank_mlb_AA)),
    team_rank = team_rank_ba_AA,
    Age_AA,
    FIP_AA_adj = (13*HR_AA + 3*BB_AA - 2*SO_AA)/(IP_AA^0.6),
    FIP_AA, 
    HR_per_FB_AA,
    SwStr_pct_AA
  ) %>% 
  ungroup()

dtrain_fip_aaa <- xgb.DMatrix(as.matrix(fip_AAA %>% select(-FIP_AAA)), label = fip_AAA$FIP_AAA)


hyperparam_fip_aaa_tuning_reg <- function(max_depth_fip_aplus, weight_fip_aplus,subsample_fip_aplus, row_num_fip_aplus){
  
  print(paste('Max Depth: ', max_depth_fip_aplus))
  print(paste('Weight: ', weight_fip_aplus))
  print(paste('Subsample: ', subsample_fip_aplus))
  print(paste('Row Number: ', row_num_fip_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_fip_aplus,
      min_child_weight = weight_fip_aplus,
      subsample = subsample_fip_aplus
    ),
    data = dtrain_fip_aaa,
    nrounds = 20000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_fip_aaa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_fip_aaa_df$rmse <- pmap_dbl(list(reg_tuning_fip_aaa_df$max_depth, reg_tuning_fip_aaa_df$weight,
                                            reg_tuning_fip_aaa_df$subsample, reg_tuning_fip_aaa_df$row_num),
                                       hyperparam_fip_aaa_tuning_reg, .progress = TRUE)

reg_tuning_fip_aaa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_fip_aaa_best <- reg_tuning_fip_aaa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_fip_aaa <- reg_tuning_fip_aaa_best$max_depth # 8
weight_fip_aaa <- reg_tuning_fip_aaa_best$weight # 1
subsample_fip_aaa <- reg_tuning_fip_aaa_best$subsample # 1

reg_tuning_fip_aaa_best$rmse/sd(fip_AAA$FIP_AAA) #0.645 #XGBoost Better


### FIP AAA to MLB ####
fip_mlb <- mlb_sp_pitching %>%
  bind_rows(mlb_rp_pitching %>% 
              rename('FIP_MLB' = 'FIP',
                     'rank_ba' = 'rank_ba_AAA', 
                     'rank_mlb' = 'rank_mlb_AAA',
                     'team_rank_ba' = 'team_rank_ba_AAA')) %>% 
  drop_na(Name_AAA) %>% 
  rowwise() %>% 
  reframe(
    FIP_MLB,
    top_100_rank = mean(c(rank_ba, rank_mlb)),
    team_rank = team_rank_ba,
    Age_AAA,
    FIP_AAA_adj = (13*HR_AAA + 3*BB_AAA - 2*SO_AAA)/(IP_AAA^0.6),
    FIP_AAA, 
    HR_per_FB_AAA,
    SwStr_pct_AAA
  ) %>% 
  ungroup()


dtrain_fip_mlb <- xgb.DMatrix(as.matrix(fip_mlb %>% select(-FIP_MLB)), label = fip_mlb$FIP_MLB)


hyperparam_fip_mlb_tuning_reg <- function(max_depth_fip_aplus, weight_fip_aplus,subsample_fip_aplus, row_num_fip_aplus){
  
  print(paste('Max Depth: ', max_depth_fip_aplus))
  print(paste('Weight: ', weight_fip_aplus))
  print(paste('Subsample: ', subsample_fip_aplus))
  print(paste('Row Number: ', row_num_fip_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_fip_aplus,
      min_child_weight = weight_fip_aplus,
      subsample = subsample_fip_aplus
    ),
    data = dtrain_fip_mlb,
    nrounds = 20000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_fip_mlb_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_fip_mlb_df$rmse <- pmap_dbl(list(reg_tuning_fip_mlb_df$max_depth, reg_tuning_fip_mlb_df$weight,
                                            reg_tuning_fip_mlb_df$subsample, reg_tuning_fip_mlb_df$row_num),
                                       hyperparam_fip_mlb_tuning_reg, .progress = TRUE)

reg_tuning_fip_mlb_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_fip_mlb_best <- reg_tuning_fip_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_fip_mlb <- reg_tuning_fip_mlb_best$max_depth # 8
weight_fip_mlb <- reg_tuning_fip_mlb_best$weight # 1
subsample_fip_mlb <- reg_tuning_fip_mlb_best$subsample # 0.6

reg_tuning_fip_mlb_best$rmse/sd(fip_mlb$FIP_MLB) #0.690




### K% A to A+ ####
kpctpit_aplus <- minor_league_pitching_Aplus %>% 
  drop_na(Name_A) %>% 
  rowwise() %>% 
  reframe(
    K_pct_Aplus,
    top_100_rank = mean(c(rank_ba_A, rank_mlb_A)),
    team_rank = team_rank_ba_A,
    Age_A,
    K_pct_A_adj = SO_A/(TBF_A^0.8),
    ERA_A,
    SwStr_pct_A
  ) %>% 
  ungroup()


dtrain_kpctpit_aplus <- xgb.DMatrix(as.matrix(kpctpit_aplus %>% select(-K_pct_Aplus)), label = kpctpit_aplus$K_pct_Aplus)


hyperparam_kpctpit_aplus_tuning_reg <- function(max_depth_kpctpit_aplus, weight_kpctpit_aplus,subsample_kpctpit_aplus, row_num_kpctpit_aplus){
  
  print(paste('Max Depth: ', max_depth_kpctpit_aplus))
  print(paste('Weight: ', weight_kpctpit_aplus))
  print(paste('Subsample: ', subsample_kpctpit_aplus))
  print(paste('Row Number: ', row_num_kpctpit_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_kpctpit_aplus,
      min_child_weight = weight_kpctpit_aplus,
      subsample = subsample_kpctpit_aplus
    ),
    data = dtrain_kpctpit_aplus,
    nrounds = 2000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_kpctpit_aplus_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_kpctpit_aplus_df$rmse <- pmap_dbl(list(reg_tuning_kpctpit_aplus_df$max_depth, reg_tuning_kpctpit_aplus_df$weight,
                                              reg_tuning_kpctpit_aplus_df$subsample, reg_tuning_kpctpit_aplus_df$row_num),
                                         hyperparam_kpctpit_aplus_tuning_reg, .progress = TRUE)

reg_tuning_kpctpit_aplus_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_kpctpit_aplus_best <- reg_tuning_kpctpit_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_kpctpit_aplus <- reg_tuning_kpctpit_aplus_best$max_depth # 8
weight_kpctpit_aplus <- reg_tuning_kpctpit_aplus_best$weight # 4
subsample_kpctpit_aplus <- reg_tuning_kpctpit_aplus_best$subsample # 1

reg_tuning_kpctpit_aplus_best$rmse/sd(kpctpit_aplus$K_pct_Aplus) #0.866




### K% A+ to AA ####
kpctpit_AA <- minor_league_pitching_AA %>% 
  drop_na(Name_Aplus) %>%  
  rowwise() %>% 
  reframe(
    K_pct_AA,
    top_100_rank = mean(c(rank_ba_Aplus, rank_mlb_Aplus)),
    team_rank = team_rank_ba_Aplus,
    Age_Aplus,
    K_pct_Aplus_adj = SO_Aplus/(TBF_Aplus^0.8),
    ERA_Aplus,
    SwStr_pct_Aplus
  ) %>% 
  ungroup()

dtrain_kpctpit_aa <- xgb.DMatrix(as.matrix(kpctpit_AA %>% select(-K_pct_AA)), label = kpctpit_AA$K_pct_AA)


hyperparam_kpctpit_aa_tuning_reg <- function(max_depth_kpctpit_aplus, weight_kpctpit_aplus,subsample_kpctpit_aplus, row_num_kpctpit_aplus){
  
  print(paste('Max Depth: ', max_depth_kpctpit_aplus))
  print(paste('Weight: ', weight_kpctpit_aplus))
  print(paste('Subsample: ', subsample_kpctpit_aplus))
  print(paste('Row Number: ', row_num_kpctpit_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_kpctpit_aplus,
      min_child_weight = weight_kpctpit_aplus,
      subsample = subsample_kpctpit_aplus
    ),
    data = dtrain_kpctpit_aa,
    nrounds = 2000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_kpctpit_aa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_kpctpit_aa_df$rmse <- pmap_dbl(list(reg_tuning_kpctpit_aa_df$max_depth, reg_tuning_kpctpit_aa_df$weight,
                                           reg_tuning_kpctpit_aa_df$subsample, reg_tuning_kpctpit_aa_df$row_num),
                                      hyperparam_kpctpit_aa_tuning_reg, .progress = TRUE)

reg_tuning_kpctpit_aa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_kpctpit_aa_best <- reg_tuning_kpctpit_aa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_kpctpit_aa <- reg_tuning_kpctpit_aa_best$max_depth # 8
weight_kpctpit_aa <- reg_tuning_kpctpit_aa_best$weight # 4
subsample_kpctpit_aa <- reg_tuning_kpctpit_aa_best$subsample # 1

reg_tuning_kpctpit_aa_best$rmse/sd(kpctpit_AA$K_pct_AA) #0.809

### K% AA to AAA ####
kpctpit_AAA <- minor_league_pitching_AAA %>% 
  drop_na(Name_AA) %>%
  rowwise() %>% 
  reframe(
    K_pct_AAA,
    top_100_rank = mean(c(rank_ba_AA, rank_mlb_AA)),
    team_rank = team_rank_ba_AA,
    Age_AA,
    K_pct_AA_adj = SO_AA/(TBF_AA^0.8),
    ERA_AA,
    SwStr_pct_AA
  ) %>% 
  ungroup()

dtrain_kpctpit_aaa <- xgb.DMatrix(as.matrix(kpctpit_AAA %>% select(-K_pct_AAA)), label = kpctpit_AAA$K_pct_AAA)


hyperparam_kpctpit_aaa_tuning_reg <- function(max_depth_kpctpit_aplus, weight_kpctpit_aplus,subsample_kpctpit_aplus, row_num_kpctpit_aplus){
  
  print(paste('Max Depth: ', max_depth_kpctpit_aplus))
  print(paste('Weight: ', weight_kpctpit_aplus))
  print(paste('Subsample: ', subsample_kpctpit_aplus))
  print(paste('Row Number: ', row_num_kpctpit_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_kpctpit_aplus,
      min_child_weight = weight_kpctpit_aplus,
      subsample = subsample_kpctpit_aplus
    ),
    data = dtrain_kpctpit_aaa,
    nrounds = 2000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_kpctpit_aaa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_kpctpit_aaa_df$rmse <- pmap_dbl(list(reg_tuning_kpctpit_aaa_df$max_depth, reg_tuning_kpctpit_aaa_df$weight,
                                            reg_tuning_kpctpit_aaa_df$subsample, reg_tuning_kpctpit_aaa_df$row_num),
                                       hyperparam_kpctpit_aaa_tuning_reg, .progress = TRUE)

reg_tuning_kpctpit_aaa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_kpctpit_aaa_best <- reg_tuning_kpctpit_aaa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_kpctpit_aaa <- reg_tuning_kpctpit_aaa_best$max_depth # 8
weight_kpctpit_aaa <- reg_tuning_kpctpit_aaa_best$weight # 4
subsample_kpctpit_aaa <- reg_tuning_kpctpit_aaa_best$subsample # 1

reg_tuning_kpctpit_aaa_best$rmse/sd(kpctpit_AAA$K_pct_AAA) #0.698


### K% AAA to MLB ####
kpctpit_mlb <- mlb_sp_pitching %>%
  bind_rows(mlb_rp_pitching %>% 
              rename('K_pct_MLB' = 'K_pct',
                     'rank_ba' = 'rank_ba_AAA', 
                     'rank_mlb' = 'rank_mlb_AAA',
                     'team_rank_ba' = 'team_rank_ba_AAA')) %>% 
  drop_na(Name_AAA) %>% 
  rowwise() %>% 
  reframe(
    K_pct_MLB,
    top_100_rank = mean(c(rank_ba, rank_mlb)),
    team_rank = team_rank_ba,
    Age_AAA,
    K_pct_AAA_adj = SO_AAA/(TBF_AAA^0.8),
    ERA_AAA,
    SwStr_pct_AAA
  ) %>% 
  ungroup()


dtrain_kpctpit_mlb <- xgb.DMatrix(as.matrix(kpctpit_mlb %>% select(-K_pct_MLB)), label = kpctpit_mlb$K_pct_MLB)


hyperparam_kpctpit_mlb_tuning_reg <- function(max_depth_kpctpit_aplus, weight_kpctpit_aplus,subsample_kpctpit_aplus, row_num_kpctpit_aplus){
  
  print(paste('Max Depth: ', max_depth_kpctpit_aplus))
  print(paste('Weight: ', weight_kpctpit_aplus))
  print(paste('Subsample: ', subsample_kpctpit_aplus))
  print(paste('Row Number: ', row_num_kpctpit_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_kpctpit_aplus,
      min_child_weight = weight_kpctpit_aplus,
      subsample = subsample_kpctpit_aplus
    ),
    data = dtrain_kpctpit_mlb,
    nrounds = 2000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_kpctpit_mlb_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_kpctpit_mlb_df$rmse <- pmap_dbl(list(reg_tuning_kpctpit_mlb_df$max_depth, reg_tuning_kpctpit_mlb_df$weight,
                                            reg_tuning_kpctpit_mlb_df$subsample, reg_tuning_kpctpit_mlb_df$row_num),
                                       hyperparam_kpctpit_mlb_tuning_reg, .progress = TRUE)

reg_tuning_kpctpit_mlb_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_kpctpit_mlb_best <- reg_tuning_kpctpit_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_kpctpit_mlb <- reg_tuning_kpctpit_mlb_best$max_depth # 8
weight_kpctpit_mlb <- reg_tuning_kpctpit_mlb_best$weight # 4
subsample_kpctpit_mlb <- reg_tuning_kpctpit_mlb_best$subsample # 1

reg_tuning_kpctpit_mlb_best$rmse/sd(kpctpit_mlb$K_pct_MLB) #0.892



### BB% A to A+ ####
bbpctpit_aplus <- minor_league_pitching_Aplus %>% 
  drop_na(Name_A) %>% 
  rowwise() %>% 
  reframe(
    BB_pct_Aplus,
    top_100_rank = mean(c(rank_ba_A, rank_mlb_A)),
    team_rank = team_rank_ba_A,
    Age_A,
    BB_pct_A_adj = BB_A/(TBF_A^0.6),
    BB_pct_A,
    ERA_A,
    SwStr_pct_A
  ) %>% 
  ungroup()


dtrain_bbpctpit_aplus <- xgb.DMatrix(as.matrix(bbpctpit_aplus %>% select(-BB_pct_Aplus)), label = bbpctpit_aplus$BB_pct_Aplus)


hyperparam_bbpctpit_aplus_tuning_reg <- function(max_depth_bbpctpit_aplus, weight_bbpctpit_aplus,subsample_bbpctpit_aplus, row_num_bbpctpit_aplus){
  
  print(paste('Max Depth: ', max_depth_bbpctpit_aplus))
  print(paste('Weight: ', weight_bbpctpit_aplus))
  print(paste('Subsample: ', subsample_bbpctpit_aplus))
  print(paste('Row Number: ', row_num_bbpctpit_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_bbpctpit_aplus,
      min_child_weight = weight_bbpctpit_aplus,
      subsample = subsample_bbpctpit_aplus
    ),
    data = dtrain_bbpctpit_aplus,
    nrounds = 2000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_bbpctpit_aplus_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_bbpctpit_aplus_df$rmse <- pmap_dbl(list(reg_tuning_bbpctpit_aplus_df$max_depth, reg_tuning_bbpctpit_aplus_df$weight,
                                              reg_tuning_bbpctpit_aplus_df$subsample, reg_tuning_bbpctpit_aplus_df$row_num),
                                         hyperparam_bbpctpit_aplus_tuning_reg, .progress = TRUE)

reg_tuning_bbpctpit_aplus_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_bbpctpit_aplus_best <- reg_tuning_bbpctpit_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_bbpctpit_aplus <- reg_tuning_bbpctpit_aplus_best$max_depth # 8
weight_bbpctpit_aplus <- reg_tuning_bbpctpit_aplus_best$weight # 10
subsample_bbpctpit_aplus <- reg_tuning_bbpctpit_aplus_best$subsample # 1

reg_tuning_bbpctpit_aplus_best$rmse/sd(bbpctpit_aplus$BB_pct_Aplus) #0.825




### BB% A+ to AA ####
bbpctpit_AA <- minor_league_pitching_AA %>% 
  drop_na(Name_Aplus) %>%  
  rowwise() %>% 
  reframe(
    BB_pct_AA,
    top_100_rank = mean(c(rank_ba_Aplus, rank_mlb_Aplus)),
    team_rank = team_rank_ba_Aplus,
    Age_Aplus,
    BB_pct_Aplus_adj = BB_Aplus/(TBF_Aplus^0.6),
    BB_pct_Aplus,
    ERA_Aplus,
    SwStr_pct_Aplus
  ) %>% 
  ungroup()

dtrain_bbpctpit_aa <- xgb.DMatrix(as.matrix(bbpctpit_AA %>% select(-BB_pct_AA)), label = bbpctpit_AA$BB_pct_AA)


hyperparam_bbpctpit_aa_tuning_reg <- function(max_depth_bbpctpit_aplus, weight_bbpctpit_aplus,subsample_bbpctpit_aplus, row_num_bbpctpit_aplus){
  
  print(paste('Max Depth: ', max_depth_bbpctpit_aplus))
  print(paste('Weight: ', weight_bbpctpit_aplus))
  print(paste('Subsample: ', subsample_bbpctpit_aplus))
  print(paste('Row Number: ', row_num_bbpctpit_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_bbpctpit_aplus,
      min_child_weight = weight_bbpctpit_aplus,
      subsample = subsample_bbpctpit_aplus
    ),
    data = dtrain_bbpctpit_aa,
    nrounds = 2000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_bbpctpit_aa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_bbpctpit_aa_df$rmse <- pmap_dbl(list(reg_tuning_bbpctpit_aa_df$max_depth, reg_tuning_bbpctpit_aa_df$weight,
                                           reg_tuning_bbpctpit_aa_df$subsample, reg_tuning_bbpctpit_aa_df$row_num),
                                      hyperparam_bbpctpit_aa_tuning_reg, .progress = TRUE)

reg_tuning_bbpctpit_aa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_bbpctpit_aa_best <- reg_tuning_bbpctpit_aa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_bbpctpit_aa <- reg_tuning_bbpctpit_aa_best$max_depth # 8
weight_bbpctpit_aa <- reg_tuning_bbpctpit_aa_best$weight # 4
subsample_bbpctpit_aa <- reg_tuning_bbpctpit_aa_best$subsample # 1

reg_tuning_bbpctpit_aa_best$rmse/sd(bbpctpit_AA$BB_pct_AA) #0.765

### BB% AA to AAA ####
bbpctpit_AAA <- minor_league_pitching_AAA %>% 
  drop_na(Name_AA) %>%
  rowwise() %>% 
  reframe(
    BB_pct_AAA,
    top_100_rank = mean(c(rank_ba_AA, rank_mlb_AA)),
    team_rank = team_rank_ba_AA,
    Age_AA,
    BB_pct_AA_adj = BB_AA/(TBF_AA^0.6),
    BB_pct_AA,
    ERA_AA,
    SwStr_pct_AA
  ) %>% 
  ungroup()

dtrain_bbpctpit_aaa <- xgb.DMatrix(as.matrix(bbpctpit_AAA %>% select(-BB_pct_AAA)), label = bbpctpit_AAA$BB_pct_AAA)


hyperparam_bbpctpit_aaa_tuning_reg <- function(max_depth_bbpctpit_aplus, weight_bbpctpit_aplus,subsample_bbpctpit_aplus, row_num_bbpctpit_aplus){
  
  print(paste('Max Depth: ', max_depth_bbpctpit_aplus))
  print(paste('Weight: ', weight_bbpctpit_aplus))
  print(paste('Subsample: ', subsample_bbpctpit_aplus))
  print(paste('Row Number: ', row_num_bbpctpit_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_bbpctpit_aplus,
      min_child_weight = weight_bbpctpit_aplus,
      subsample = subsample_bbpctpit_aplus
    ),
    data = dtrain_bbpctpit_aaa,
    nrounds = 2000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_bbpctpit_aaa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_bbpctpit_aaa_df$rmse <- pmap_dbl(list(reg_tuning_bbpctpit_aaa_df$max_depth, reg_tuning_bbpctpit_aaa_df$weight,
                                            reg_tuning_bbpctpit_aaa_df$subsample, reg_tuning_bbpctpit_aaa_df$row_num),
                                       hyperparam_bbpctpit_aaa_tuning_reg, .progress = TRUE)

reg_tuning_bbpctpit_aaa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_bbpctpit_aaa_best <- reg_tuning_bbpctpit_aaa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_bbpctpit_aaa <- reg_tuning_bbpctpit_aaa_best$max_depth # 8
weight_bbpctpit_aaa <- reg_tuning_bbpctpit_aaa_best$weight # 10
subsample_bbpctpit_aaa <- reg_tuning_bbpctpit_aaa_best$subsample # 1

reg_tuning_bbpctpit_aaa_best$rmse/sd(bbpctpit_AAA$BB_pct_AAA) #0.734


### BB% AAA to MLB ####
bbpctpit_mlb <- mlb_sp_pitching %>%
  bind_rows(mlb_rp_pitching %>% 
              rename('BB_pct_MLB' = 'BB_pct',
                     'rank_ba' = 'rank_ba_AAA', 
                     'rank_mlb' = 'rank_mlb_AAA',
                     'team_rank_ba' = 'team_rank_ba_AAA')) %>% 
  drop_na(Name_AAA) %>% 
  rowwise() %>% 
  reframe(
    BB_pct_MLB,
    top_100_rank = mean(c(rank_ba, rank_mlb)),
    team_rank = team_rank_ba,
    Age_AAA,
    BB_pct_AAA_adj = BB_AAA/(TBF_AAA^0.6),
    BB_pct_AAA,
    ERA_AAA,
    SwStr_pct_AAA
  ) %>% 
  ungroup()


dtrain_bbpctpit_mlb <- xgb.DMatrix(as.matrix(bbpctpit_mlb %>% select(-BB_pct_MLB)), label = bbpctpit_mlb$BB_pct_MLB)


hyperparam_bbpctpit_mlb_tuning_reg <- function(max_depth_bbpctpit_aplus, weight_bbpctpit_aplus,subsample_bbpctpit_aplus, row_num_bbpctpit_aplus){
  
  print(paste('Max Depth: ', max_depth_bbpctpit_aplus))
  print(paste('Weight: ', weight_bbpctpit_aplus))
  print(paste('Subsample: ', subsample_bbpctpit_aplus))
  print(paste('Row Number: ', row_num_bbpctpit_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_bbpctpit_aplus,
      min_child_weight = weight_bbpctpit_aplus,
      subsample = subsample_bbpctpit_aplus
    ),
    data = dtrain_bbpctpit_mlb,
    nrounds = 2000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_bbpctpit_mlb_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_bbpctpit_mlb_df$rmse <- pmap_dbl(list(reg_tuning_bbpctpit_mlb_df$max_depth, reg_tuning_bbpctpit_mlb_df$weight,
                                            reg_tuning_bbpctpit_mlb_df$subsample, reg_tuning_bbpctpit_mlb_df$row_num),
                                       hyperparam_bbpctpit_mlb_tuning_reg, .progress = TRUE)

reg_tuning_bbpctpit_mlb_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_bbpctpit_mlb_best <- reg_tuning_bbpctpit_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_bbpctpit_mlb <- reg_tuning_bbpctpit_mlb_best$max_depth # 6
weight_bbpctpit_mlb <- reg_tuning_bbpctpit_mlb_best$weight # 1
subsample_bbpctpit_mlb <- reg_tuning_bbpctpit_mlb_best$subsample # 1

reg_tuning_bbpctpit_mlb_best$rmse/sd(bbpctpit_mlb$BB_pct_MLB) #0.893




### SwStr% A to A+ ####
swstrpctpit_aplus <- minor_league_pitching_Aplus %>% 
  drop_na(Name_A) %>% 
  rowwise() %>% 
  reframe(
    SwStr_pct_Aplus,
    top_100_rank = mean(c(rank_ba_A, rank_mlb_A)),
    team_rank = team_rank_ba_A,
    Age_A,
    SwStr_pct_A,
    K_pct_A,
    ERA_A
  ) %>% 
  ungroup()


dtrain_swstrpctpit_aplus <- xgb.DMatrix(as.matrix(swstrpctpit_aplus %>% select(-SwStr_pct_Aplus)), label = swstrpctpit_aplus$SwStr_pct_Aplus)


hyperparam_swstrpctpit_aplus_tuning_reg <- function(max_depth_swstrpctpit_aplus, weight_swstrpctpit_aplus,subsample_swstrpctpit_aplus, row_num_swstrpctpit_aplus){
  
  print(paste('Max Depth: ', max_depth_swstrpctpit_aplus))
  print(paste('Weight: ', weight_swstrpctpit_aplus))
  print(paste('Subsample: ', subsample_swstrpctpit_aplus))
  print(paste('Row Number: ', row_num_swstrpctpit_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_swstrpctpit_aplus,
      min_child_weight = weight_swstrpctpit_aplus,
      subsample = subsample_swstrpctpit_aplus
    ),
    data = dtrain_swstrpctpit_aplus,
    nrounds = 2000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_swstrpctpit_aplus_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_swstrpctpit_aplus_df$rmse <- pmap_dbl(list(reg_tuning_swstrpctpit_aplus_df$max_depth, reg_tuning_swstrpctpit_aplus_df$weight,
                                              reg_tuning_swstrpctpit_aplus_df$subsample, reg_tuning_swstrpctpit_aplus_df$row_num),
                                         hyperparam_swstrpctpit_aplus_tuning_reg, .progress = TRUE)

reg_tuning_swstrpctpit_aplus_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_swstrpctpit_aplus_best <- reg_tuning_swstrpctpit_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_swstrpctpit_aplus <- reg_tuning_swstrpctpit_aplus_best$max_depth # 8
weight_swstrpctpit_aplus <- reg_tuning_swstrpctpit_aplus_best$weight # 1
subsample_swstrpctpit_aplus <- reg_tuning_swstrpctpit_aplus_best$subsample # 1

reg_tuning_swstrpctpit_aplus_best$rmse/sd(swstrpctpit_aplus$SwStr_pct_Aplus) #0.791




### SwStr% A+ to AA ####
swstrpctpit_AA <- minor_league_pitching_AA %>% 
  drop_na(Name_Aplus) %>%  
  rowwise() %>% 
  reframe(
    SwStr_pct_AA,
    top_100_rank = mean(c(rank_ba_Aplus, rank_mlb_Aplus)),
    team_rank = team_rank_ba_Aplus,
    Age_Aplus,
    SwStr_pct_Aplus,
    K_pct_Aplus,
    ERA_Aplus
  ) %>% 
  ungroup()

dtrain_swstrpctpit_aa <- xgb.DMatrix(as.matrix(swstrpctpit_AA %>% select(-SwStr_pct_AA)), label = swstrpctpit_AA$SwStr_pct_AA)


hyperparam_swstrpctpit_aa_tuning_reg <- function(max_depth_swstrpctpit_aplus, weight_swstrpctpit_aplus,subsample_swstrpctpit_aplus, row_num_swstrpctpit_aplus){
  
  print(paste('Max Depth: ', max_depth_swstrpctpit_aplus))
  print(paste('Weight: ', weight_swstrpctpit_aplus))
  print(paste('Subsample: ', subsample_swstrpctpit_aplus))
  print(paste('Row Number: ', row_num_swstrpctpit_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_swstrpctpit_aplus,
      min_child_weight = weight_swstrpctpit_aplus,
      subsample = subsample_swstrpctpit_aplus
    ),
    data = dtrain_swstrpctpit_aa,
    nrounds = 2000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_swstrpctpit_aa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_swstrpctpit_aa_df$rmse <- pmap_dbl(list(reg_tuning_swstrpctpit_aa_df$max_depth, reg_tuning_swstrpctpit_aa_df$weight,
                                           reg_tuning_swstrpctpit_aa_df$subsample, reg_tuning_swstrpctpit_aa_df$row_num),
                                      hyperparam_swstrpctpit_aa_tuning_reg, .progress = TRUE)

reg_tuning_swstrpctpit_aa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_swstrpctpit_aa_best <- reg_tuning_swstrpctpit_aa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_swstrpctpit_aa <- reg_tuning_swstrpctpit_aa_best$max_depth # 8
weight_swstrpctpit_aa <- reg_tuning_swstrpctpit_aa_best$weight # 4
subsample_swstrpctpit_aa <- reg_tuning_swstrpctpit_aa_best$subsample # 1

reg_tuning_swstrpctpit_aa_best$rmse/sd(swstrpctpit_AA$SwStr_pct_AA) #0.842

### SwStr% AA to AAA ####
swstrpctpit_AAA <- minor_league_pitching_AAA %>% 
  drop_na(Name_AA) %>%
  rowwise() %>% 
  reframe(
    SwStr_pct_AAA,
    top_100_rank = mean(c(rank_ba_AA, rank_mlb_AA)),
    team_rank = team_rank_ba_AA,
    Age_AA,
    SwStr_pct_AA,
    K_pct_AA,
    ERA_AA
  ) %>% 
  ungroup()

dtrain_swstrpctpit_aaa <- xgb.DMatrix(as.matrix(swstrpctpit_AAA %>% select(-SwStr_pct_AAA)), label = swstrpctpit_AAA$SwStr_pct_AAA)


hyperparam_swstrpctpit_aaa_tuning_reg <- function(max_depth_swstrpctpit_aplus, weight_swstrpctpit_aplus,subsample_swstrpctpit_aplus, row_num_swstrpctpit_aplus){
  
  print(paste('Max Depth: ', max_depth_swstrpctpit_aplus))
  print(paste('Weight: ', weight_swstrpctpit_aplus))
  print(paste('Subsample: ', subsample_swstrpctpit_aplus))
  print(paste('Row Number: ', row_num_swstrpctpit_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_swstrpctpit_aplus,
      min_child_weight = weight_swstrpctpit_aplus,
      subsample = subsample_swstrpctpit_aplus
    ),
    data = dtrain_swstrpctpit_aaa,
    nrounds = 2000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_swstrpctpit_aaa_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_swstrpctpit_aaa_df$rmse <- pmap_dbl(list(reg_tuning_swstrpctpit_aaa_df$max_depth, reg_tuning_swstrpctpit_aaa_df$weight,
                                            reg_tuning_swstrpctpit_aaa_df$subsample, reg_tuning_swstrpctpit_aaa_df$row_num),
                                       hyperparam_swstrpctpit_aaa_tuning_reg, .progress = TRUE)

reg_tuning_swstrpctpit_aaa_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_swstrpctpit_aaa_best <- reg_tuning_swstrpctpit_aaa_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_swstrpctpit_aaa <- reg_tuning_swstrpctpit_aaa_best$max_depth # 8
weight_swstrpctpit_aaa <- reg_tuning_swstrpctpit_aaa_best$weight # 4
subsample_swstrpctpit_aaa <- reg_tuning_swstrpctpit_aaa_best$subsample # 1

reg_tuning_swstrpctpit_aaa_best$rmse/sd(swstrpctpit_AAA$SwStr_pct_AAA) #0.704


### SwStr% AAA to MLB ####
swstrpctpit_mlb <- mlb_sp_pitching %>%
  bind_rows(mlb_rp_pitching %>% 
              rename('SwStr_pct_MLB' = 'SwStr_pct',
                     'rank_ba' = 'rank_ba_AAA', 
                     'rank_mlb' = 'rank_mlb_AAA',
                     'team_rank_ba' = 'team_rank_ba_AAA')) %>% 
  drop_na(Name_AAA) %>% 
  rowwise() %>% 
  reframe(
    SwStr_pct_MLB,
    top_100_rank = mean(c(rank_ba, rank_mlb)),
    team_rank = team_rank_ba,
    Age_AAA,
    SwStr_pct_AAA,
    K_pct_AAA,
    ERA_AAA
  ) %>% 
  ungroup()


dtrain_swstrpctpit_mlb <- xgb.DMatrix(as.matrix(swstrpctpit_mlb %>% select(-SwStr_pct_MLB)), label = swstrpctpit_mlb$SwStr_pct_MLB)


hyperparam_swstrpctpit_mlb_tuning_reg <- function(max_depth_swstrpctpit_aplus, weight_swstrpctpit_aplus,subsample_swstrpctpit_aplus, row_num_swstrpctpit_aplus = 1){
  
  print(paste('Max Depth: ', max_depth_swstrpctpit_aplus))
  print(paste('Weight: ', weight_swstrpctpit_aplus))
  print(paste('Subsample: ', subsample_swstrpctpit_aplus))
  print(paste('Row Number: ', row_num_swstrpctpit_aplus))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'reg:squarederror',
      eval_metric = 'rmse',
      alpha = 5,
      max_depth = max_depth_swstrpctpit_aplus,
      min_child_weight = weight_swstrpctpit_aplus,
      subsample = subsample_swstrpctpit_aplus
    ),
    data = dtrain_swstrpctpit_mlb,
    nrounds = 2000,
    nfold = 8,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 100,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_rmse_mean)
  
  return(rmse)
  
}

reg_tuning_swstrpctpit_mlb_df <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 4, 10),
  subsample = c(0.6, 0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_swstrpctpit_mlb_df$rmse <- pmap_dbl(list(reg_tuning_swstrpctpit_mlb_df$max_depth, reg_tuning_swstrpctpit_mlb_df$weight,
                                            reg_tuning_swstrpctpit_mlb_df$subsample, reg_tuning_swstrpctpit_mlb_df$row_num),
                                       hyperparam_swstrpctpit_mlb_tuning_reg, .progress = TRUE)

reg_tuning_swstrpctpit_mlb_df %>% 
  arrange(rmse) %>% 
  head(5)

reg_tuning_swstrpctpit_mlb_best <- reg_tuning_swstrpctpit_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_swstrpctpit_mlb <- reg_tuning_swstrpctpit_mlb_best$max_depth # 4
weight_swstrpctpit_mlb <- reg_tuning_swstrpctpit_mlb_best$weight # 4
subsample_swstrpctpit_mlb <- reg_tuning_swstrpctpit_mlb_best$subsample # 1

reg_tuning_swstrpctpit_mlb_best$rmse/sd(swstrpctpit_mlb$SwStr_pct_MLB) #0.902 #XGBoost Better

hyperparam_swstrpctpit_mlb_tuning_reg(8,1,1) #1537

set.seed(101);swstr_pitmlb_mod <- xgboost(
  params = list(
    eta = 0.01,
    objective = 'reg:squarederror',
    eval_metric = 'rmse',
    alpha = 5,
    max_depth = max_depth_swstrpctpit_mlb,
    min_child_weight = weight_swstrpctpit_mlb,
    subsample = subsample_swstrpctpit_mlb
  ),
  data = dtrain_swstrpctpit_mlb,
  nrounds = 1537,
  print_every_n = 100,
  nthread = 7
) 

shap.plot.summary.wrap1(swstr_pitmlb_mod, as.matrix(swstrpctpit_mlb %>% select(-SwStr_pct_MLB)))
