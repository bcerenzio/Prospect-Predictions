library(xgboost)
library(SHAPforxgboost)
library(tidyverse)
library(progress)
library(gridExtra)
library(gt)
library(yardstick)

slice <- dplyr::slice

#### Made MLB Batters ####
# Single A hitting Made MLB Model ####
made_mlb_A_hitting_xg <- minor_league_hitting_A %>% 
  left_join(first_minor_league_season_hitting_A, by = c('PlayerId', 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_hitting_A_median) %>% 
  reframe(
    made_mlb,
    Age,
    PA,
    team_rank_ba,
    avg_100_rank = (rank_ba + rank_mlb)/2,
    wRC_plus,
    BB_minus_K_pct = BB_pct - K_pct,
    Pull_pct,
    GB_per_FB,
    SwStr_pct
  )

made_mlb_A_train <- xgb.DMatrix(as.matrix(made_mlb_A_hitting_xg %>% select(-made_mlb)), label = made_mlb_A_hitting_xg$made_mlb)

scale_pos_weight_A_hit <- sum(made_mlb_A_hitting_xg$made_mlb == 0)/sum(made_mlb_A_hitting_xg$made_mlb == 1)

hyperparam_made_mlb_A_tuning_reg <- function(max_depth, weight,subsample, gamma, alpha, row_num = 1){
  
  print(paste('Max Depth: ', max_depth))
  print(paste('Weight: ', weight))
  print(paste('Subsample: ', subsample))
  print(paste('Alpha: ', alpha))
  print(paste('Gamma: ', gamma))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'binary:logistic',
      eval_metric = 'logloss',
      max_depth = max_depth,
      min_child_weight = weight,
      gamma = gamma,
      alpha = alpha,
      subsample = subsample,
      scale_pos_weight = scale_pos_weight_A_hit
    ),
    data = made_mlb_A_train,
    nrounds = 200000,
    nfold = 10,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 50,
    #maximize = TRUE,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_logloss_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_logloss_mean)
  
  return(rmse)
  
}

reg_tuning_made_mlb_hit_A <- expand_grid(
  max_depth = c(6,4,2),
  weight = c(1, 5, 10),
  subsample = c(0.6,0.8, 1),
  alpha = c(1,5,10),
  gamma = c(1,5,10)
) %>% 
  mutate(row_num = row_number())


reg_tuning_made_mlb_hit_A$logloss <- pmap_dbl(list(reg_tuning_made_mlb_hit_A$max_depth, reg_tuning_made_mlb_hit_A$weight,
                                              reg_tuning_made_mlb_hit_A$subsample, reg_tuning_made_mlb_hit_A$gamma,
                                              reg_tuning_made_mlb_hit_A$alpha,
                                              reg_tuning_made_mlb_hit_A$row_num),
                                         hyperparam_made_mlb_A_tuning_reg, .progress = TRUE)

reg_tuning_made_mlb_hit_A %>% 
  arrange(logloss) %>% 
  head(5)

reg_tuning_made_mlb_hit_A_best <- reg_tuning_made_mlb_hit_A %>% 
  slice_min(logloss, n = 1) %>% 
  dplyr::slice(1)

(max_depth_made_mlb_hitA <- reg_tuning_made_mlb_hit_A_best$max_depth) #6
(weight_made_mlb_hitA <- reg_tuning_made_mlb_hit_A_best$weight) # 1
(subsample_made_mlb_hitA <- reg_tuning_made_mlb_hit_A_best$subsample) #0.6
(alpha_made_mlb_hitA <- reg_tuning_made_mlb_hit_A_best$alpha) #1
(gamma_made_mlb_hitA <- reg_tuning_made_mlb_hit_A_best$gamma) #1

reg_tuning_made_mlb_hit_A_best$logloss # 0.320

hyperparam_made_mlb_A_tuning_reg(6,1,0.6,1,1) #1067


# A+ Hitting Made MLB Model ####
made_mlb_Aplus_hitting_xg <- minor_league_hitting_Aplus %>% 
  left_join(first_minor_league_season_hitting_Aplus, by = c('PlayerId', 'Name_Aplus' = 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_hitting_Aplus_median) %>% 
  reframe(
    made_mlb,
    Age_Aplus,
    PA_Aplus,
    team_rank_ba_Aplus,
    avg_100_rank = (rank_ba_Aplus + rank_mlb_Aplus)/2,
    wRC_plus_Aplus,
    BB_minus_K_pct = BB_pct_Aplus - K_pct_Aplus,
    Pull_pct_Aplus,
    SwStr_pct_Aplus,
    GB_per_FB_Aplus
  )

made_mlb_Aplus_train <- xgb.DMatrix(as.matrix(made_mlb_Aplus_hitting_xg %>% select(-made_mlb)), label = made_mlb_Aplus_hitting_xg$made_mlb)

scale_pos_weight_Aplus_hit <- sum(made_mlb_Aplus_hitting_xg$made_mlb == 0)/sum(made_mlb_Aplus_hitting_xg$made_mlb == 1)


hyperparam_made_mlb_Aplus_tuning_reg <- function(max_depth, weight,subsample, gamma, alpha, row_num = 1){
  
  print(paste('Max Depth: ', max_depth))
  print(paste('Weight: ', weight))
  print(paste('Subsample: ', subsample))
  print(paste('Alpha: ', alpha))
  print(paste('Gamma: ', gamma))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'binary:logistic',
      eval_metric = 'logloss',
      max_depth = max_depth,
      min_child_weight = weight,
      gamma = gamma,
      alpha = alpha,
      subsample = subsample,
      scale_pos_weight = scale_pos_weight_Aplus_hit
    ),
    data = made_mlb_Aplus_train,
    nrounds = 200000,
    nfold = 10,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 50,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_logloss_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_logloss_mean)
  
  return(rmse)
  
}

reg_tuning_made_mlb_hit_Aplus <- expand_grid(
  max_depth = c(6,4,2),
  weight = c(1, 5, 10),
  subsample = c(0.6,0.8, 1),
  alpha = c(1,5,10),
  gamma = c(1,5,10)
) %>% 
  mutate(row_num = row_number())


reg_tuning_made_mlb_hit_Aplus$logloss <- pmap_dbl(list(reg_tuning_made_mlb_hit_Aplus$max_depth, reg_tuning_made_mlb_hit_Aplus$weight,
                                                 reg_tuning_made_mlb_hit_Aplus$subsample, reg_tuning_made_mlb_hit_Aplus$gamma,
                                                 reg_tuning_made_mlb_hit_Aplus$alpha,
                                                 reg_tuning_made_mlb_hit_Aplus$row_num),
                                            hyperparam_made_mlb_Aplus_tuning_reg, .progress = TRUE)

reg_tuning_made_mlb_hit_Aplus %>% 
  arrange(logloss) %>% 
  head(5)

reg_tuning_made_mlb_hit_Aplus_best <- reg_tuning_made_mlb_hit_Aplus %>% 
  slice_min(logloss, n = 1) %>% 
  dplyr::slice(1)

(max_depth_made_mlb_hitAplus <- reg_tuning_made_mlb_hit_Aplus_best$max_depth) # 6
(weight_made_mlb_hitAplus <- reg_tuning_made_mlb_hit_Aplus_best$weight) # 1
(subsample_made_mlb_hitAplus <- reg_tuning_made_mlb_hit_Aplus_best$subsample) # 0.6
(alpha_made_mlb_hitAplus <- reg_tuning_made_mlb_hit_Aplus_best$alpha) #1
(gamma_made_mlb_hitAplus <- reg_tuning_made_mlb_hit_Aplus_best$gamma) #1

reg_tuning_made_mlb_hit_Aplus_best$logloss # 0.330

hyperparam_made_mlb_Aplus_tuning_reg(6,1,0.6,1,1) #1080 rounds

# AA Hitting Made MLB Model ####
made_mlb_AA_hitting_xg <- minor_league_hitting_AA %>% 
  left_join(first_minor_league_season_hitting_AA, by = c('PlayerId', 'Name_AA' = 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_hitting_AA_median) %>% 
  reframe(
    made_mlb,
    Age_AA,
    PA_AA,
    team_rank_ba_AA,
    avg_100_rank = (rank_ba_AA + rank_mlb_AA)/2,
    wRC_plus_AA,
    BB_minus_K_pct = BB_pct_AA - K_pct_AA,
    Pull_pct_AA,
    SwStr_pct_AA,
    GB_per_FB_AA
  )

made_mlb_AA_train <- xgb.DMatrix(as.matrix(made_mlb_AA_hitting_xg %>% select(-made_mlb)), label = made_mlb_AA_hitting_xg$made_mlb)

scale_pos_weight_AA_hit <- sum(made_mlb_AA_hitting_xg$made_mlb == 0)/sum(made_mlb_AA_hitting_xg$made_mlb == 1)

hyperparam_made_mlb_AA_tuning_reg <- function(max_depth, weight,subsample, gamma, alpha, row_num = 1){
  
  print(paste('Max Depth: ', max_depth))
  print(paste('Weight: ', weight))
  print(paste('Subsample: ', subsample))
  print(paste('Alpha: ', alpha))
  print(paste('Gamma: ', gamma))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'binary:logistic',
      eval_metric = 'logloss',
      max_depth = max_depth,
      min_child_weight = weight,
      gamma = gamma,
      alpha = alpha,
      subsample = subsample,
      scale_pos_weight = scale_pos_weight_AA_hit
    ),
    data = made_mlb_AA_train,
    nrounds = 200000,
    nfold = 10,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 50,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_logloss_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_logloss_mean)
  
  return(rmse)
  
}

reg_tuning_made_mlb_hit_AA <- expand_grid(
  max_depth = c(6,4,2),
  weight = c(1, 5, 10),
  subsample = c(0.6,0.8, 1),
  alpha = c(1,5,10),
  gamma = c(1,5,10)
) %>% 
  mutate(row_num = row_number())


reg_tuning_made_mlb_hit_AA$logloss <- pmap_dbl(list(reg_tuning_made_mlb_hit_AA$max_depth, reg_tuning_made_mlb_hit_AA$weight,
                                                     reg_tuning_made_mlb_hit_AA$subsample, reg_tuning_made_mlb_hit_AA$gamma,
                                                     reg_tuning_made_mlb_hit_AA$alpha,
                                                     reg_tuning_made_mlb_hit_AA$row_num),
                                                hyperparam_made_mlb_AA_tuning_reg, .progress = TRUE)

reg_tuning_made_mlb_hit_AA %>% 
  arrange(logloss) %>% 
  head(5)

reg_tuning_made_mlb_hit_AA_best <- reg_tuning_made_mlb_hit_AA %>% 
  slice_min(logloss, n = 1) %>% 
  dplyr::slice(1)

(max_depth_made_mlb_hitAA <- reg_tuning_made_mlb_hit_AA_best$max_depth) # 6
(weight_made_mlb_hitAA <- reg_tuning_made_mlb_hit_AA_best$weight) # 1
(subsample_made_mlb_hitAA <- reg_tuning_made_mlb_hit_AA_best$subsample) # 0.6
(alpha_made_mlb_hitAA <- reg_tuning_made_mlb_hit_AA_best$alpha) # 1
(gamma_made_mlb_hitAA <- reg_tuning_made_mlb_hit_AA_best$gamma) # 1

reg_tuning_made_mlb_hit_AA_best$logloss # 0.342

hyperparam_made_mlb_AA_tuning_reg(6,1,0.6,1,1) #573 rounds

# AAA Hitting Made MLB Model ####
made_mlb_AAA_hitting_xg <- minor_league_hitting_AAA %>% 
  left_join(first_minor_league_season_hitting_AAA, by = c('PlayerId', 'Name_AAA' = 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_hitting_AAA_median) %>% 
  reframe(
    made_mlb,
    Age_AAA,
    PA_AAA,
    team_rank_ba_AAA,
    avg_100_rank = (rank_ba_AAA + rank_mlb_AAA)/2,
    wRC_plus_AAA,
    BB_minus_K_pct = BB_pct_AAA - K_pct_AAA,
    Pull_pct_AAA,
    SwStr_pct_AAA,
    GB_per_FB_AAA
  )

made_mlb_AAA_train <- xgb.DMatrix(as.matrix(made_mlb_AAA_hitting_xg %>% select(-made_mlb)), label = made_mlb_AAA_hitting_xg$made_mlb)

scale_pos_weight_AAA_hit <- sum(made_mlb_AAA_hitting_xg$made_mlb == 0)/sum(made_mlb_AAA_hitting_xg$made_mlb == 1)

hyperparam_made_mlb_AAA_tuning_reg <- function(max_depth, weight,subsample, lambda, row_num = 1){
  
  print(paste('Max Depth: ', max_depth))
  print(paste('Weight: ', weight))
  print(paste('Subsample: ', subsample))
  print(paste('Lambda: ', lambda))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.01,
      objective = 'binary:logistic',
      eval_metric = 'logloss',
      max_depth = max_depth,
      min_child_weight = weight,
      lambda = lambda,
      alpha = 1,
      subsample = subsample,
      scale_pos_weight = scale_pos_weight_AAA_hit
    ),
    data = made_mlb_AAA_train,
    nrounds = 200000,
    nfold = 10,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 50,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_logloss_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_logloss_mean)
  
  return(rmse)
  
}

reg_tuning_made_mlb_hit_AAA <- expand_grid(
  max_depth = c(8,6,4),
  weight = c(1, 5, 10),
  subsample = c(0.6,0.8, 1),
  lambda = c(5,10,15)
) %>% 
  mutate(row_num = row_number())


reg_tuning_made_mlb_hit_AAA$logloss <- pmap_dbl(list(reg_tuning_made_mlb_hit_AAA$max_depth, reg_tuning_made_mlb_hit_AAA$weight,
                                                     reg_tuning_made_mlb_hit_AAA$subsample, reg_tuning_made_mlb_hit_AAA$lambda,
                                                     reg_tuning_made_mlb_hit_AAA$row_num),
                                                hyperparam_made_mlb_AAA_tuning_reg, .progress = TRUE)

reg_tuning_made_mlb_hit_AAA %>% 
  arrange(logloss) %>% 
  head(5)

reg_tuning_made_mlb_hit_AAA_best <- reg_tuning_made_mlb_hit_AAA %>% 
  slice_min(logloss, n = 1) %>% 
  dplyr::slice(1)

(max_depth_made_mlb_hitAAA <- reg_tuning_made_mlb_hit_AAA_best$max_depth) # 8
(weight_made_mlb_hitAAA <- reg_tuning_made_mlb_hit_AAA_best$weight) # 1
(subsample_made_mlb_hitAAA <- reg_tuning_made_mlb_hit_AAA_best$subsample) # 0.6
(lambda_made_mlb_hitAAA <- reg_tuning_made_mlb_hit_AAA_best$lambda) #5

reg_tuning_made_mlb_hit_AAA_best$logloss # 0.332

hyperparam_made_mlb_AAA_tuning_reg(8,1,0.6,5) # 675 trees

#### Made MLB Pitches (Multiclass) ####
# Single A Pitchers Made MLB Model ####
made_mlb_pit_A <- minor_league_pitching_A %>% 
  mutate(made_mlb_multiclass = case_when(
    made_mlb_sp == 0 & made_mlb_rp == 0 ~ 0,
    made_mlb_sp == 0 & made_mlb_rp == 1 ~ 1,
    made_mlb_sp == 1 & made_mlb_rp == 1 ~ 2, #only a few instances across all minor league levls; setting it to made_mlb_sp
    made_mlb_sp == 1 & made_mlb_rp == 0 ~ 2
  )) %>% 
  left_join(first_minor_league_season_pit_A, by = c('PlayerId', 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_sp_A_median) %>% 
  reframe(
    made_mlb_multiclass,
    avg_100_rank = (rank_ba + rank_mlb)/2,
    team_rank_ba,
    Age,
    GS,
    ERA,
    FIP,
    GB_pct,
    K_minus_BB_pct = K_pct - BB_pct,
    SwStr_pct
  )


made_mlb_Apit_train <- xgb.DMatrix(as.matrix(made_mlb_pit_A %>% select(-made_mlb_multiclass)), label = made_mlb_pit_A$made_mlb_multiclass)


hyperparam_made_mlb_Apit_tuning_reg <- function(max_depth, weight,subsample, row_num = 1){
  
  print(paste('Max Depth: ', max_depth))
  print(paste('Weight: ', weight))
  print(paste('Subsample: ', subsample))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.05,
      objective = 'multi:softprob',
      eval_metric = 'mlogloss',
      max_depth = max_depth,
      min_child_weight = weight,
      subsample = subsample,
      gamma = 5,
      num_class = 3
    ),
    data = made_mlb_Apit_train,
    nrounds = 200000,
    nfold = 10,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 200,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_mlogloss_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_mlogloss_mean)
  
  return(rmse)
  
}

reg_tuning_made_mlb_pit_A <- expand_grid(
  max_depth = c(6,4,2),
  weight = c(1, 5, 10),
  subsample = c(0.6,0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_made_mlb_pit_A$mlogloss <- pmap_dbl(list(reg_tuning_made_mlb_pit_A$max_depth, reg_tuning_made_mlb_pit_A$weight,
                                                    reg_tuning_made_mlb_pit_A$subsample,reg_tuning_made_mlb_pit_A$row_num),
                                               hyperparam_made_mlb_Apit_tuning_reg, .progress = TRUE)

reg_tuning_made_mlb_pit_A %>% 
  arrange(mlogloss) %>% 
  head(5)

reg_tuning_made_mlb_pit_A_best <- reg_tuning_made_mlb_pit_A %>% 
  slice_min(mlogloss, n = 1) %>% 
  dplyr::slice(1)

(max_depth_made_mlb_pitA <- reg_tuning_made_mlb_pit_A_best$max_depth) #6
(weight_made_mlb_pitA <- reg_tuning_made_mlb_pit_A_best$weight) # 10
(subsample_made_mlb_pitA <- reg_tuning_made_mlb_pit_A_best$subsample) #0.8

reg_tuning_made_mlb_pit_A_best$mlogloss # 0.506
hyperparam_made_mlb_Apit_tuning_reg(6,10,0.8) #754 trees


# A+ Pitchers Made MLB Model ####
made_mlb_pit_Aplus <- minor_league_pitching_Aplus %>% 
  mutate(made_mlb_multiclass = case_when(
    made_mlb_sp == 0 & made_mlb_rp == 0 ~ 0,
    made_mlb_sp == 0 & made_mlb_rp == 1 ~ 1,
    made_mlb_sp == 1 & made_mlb_rp == 1 ~ 2, #only a few instances across all minor league levls; setting it to made_mlb_sp
    made_mlb_sp == 1 & made_mlb_rp == 0 ~ 2
  )) %>% 
  left_join(first_minor_league_season_pit_Aplus, by = c('PlayerId', 'Name_Aplus' = 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_sp_Aplus_median) %>% 
  reframe(
    made_mlb_multiclass,
    avg_100_rank = (rank_ba_Aplus + rank_mlb_Aplus)/2,
    team_rank_ba_Aplus,
    Age_Aplus,
    GS_Aplus,
    ERA_Aplus,
    FIP_Aplus,
    GB_pct_Aplus,
    K_minus_BB_pct = K_pct_Aplus - BB_pct_Aplus,
    SwStr_pct_Aplus
  )

made_mlb_Apluspit_train <- xgb.DMatrix(as.matrix(made_mlb_pit_Aplus %>% select(-made_mlb_multiclass)), label = made_mlb_pit_Aplus$made_mlb_multiclass)

hyperparam_made_mlb_Apluspit_tuning_reg <- function(max_depth, weight,subsample, row_num = 1){
  
  print(paste('Max Depth: ', max_depth))
  print(paste('Weight: ', weight))
  print(paste('Subsample: ', subsample))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.05,
      objective = 'multi:softprob',
      eval_metric = 'mlogloss',
      max_depth = max_depth,
      min_child_weight = weight,
      subsample = subsample,
      gamma = 5,
      num_class = 3
    ),
    data = made_mlb_Apluspit_train,
    nrounds = 200000,
    nfold = 10,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 200,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_mlogloss_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_mlogloss_mean)
  
  return(rmse)
  
}

reg_tuning_made_mlb_pit_Aplus <- expand_grid(
  max_depth = c(6,4,2),
  weight = c(1, 5, 10),
  subsample = c(0.6,0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_made_mlb_pit_Aplus$mlogloss <- pmap_dbl(list(reg_tuning_made_mlb_pit_Aplus$max_depth, reg_tuning_made_mlb_pit_Aplus$weight,
                                                        reg_tuning_made_mlb_pit_Aplus$subsample,reg_tuning_made_mlb_pit_Aplus$row_num),
                                               hyperparam_made_mlb_Apluspit_tuning_reg, .progress = TRUE)

reg_tuning_made_mlb_pit_Aplus %>% 
  arrange(mlogloss) %>% 
  head(5)

reg_tuning_made_mlb_pit_Aplus_best <- reg_tuning_made_mlb_pit_Aplus %>% 
  slice_min(mlogloss, n = 1) %>% 
  dplyr::slice(1)

(max_depth_made_mlb_pitAplus <- reg_tuning_made_mlb_pit_Aplus_best$max_depth) #
(weight_made_mlb_pitAplus <- reg_tuning_made_mlb_pit_Aplus_best$weight) # 
(subsample_made_mlb_pitAplus <- reg_tuning_made_mlb_pit_Aplus_best$subsample) #

reg_tuning_made_mlb_pit_Aplus_best$mlogloss # 


# AA Pitchers Made MLB Model ####
made_mlb_pit_AA <- minor_league_pitching_AA %>% 
  mutate(made_mlb_multiclass = case_when(
    made_mlb_sp == 0 & made_mlb_rp == 0 ~ 0,
    made_mlb_sp == 0 & made_mlb_rp == 1 ~ 1,
    made_mlb_sp == 1 & made_mlb_rp == 1 ~ 2, #only a few instances across all minor league levls; setting it to made_mlb_sp
    made_mlb_sp == 1 & made_mlb_rp == 0 ~ 2
  )) %>% 
  left_join(first_minor_league_season_pit_AA, by = c('PlayerId', 'Name_AA' = 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_sp_AA_median) %>% 
  reframe(
    made_mlb_multiclass,
    avg_100_rank = (rank_ba_AA + rank_mlb_AA)/2,
    team_rank_ba_AA,
    Age_AA,
    GS_AA,
    ERA_AA,
    FIP_AA,
    GB_pct_AA,
    K_minus_BB_pct = K_pct_AA - BB_pct_AA,
    SwStr_pct_AA
  )

made_mlb_AApit_train <- xgb.DMatrix(as.matrix(made_mlb_pit_AA %>% select(-made_mlb_multiclass)), label = made_mlb_pit_AA$made_mlb_multiclass)

hyperparam_made_mlb_AApit_tuning_reg <- function(max_depth, weight,subsample, row_num = 1){
  
  print(paste('Max Depth: ', max_depth))
  print(paste('Weight: ', weight))
  print(paste('Subsample: ', subsample))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.05,
      objective = 'multi:softprob',
      eval_metric = 'mlogloss',
      max_depth = max_depth,
      min_child_weight = weight,
      subsample = subsample,
      gamma = 5,
      num_class = 3
    ),
    data = made_mlb_AApit_train,
    nrounds = 200000,
    nfold = 10,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 200,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_mlogloss_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_mlogloss_mean)
  
  return(rmse)
  
}

reg_tuning_made_mlb_pit_AA <- expand_grid(
  max_depth = c(6,4,2),
  weight = c(1, 5, 10),
  subsample = c(0.6,0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_made_mlb_pit_AA$mlogloss <- pmap_dbl(list(reg_tuning_made_mlb_pit_AA$max_depth, reg_tuning_made_mlb_pit_AA$weight,
                                                        reg_tuning_made_mlb_pit_AA$subsample,reg_tuning_made_mlb_pit_AA$row_num),
                                                   hyperparam_made_mlb_AApit_tuning_reg, .progress = TRUE)

reg_tuning_made_mlb_pit_AA %>% 
  arrange(mlogloss) %>% 
  head(5)

reg_tuning_made_mlb_pit_AA_best <- reg_tuning_made_mlb_pit_AA %>% 
  slice_min(mlogloss, n = 1) %>% 
  dplyr::slice(1)

(max_depth_made_mlb_pitAA <- reg_tuning_made_mlb_pit_AA_best$max_depth) #
(weight_made_mlb_pitAA <- reg_tuning_made_mlb_pit_AA_best$weight) # 
(subsample_made_mlb_pitAA <- reg_tuning_made_mlb_pit_AA_best$subsample) #

reg_tuning_made_mlb_pit_AA_best$mlogloss # 


# AAA Pitchers Made MLB Model ####
made_mlb_pit_AAA <- minor_league_pitching_AAA %>% 
  mutate(made_mlb_multiclass = case_when(
    made_mlb_sp == 0 & made_mlb_rp == 0 ~ 0,
    made_mlb_sp == 0 & made_mlb_rp == 1 ~ 1,
    made_mlb_sp == 1 & made_mlb_rp == 1 ~ 2, #only a few instances across all minor league levls; setting it to made_mlb_sp
    made_mlb_sp == 1 & made_mlb_rp == 0 ~ 2
  )) %>% left_join(first_minor_league_season_pit_AAA, by = c('PlayerId', 'Name_AAA' = 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_sp_AAA_median) %>% 
  reframe(
    made_mlb_multiclass,
    avg_100_rank = (rank_ba_AAA + rank_mlb_AAA)/2,
    team_rank_ba_AAA,
    Age_AAA,
    GS_AAA,
    ERA_AAA,
    FIP_AAA,
    GB_pct_AAA,
    K_minus_BB_pct = K_pct_AAA - BB_pct_AAA,
    SwStr_pct_AAA
  )

made_mlb_AAApit_train <- xgb.DMatrix(as.matrix(made_mlb_pit_AAA %>% select(-made_mlb_multiclass)), label = made_mlb_pit_AAA$made_mlb_multiclass)

hyperparam_made_mlb_AAApit_tuning_reg <- function(max_depth, weight,subsample, row_num = 1){
  
  print(paste('Max Depth: ', max_depth))
  print(paste('Weight: ', weight))
  print(paste('Subsample: ', subsample))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.cv(
    params = list(
      eta = 0.05,
      objective = 'multi:softprob',
      eval_metric = 'mlogloss',
      max_depth = max_depth,
      min_child_weight = weight,
      subsample = subsample,
      gamma = 5,
      num_class = 3
    ),
    data = made_mlb_AAApit_train,
    nrounds = 200000,
    nfold = 10,
    seed = 101,
    print_every_n = 100,
    early_stopping_rounds = 200,
    nthread = 7
  ) 
  rmse <- mod$evaluation_log %>% 
    slice_min(test_mlogloss_mean, n = 1) %>% 
    slice(1) %>% 
    pull(test_mlogloss_mean)
  
  return(rmse)
  
}

reg_tuning_made_mlb_pit_AAA <- expand_grid(
  max_depth = c(6,4,2),
  weight = c(1, 5, 10),
  subsample = c(0.6,0.8, 1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_made_mlb_pit_AAA$mlogloss <- pmap_dbl(list(reg_tuning_made_mlb_pit_AAA$max_depth, reg_tuning_made_mlb_pit_AAA$weight,
                                                        reg_tuning_made_mlb_pit_AAA$subsample,reg_tuning_made_mlb_pit_AAA$row_num),
                                                   hyperparam_made_mlb_AAApit_tuning_reg, .progress = TRUE)

reg_tuning_made_mlb_pit_AAA %>% 
  arrange(mlogloss) %>% 
  head(5)

reg_tuning_made_mlb_pit_AAA_best <- reg_tuning_made_mlb_pit_AAA %>% 
  slice_min(mlogloss, n = 1) %>% 
  dplyr::slice(1)

(max_depth_made_mlb_pitAAA <- reg_tuning_made_mlb_pit_AAA_best$max_depth) #
(weight_made_mlb_pitAAA <- reg_tuning_made_mlb_pit_AAA_best$weight) # 
(subsample_made_mlb_pitAAA <- reg_tuning_made_mlb_pit_AAA_best$subsample) #

reg_tuning_made_mlb_pit_AAA_best$mlogloss # 



#### Final/SHAP Models ####
# Single A Hitters #
made_mlb_hitA <- xgboost(
  params = list(
    eta = 0.01,
    objective = 'binary:logistic',
    eval_metric = 'logloss',
    max_depth = max_depth_made_mlb_hitA,
    min_child_weight = weight_made_mlb_hitA,
    alpha = 1,
    lambda = lambda_made_mlb_hitA,
    subsample = subsample_made_mlb_hitA,
    scale_pos_weight = scale_pos_weight_A_hit
  ),
  data = made_mlb_A_train,
  nrounds = 1067,
  print_every_n = 100,
  #maximize = TRUE,
  nthread = 7
) 

(made_mlb_hitA_shap_plot <- shap.plot.summary.wrap1(made_mlb_hitA, as.matrix(made_mlb_A_hitting_xg %>% select(-made_mlb)))) 

made_mlb_hitA_shap_plot_labeled <- made_mlb_hitA_shap_plot + labs(title = 'Made MLB SHAP Plot (Level A Hitters)')

shap.plot.dependence(shap.prep(made_mlb_hitA, 
                               X_train = as.matrix(made_mlb_A_hitting_xg %>% select(-made_mlb))),
                     x = 'wRC_plus')

# confusion matrix
set.seed(101);made_mlb_hitA_confmat <- sample(1:nrow(made_mlb_A_hitting_xg), size = nrow(made_mlb_A_hitting_xg)*0.5)

made_mlb_hitA_confmat_df <- made_mlb_A_hitting_xg[made_mlb_hitA_confmat,]
hitA_confmat_test <- made_mlb_A_hitting_xg[-made_mlb_hitA_confmat,]

made_mlb_hitA_confmat_xg <- xgb.DMatrix(data = as.matrix(made_mlb_hitA_confmat_df %>% select(-made_mlb)), label = made_mlb_hitA_confmat_df$made_mlb)

set.seed(101);made_mlb_A_confmat <- xgboost(
  params = list(
    eta = 0.01,
    objective = 'binary:logistic',
    eval_metric = 'logloss',
    max_depth = max_depth_made_mlb_hitA,
    min_child_weight = weight_made_mlb_hitA,
    alpha = 1,
    lambda = lambda_made_mlb_hitA,
    subsample = subsample_made_mlb_hitA,
    scale_pos_weight = scale_pos_weight_A_hit
  ),
  data = made_mlb_hitA_confmat_xg,
  nrounds = 1067,
  print_every_n = 100,
  #maximize = TRUE,
  nthread = 7
) 

preds <- predict(object = made_mlb_A_confmat, as.matrix(hitA_confmat_test %>% select(-made_mlb)))

table('Truth' = hitA_confmat_test$made_mlb, 'Predictions' = ifelse(preds > 0.5, 1,0))

yardstick::accuracy_vec(truth = as.factor(hitA_confmat_test$made_mlb), as.factor(ifelse(preds > 0.5, 1,0)))
yardstick::pr_auc_vec(truth = as.factor(hitA_confmat_test$made_mlb),ifelse(preds > 0.5, 1,0))
yardstick::pr_auc_vec(truth = as.factor(hitA_confmat_test$made_mlb),ifelse(preds > 0.5, 1,0))/mean(hitA_confmat_test$made_mlb)

conf_mat_hitA <- as.data.frame(table('Truth' = ifelse(hitA_confmat_test$made_mlb == 1, 'Yes', 'No'), 'Predictions' = ifelse(preds > 0.5, 'Yes','No')))

conf_mat_hitA <- conf_mat_hitA %>% pivot_wider(names_from = Predictions, values_from = Freq, values_fill = 0)

gt(conf_mat_hitA) %>%
  cols_label(
    Truth = "Actual \\ Predicted",
    No = "No",
    Yes = "Yes"
  ) %>%
  tab_header(
    title = "Made MLB Confusion Matrix",
    subtitle = 'Hitters Level A'
  )

# A+ Hitters #
set.seed(101);made_mlb_hitAplus_mod <- xgboost(
  params = list(
    eta = 0.01,
    objective = 'binary:logistic',
    eval_metric = 'logloss',
    max_depth = max_depth_made_mlb_hitAplus,
    min_child_weight = weight_made_mlb_hitAplus,
    gamma = gamma_made_mlb_hitAplus,
    alpha = alpha_made_mlb_hitAplus,
    subsample = subsample_made_mlb_hitAplus
  ),
  data = made_mlb_Aplus_train,
  nrounds = 1080,
  print_every_n = 100,
  nthread = 7
) 

(made_mlb_hitAplus_shap_plot <- shap.plot.summary.wrap1(made_mlb_hitAplus_mod, as.matrix(made_mlb_Aplus_hitting_xg %>% select(-made_mlb)))) 

made_mlb_hitAplus_shap_plot_labeled <- made_mlb_hitAplus_shap_plot + labs(title = 'Made MLB SHAP Plot (Level A+ Hitters)')


# AA Hitters #
set.seed(101);made_mlb_hitAA_mod <- xgboost(
  params = list(
    eta = 0.01,
    objective = 'binary:logistic',
    eval_metric = 'logloss',
    max_depth = max_depth_made_mlb_hitAA,
    min_child_weight = weight_made_mlb_hitAA,
    gamma = gamma_made_mlb_hitAA,
    alpha = alpha_made_mlb_hitAA,
    subsample = subsample_made_mlb_hitAA
  ),
  data = made_mlb_AA_train,
  nrounds = 573,
  print_every_n = 100,
  maximize = TRUE,
  nthread = 7
) 

(made_mlb_hitAA_shap_plot <- shap.plot.summary.wrap1(made_mlb_hitAA_mod, as.matrix(made_mlb_AA_hitting_xg %>% select(-made_mlb)))) 

made_mlb_hitAA_shap_plot_labeled <- made_mlb_hitAA_shap_plot + labs(title = 'Made MLB SHAP Plot (Level AA Hitters)')


# AAA Hitters #
set.seed(101);made_mlb_hitAAA_mod <- xgboost(
  params = list(
    eta = 0.01,
    objective = 'binary:logistic',
    eval_metric = 'logloss',
    max_depth = max_depth_made_mlb_hitAAA,
    min_child_weight = weight_made_mlb_hitAAA,
    alpha = 1, 
    lambda = lambda_made_mlb_hitAAA,
    subsample = subsample_made_mlb_hitAAA
  ),
  data = made_mlb_AAA_train,
  nrounds = 522,
  print_every_n = 100,
  nthread = 7
) 

(made_mlb_hitAAA_shap_plot <- shap.plot.summary.wrap1(made_mlb_hitAAA_mod, as.matrix(made_mlb_AAA_hitting_xg %>% select(-made_mlb)))) 

made_mlb_hitAAA_shap_plot_labeled <- made_mlb_hitAAA_shap_plot + labs(title = 'Made MLB SHAP Plot (Level AAA Hitters)')

# confusion matrix
set.seed(101);made_mlb_hitAAA_confmat <- sample(1:nrow(made_mlb_AAA_hitting_xg), size = nrow(made_mlb_AAA_hitting_xg)*0.5)

made_mlb_hitAAA_confmat_df <- made_mlb_AAA_hitting_xg[made_mlb_hitAAA_confmat,]
hitAAA_confmat_test <- made_mlb_AAA_hitting_xg[-made_mlb_hitAAA_confmat,]

made_mlb_hitAAA_confmat_xg <- xgb.DMatrix(data = as.matrix(made_mlb_hitAAA_confmat_df %>% select(-made_mlb)), label = made_mlb_hitAAA_confmat_df$made_mlb)


set.seed(101);made_mlb_hitAAA_confmat <- xgboost(
  params = list(
    eta = 0.01,
    objective = 'binary:logistic',
    eval_metric = 'logloss',
    max_depth = max_depth_made_mlb_hitAAA,
    min_child_weight = weight_made_mlb_hitAAA,
    gamma = gamma_made_mlb_hitAAA,
    alpha = alpha_made_mlb_hitAAA,
    subsample = subsample_made_mlb_hitAAA
  ),
  data = made_mlb_hitAAA_confmat_xg,
  nrounds = 522,
  print_every_n = 100,
  nthread = 7
) 

preds_AAA <- predict(object = made_mlb_hitAAA_confmat, as.matrix(hitAAA_confmat_test %>% select(-made_mlb)))

table('Truth' = hitAAA_confmat_test$made_mlb, 'Predictions' = ifelse(preds_AAA > 0.5, 1,0))
summary(preds_AAA)

yardstick::accuracy_vec(truth = as.factor(hitAAA_confmat_test$made_mlb), as.factor(ifelse(preds_AAA > 0.5, 1,0)))

conf_mat_hitAAA <- as.data.frame(table('Truth' = ifelse(hitAAA_confmat_test$made_mlb == 1, 'Yes', 'No'), 'Predictions' = ifelse(preds_AAA > 0.5, 'Yes','No')))

conf_mat_hitAAA <- conf_mat_hitAAA %>% pivot_wider(names_from = Predictions, values_from = Freq, values_fill = 0)

gt(conf_mat_hitAAA) %>%
  cols_label(
    Truth = "Actual \\ Predicted",
    No = "No",
    Yes = "Yes"
  ) %>%
  tab_header(
    title = "Made MLB Confusion Matrix (Hitters Level AAA)"
  )

# Final Graphic #
(made_mlb_shap_plots <- grid.arrange(made_mlb_hitA_shap_plot_labeled, made_mlb_hitAplus_shap_plot_labeled,
                                    made_mlb_hitAA_shap_plot_labeled, made_mlb_hitAAA_shap_plot_labeled, ncol = 2))



#### Pitchers ####
# Single A Pitchers #
set.seed(101);made_mlb_A_pitchers <- xgboost(
  params = list(
    eta = 0.05,
    objective = 'multi:softprob',
    eval_metric = 'mlogloss',
    max_depth = max_depth_made_mlb_pitA,
    min_child_weight = weight_made_mlb_pitA,
    subsample = subsample_made_mlb_pitA,
    gamma = 5,
    num_class = 3
  ),
  data = made_mlb_Apit_train,
  nrounds = 754,
  print_every_n = 100,
  nthread = 7
) 

shap_values <- predict(made_mlb_A_pitchers, as.matrix(made_mlb_pit_A %>% select(-made_mlb_multiclass)), 
                       predcontrib = TRUE, 
                       reshape = TRUE)

shap_nomlb <- shap_values[[1]]
shap_mlbrp <- shap_values[[2]]
shap_mlbsp <- shap_values[[3]]
# no mlb shap
# Note: [,1:9] removes bias column
shap_nomlb_prep <- shap.prep(shap_contrib = as.data.frame(shap_nomlb[,1:9]), X_train = as.matrix(made_mlb_pit_A %>% select(-made_mlb_multiclass)))
(nomlb_A_pit <- shap.plot.summary(shap_nomlb_prep) + labs(title = "Pitcher Doesn't Make MLB\nSHAP Plot (Level A)"))

# MLB as RP SHAP
# Note: [,1:9] removes bias column
shap_mlbrp_prep <- shap.prep(shap_contrib = as.data.frame(shap_mlbrp[,1:9]), X_train = as.matrix(made_mlb_pit_A %>% select(-made_mlb_multiclass)))
(mlbrp_A_pit <- shap.plot.summary(shap_mlbrp_prep) + labs(title = 'Pitcher Makes MLB\nas RP SHAP Plot (Level A)'))

# MLB as SP SHAP
# Note: [,1:9] removes bias column
shap_mlbsp_prep <- shap.prep(shap_contrib = as.data.frame(shap_mlbsp[,1:9]), X_train = as.matrix(made_mlb_pit_A %>% select(-made_mlb_multiclass)))
(mlbsp_A_pit <- shap.plot.summary(shap_mlbsp_prep) + labs(title = 'Pitcher Makes MLB\nas SP Shap Plot (Level A)'))

# Final SHAP Plot
grid.arrange(nomlb_A_pit, mlbrp_A_pit, mlbsp_A_pit, nrow = 1)
  
# gets vector prediction
preds_pit_A <- predict(made_mlb_A_pitchers, as.matrix(made_mlb_pit_A %>% select(-made_mlb_multiclass)))

# converts to matrix
preds_pit_A_matrix <- matrix(preds_pit_A, ncol = 3, byrow = TRUE)

colnames(preds_pit_A_matrix) <- c('no_mlb', 'mlb_rp','mlb_sp')

#confusion matrix
table('Truth' = made_mlb_pit_A$made_mlb_multiclass, 'Predicted' = (
  as_tibble(preds_pit_A_matrix) %>% 
    mutate(prediction = case_when(
      no_mlb > mlb_rp & no_mlb > mlb_sp ~ 0,
      mlb_rp > no_mlb & mlb_rp > mlb_sp ~ 1,
      mlb_sp > no_mlb & mlb_sp > mlb_rp ~ 2
    )) %>% 
    pull(prediction)
))
