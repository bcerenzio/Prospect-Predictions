library(dbarts)
library(tidyverse)
library(parallel)
library(tictoc)
library(corrr)
library(progress)
library(pdp)

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

#finding the optimal number of CVs
bart_avg_aplus_cvs <- function(cv){
  set.seed(101);avg_aplus$fold <- sample(1:cv, nrow(avg_aplus), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- avg_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- avg_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-AVG_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(AVG_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$AVG_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_avg_aplus_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_avg_aplus_cvs))

bart_avg_aplus_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #4


hyperparam_bart_avg_aplus <- function(sigdf_avg_aplus, sigquant_avg_aplus, k_avg_aplus, power_avg_aplus, base_avg_aplus,row_num = 1){
  set.seed(101);avg_aplus$fold <- sample(1:4, nrow(avg_aplus), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_avg_aplus))
  print(paste('Sigquant: ', sigquant_avg_aplus))
  print(paste('K: ', k_avg_aplus))
  print(paste('Power: ', power_avg_aplus))
  print(paste('Base: ', base_avg_aplus))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:4){
    print(paste('Iteration: ', i))
    train_data <- avg_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- avg_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-AVG_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(AVG_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_avg_aplus,
                   sigquant = sigquant_avg_aplus,
                   base = base_avg_aplus,
                   k = k_avg_aplus,
                   power = power_avg_aplus,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$AVG_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_avg_aplus_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5, 0.75, 0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_avg_aplus_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_avg_aplus_0.75_df$sigdf,
                                                        hyperparam_bart_avg_aplus_0.75_df$sigquant, 
                                                        hyperparam_bart_avg_aplus_0.75_df$k, 
                                                        hyperparam_bart_avg_aplus_0.75_df$power,
                                                        hyperparam_bart_avg_aplus_0.75_df$base,
                                                        hyperparam_bart_avg_aplus_0.75_df$row_num), hyperparam_bart_avg_aplus, .progress = TRUE)

hyperparam_bart_avg_aplus_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5, 0.75, 0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_avg_aplus_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_avg_aplus_0.9_df$sigdf,
                                                       hyperparam_bart_avg_aplus_0.9_df$sigquant, 
                                                       hyperparam_bart_avg_aplus_0.9_df$k, 
                                                       hyperparam_bart_avg_aplus_0.9_df$power,
                                                       hyperparam_bart_avg_aplus_0.9_df$base,
                                                       hyperparam_bart_avg_aplus_0.9_df$row_num), hyperparam_bart_avg_aplus, .progress = TRUE)

hyperparam_bart_avg_aplus_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5, 0.75, 0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_avg_aplus_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_avg_aplus_0.99_df$sigdf,
                                                        hyperparam_bart_avg_aplus_0.99_df$sigquant, 
                                                        hyperparam_bart_avg_aplus_0.99_df$k, 
                                                        hyperparam_bart_avg_aplus_0.99_df$power,
                                                        hyperparam_bart_avg_aplus_0.99_df$base,
                                                        hyperparam_bart_avg_aplus_0.99_df$row_num), hyperparam_bart_avg_aplus)



hyperparam_bart_avg_aplus_df <- bind_rows(hyperparam_bart_avg_aplus_0.75_df, hyperparam_bart_avg_aplus_0.9_df, hyperparam_bart_avg_aplus_0.99_df)

hyperparam_bart_avg_aplus_df <- hyperparam_bart_avg_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_avg_aplus <- hyperparam_bart_avg_aplus_df$sigdf) #3
(sigquant_avg_aplus <- hyperparam_bart_avg_aplus_df$sigquant) #0.9
(k_avg_aplus <- hyperparam_bart_avg_aplus_df$k) #6
(power_avg_aplus <- hyperparam_bart_avg_aplus_df$power) #4
(base_avg_aplus <- hyperparam_bart_avg_aplus_df$base) #0.95

hyperparam_bart_avg_aplus_df$rmse/sd(avg_aplus$AVG_Aplus) #0.915 #BART Better


hyperparam_bart_avg_aplus_trees <- function(trees_avg_aplus){
  set.seed(101);avg_aplus$fold <- sample(1:4, nrow(avg_aplus), replace = TRUE)
  
  print(paste('Trees: ', trees_avg_aplus))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:4){
    print(paste('Iteration: ', i))
    train_data <- avg_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- avg_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-AVG_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(AVG_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_avg_aplus,
                   sigquant = sigquant_avg_aplus,
                   k = k_avg_aplus,
                   power = power_avg_aplus,
                   base = base_avg_aplus,
                   n.trees = trees_avg_aplus,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$AVG_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_avg_aplus_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_avg_aplus_trees, .progress = TRUE))


bart_avg_aplus_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() 

(avg_aplus_error_ratio <- (bart_avg_aplus_trees_df %>% filter(trees == 1200) %>% pull(rmse))/sd(avg_aplus$AVG_Aplus)) #0.910 #BART Better

## Final Model AVG A+ ####

avg_aplus_mod <- bart2(AVG_Aplus ~ ., 
      data = avg_aplus, 
      sigdf = sigdf_avg_aplus,
      sigquant = sigquant_avg_aplus,
      k = k_avg_aplus,
      power = power_avg_aplus,
      base = base_avg_aplus,
      n.trees = 1200,
      n.samples = 300,
      n.burn = 300,
      n.threads = parallel::detectCores()-1, 
      n.chains = parallel::detectCores() -1,
      seed = 101, # ensures reproducability
      keepTrees = TRUE, # needed for prediction,
      verbose = TRUE # give more information about model building process
)

save(avg_aplus_mod, file = 'avg_aplus_mod.RData')

write_rds(avg_aplus_mod, file = 'avg_aplus_mod.rds', compress = 'gz')

# Summing varcount over chains and MCMC samples
avg_aplus_vc <- avg_aplus_mod$varcount  # [chains x samples x predictors]

# Total variable usage across all chains and iterations
avg_aplus_var_importance <- apply(avg_aplus_vc, 3, sum)  # sum over first two dims → vector of length [# predictors]

# Normalize to get relative importance
avg_aplus_var_importance_norm <- avg_aplus_var_importance / sum(avg_aplus_var_importance)

# Name variables if you have them
names(avg_aplus_var_importance_norm) <- colnames(avg_aplus)[-1]  # assuming first column was response

# View
print(round(avg_aplus_var_importance_norm, 4))

avg_aplus_var_importance_norm <- as.data.frame(avg_aplus_var_importance_norm) %>% 
  rownames_to_column(var = 'var_name');colnames(avg_aplus_var_importance_norm) <- c('var_name', 'importance')

avg_aplus_var_importance_norm <- avg_aplus_var_importance_norm %>% 
  arrange(desc(importance))

avg_aplus_var_importance_norm$var_name <- factor(avg_aplus_var_importance_norm$var_name, levels = rev(avg_aplus_var_importance_norm$var_name))

avg_aplus_var_importance_norm %>% 
  ggplot(aes(x = importance, y = var_name)) +
  geom_col(fill = 'blue', color = 'black') +
  theme_classic() +
  labs(
    title = 'BART AVG (A to A+) Importance Plot',
    y = 'Feature',
    x = 'Standardized Importance'
  )


(age_a_partial <- pdp::partial(object = avg_aplus_mod, pred.var = c('Age_A'), type = 'regression',
                              plot = TRUE, plot.engine = 'ggplot2', smooth = TRUE, rug = TRUE))

save(age_a_partial, file = 'age_a_partial.RData')

age_a_partial + theme_bw() + ylab('AVG (A+)')

(H_A_adj_partial <- pdp::partial(object = avg_aplus_mod, pred.var = c('H_A_adj'), type = 'regression',
                                 plot = TRUE, plot.engine = 'ggplot2', smooth = TRUE, rug = TRUE))

save(H_A_adj_partial, file = 'H_A_adj_partial.RData')

H_A_adj_partial + theme_bw() + ylab('AVG (A+)')

asfs <- residuals(avg_aplus_mod)

asfs1 %>% 
  ggplot(aes(Avg_Aplus)) +
  geom_density(fill = 'blue', alpha = 0.4) +
  geom_density(aes(predictions),fill = 'red', alpha = 0.4, adjust = 0.5) +
  theme_classic()

### AVG A+ to AA ####
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

#finding the optimal number of CVs
bart_avg_AA_cvs <- function(cv){
  set.seed(101);avg_AA$fold <- sample(1:cv, nrow(avg_AA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- avg_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- avg_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-AVG_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(AVG_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$AVG_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  
  df <- tibble(
    rmse = mean(rmse_val), 
    time_elapsed = time_elapsed
  )
  
  return(df)
  
  
}

bart_avg_AA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_df(cv, bart_avg_AA_cvs)$rmse,
         time_elapsed = map_df(cv, bart_avg_AA_cvs)$time_elapsed)

bart_avg_AA_cv_df %>% 
  ggplot(aes(cv, rmse, color = time_elapsed)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  scale_color_continuous(low = 'green', high = 'red', name = 'Time Elapsed (s)') +
  geom_text(aes(cv, rmse - 0.00003, label = round(time_elapsed, 1)), color = 'black')+
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #16


hyperparam_bart_avg_AA <- function(sigdf_avg_AA, sigquant_avg_AA, k_avg_AA, power_avg_AA, base_avg_AA, row_num){
  set.seed(101);avg_AA$fold <- sample(1:16, nrow(avg_AA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_avg_AA))
  print(paste('Sigquant: ', sigquant_avg_AA))
  print(paste('K: ', k_avg_AA))
  print(paste('Power: ', power_avg_AA))
  print(paste('Base: ', base_avg_AA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:16){
    print(paste('Iteration: ', i))
    train_data <- avg_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- avg_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-AVG_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(AVG_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_avg_AA,
                   sigquant = sigquant_avg_AA,
                   k = k_avg_AA,
                   power = power_avg_AA,
                   base = base_avg_AA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$AVG_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_avg_AA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5, 0.75, 0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_avg_AA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_avg_AA_0.75_df$sigdf,
                                                   hyperparam_bart_avg_AA_0.75_df$sigquant, 
                                                   hyperparam_bart_avg_AA_0.75_df$k, 
                                                   hyperparam_bart_avg_AA_0.75_df$power,
                                                   hyperparam_bart_avg_AA_0.75_df$base,
                                                   hyperparam_bart_avg_AA_0.75_df$row_num), hyperparam_bart_avg_AA, .progress = TRUE)

hyperparam_bart_avg_AA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5, 0.75, 0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_avg_AA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_avg_AA_0.9_df$sigdf,
                                                       hyperparam_bart_avg_AA_0.9_df$sigquant, 
                                                  hyperparam_bart_avg_AA_0.9_df$k, 
                                                  hyperparam_bart_avg_AA_0.9_df$power,
                                                  hyperparam_bart_avg_AA_0.9_df$base,
                                                  hyperparam_bart_avg_AA_0.9_df$row_num), hyperparam_bart_avg_AA, .progress = TRUE)

hyperparam_bart_avg_AA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5, 0.75, 0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_avg_AA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_avg_AA_0.99_df$sigdf,
                                                        hyperparam_bart_avg_AA_0.99_df$sigquant, 
                                                        hyperparam_bart_avg_AA_0.99_df$k, 
                                                        hyperparam_bart_avg_AA_0.99_df$power,
                                                        hyperparam_bart_avg_AA_0.99_df$base,
                                                        hyperparam_bart_avg_AA_0.99_df$row_num), hyperparam_bart_avg_AA, .progress = TRUE)



hyperparam_bart_avg_AA_df <- bind_rows(hyperparam_bart_avg_AA_0.75_df, hyperparam_bart_avg_AA_0.9_df, hyperparam_bart_avg_AA_0.99_df)

hyperparam_bart_avg_AA_df <- hyperparam_bart_avg_AA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_avg_AA <- hyperparam_bart_avg_AA_df$sigdf) #10
(sigquant_avg_AA <- hyperparam_bart_avg_AA_df$sigquant) #0.75
(k_avg_AA <- hyperparam_bart_avg_AA_df$k) #6
(power_avg_AA <- hyperparam_bart_avg_AA_df$power) #3
(base_avg_AA <- hyperparam_bart_avg_AA_df$base) #0.5

hyperparam_bart_avg_AA_df$rmse/sd(avg_AA$AVG_AA) #0.889 #BART Better

hyperparam_bart_avg_AA_trees <- function(trees_avg_AA){
  set.seed(101);avg_AA$fold <- sample(1:16, nrow(avg_AA), replace = TRUE)
  
  print(paste('Trees: ', trees_avg_AA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:16){
    print(paste('Iteration: ', i))
    train_data <- avg_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- avg_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-AVG_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(AVG_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_avg_AA,
                   sigquant = sigquant_avg_AA,
                   k = k_avg_AA,
                   power = power_avg_AA,
                   n.trees = trees_avg_AA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$AVG_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_avg_AA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_avg_AA_trees, .progress = TRUE))


bart_avg_AA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #200



(avg_aa_error_ratio <- (bart_avg_AA_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(avg_AA$AVG_AA)) #0.889 #BART Better

### Final Model AVG AA ####

avg_aa_mod <- bart2(AVG_AA ~ ., 
      data = avg_AA, 
      sigdf = sigdf_avg_AA,
      sigquant = sigquant_avg_AA,
      k = k_avg_AA,
      power = power_avg_AA,
      n.trees = 200,
      n.samples = 300,
      n.burn = 300,
      n.threads = parallel::detectCores()-1, 
      n.chains = parallel::detectCores() -1,
      seed = 101, # ensures reproducability
      keepTrees = TRUE, # needed for prediction,
      printEvery = 1000,
      verbose = FALSE # give more information about model building process
)

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

#finding the optimal number of CVs
bart_avg_AAA_cvs <- function(cv){
  set.seed(101);avg_AAA$fold <- sample(1:cv, nrow(avg_AAA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- avg_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- avg_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-AVG_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(AVG_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$AVG_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_avg_AAA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_avg_AAA_cvs))

bart_avg_AAA_cv_df %>% 
  ggplot(aes(cv, rmse, color = time_elapsed)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  scale_color_continuous(low = 'green', high = 'red', name = 'Time Elapsed (s)') +
  geom_text(aes(cv, rmse - 0.00003, label = round(time_elapsed, 1)), color = 'black')+
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #10

hyperparam_bart_avg_AAA <- function(sigdf_avg_AAA, sigquant_avg_AAA, k_avg_AAA, power_avg_AAA, base_avg_AAA,row_num){
  set.seed(101);avg_AAA$fold <- sample(1:10, nrow(avg_AAA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_avg_AAA))
  print(paste('Sigquant: ', sigquant_avg_AAA))
  print(paste('K: ', k_avg_AAA))
  print(paste('Power: ', power_avg_AAA))
  print(paste('Base: ', base_avg_AAA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- avg_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- avg_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-AVG_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(AVG_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_avg_AAA,
                   sigquant = sigquant_avg_AAA,
                   k = k_avg_AAA,
                   power = power_avg_AAA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$AVG_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_avg_AAA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5, 0.75, 0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_avg_AAA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_avg_AAA_0.75_df$sigdf,
                                                     hyperparam_bart_avg_AAA_0.75_df$sigquant, 
                                                     hyperparam_bart_avg_AAA_0.75_df$k, 
                                                     hyperparam_bart_avg_AAA_0.75_df$power,
                                                     hyperparam_bart_avg_AAA_0.75_df$base,
                                                     hyperparam_bart_avg_AAA_0.75_df$row_num), hyperparam_bart_avg_AAA, .progress = TRUE)

hyperparam_bart_avg_AAA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5, 0.75, 0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_avg_AAA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_avg_AAA_0.9_df$sigdf,
                                                    hyperparam_bart_avg_AAA_0.9_df$sigquant, 
                                                    hyperparam_bart_avg_AAA_0.9_df$k, 
                                                    hyperparam_bart_avg_AAA_0.9_df$power,
                                                    hyperparam_bart_avg_AAA_0.9_df$base,
                                                    hyperparam_bart_avg_AAA_0.9_df$row_num), hyperparam_bart_avg_AAA, .progress = TRUE)

hyperparam_bart_avg_AAA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5, 0.75, 0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_avg_AAA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_avg_AAA_0.99_df$sigdf,
                                                     hyperparam_bart_avg_AAA_0.99_df$sigquant, 
                                                     hyperparam_bart_avg_AAA_0.99_df$k, 
                                                     hyperparam_bart_avg_AAA_0.99_df$power,
                                                     hyperparam_bart_avg_AAA_0.99_df$base,
                                                     hyperparam_bart_avg_AAA_0.99_df$row_num), hyperparam_bart_avg_AAA, .progress = TRUE)



hyperparam_bart_avg_AAA_df <- bind_rows(hyperparam_bart_avg_AAA_0.75_df, hyperparam_bart_avg_AAA_0.9_df, hyperparam_bart_avg_AAA_0.99_df)

hyperparam_bart_avg_AAA_df <- hyperparam_bart_avg_AAA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_avg_AAA <- hyperparam_bart_avg_AAA_df$sigdf) #3
(sigquant_avg_AAA <- hyperparam_bart_avg_AAA_df$sigquant) #0.9
(k_avg_AAA <- hyperparam_bart_avg_AAA_df$k) #6
(power_avg_AAA <- hyperparam_bart_avg_AAA_df$power) #6
(base_avg_AAA <- hyperparam_bart_avg_AAA_df$base) #0.5

hyperparam_bart_avg_AAA_df$rmse/sd(avg_AAA$AVG_AAA) #0.901 #BART Better


hyperparam_bart_avg_AAA_trees <- function(trees_avg_AAA){
  set.seed(101);avg_AAA$fold <- sample(1:10, nrow(avg_AAA), replace = TRUE)
  
  print(paste('Trees: ', trees_avg_AAA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- avg_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- avg_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-AVG_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(AVG_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_avg_AAA,
                   sigquant = sigquant_avg_AAA,
                   k = k_avg_AAA,
                   power = power_avg_AAA,
                   base = base_avg_AAA,
                   n.trees = trees_avg_AAA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$AVG_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_avg_AAA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_avg_AAA_trees, .progress = TRUE))


bart_avg_AAA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  #scale_y_continuous(limits = c(0.0385, 0.0395)) +
  theme_bw() #600

(avg_aaa_error_ratio <- (bart_avg_AAA_trees_df %>% filter(trees == 600) %>% pull(rmse))/sd(avg_AAA$AVG_AAA)) #0.901 #BART Better


### Final Model AVG AAA ####

avg_AAA_mod <- bart2(AVG_AAA ~ ., 
               data = avg_AAA,
               sigdf = sigdf_avg_AAA,
               sigquant = sigquant_avg_AAA,
               k = k_avg_AAA,
               power = power_avg_AAA,
               base = base_avg_AAA,
               n.trees = 600,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)


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

#finding the optimal number of CVs
bart_avg_mlb_cvs <- function(cv){
  set.seed(101);avg_mlb$fold <- sample(1:cv, nrow(avg_mlb), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- avg_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- avg_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-AVG_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(AVG_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$AVG_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_avg_mlb_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_avg_mlb_cvs))

bart_avg_mlb_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_classic() +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #14


hyperparam_bart_avg_mlb <- function(sigdf_avg_mlb, sigquant_avg_mlb, k_avg_mlb, power_avg_mlb, base_avg_mlb,row_num){
  set.seed(101);avg_mlb$fold <- sample(1:14, nrow(avg_mlb), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_avg_mlb))
  print(paste('Sigquant: ', sigquant_avg_mlb))
  print(paste('K: ', k_avg_mlb))
  print(paste('Power: ', power_avg_mlb))
  print(paste('Base: ', base_avg_mlb))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:14){
    print(paste('Iteration: ', i))
    train_data <- avg_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- avg_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-AVG_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(AVG_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_avg_mlb,
                   sigquant = sigquant_avg_mlb,
                   k = k_avg_mlb,
                   power = power_avg_mlb,
                   base = base_avg_mlb,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$AVG_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_avg_mlb_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_avg_mlb_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_avg_mlb_0.75_df$sigdf,
                                                        hyperparam_bart_avg_mlb_0.75_df$sigquant, 
                                                        hyperparam_bart_avg_mlb_0.75_df$k, 
                                                        hyperparam_bart_avg_mlb_0.75_df$power,
                                                      hyperparam_bart_avg_mlb_0.75_df$base,
                                                        hyperparam_bart_avg_mlb_0.75_df$row_num), hyperparam_bart_avg_mlb, .progress = TRUE)

hyperparam_bart_avg_mlb_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_avg_mlb_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_avg_mlb_0.9_df$sigdf,
                                                       hyperparam_bart_avg_mlb_0.9_df$sigquant, 
                                                       hyperparam_bart_avg_mlb_0.9_df$k, 
                                                       hyperparam_bart_avg_mlb_0.9_df$power,
                                                     hyperparam_bart_avg_mlb_0.9_df$base,
                                                       hyperparam_bart_avg_mlb_0.9_df$row_num), hyperparam_bart_avg_mlb, .progress = TRUE)

hyperparam_bart_avg_mlb_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_avg_mlb_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_avg_mlb_0.99_df$sigdf,
                                                        hyperparam_bart_avg_mlb_0.99_df$sigquant, 
                                                        hyperparam_bart_avg_mlb_0.99_df$k, 
                                                        hyperparam_bart_avg_mlb_0.99_df$power,
                                                      hyperparam_bart_avg_mlb_0.99_df$base,
                                                        hyperparam_bart_avg_mlb_0.99_df$row_num), hyperparam_bart_avg_mlb, .progress = TRUE)



hyperparam_bart_avg_mlb_df <- bind_rows(hyperparam_bart_avg_mlb_0.75_df, hyperparam_bart_avg_mlb_0.9_df, hyperparam_bart_avg_mlb_0.99_df)

hyperparam_bart_avg_mlb_df <- hyperparam_bart_avg_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_avg_mlb <- hyperparam_bart_avg_mlb_df$sigdf) #10
(sigquant_avg_mlb <- hyperparam_bart_avg_mlb_df$sigquant) #0.75
(k_avg_mlb <- hyperparam_bart_avg_mlb_df$k) #3
(power_avg_mlb <- hyperparam_bart_avg_mlb_df$power) #3
(base_avg_mlb <- hyperparam_bart_avg_mlb_df$base) #0.5

hyperparam_bart_avg_mlb_df$rmse/sd(avg_mlb$AVG_MLB) #0.953


hyperparam_bart_avg_mlb_trees <- function(trees_avg_mlb){
  set.seed(101);avg_mlb$fold <- sample(1:14, nrow(avg_mlb), replace = TRUE)
  
  print(paste('Trees: ', trees_avg_mlb))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:14){
    print(paste('Iteration: ', i))
    train_data <- avg_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- avg_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-AVG_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(AVG_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_avg_mlb,
                   sigquant = sigquant_avg_mlb,
                   k = k_avg_mlb,
                   power = power_avg_mlb,
                   base = base_avg_mlb,
                   n.trees = trees_avg_mlb,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$AVG_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_avg_mlb_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_avg_mlb_trees, .progress = TRUE))


bart_avg_mlb_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #200

(avg_mlb_error_ratio <- (bart_avg_mlb_trees_df %>% filter(trees == 200) %>% pull(rmse)/sd(avg_mlb$AVG_MLB))) #0.953 #BART Better


### Final Model AVG MLB ####

avg_mlb_mod <- bart2(AVG_MLB ~ ., 
               data = avg_mlb,
               sigdf = sigdf_avg_mlb,
               sigquant = sigquant_avg_mlb,
               k = k_avg_mlb,
               power = power_avg_mlb,
               base = base_avg_mlb,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

predictions_avg_mlb <- predict(avg_mlb_mod, mlb_hitting %>% 
                                 drop_na(Name_AAA) %>% 
                                 mutate(top_100_rank = mean(c(rank_ba, rank_mlb)),
                                        AVG_AAA_adj = H_AAA/(AB_AAA^0.8),
                                        team_rank = team_rank_ba,
                                        BB_minus_K_pct_AAA = BB_pct_AAA - K_pct_AAA))
preds1 <- numeric();for (pred in 1:nrow(mlb_hitting %>% 
                    drop_na(Name_AAA))){
  col <- paste0('V', pred)
  preds1 <- c(preds1, mean(as_tibble(predictions_avg_mlb) %>% pull(!!col), names = FALSE))
}

summary(preds1)

which.max(preds1)

summary(as_tibble(predictions_avg_mlb) %>% pull(V83))

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

#finding the optimal number of CVs
bart_obp_aplus_cvs <- function(cv){
  set.seed(101);obp_aplus$fold <- sample(1:cv, nrow(obp_aplus), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- obp_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- obp_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-OBP_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(OBP_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$OBP_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_obp_aplus_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_obp_aplus_cvs))

bart_obp_aplus_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #4

hyperparam_bart_obp_aplus <- function(sigdf_obp_aplus, sigquant_obp_aplus, k_obp_aplus, power_obp_aplus, base_obp_aplus, row_num = 1){
  set.seed(101);obp_aplus$fold <- sample(1:4, nrow(obp_aplus), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_obp_aplus))
  print(paste('Sigquant: ', sigquant_obp_aplus))
  print(paste('K: ', k_obp_aplus))
  print(paste('Power: ', power_obp_aplus))
  print(paste('Base: ', base_obp_aplus))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:4){
    print(paste('Iteration: ', i))
    train_data <- obp_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- obp_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-OBP_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(OBP_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_obp_aplus,
                   sigquant = sigquant_obp_aplus,
                   k = k_obp_aplus,
                   power = power_obp_aplus,
                   base = base_obp_aplus,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$OBP_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_obp_aplus_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_obp_aplus_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_obp_aplus_0.75_df$sigdf,
                                                        hyperparam_bart_obp_aplus_0.75_df$sigquant, 
                                                        hyperparam_bart_obp_aplus_0.75_df$k, 
                                                        hyperparam_bart_obp_aplus_0.75_df$power,
                                                        hyperparam_bart_obp_aplus_0.75_df$base,
                                                        hyperparam_bart_obp_aplus_0.75_df$row_num), hyperparam_bart_obp_aplus,.progress = TRUE)

hyperparam_bart_obp_aplus_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_obp_aplus_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_obp_aplus_0.9_df$sigdf,
                                                       hyperparam_bart_obp_aplus_0.9_df$sigquant, 
                                                       hyperparam_bart_obp_aplus_0.9_df$k, 
                                                       hyperparam_bart_obp_aplus_0.9_df$power,
                                                       hyperparam_bart_obp_aplus_0.9_df$base,
                                                       hyperparam_bart_obp_aplus_0.9_df$row_num), hyperparam_bart_obp_aplus, .progress = TRUE)

hyperparam_bart_obp_aplus_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_obp_aplus_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_obp_aplus_0.99_df$sigdf,
                                                        hyperparam_bart_obp_aplus_0.99_df$sigquant, 
                                                        hyperparam_bart_obp_aplus_0.99_df$k, 
                                                        hyperparam_bart_obp_aplus_0.99_df$power,
                                                        hyperparam_bart_obp_aplus_0.99_df$base,
                                                        hyperparam_bart_obp_aplus_0.99_df$row_num), hyperparam_bart_obp_aplus, .progress = TRUE)



hyperparam_bart_obp_aplus_df <- bind_rows(hyperparam_bart_obp_aplus_0.75_df, hyperparam_bart_obp_aplus_0.9_df, hyperparam_bart_obp_aplus_0.99_df)

hyperparam_bart_obp_aplus_df <- hyperparam_bart_obp_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_obp_aplus <- hyperparam_bart_obp_aplus_df$sigdf) #3
(sigquant_obp_aplus <- hyperparam_bart_obp_aplus_df$sigquant) #0.99
(k_obp_aplus <- hyperparam_bart_obp_aplus_df$k) #4
(power_obp_aplus <- hyperparam_bart_obp_aplus_df$power) #3
(base_obp_aplus <- hyperparam_bart_obp_aplus_df$base) #0.95


hyperparam_bart_obp_aplus_df$rmse/sd(obp_aplus$OBP_Aplus) #0.894 #BART Better

hyperparam_bart_obp_aplus_trees <- function(trees_obp_aplus){
  set.seed(101);obp_aplus$fold <- sample(1:4, nrow(obp_aplus), replace = TRUE)
  
  print(paste('Trees: ', trees_obp_aplus))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:4){
    print(paste('Iteration: ', i))
    train_data <- obp_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- obp_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-OBP_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(OBP_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_obp_aplus,
                   sigquant = sigquant_obp_aplus,
                   k = k_obp_aplus,
                   power = power_obp_aplus,
                   base = base_obp_aplus,
                   n.trees = trees_obp_aplus,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$OBP_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_obp_aplus_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_obp_aplus_trees, .progress = TRUE))


bart_obp_aplus_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #400

(obp_aplus_error_ratio <- (bart_obp_aplus_trees_df %>% filter(trees == 400) %>% pull(rmse))/sd(obp_aplus$OBP_Aplus)) #0.894 #BART Better

### Final Model OBP A+ ####

obp_aplus_mod <- bart2(OBP_Aplus ~ ., 
               data = obp_aplus, 
               sigdf = sigdf_obp_aplus,
               sigquant = sigquant_obp_aplus,
               k = k_obp_aplus,
               power = power_obp_aplus,
               base = base_obp_aplus,
               n.trees = 400,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)


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

#finding the optimal number of CVs
bart_obp_AA_cvs <- function(cv){
  set.seed(101);obp_AA$fold <- sample(1:cv, nrow(obp_AA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- obp_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- obp_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-OBP_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(OBP_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$OBP_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  
  df <- tibble(
    rmse = mean(rmse_val), 
    time_elapsed = time_elapsed
  )
  
  return(df)
  
  
}

bart_obp_AA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_df(cv, bart_obp_AA_cvs)$rmse)

bart_obp_AA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #14

hyperparam_bart_obp_AA <- function(sigdf_obp_AA, sigquant_obp_AA, k_obp_AA, power_obp_AA, base_obp_AA,row_num){
  set.seed(101);obp_AA$fold <- sample(1:14, nrow(obp_AA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_obp_AA))
  print(paste('Sigquant: ', sigquant_obp_AA))
  print(paste('K: ', k_obp_AA))
  print(paste('Power: ', power_obp_AA))
  print(paste('Base: ', base_obp_AA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:14){
    print(paste('Iteration: ', i))
    train_data <- obp_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- obp_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-OBP_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(OBP_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_obp_AA,
                   sigquant = sigquant_obp_AA,
                   k = k_obp_AA,
                   power = power_obp_AA,
                   base = base_obp_AA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$OBP_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_obp_AA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_obp_AA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_obp_AA_0.75_df$sigdf,
                                                     hyperparam_bart_obp_AA_0.75_df$sigquant, 
                                                     hyperparam_bart_obp_AA_0.75_df$k, 
                                                     hyperparam_bart_obp_AA_0.75_df$power,
                                                     hyperparam_bart_obp_AA_0.75_df$base,
                                                     hyperparam_bart_obp_AA_0.75_df$row_num), hyperparam_bart_obp_AA, .progress = TRUE)

hyperparam_bart_obp_AA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_obp_AA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_obp_AA_0.9_df$sigdf,
                                                    hyperparam_bart_obp_AA_0.9_df$sigquant, 
                                                    hyperparam_bart_obp_AA_0.9_df$k, 
                                                    hyperparam_bart_obp_AA_0.9_df$power,
                                                    hyperparam_bart_obp_AA_0.9_df$base,
                                                    hyperparam_bart_obp_AA_0.9_df$row_num), hyperparam_bart_obp_AA, .progress = TRUE)

hyperparam_bart_obp_AA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_obp_AA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_obp_AA_0.99_df$sigdf,
                                                     hyperparam_bart_obp_AA_0.99_df$sigquant, 
                                                     hyperparam_bart_obp_AA_0.99_df$k, 
                                                     hyperparam_bart_obp_AA_0.99_df$power,
                                                     hyperparam_bart_obp_AA_0.99_df$base,
                                                     hyperparam_bart_obp_AA_0.99_df$row_num), hyperparam_bart_obp_AA, .progress = TRUE)



hyperparam_bart_obp_AA_df <- bind_rows(hyperparam_bart_obp_AA_0.75_df, hyperparam_bart_obp_AA_0.9_df, hyperparam_bart_obp_AA_0.99_df)

hyperparam_bart_obp_AA_df <- hyperparam_bart_obp_AA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_obp_AA <- hyperparam_bart_obp_AA_df$sigdf) #3
(sigquant_obp_AA <- hyperparam_bart_obp_AA_df$sigquant) #0.99
(k_obp_AA <- hyperparam_bart_obp_AA_df$k) #6
(power_obp_AA <- hyperparam_bart_obp_AA_df$power) #6
(base_obp_AA <- hyperparam_bart_obp_AA_df$base) #0.95

hyperparam_bart_obp_AA_df$rmse/sd(obp_AA$OBP_AA) #0.887 #BART Better


hyperparam_bart_obp_AA_trees <- function(trees_obp_AA){
  set.seed(101);obp_AA$fold <- sample(1:14, nrow(obp_AA), replace = TRUE)
  
  print(paste('Trees: ', trees_obp_AA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:14){
    print(paste('Iteration: ', i))
    train_data <- obp_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- obp_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-OBP_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(OBP_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_obp_AA,
                   sigquant = sigquant_obp_AA,
                   k = k_obp_AA,
                   power = power_obp_AA,
                   base = base_obp_AA,
                   n.trees = trees_obp_AA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$OBP_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_obp_AA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_obp_AA_trees, .progress = TRUE))


bart_obp_AA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  #scale_y_continuous(limits = c(0.0416, 0.0418)) +
  theme_bw() #200


(obp_AA_error_ratio <- (bart_obp_AA_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(obp_AA$OBP_AA)) #0.886 #BART better

### Final Model OBP AA ####

obp_AA_mod <- bart2(OBP_AA ~ ., 
               data = obp_AA,
               sigdf = sigdf_obp_AA,
               sigquant = sigquant_obp_AA,
               k = k_obp_AA,
               power = power_obp_AA,
               base = base_obp_AA,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)



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

#finding the optimal number of CVs
bart_obp_AAA_cvs <- function(cv){
  set.seed(101);obp_AAA$fold <- sample(1:cv, nrow(obp_AAA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- obp_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- obp_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-OBP_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(OBP_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$OBP_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_obp_AAA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_obp_AAA_cvs))

bart_obp_AAA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  scale_y_continuous(limits = c(0.042, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #14

hyperparam_bart_obp_AAA <- function(sigdf_obp_AAA, sigquant_obp_AAA, k_obp_AAA, power_obp_AAA, base_obp_AAA, row_num){
  set.seed(101);obp_AAA$fold <- sample(1:14, nrow(obp_AAA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_obp_AAA))
  print(paste('Sigquant: ', sigquant_obp_AAA))
  print(paste('K: ', k_obp_AAA))
  print(paste('Power: ', power_obp_AAA))
  print(paste('Base: ', base_obp_AAA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:14){
    print(paste('Iteration: ', i))
    train_data <- obp_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- obp_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-OBP_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(OBP_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_obp_AAA,
                   sigquant = sigquant_obp_AAA,
                   k = k_obp_AAA,
                   base = base_obp_AAA,
                   power = power_obp_AAA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$OBP_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_obp_AAA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_obp_AAA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_obp_AAA_0.75_df$sigdf,
                                                      hyperparam_bart_obp_AAA_0.75_df$sigquant, 
                                                      hyperparam_bart_obp_AAA_0.75_df$k, 
                                                      hyperparam_bart_obp_AAA_0.75_df$power,
                                                      hyperparam_bart_obp_AAA_0.75_df$base, 
                                                      hyperparam_bart_obp_AAA_0.75_df$row_num), hyperparam_bart_obp_AAA, .progress = TRUE)

hyperparam_bart_obp_AAA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_obp_AAA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_obp_AAA_0.9_df$sigdf,
                                                     hyperparam_bart_obp_AAA_0.9_df$sigquant, 
                                                     hyperparam_bart_obp_AAA_0.9_df$k, 
                                                     hyperparam_bart_obp_AAA_0.9_df$power,
                                                     hyperparam_bart_obp_AAA_0.9_df$base,
                                                     hyperparam_bart_obp_AAA_0.9_df$row_num), hyperparam_bart_obp_AAA, .progress = TRUE)

hyperparam_bart_obp_AAA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_obp_AAA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_obp_AAA_0.99_df$sigdf,
                                                      hyperparam_bart_obp_AAA_0.99_df$sigquant, 
                                                      hyperparam_bart_obp_AAA_0.99_df$k, 
                                                      hyperparam_bart_obp_AAA_0.99_df$power,
                                                      hyperparam_bart_obp_AAA_0.99_df$base,
                                                      hyperparam_bart_obp_AAA_0.99_df$row_num), hyperparam_bart_obp_AAA, .progress = TRUE)



hyperparam_bart_obp_AAA_df <- bind_rows(hyperparam_bart_obp_AAA_0.75_df, hyperparam_bart_obp_AAA_0.9_df, hyperparam_bart_obp_AAA_0.99_df)

hyperparam_bart_obp_AAA_df <- hyperparam_bart_obp_AAA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_obp_AAA <- hyperparam_bart_obp_AAA_df$sigdf) #3
(sigquant_obp_AAA <- hyperparam_bart_obp_AAA_df$sigquant) #0.99
(k_obp_AAA <- hyperparam_bart_obp_AAA_df$k) #6
(power_obp_AAA <- hyperparam_bart_obp_AAA_df$power) #3
(base_obp_AAA <- hyperparam_bart_obp_AAA_df$base) #0.5

hyperparam_bart_obp_AAA_df$rmse/sd(obp_AAA$OBP_AAA) #0.883 #BART Better

hyperparam_bart_obp_AAA_trees <- function(trees_obp_AAA){
  set.seed(101);obp_AAA$fold <- sample(1:14, nrow(obp_AAA), replace = TRUE)
  
  print(paste('Trees: ', trees_obp_AAA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:14){
    print(paste('Iteration: ', i))
    train_data <- obp_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- obp_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-OBP_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(OBP_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_obp_AAA,
                   sigquant = sigquant_obp_AAA,
                   k = k_obp_AAA,
                   power = power_obp_AAA,
                   base = base_obp_AAA,
                   n.trees = trees_obp_AAA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$OBP_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_obp_AAA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_obp_AAA_trees, .progress = TRUE))


bart_obp_AAA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
 # scale_y_continuous(limits = c(0.038, 0.0383)) +
  theme_bw() #200

(obp_aaa_error_ratio <- (bart_obp_AAA_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(obp_AAA$OBP_AAA)) #0.883 #BART Better

### Final Model OBP AAA ####

obp_AAA_mod <- bart2(OBP_AAA ~ ., 
               data = obp_AAA,
               sigdf = sigdf_obp_AAA,
               sigquant = sigquant_obp_AAA,
               k = k_obp_AAA,
               power = power_obp_AAA,
               base = base_obp_AAA,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

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

#finding the optimal number of CVs
bart_obp_mlb_cvs <- function(cv){
  set.seed(101);obp_mlb$fold <- sample(1:cv, nrow(obp_mlb), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- obp_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- obp_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-OBP_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(OBP_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$OBP_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_obp_mlb_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_obp_mlb_cvs))

bart_obp_mlb_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #16


hyperparam_bart_obp_mlb <- function(sigdf_obp_mlb, sigquant_obp_mlb, k_obp_mlb, power_obp_mlb, base_obp_mlb,row_num = 1){
  set.seed(101);obp_mlb$fold <- sample(1:16, nrow(obp_mlb), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_obp_mlb))
  print(paste('Sigquant: ', sigquant_obp_mlb))
  print(paste('K: ', k_obp_mlb))
  print(paste('Power: ', power_obp_mlb))
  print(paste('Base: ', base_obp_mlb))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:16){
    print(paste('Iteration: ', i))
    train_data <- obp_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- obp_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-OBP_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(OBP_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_obp_mlb,
                   sigquant = sigquant_obp_mlb,
                   k = k_obp_mlb,
                   power = power_obp_mlb,
                   base = base_obp_mlb,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$OBP_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_obp_mlb_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_obp_mlb_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_obp_mlb_0.75_df$sigdf,
                                                      hyperparam_bart_obp_mlb_0.75_df$sigquant, 
                                                      hyperparam_bart_obp_mlb_0.75_df$k, 
                                                      hyperparam_bart_obp_mlb_0.75_df$power,
                                                      hyperparam_bart_obp_mlb_0.75_df$base,
                                                      hyperparam_bart_obp_mlb_0.75_df$row_num), hyperparam_bart_obp_mlb, .progress = TRUE)

hyperparam_bart_obp_mlb_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_obp_mlb_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_obp_mlb_0.9_df$sigdf,
                                                     hyperparam_bart_obp_mlb_0.9_df$sigquant, 
                                                     hyperparam_bart_obp_mlb_0.9_df$k, 
                                                     hyperparam_bart_obp_mlb_0.9_df$power,
                                                     hyperparam_bart_obp_mlb_0.9_df$base,
                                                     hyperparam_bart_obp_mlb_0.9_df$row_num), hyperparam_bart_obp_mlb, .progress = TRUE)

hyperparam_bart_obp_mlb_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_obp_mlb_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_obp_mlb_0.99_df$sigdf,
                                                      hyperparam_bart_obp_mlb_0.99_df$sigquant, 
                                                      hyperparam_bart_obp_mlb_0.99_df$k, 
                                                      hyperparam_bart_obp_mlb_0.99_df$power,
                                                      hyperparam_bart_obp_mlb_0.99_df$base,
                                                      hyperparam_bart_obp_mlb_0.99_df$row_num), hyperparam_bart_obp_mlb, .progress = TRUE)



hyperparam_bart_obp_mlb_df <- bind_rows(hyperparam_bart_obp_mlb_0.75_df, hyperparam_bart_obp_mlb_0.9_df, hyperparam_bart_obp_mlb_0.99_df)

hyperparam_bart_obp_mlb_df <- hyperparam_bart_obp_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_obp_mlb <- hyperparam_bart_obp_mlb_df$sigdf) #3
(sigquant_obp_mlb <- hyperparam_bart_obp_mlb_df$sigquant) #0.99
(k_obp_mlb <- hyperparam_bart_obp_mlb_df$k) #3
(power_obp_mlb <- hyperparam_bart_obp_mlb_df$power) #3
(base_obp_mlb <- hyperparam_bart_obp_mlb_df$base) #0.5

hyperparam_bart_obp_mlb_df$rmse/sd(obp_mlb$OBP_MLB) #0.945 #BART Better

hyperparam_bart_obp_mlb_trees <- function(trees_obp_mlb){
  set.seed(101);obp_mlb$fold <- sample(1:16, nrow(obp_mlb), replace = TRUE)
  
  print(paste('Trees: ', trees_obp_mlb))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:16){
    print(paste('Iteration: ', i))
    train_data <- obp_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- obp_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-OBP_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(OBP_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_obp_mlb,
                   sigquant = sigquant_obp_mlb,
                   k = k_obp_mlb,
                   power = power_obp_mlb,
                   base = base_obp_mlb,
                   n.trees = trees_obp_mlb,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$OBP_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_obp_mlb_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_obp_mlb_trees, .progress = TRUE))


bart_obp_mlb_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw()
  #scale_y_continuous(limits = c(0.0338, 0.0339))#200


(obp_mlb_error_ratio <- (bart_obp_mlb_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(obp_mlb$OBP_MLB)) #0.945 #BART Better

### Final Model OBP MLB ####

obp_mlb_mod <- bart2(OBP_MLB ~ ., 
               data = obp_mlb,
               sigdf = sigdf_obp_mlb,
               sigquant = sigquant_obp_mlb,
               k = k_obp_mlb,
               power = power_obp_mlb,
               base = base_obp_mlb,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

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

#finding the optimal number of CVs
bart_iso_aplus_cvs <- function(cv){
  set.seed(101);iso_aplus$fold <- sample(1:cv, nrow(iso_aplus), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- iso_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- iso_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ISO_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ISO_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ISO_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_iso_aplus_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_iso_aplus_cvs))

bart_iso_aplus_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.043, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #14



hyperparam_bart_iso_aplus <- function(sigdf_iso_aplus, sigquant_iso_aplus, k_iso_aplus, power_iso_aplus, base_iso_aplus, row_num){
  set.seed(101);iso_aplus$fold <- sample(1:14, nrow(iso_aplus), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_iso_aplus))
  print(paste('Sigquant: ', sigquant_iso_aplus))
  print(paste('K: ', k_iso_aplus))
  print(paste('Power: ', power_iso_aplus))
  print(paste('Base: ', base_iso_aplus))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:14){
    print(paste('Iteration: ', i))
    train_data <- iso_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- iso_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ISO_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ISO_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_iso_aplus,
                   sigquant = sigquant_iso_aplus,
                   k = k_iso_aplus,
                   power = power_iso_aplus,
                   base = base_iso_aplus,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ISO_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_iso_aplus_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_iso_aplus_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_iso_aplus_0.75_df$sigdf,
                                                        hyperparam_bart_iso_aplus_0.75_df$sigquant, 
                                                        hyperparam_bart_iso_aplus_0.75_df$k, 
                                                        hyperparam_bart_iso_aplus_0.75_df$power,
                                                        hyperparam_bart_iso_aplus_0.75_df$base,
                                                        hyperparam_bart_iso_aplus_0.75_df$row_num), hyperparam_bart_iso_aplus, .progress = TRUE)

hyperparam_bart_iso_aplus_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_iso_aplus_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_iso_aplus_0.9_df$sigdf,
                                                       hyperparam_bart_iso_aplus_0.9_df$sigquant, 
                                                       hyperparam_bart_iso_aplus_0.9_df$k, 
                                                       hyperparam_bart_iso_aplus_0.9_df$power,
                                                       hyperparam_bart_iso_aplus_0.9_df$base,
                                                       hyperparam_bart_iso_aplus_0.9_df$row_num), hyperparam_bart_iso_aplus, .progress = TRUE)

hyperparam_bart_iso_aplus_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_iso_aplus_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_iso_aplus_0.99_df$sigdf,
                                                        hyperparam_bart_iso_aplus_0.99_df$sigquant, 
                                                        hyperparam_bart_iso_aplus_0.99_df$k, 
                                                        hyperparam_bart_iso_aplus_0.99_df$power,
                                                        hyperparam_bart_iso_aplus_0.99_df$base,
                                                        hyperparam_bart_iso_aplus_0.99_df$row_num), hyperparam_bart_iso_aplus, .progress = TRUE)



hyperparam_bart_iso_aplus_df <- bind_rows(hyperparam_bart_iso_aplus_0.75_df, hyperparam_bart_iso_aplus_0.9_df, hyperparam_bart_iso_aplus_0.99_df)

hyperparam_bart_iso_aplus_df <- hyperparam_bart_iso_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_iso_aplus <- hyperparam_bart_iso_aplus_df$sigdf) #3
(sigquant_iso_aplus <- hyperparam_bart_iso_aplus_df$sigquant) #0.9
(k_iso_aplus <- hyperparam_bart_iso_aplus_df$k) # 6
(power_iso_aplus <- hyperparam_bart_iso_aplus_df$power) # 5
(base_iso_aplus <- hyperparam_bart_iso_aplus_df$base) # 0.95

hyperparam_bart_iso_aplus_df$rmse/sd(iso_aplus$ISO_Aplus) #0.866 #BART Better

hyperparam_bart_iso_aplus_trees <- function(trees_iso_aplus){
  set.seed(101);iso_aplus$fold <- sample(1:14, nrow(iso_aplus), replace = TRUE)
  
  print(paste('Trees: ', trees_iso_aplus))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:14){
    print(paste('Iteration: ', i))
    train_data <- iso_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- iso_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ISO_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ISO_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_iso_aplus,
                   sigquant = sigquant_iso_aplus,
                   k = k_iso_aplus,
                   power = power_iso_aplus,
                   base = base_iso_aplus,
                   n.trees = trees_iso_aplus,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ISO_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_iso_aplus_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_iso_aplus_trees, .progress = TRUE))


bart_iso_aplus_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #200
 
(iso_aplus_error_ratio <- (bart_iso_aplus_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(iso_aplus$ISO_Aplus)) #0.865 #BART Better


### Final Model ISO A+ ####

iso_aplus_mod <- bart2(ISO_Aplus ~ ., 
               data = iso_aplus,
               sigdf = sigdf_iso_aplus,
               sigquant = sigquant_iso_aplus,
               k = k_iso_aplus,
               power = power_iso_aplus,
               base = base_iso_aplus,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)


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

#finding the optimal number of CVs
bart_iso_AA_cvs <- function(cv){
  set.seed(101);iso_AA$fold <- sample(1:cv, nrow(iso_AA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- iso_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- iso_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ISO_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ISO_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ISO_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_iso_AA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_iso_AA_cvs))

bart_iso_AA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #6


hyperparam_bart_iso_AA <- function(sigdf_iso_AA, sigquant_iso_AA, k_iso_AA, power_iso_AA, base_iso_AA,row_num){
  set.seed(101);iso_AA$fold <- sample(1:6, nrow(iso_AA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_iso_AA))
  print(paste('Sigquant: ', sigquant_iso_AA))
  print(paste('K: ', k_iso_AA))
  print(paste('Power: ', power_iso_AA))
  print(paste('Base: ', base_iso_AA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:6){
    print(paste('Iteration: ', i))
    train_data <- iso_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- iso_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ISO_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ISO_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_iso_AA,
                   sigquant = sigquant_iso_AA,
                   k = k_iso_AA,
                   power = power_iso_AA,
                   base = base_iso_AA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ISO_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_iso_AA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_iso_AA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_iso_AA_0.75_df$sigdf,
                                                     hyperparam_bart_iso_AA_0.75_df$sigquant, 
                                                     hyperparam_bart_iso_AA_0.75_df$k, 
                                                     hyperparam_bart_iso_AA_0.75_df$power,
                                                     hyperparam_bart_iso_AA_0.75_df$base,
                                                     hyperparam_bart_iso_AA_0.75_df$row_num), hyperparam_bart_iso_AA, .progress = TRUE)

hyperparam_bart_iso_AA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_iso_AA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_iso_AA_0.9_df$sigdf,
                                                    hyperparam_bart_iso_AA_0.9_df$sigquant, 
                                                    hyperparam_bart_iso_AA_0.9_df$k, 
                                                    hyperparam_bart_iso_AA_0.9_df$power,
                                                    hyperparam_bart_iso_AA_0.9_df$base,
                                                    hyperparam_bart_iso_AA_0.9_df$row_num), hyperparam_bart_iso_AA, .progress = TRUE)

hyperparam_bart_iso_AA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_iso_AA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_iso_AA_0.99_df$sigdf,
                                                     hyperparam_bart_iso_AA_0.99_df$sigquant, 
                                                     hyperparam_bart_iso_AA_0.99_df$k, 
                                                     hyperparam_bart_iso_AA_0.99_df$power,
                                                     hyperparam_bart_iso_AA_0.99_df$base,
                                                     hyperparam_bart_iso_AA_0.99_df$row_num), hyperparam_bart_iso_AA, .progress = TRUE)



hyperparam_bart_iso_AA_df <- bind_rows(hyperparam_bart_iso_AA_0.75_df, hyperparam_bart_iso_AA_0.9_df, hyperparam_bart_iso_AA_0.99_df)

hyperparam_bart_iso_AA_df <- hyperparam_bart_iso_AA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_iso_AA <- hyperparam_bart_iso_AA_df$sigdf) #3
(sigquant_iso_AA <- hyperparam_bart_iso_AA_df$sigquant) #0.9
(k_iso_AA <- hyperparam_bart_iso_AA_df$k) #6
(power_iso_AA <- hyperparam_bart_iso_AA_df$power) #3
(base_iso_AA <- hyperparam_bart_iso_AA_df$base) #0.95

hyperparam_bart_iso_AA_df$rmse/sd(iso_AA$ISO_AA) #0.824 #BART Better

hyperparam_bart_iso_AA_trees <- function(trees_iso_AA){
  set.seed(101);iso_AA$fold <- sample(1:6, nrow(iso_AA), replace = TRUE)
  
  print(paste('Trees: ', trees_iso_AA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:6){
    print(paste('Iteration: ', i))
    train_data <- iso_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- iso_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ISO_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ISO_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_iso_AA,
                   sigquant = sigquant_iso_AA,
                   k = k_iso_AA,
                   power = power_iso_AA,
                   base = base_iso_AA,
                   n.trees = trees_iso_AA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ISO_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_iso_AA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_iso_AA_trees, .progress = TRUE))


bart_iso_AA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #800

(iso_aa_error_ratio <- (bart_iso_AA_trees_df %>% filter(trees == 800) %>% pull(rmse))/sd(iso_AA$ISO_AA)) #0.823 #BART Better


### Final Model ISO AA ####

iso_AA_mod <- bart2(ISO_AA ~ ., 
               data = iso_AA,
               sigdf = sigdf_iso_AA,
               sigquant = sigquant_iso_AA,
               k = k_iso_AA,
               power = power_iso_AA,
               base = base_iso_AA,
               n.trees = 800,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)


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

#finding the optimal number of CVs
bart_iso_AAA_cvs <- function(cv){
  set.seed(101);iso_AAA$fold <- sample(1:cv, nrow(iso_AAA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- iso_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- iso_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ISO_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ISO_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ISO_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_iso_AAA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_iso_AAA_cvs))

bart_iso_AAA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.042, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #16


hyperparam_bart_iso_AAA <- function(sigdf_iso_AAA, sigquant_iso_AAA, k_iso_AAA, power_iso_AAA, base_iso_AAA, row_num = 1){
  set.seed(101);iso_AAA$fold <- sample(1:16, nrow(iso_AAA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_iso_AAA))
  print(paste('Sigquant: ', sigquant_iso_AAA))
  print(paste('K: ', k_iso_AAA))
  print(paste('Power: ', power_iso_AAA))
  print(paste('Base: ', base_iso_AAA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:16){
    print(paste('Iteration: ', i))
    train_data <- iso_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- iso_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ISO_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ISO_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_iso_AAA,
                   sigquant = sigquant_iso_AAA,
                   k = k_iso_AAA,
                   base = base_iso_AAA,
                   power = power_iso_AAA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ISO_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_iso_AAA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_iso_AAA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_iso_AAA_0.75_df$sigdf,
                                                      hyperparam_bart_iso_AAA_0.75_df$sigquant, 
                                                      hyperparam_bart_iso_AAA_0.75_df$k, 
                                                      hyperparam_bart_iso_AAA_0.75_df$power,
                                                      hyperparam_bart_iso_AAA_0.75_df$base, 
                                                      hyperparam_bart_iso_AAA_0.75_df$row_num), hyperparam_bart_iso_AAA, .progress = TRUE)

hyperparam_bart_iso_AAA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_iso_AAA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_iso_AAA_0.9_df$sigdf,
                                                     hyperparam_bart_iso_AAA_0.9_df$sigquant, 
                                                     hyperparam_bart_iso_AAA_0.9_df$k, 
                                                     hyperparam_bart_iso_AAA_0.9_df$power,
                                                     hyperparam_bart_iso_AAA_0.9_df$base,
                                                     hyperparam_bart_iso_AAA_0.9_df$row_num), hyperparam_bart_iso_AAA, .progress = TRUE)

hyperparam_bart_iso_AAA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_iso_AAA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_iso_AAA_0.99_df$sigdf,
                                                      hyperparam_bart_iso_AAA_0.99_df$sigquant, 
                                                      hyperparam_bart_iso_AAA_0.99_df$k, 
                                                      hyperparam_bart_iso_AAA_0.99_df$power,
                                                      hyperparam_bart_iso_AAA_0.99_df$base,
                                                      hyperparam_bart_iso_AAA_0.99_df$row_num), hyperparam_bart_iso_AAA, .progress = TRUE)



hyperparam_bart_iso_AAA_df <- bind_rows(hyperparam_bart_iso_AAA_0.75_df, hyperparam_bart_iso_AAA_0.9_df, hyperparam_bart_iso_AAA_0.99_df)

hyperparam_bart_iso_AAA_df <- hyperparam_bart_iso_AAA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_iso_AAA <- hyperparam_bart_iso_AAA_df$sigdf) #3
(sigquant_iso_AAA <- hyperparam_bart_iso_AAA_df$sigquant) #0.99
(k_iso_AAA <- hyperparam_bart_iso_AAA_df$k) #6
(power_iso_AAA <- hyperparam_bart_iso_AAA_df$power) #3
(base_iso_AAA <- hyperparam_bart_iso_AAA_df$base) #0.95

hyperparam_bart_iso_AAA_df$rmse/sd(iso_AAA$ISO_AAA) #0.801 #BART Better

hyperparam_bart_iso_AAA_trees <- function(trees_iso_AAA){
  set.seed(101);iso_AAA$fold <- sample(1:16, nrow(iso_AAA), replace = TRUE)
  
  print(paste('Trees: ', trees_iso_AAA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:16){
    print(paste('Iteration: ', i))
    train_data <- iso_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- iso_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ISO_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ISO_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_iso_AAA,
                   sigquant = sigquant_iso_AAA,
                   k = k_iso_AAA,
                   power = power_iso_AAA,
                   base = base_iso_AAA,
                   n.trees = trees_iso_AAA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ISO_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_iso_AAA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_iso_AAA_trees, .progress = TRUE))


bart_iso_AAA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  # scale_y_continuous(limits = c(0.038, 0.0383)) +
  theme_bw() #200

(iso_aaa_error_ratio <- (bart_iso_AAA_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(iso_AAA$ISO_AAA)) #0.801

### Final Model ISO AAA ####

iso_AAA_mod <- bart2(ISO_AAA ~ ., 
               data = iso_AAA,
               sigdf = sigdf_iso_AAA,
               sigquant = sigquant_iso_AAA,
               k = k_iso_AAA,
               power = power_iso_AAA,
               base = base_iso_AAA,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

### ISO AAA to MLB ####
iso_mlb <- mlb_hitting %>% 
  drop_na(Name_AAA) %>% 
  rowwise() %>% 
  mutate(TB_AAA = x1B_AAA + 2*x2B_AAA + 3*x3B_AAA + 4*HR_AAA) %>% 
  reframe(
    ISO_MLB,
    top_100_rank = mean(c(rank_ba, rank_mlb)),
    team_rank = team_rank_ba,
    iso_AAA_adj = (TB_AAA - H_AAA)/(AB_AAA^0.8),
    GB_per_FB_AAA,
    LD_pct_AAA,
    wRC_plus_AAA,
    BB_minus_K_pct_AAA = BB_pct_AAA - K_pct_AAA,
    SwStr_pct_AAA
  ) %>% 
  ungroup()

#finding the optimal number of CVs
bart_iso_mlb_cvs <- function(cv){
  set.seed(101);iso_mlb$fold <- sample(1:cv, nrow(iso_mlb), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- iso_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- iso_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ISO_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ISO_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ISO_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_iso_mlb_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_iso_mlb_cvs))

bart_iso_mlb_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #6


hyperparam_bart_iso_mlb <- function(sigdf_iso_mlb, sigquant_iso_mlb, k_iso_mlb, power_iso_mlb, base_iso_mlb,row_num = 1){
  set.seed(101);iso_mlb$fold <- sample(1:6, nrow(iso_mlb), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_iso_mlb))
  print(paste('Sigquant: ', sigquant_iso_mlb))
  print(paste('K: ', k_iso_mlb))
  print(paste('Power: ', power_iso_mlb))
  print(paste('Base: ', base_iso_mlb))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:6){
    print(paste('Iteration: ', i))
    train_data <- iso_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- iso_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ISO_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ISO_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_iso_mlb,
                   sigquant = sigquant_iso_mlb,
                   k = k_iso_mlb,
                   power = power_iso_mlb,
                   base = base_iso_mlb,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ISO_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_iso_mlb_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_iso_mlb_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_iso_mlb_0.75_df$sigdf,
                                                      hyperparam_bart_iso_mlb_0.75_df$sigquant, 
                                                      hyperparam_bart_iso_mlb_0.75_df$k, 
                                                      hyperparam_bart_iso_mlb_0.75_df$power,
                                                      hyperparam_bart_iso_mlb_0.75_df$base,
                                                      hyperparam_bart_iso_mlb_0.75_df$row_num), hyperparam_bart_iso_mlb, .progress = TRUE)

hyperparam_bart_iso_mlb_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_iso_mlb_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_iso_mlb_0.9_df$sigdf,
                                                     hyperparam_bart_iso_mlb_0.9_df$sigquant, 
                                                     hyperparam_bart_iso_mlb_0.9_df$k, 
                                                     hyperparam_bart_iso_mlb_0.9_df$power,
                                                     hyperparam_bart_iso_mlb_0.9_df$base,
                                                     hyperparam_bart_iso_mlb_0.9_df$row_num), hyperparam_bart_iso_mlb, .progress = TRUE)

hyperparam_bart_iso_mlb_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_iso_mlb_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_iso_mlb_0.99_df$sigdf,
                                                      hyperparam_bart_iso_mlb_0.99_df$sigquant, 
                                                      hyperparam_bart_iso_mlb_0.99_df$k, 
                                                      hyperparam_bart_iso_mlb_0.99_df$power,
                                                      hyperparam_bart_iso_mlb_0.99_df$base,
                                                      hyperparam_bart_iso_mlb_0.99_df$row_num), hyperparam_bart_iso_mlb, .progress = TRUE)



hyperparam_bart_iso_mlb_df <- bind_rows(hyperparam_bart_iso_mlb_0.75_df, hyperparam_bart_iso_mlb_0.9_df, hyperparam_bart_iso_mlb_0.99_df)

hyperparam_bart_iso_mlb_df <- hyperparam_bart_iso_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_iso_mlb <- hyperparam_bart_iso_mlb_df$sigdf) #3
(sigquant_iso_mlb <- hyperparam_bart_iso_mlb_df$sigquant) #0.99
(k_iso_mlb <- hyperparam_bart_iso_mlb_df$k) #3
(power_iso_mlb <- hyperparam_bart_iso_mlb_df$power) #2
(base_iso_mlb <- hyperparam_bart_iso_mlb_df$base) #0.5

hyperparam_bart_iso_mlb_df$rmse/sd(iso_mlb$ISO_MLB) #0.854 #BART Better

hyperparam_bart_iso_mlb_trees <- function(trees_iso_mlb){
  set.seed(101);iso_mlb$fold <- sample(1:6, nrow(iso_mlb), replace = TRUE)
  
  print(paste('Trees: ', trees_iso_mlb))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:6){
    print(paste('Iteration: ', i))
    train_data <- iso_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- iso_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ISO_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ISO_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_iso_mlb,
                   sigquant = sigquant_iso_mlb,
                   k = k_iso_mlb,
                   power = power_iso_mlb,
                   base = base_iso_mlb,
                   n.trees = trees_iso_mlb,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ISO_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_iso_mlb_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_iso_mlb_trees, .progress = TRUE))


bart_iso_mlb_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw()

(iso_mlb_error_ratio <- (bart_iso_mlb_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(iso_mlb$ISO_MLB)) #0.854 #BART Better

### Final Model ISO MLB ####
iso_mlb_mod <- bart2(ISO_MLB ~ ., 
               data = iso_mlb,
               sigdf = sigdf_iso_mlb,
               sigquant = sigquant_iso_mlb,
               k = k_iso_mlb,
               power = power_iso_mlb,
               base = base_iso_mlb,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(mlb_hitting %>% drop_na(Name_AAA) %>% pull(ISO_MLB) - residuals(iso_mlb_mod))


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

#finding the optimal number of CVs
bart_wrcplus_aplus_cvs <- function(cv){
  set.seed(101);wrcplus_aplus$fold <- sample(1:cv, nrow(wrcplus_aplus), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- wrcplus_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- wrcplus_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-wRC_plus_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(wRC_plus_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$wRC_plus_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_wrcplus_aplus_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_wrcplus_aplus_cvs))

bart_wrcplus_aplus_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.043, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #14


hyperparam_bart_wrcplus_aplus <- function(sigdf_wrcplus_aplus, sigquant_wrcplus_aplus, k_wrcplus_aplus, power_wrcplus_aplus, base_wrcplus_aplus, row_num){
  set.seed(101);wrcplus_aplus$fold <- sample(1:14, nrow(wrcplus_aplus), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_wrcplus_aplus))
  print(paste('Sigquant: ', sigquant_wrcplus_aplus))
  print(paste('K: ', k_wrcplus_aplus))
  print(paste('Power: ', power_wrcplus_aplus))
  print(paste('Base: ', base_wrcplus_aplus))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:14){
    print(paste('Iteration: ', i))
    train_data <- wrcplus_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- wrcplus_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-wRC_plus_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(wRC_plus_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_wrcplus_aplus,
                   sigquant = sigquant_wrcplus_aplus,
                   k = k_wrcplus_aplus,
                   power = power_wrcplus_aplus,
                   base = base_wrcplus_aplus,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$wRC_plus_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_wrcplus_aplus_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_wrcplus_aplus_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_wrcplus_aplus_0.75_df$sigdf,
                                                        hyperparam_bart_wrcplus_aplus_0.75_df$sigquant, 
                                                        hyperparam_bart_wrcplus_aplus_0.75_df$k, 
                                                        hyperparam_bart_wrcplus_aplus_0.75_df$power,
                                                        hyperparam_bart_wrcplus_aplus_0.75_df$base,
                                                        hyperparam_bart_wrcplus_aplus_0.75_df$row_num), hyperparam_bart_wrcplus_aplus, .progress = TRUE)

hyperparam_bart_wrcplus_aplus_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_wrcplus_aplus_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_wrcplus_aplus_0.9_df$sigdf,
                                                       hyperparam_bart_wrcplus_aplus_0.9_df$sigquant, 
                                                       hyperparam_bart_wrcplus_aplus_0.9_df$k, 
                                                       hyperparam_bart_wrcplus_aplus_0.9_df$power,
                                                       hyperparam_bart_wrcplus_aplus_0.9_df$base,
                                                       hyperparam_bart_wrcplus_aplus_0.9_df$row_num), hyperparam_bart_wrcplus_aplus, .progress = TRUE)

hyperparam_bart_wrcplus_aplus_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_wrcplus_aplus_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_wrcplus_aplus_0.99_df$sigdf,
                                                        hyperparam_bart_wrcplus_aplus_0.99_df$sigquant, 
                                                        hyperparam_bart_wrcplus_aplus_0.99_df$k, 
                                                        hyperparam_bart_wrcplus_aplus_0.99_df$power,
                                                        hyperparam_bart_wrcplus_aplus_0.99_df$base,
                                                        hyperparam_bart_wrcplus_aplus_0.99_df$row_num), hyperparam_bart_wrcplus_aplus, .progress = TRUE)



hyperparam_bart_wrcplus_aplus_df <- bind_rows(hyperparam_bart_wrcplus_aplus_0.75_df, hyperparam_bart_wrcplus_aplus_0.9_df, hyperparam_bart_wrcplus_aplus_0.99_df)

hyperparam_bart_wrcplus_aplus_df <- hyperparam_bart_wrcplus_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_wrcplus_aplus <- hyperparam_bart_wrcplus_aplus_df$sigdf) #3
(sigquant_wrcplus_aplus <- hyperparam_bart_wrcplus_aplus_df$sigquant) #0.9
(k_wrcplus_aplus <- hyperparam_bart_wrcplus_aplus_df$k) # 6
(power_wrcplus_aplus <- hyperparam_bart_wrcplus_aplus_df$power) # 3
(base_wrcplus_aplus <- hyperparam_bart_wrcplus_aplus_df$base) # 0.95

hyperparam_bart_wrcplus_aplus_df$rmse/sd(wrcplus_aplus$wRC_plus_Aplus) #0.892 #BART Better


hyperparam_bart_wrcplus_aplus_trees <- function(trees_wrcplus_aplus){
  set.seed(101);wrcplus_aplus$fold <- sample(1:14, nrow(wrcplus_aplus), replace = TRUE)
  
  print(paste('Trees: ', trees_wrcplus_aplus))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:14){
    print(paste('Iteration: ', i))
    train_data <- wrcplus_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- wrcplus_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-wRC_plus_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(wRC_plus_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_wrcplus_aplus,
                   sigquant = sigquant_wrcplus_aplus,
                   k = k_wrcplus_aplus,
                   power = power_wrcplus_aplus,
                   base = base_wrcplus_aplus,
                   n.trees = trees_wrcplus_aplus,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$wRC_plus_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_wrcplus_aplus_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_wrcplus_aplus_trees, .progress = TRUE))


bart_wrcplus_aplus_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #1200


(wrcplus_aplus_error_ratio <- (bart_wrcplus_aplus_trees_df %>% filter(trees == 1200) %>% pull(rmse))/sd(wrcplus_aplus$wRC_plus_Aplus)) #0.892 #BART Better

### Final Model wRC+ A+ ####

wrcplus_aplus_mod <- bart2(wRC_plus_Aplus ~ ., 
               data = wrcplus_aplus,
               sigdf = sigdf_wrcplus_aplus,
               sigquant = sigquant_wrcplus_aplus,
               k = k_wrcplus_aplus,
               power = power_wrcplus_aplus,
               base = base_wrcplus_aplus,
               n.trees = 1200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(wrcplus_aplus$wRC_plus_Aplus - residuals(wrcplus_aplus_mod))


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

#finding the optimal number of CVs
bart_wrcplus_AA_cvs <- function(cv){
  set.seed(101);wrcplus_AA$fold <- sample(1:cv, nrow(wrcplus_AA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- wrcplus_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- wrcplus_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-wRC_plus_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(wRC_plus_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$wRC_plus_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_wrcplus_AA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_wrcplus_AA_cvs))

bart_wrcplus_AA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #6


hyperparam_bart_wrcplus_AA <- function(sigdf_wrcplus_AA, sigquant_wrcplus_AA, k_wrcplus_AA, power_wrcplus_AA, base_wrcplus_AA,row_num = 1){
  set.seed(101);wrcplus_AA$fold <- sample(1:6, nrow(wrcplus_AA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_wrcplus_AA))
  print(paste('Sigquant: ', sigquant_wrcplus_AA))
  print(paste('K: ', k_wrcplus_AA))
  print(paste('Power: ', power_wrcplus_AA))
  print(paste('Base: ', base_wrcplus_AA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:6){
    print(paste('Iteration: ', i))
    train_data <- wrcplus_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- wrcplus_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-wRC_plus_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(wRC_plus_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_wrcplus_AA,
                   sigquant = sigquant_wrcplus_AA,
                   k = k_wrcplus_AA,
                   power = power_wrcplus_AA,
                   base = base_wrcplus_AA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$wRC_plus_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_wrcplus_AA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_wrcplus_AA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_wrcplus_AA_0.75_df$sigdf,
                                                     hyperparam_bart_wrcplus_AA_0.75_df$sigquant, 
                                                     hyperparam_bart_wrcplus_AA_0.75_df$k, 
                                                     hyperparam_bart_wrcplus_AA_0.75_df$power,
                                                     hyperparam_bart_wrcplus_AA_0.75_df$base,
                                                     hyperparam_bart_wrcplus_AA_0.75_df$row_num), hyperparam_bart_wrcplus_AA, .progress = TRUE)

hyperparam_bart_wrcplus_AA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_wrcplus_AA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_wrcplus_AA_0.9_df$sigdf,
                                                    hyperparam_bart_wrcplus_AA_0.9_df$sigquant, 
                                                    hyperparam_bart_wrcplus_AA_0.9_df$k, 
                                                    hyperparam_bart_wrcplus_AA_0.9_df$power,
                                                    hyperparam_bart_wrcplus_AA_0.9_df$base,
                                                    hyperparam_bart_wrcplus_AA_0.9_df$row_num), hyperparam_bart_wrcplus_AA, .progress = TRUE)

hyperparam_bart_wrcplus_AA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_wrcplus_AA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_wrcplus_AA_0.99_df$sigdf,
                                                     hyperparam_bart_wrcplus_AA_0.99_df$sigquant, 
                                                     hyperparam_bart_wrcplus_AA_0.99_df$k, 
                                                     hyperparam_bart_wrcplus_AA_0.99_df$power,
                                                     hyperparam_bart_wrcplus_AA_0.99_df$base,
                                                     hyperparam_bart_wrcplus_AA_0.99_df$row_num), hyperparam_bart_wrcplus_AA, .progress = TRUE)



hyperparam_bart_wrcplus_AA_df <- bind_rows(hyperparam_bart_wrcplus_AA_0.75_df, hyperparam_bart_wrcplus_AA_0.9_df, hyperparam_bart_wrcplus_AA_0.99_df)

hyperparam_bart_wrcplus_AA_df <- hyperparam_bart_wrcplus_AA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_wrcplus_AA <- hyperparam_bart_wrcplus_AA_df$sigdf) #3
(sigquant_wrcplus_AA <- hyperparam_bart_wrcplus_AA_df$sigquant) #0.99
(k_wrcplus_AA <- hyperparam_bart_wrcplus_AA_df$k) #6
(power_wrcplus_AA <- hyperparam_bart_wrcplus_AA_df$power) #3
(base_wrcplus_AA <- hyperparam_bart_wrcplus_AA_df$base) #0.95

hyperparam_bart_wrcplus_AA_df$rmse/sd(wrcplus_AA$wRC_plus_AA) #0.865 #BART Better

hyperparam_bart_wrcplus_AA_trees <- function(trees_wrcplus_AA){
  set.seed(101);wrcplus_AA$fold <- sample(1:6, nrow(wrcplus_AA), replace = TRUE)
  
  print(paste('Trees: ', trees_wrcplus_AA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:6){
    print(paste('Iteration: ', i))
    train_data <- wrcplus_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- wrcplus_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-wRC_plus_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(wRC_plus_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_wrcplus_AA,
                   sigquant = sigquant_wrcplus_AA,
                   k = k_wrcplus_AA,
                   power = power_wrcplus_AA,
                   base = base_wrcplus_AA,
                   n.trees = trees_wrcplus_AA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$wRC_plus_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_wrcplus_AA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_wrcplus_AA_trees, .progress = TRUE))


bart_wrcplus_AA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #400

(wrcplus_aa_error_ratio <- (bart_wrcplus_AA_trees_df %>% filter(trees == 400) %>% pull(rmse))/sd(wrcplus_AA$wRC_plus_AA)) #0.865


### Final Model wRC+ AA ####

wrcplus_AA_mod <- bart2(wRC_plus_AA ~ ., 
               data = wrcplus_AA,
               sigdf = sigdf_wrcplus_AA,
               sigquant = sigquant_wrcplus_AA,
               k = k_wrcplus_AA,
               power = power_wrcplus_AA,
               base = base_wrcplus_AA,
               n.trees = 400,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

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

#finding the optimal number of CVs
bart_wrcplus_AAA_cvs <- function(cv){
  set.seed(101);wrcplus_AAA$fold <- sample(1:cv, nrow(wrcplus_AAA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- wrcplus_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- wrcplus_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-wRC_plus_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(wRC_plus_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$wRC_plus_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_wrcplus_AAA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_wrcplus_AAA_cvs))

bart_wrcplus_AAA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.042, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #10


hyperparam_bart_wrcplus_AAA <- function(sigdf_wrcplus_AAA, sigquant_wrcplus_AAA, k_wrcplus_AAA, power_wrcplus_AAA, base_wrcplus_AAA, row_num = 1){
  set.seed(101);wrcplus_AAA$fold <- sample(1:10, nrow(wrcplus_AAA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_wrcplus_AAA))
  print(paste('Sigquant: ', sigquant_wrcplus_AAA))
  print(paste('K: ', k_wrcplus_AAA))
  print(paste('Power: ', power_wrcplus_AAA))
  print(paste('Base: ', base_wrcplus_AAA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- wrcplus_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- wrcplus_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-wRC_plus_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(wRC_plus_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_wrcplus_AAA,
                   sigquant = sigquant_wrcplus_AAA,
                   k = k_wrcplus_AAA,
                   base = base_wrcplus_AAA,
                   power = power_wrcplus_AAA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$wRC_plus_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_wrcplus_AAA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_wrcplus_AAA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_wrcplus_AAA_0.75_df$sigdf,
                                                      hyperparam_bart_wrcplus_AAA_0.75_df$sigquant, 
                                                      hyperparam_bart_wrcplus_AAA_0.75_df$k, 
                                                      hyperparam_bart_wrcplus_AAA_0.75_df$power,
                                                      hyperparam_bart_wrcplus_AAA_0.75_df$base, 
                                                      hyperparam_bart_wrcplus_AAA_0.75_df$row_num), hyperparam_bart_wrcplus_AAA, .progress = TRUE)

hyperparam_bart_wrcplus_AAA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_wrcplus_AAA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_wrcplus_AAA_0.9_df$sigdf,
                                                     hyperparam_bart_wrcplus_AAA_0.9_df$sigquant, 
                                                     hyperparam_bart_wrcplus_AAA_0.9_df$k, 
                                                     hyperparam_bart_wrcplus_AAA_0.9_df$power,
                                                     hyperparam_bart_wrcplus_AAA_0.9_df$base,
                                                     hyperparam_bart_wrcplus_AAA_0.9_df$row_num), hyperparam_bart_wrcplus_AAA, .progress = TRUE)

hyperparam_bart_wrcplus_AAA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_wrcplus_AAA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_wrcplus_AAA_0.99_df$sigdf,
                                                      hyperparam_bart_wrcplus_AAA_0.99_df$sigquant, 
                                                      hyperparam_bart_wrcplus_AAA_0.99_df$k, 
                                                      hyperparam_bart_wrcplus_AAA_0.99_df$power,
                                                      hyperparam_bart_wrcplus_AAA_0.99_df$base,
                                                      hyperparam_bart_wrcplus_AAA_0.99_df$row_num), hyperparam_bart_wrcplus_AAA, .progress = TRUE)



hyperparam_bart_wrcplus_AAA_df <- bind_rows(hyperparam_bart_wrcplus_AAA_0.75_df, hyperparam_bart_wrcplus_AAA_0.9_df, hyperparam_bart_wrcplus_AAA_0.99_df)

hyperparam_bart_wrcplus_AAA_df <- hyperparam_bart_wrcplus_AAA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_wrcplus_AAA <- hyperparam_bart_wrcplus_AAA_df$sigdf) #10
(sigquant_wrcplus_AAA <- hyperparam_bart_wrcplus_AAA_df$sigquant) #0.75
(k_wrcplus_AAA <- hyperparam_bart_wrcplus_AAA_df$k) #5
(power_wrcplus_AAA <- hyperparam_bart_wrcplus_AAA_df$power) #5
(base_wrcplus_AAA <- hyperparam_bart_wrcplus_AAA_df$base) #0.5

hyperparam_bart_wrcplus_AAA_df$rmse/sd(wrcplus_AAA$wRC_plus_AAA) #0.864 #BART Better

hyperparam_bart_wrcplus_AAA_trees <- function(trees_wrcplus_AAA){
  set.seed(101);wrcplus_AAA$fold <- sample(1:10, nrow(wrcplus_AAA), replace = TRUE)
  
  print(paste('Trees: ', trees_wrcplus_AAA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- wrcplus_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- wrcplus_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-wRC_plus_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(wRC_plus_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_wrcplus_AAA,
                   sigquant = sigquant_wrcplus_AAA,
                   k = k_wrcplus_AAA,
                   power = power_wrcplus_AAA,
                   base = base_wrcplus_AAA,
                   n.trees = trees_wrcplus_AAA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$wRC_plus_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_wrcplus_AAA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_wrcplus_AAA_trees, .progress = TRUE))


bart_wrcplus_AAA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  # scale_y_continuous(limits = c(0.038, 0.0383)) +
  theme_bw() #200

(wrcplus_aaa_error_ratio <- (bart_wrcplus_AAA_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(wrcplus_AAA$wRC_plus_AAA)) #0.864 #BART Better

### Final Model wRC+ AAA ####

wrcplus_AAA_mod <- bart2(wRC_plus_AAA ~ ., 
               data = wrcplus_AAA,
               sigdf = sigdf_wrcplus_AAA,
               sigquant = sigquant_wrcplus_AAA,
               k = k_wrcplus_AAA,
               power = power_wrcplus_AAA,
               base = base_wrcplus_AAA,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(wrcplus_AAA$wRC_plus_AAA - residuals(wrcplus_AAA_mod))

summary(wrcplus_AAA$wRC_plus_AAA)


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

#finding the optimal number of CVs
bart_wrcplus_mlb_cvs <- function(cv){
  set.seed(101);wrcplus_mlb$fold <- sample(1:cv, nrow(wrcplus_mlb), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- wrcplus_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- wrcplus_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-wRC_plus_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(wRC_plus_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$wRC_plus_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_wrcplus_mlb_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_wrcplus_mlb_cvs))

bart_wrcplus_mlb_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #16


hyperparam_bart_wrcplus_mlb <- function(sigdf_wrcplus_mlb, sigquant_wrcplus_mlb, k_wrcplus_mlb, power_wrcplus_mlb, base_wrcplus_mlb,row_num = 1){
  set.seed(101);wrcplus_mlb$fold <- sample(1:16, nrow(wrcplus_mlb), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_wrcplus_mlb))
  print(paste('Sigquant: ', sigquant_wrcplus_mlb))
  print(paste('K: ', k_wrcplus_mlb))
  print(paste('Power: ', power_wrcplus_mlb))
  print(paste('Base: ', base_wrcplus_mlb))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:16){
    print(paste('Iteration: ', i))
    train_data <- wrcplus_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- wrcplus_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-wRC_plus_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(wRC_plus_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_wrcplus_mlb,
                   sigquant = sigquant_wrcplus_mlb,
                   k = k_wrcplus_mlb,
                   power = power_wrcplus_mlb,
                   base = base_wrcplus_mlb,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$wRC_plus_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_wrcplus_mlb_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_wrcplus_mlb_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_wrcplus_mlb_0.75_df$sigdf,
                                                      hyperparam_bart_wrcplus_mlb_0.75_df$sigquant, 
                                                      hyperparam_bart_wrcplus_mlb_0.75_df$k, 
                                                      hyperparam_bart_wrcplus_mlb_0.75_df$power,
                                                      hyperparam_bart_wrcplus_mlb_0.75_df$base,
                                                      hyperparam_bart_wrcplus_mlb_0.75_df$row_num), hyperparam_bart_wrcplus_mlb, .progress = TRUE)

hyperparam_bart_wrcplus_mlb_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_wrcplus_mlb_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_wrcplus_mlb_0.9_df$sigdf,
                                                     hyperparam_bart_wrcplus_mlb_0.9_df$sigquant, 
                                                     hyperparam_bart_wrcplus_mlb_0.9_df$k, 
                                                     hyperparam_bart_wrcplus_mlb_0.9_df$power,
                                                     hyperparam_bart_wrcplus_mlb_0.9_df$base,
                                                     hyperparam_bart_wrcplus_mlb_0.9_df$row_num), hyperparam_bart_wrcplus_mlb, .progress = TRUE)

hyperparam_bart_wrcplus_mlb_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_wrcplus_mlb_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_wrcplus_mlb_0.99_df$sigdf,
                                                      hyperparam_bart_wrcplus_mlb_0.99_df$sigquant, 
                                                      hyperparam_bart_wrcplus_mlb_0.99_df$k, 
                                                      hyperparam_bart_wrcplus_mlb_0.99_df$power,
                                                      hyperparam_bart_wrcplus_mlb_0.99_df$base,
                                                      hyperparam_bart_wrcplus_mlb_0.99_df$row_num), hyperparam_bart_wrcplus_mlb, .progress = TRUE)



hyperparam_bart_wrcplus_mlb_df <- bind_rows(hyperparam_bart_wrcplus_mlb_0.75_df, hyperparam_bart_wrcplus_mlb_0.9_df, hyperparam_bart_wrcplus_mlb_0.99_df)

hyperparam_bart_wrcplus_mlb_df <- hyperparam_bart_wrcplus_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_wrcplus_mlb <- hyperparam_bart_wrcplus_mlb_df$sigdf) #3
(sigquant_wrcplus_mlb <- hyperparam_bart_wrcplus_mlb_df$sigquant) #0.9
(k_wrcplus_mlb <- hyperparam_bart_wrcplus_mlb_df$k) #4
(power_wrcplus_mlb <- hyperparam_bart_wrcplus_mlb_df$power) #6
(base_wrcplus_mlb <- hyperparam_bart_wrcplus_mlb_df$base) #0.5

hyperparam_bart_wrcplus_mlb_df$rmse/sd(wrcplus_mlb$wRC_plus_MLB) #0.944 #BART Better

hyperparam_bart_wrcplus_mlb_trees <- function(trees_wrcplus_mlb){
  set.seed(101);wrcplus_mlb$fold <- sample(1:16, nrow(wrcplus_mlb), replace = TRUE)
  
  print(paste('Trees: ', trees_wrcplus_mlb))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:16){
    print(paste('Iteration: ', i))
    train_data <- wrcplus_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- wrcplus_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-wRC_plus_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(wRC_plus_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_wrcplus_mlb,
                   sigquant = sigquant_wrcplus_mlb,
                   k = k_wrcplus_mlb,
                   power = power_wrcplus_mlb,
                   base = base_wrcplus_mlb,
                   n.trees = trees_wrcplus_mlb,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$wRC_plus_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_wrcplus_mlb_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_wrcplus_mlb_trees, .progress = TRUE))


bart_wrcplus_mlb_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() 

(wrcplus_mlb_error_ratio <- (bart_wrcplus_mlb_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(wrcplus_mlb$wRC_plus_MLB)) #0.944 #BART Better


### Final Model wRC+ MLB ####

wrcplus_mlb_mod <- bart2(wRC_plus_MLB ~ ., 
               data = wrcplus_mlb,
               sigdf = sigdf_wrcplus_mlb,
               sigquant = sigquant_wrcplus_mlb,
               k = k_wrcplus_mlb,
               power = power_wrcplus_mlb,
               base = base_wrcplus_mlb,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(wrcplus_mlb$wRC_plus_MLB - residuals(wrcplus_mlb_mod))


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

#finding the optimal number of CVs
bart_kpct_aplus_cvs <- function(cv){
  set.seed(101);kpct_aplus$fold <- sample(1:cv, nrow(kpct_aplus), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- kpct_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpct_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_kpct_aplus_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_kpct_aplus_cvs))

bart_kpct_aplus_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.043, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #4


hyperparam_bart_kpct_aplus <- function(sigdf_kpct_aplus, sigquant_kpct_aplus, k_kpct_aplus, power_kpct_aplus, base_kpct_aplus, row_num){
  set.seed(101);kpct_aplus$fold <- sample(1:4, nrow(kpct_aplus), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_kpct_aplus))
  print(paste('Sigquant: ', sigquant_kpct_aplus))
  print(paste('K: ', k_kpct_aplus))
  print(paste('Power: ', power_kpct_aplus))
  print(paste('Base: ', base_kpct_aplus))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:4){
    print(paste('Iteration: ', i))
    train_data <- kpct_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpct_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_kpct_aplus,
                   sigquant = sigquant_kpct_aplus,
                   k = k_kpct_aplus,
                   power = power_kpct_aplus,
                   base = base_kpct_aplus,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_kpct_aplus_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_kpct_aplus_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_kpct_aplus_0.75_df$sigdf,
                                                            hyperparam_bart_kpct_aplus_0.75_df$sigquant, 
                                                            hyperparam_bart_kpct_aplus_0.75_df$k, 
                                                            hyperparam_bart_kpct_aplus_0.75_df$power,
                                                            hyperparam_bart_kpct_aplus_0.75_df$base,
                                                            hyperparam_bart_kpct_aplus_0.75_df$row_num), hyperparam_bart_kpct_aplus, .progress = TRUE)

hyperparam_bart_kpct_aplus_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_kpct_aplus_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_kpct_aplus_0.9_df$sigdf,
                                                           hyperparam_bart_kpct_aplus_0.9_df$sigquant, 
                                                           hyperparam_bart_kpct_aplus_0.9_df$k, 
                                                           hyperparam_bart_kpct_aplus_0.9_df$power,
                                                           hyperparam_bart_kpct_aplus_0.9_df$base,
                                                           hyperparam_bart_kpct_aplus_0.9_df$row_num), hyperparam_bart_kpct_aplus, .progress = TRUE)

hyperparam_bart_kpct_aplus_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_kpct_aplus_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_kpct_aplus_0.99_df$sigdf,
                                                            hyperparam_bart_kpct_aplus_0.99_df$sigquant, 
                                                            hyperparam_bart_kpct_aplus_0.99_df$k, 
                                                            hyperparam_bart_kpct_aplus_0.99_df$power,
                                                            hyperparam_bart_kpct_aplus_0.99_df$base,
                                                            hyperparam_bart_kpct_aplus_0.99_df$row_num), hyperparam_bart_kpct_aplus, .progress = TRUE)



hyperparam_bart_kpct_aplus_df <- bind_rows(hyperparam_bart_kpct_aplus_0.75_df, hyperparam_bart_kpct_aplus_0.9_df, hyperparam_bart_kpct_aplus_0.99_df)

hyperparam_bart_kpct_aplus_df <- hyperparam_bart_kpct_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_kpct_aplus <- hyperparam_bart_kpct_aplus_df$sigdf) #10
(sigquant_kpct_aplus <- hyperparam_bart_kpct_aplus_df$sigquant) #0.75
(k_kpct_aplus <- hyperparam_bart_kpct_aplus_df$k) # 4
(power_kpct_aplus <- hyperparam_bart_kpct_aplus_df$power) # 1
(base_kpct_aplus <- hyperparam_bart_kpct_aplus_df$base) # 0.95

hyperparam_bart_kpct_aplus_df$rmse/sd(kpct_aplus$K_pct_Aplus) #0.678 #BART Better


hyperparam_bart_kpct_aplus_trees <- function(trees_kpct_aplus){
  set.seed(101);kpct_aplus$fold <- sample(1:4, nrow(kpct_aplus), replace = TRUE)
  
  print(paste('Trees: ', trees_kpct_aplus))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:4){
    print(paste('Iteration: ', i))
    train_data <- kpct_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpct_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_kpct_aplus,
                   sigquant = sigquant_kpct_aplus,
                   k = k_kpct_aplus,
                   power = power_kpct_aplus,
                   base = base_kpct_aplus,
                   n.trees = trees_kpct_aplus,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_kpct_aplus_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_kpct_aplus_trees, .progress = TRUE))


bart_kpct_aplus_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #200

(kpct_aplus_error_ratio <- (bart_kpct_aplus_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(kpct_aplus$K_pct_Aplus)) #0.678 #BART Better


### Final Model K% A+ ####
kpct_aplus_mod <- bart2(K_pct_Aplus ~ ., 
               data = kpct_aplus,
               sigdf = sigdf_kpct_aplus,
               sigquant = sigquant_kpct_aplus,
               k = k_kpct_aplus,
               power = power_kpct_aplus,
               base = base_kpct_aplus,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(kpct_aplus$K_pct_Aplus - residuals(kpct_aplus_mod))


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

#finding the optimal number of CVs
bart_kpct_AA_cvs <- function(cv){
  set.seed(101);kpct_AA$fold <- sample(1:cv, nrow(kpct_AA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- kpct_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpct_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_kpct_AA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_kpct_AA_cvs))

bart_kpct_AA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #12


hyperparam_bart_kpct_AA <- function(sigdf_kpct_AA, sigquant_kpct_AA, k_kpct_AA, power_kpct_AA, base_kpct_AA,row_num =1){
  set.seed(101);kpct_AA$fold <- sample(1:12, nrow(kpct_AA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_kpct_AA))
  print(paste('Sigquant: ', sigquant_kpct_AA))
  print(paste('K: ', k_kpct_AA))
  print(paste('Power: ', power_kpct_AA))
  print(paste('Base: ', base_kpct_AA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:12){
    print(paste('Iteration: ', i))
    train_data <- kpct_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpct_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_kpct_AA,
                   sigquant = sigquant_kpct_AA,
                   k = k_kpct_AA,
                   power = power_kpct_AA,
                   base = base_kpct_AA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_kpct_AA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_kpct_AA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_kpct_AA_0.75_df$sigdf,
                                                         hyperparam_bart_kpct_AA_0.75_df$sigquant, 
                                                         hyperparam_bart_kpct_AA_0.75_df$k, 
                                                         hyperparam_bart_kpct_AA_0.75_df$power,
                                                         hyperparam_bart_kpct_AA_0.75_df$base,
                                                         hyperparam_bart_kpct_AA_0.75_df$row_num), hyperparam_bart_kpct_AA, .progress = TRUE)

hyperparam_bart_kpct_AA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_kpct_AA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_kpct_AA_0.9_df$sigdf,
                                                        hyperparam_bart_kpct_AA_0.9_df$sigquant, 
                                                        hyperparam_bart_kpct_AA_0.9_df$k, 
                                                        hyperparam_bart_kpct_AA_0.9_df$power,
                                                        hyperparam_bart_kpct_AA_0.9_df$base,
                                                        hyperparam_bart_kpct_AA_0.9_df$row_num), hyperparam_bart_kpct_AA, .progress = TRUE)

hyperparam_bart_kpct_AA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_kpct_AA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_kpct_AA_0.99_df$sigdf,
                                                         hyperparam_bart_kpct_AA_0.99_df$sigquant, 
                                                         hyperparam_bart_kpct_AA_0.99_df$k, 
                                                         hyperparam_bart_kpct_AA_0.99_df$power,
                                                         hyperparam_bart_kpct_AA_0.99_df$base,
                                                         hyperparam_bart_kpct_AA_0.99_df$row_num), hyperparam_bart_kpct_AA, .progress = TRUE)



hyperparam_bart_kpct_AA_df <- bind_rows(hyperparam_bart_kpct_AA_0.75_df, hyperparam_bart_kpct_AA_0.9_df, hyperparam_bart_kpct_AA_0.99_df)

hyperparam_bart_kpct_AA_df <- hyperparam_bart_kpct_AA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_kpct_AA <- hyperparam_bart_kpct_AA_df$sigdf) #3
(sigquant_kpct_AA <- hyperparam_bart_kpct_AA_df$sigquant) #0.99
(k_kpct_AA <- hyperparam_bart_kpct_AA_df$k) #5
(power_kpct_AA <- hyperparam_bart_kpct_AA_df$power) #3
(base_kpct_AA <- hyperparam_bart_kpct_AA_df$base) #0.95

hyperparam_bart_kpct_AA_df$rmse/sd(kpct_AA$K_pct_AA) #0.682 #BART Better



hyperparam_bart_kpct_AA_trees <- function(trees_kpct_AA){
  set.seed(101);kpct_AA$fold <- sample(1:12, nrow(kpct_AA), replace = TRUE)
  
  print(paste('Trees: ', trees_kpct_AA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:12){
    print(paste('Iteration: ', i))
    train_data <- kpct_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpct_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_kpct_AA,
                   sigquant = sigquant_kpct_AA,
                   k = k_kpct_AA,
                   power = power_kpct_AA,
                   base = base_kpct_AA,
                   n.trees = trees_kpct_AA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_kpct_AA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_kpct_AA_trees, .progress = TRUE))


bart_kpct_AA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #200

(kpct_AA_error_ratio <- (bart_kpct_AA_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(kpct_AA$K_pct_AA)) #0.682 #BART Better



### Final Model K% AA ####
kpct_AA_mod <- bart2(K_pct_AA ~ ., 
               data = kpct_AA,
               sigdf = sigdf_kpct_AA,
               sigquant = sigquant_kpct_AA,
               k = k_kpct_AA,
               power = power_kpct_AA,
               base = base_kpct_AA,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(kpct_AA$K_pct_AA - residuals(kpct_AA_mod))


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

#finding the optimal number of CVs
bart_kpct_AAA_cvs <- function(cv){
  set.seed(101);kpct_AAA$fold <- sample(1:cv, nrow(kpct_AAA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- kpct_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpct_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_kpct_AAA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_kpct_AAA_cvs))

bart_kpct_AAA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.042, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #6


hyperparam_bart_kpct_AAA <- function(sigdf_kpct_AAA, sigquant_kpct_AAA, k_kpct_AAA, power_kpct_AAA, base_kpct_AAA, row_num){
  set.seed(101);kpct_AAA$fold <- sample(1:6, nrow(kpct_AAA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_kpct_AAA))
  print(paste('Sigquant: ', sigquant_kpct_AAA))
  print(paste('K: ', k_kpct_AAA))
  print(paste('Power: ', power_kpct_AAA))
  print(paste('Base: ', base_kpct_AAA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:6){
    print(paste('Iteration: ', i))
    train_data <- kpct_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpct_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_kpct_AAA,
                   sigquant = sigquant_kpct_AAA,
                   k = k_kpct_AAA,
                   base = base_kpct_AAA,
                   power = power_kpct_AAA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_kpct_AAA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_kpct_AAA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_kpct_AAA_0.75_df$sigdf,
                                                          hyperparam_bart_kpct_AAA_0.75_df$sigquant, 
                                                          hyperparam_bart_kpct_AAA_0.75_df$k, 
                                                          hyperparam_bart_kpct_AAA_0.75_df$power,
                                                          hyperparam_bart_kpct_AAA_0.75_df$base, 
                                                          hyperparam_bart_kpct_AAA_0.75_df$row_num), hyperparam_bart_kpct_AAA, .progress = TRUE)

hyperparam_bart_kpct_AAA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_kpct_AAA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_kpct_AAA_0.9_df$sigdf,
                                                         hyperparam_bart_kpct_AAA_0.9_df$sigquant, 
                                                         hyperparam_bart_kpct_AAA_0.9_df$k, 
                                                         hyperparam_bart_kpct_AAA_0.9_df$power,
                                                         hyperparam_bart_kpct_AAA_0.9_df$base,
                                                         hyperparam_bart_kpct_AAA_0.9_df$row_num), hyperparam_bart_kpct_AAA, .progress = TRUE)

hyperparam_bart_kpct_AAA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_kpct_AAA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_kpct_AAA_0.99_df$sigdf,
                                                          hyperparam_bart_kpct_AAA_0.99_df$sigquant, 
                                                          hyperparam_bart_kpct_AAA_0.99_df$k, 
                                                          hyperparam_bart_kpct_AAA_0.99_df$power,
                                                          hyperparam_bart_kpct_AAA_0.99_df$base,
                                                          hyperparam_bart_kpct_AAA_0.99_df$row_num), hyperparam_bart_kpct_AAA, .progress = TRUE)



hyperparam_bart_kpct_AAA_df <- bind_rows(hyperparam_bart_kpct_AAA_0.75_df, hyperparam_bart_kpct_AAA_0.9_df, hyperparam_bart_kpct_AAA_0.99_df)

hyperparam_bart_kpct_AAA_df <- hyperparam_bart_kpct_AAA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

sigdf_kpct_AAA <-hyperparam_bart_kpct_AAA_df$sigdf #10
sigquant_kpct_AAA <- hyperparam_bart_kpct_AAA_df$sigquant #0.75
k_kpct_AAA <- hyperparam_bart_kpct_AAA_df$k #6
power_kpct_AAA <- hyperparam_bart_kpct_AAA_df$power #3
base_kpct_AAA <- hyperparam_bart_kpct_AAA_df$base #0.95

hyperparam_bart_kpct_AAA_df$rmse/sd(kpct_AAA$K_pct_AAA) # 0.698 #BART Better

hyperparam_bart_kpct_AAA_trees <- function(trees_kpct_AAA){
  set.seed(101);kpct_AAA$fold <- sample(1:6, nrow(kpct_AAA), replace = TRUE)
  
  print(paste('Trees: ', trees_kpct_AAA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:6){
    print(paste('Iteration: ', i))
    train_data <- kpct_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpct_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_kpct_AAA,
                   sigquant = sigquant_kpct_AAA,
                   k = k_kpct_AAA,
                   power = power_kpct_AAA,
                   base = base_kpct_AAA,
                   n.trees = trees_kpct_AAA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_kpct_AAA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_kpct_AAA_trees))


bart_kpct_AAA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  # scale_y_continuous(limits = c(0.038, 0.0383)) +
  theme_bw() #200

(kpct_AAA_error_ratio <- (bart_kpct_AAA_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(kpct_AAA$K_pct_AAA)) #0.698 #BART Better

### Final Model K% AAA ####
kpct_AAA_mod <- bart2(K_pct_AAA ~ ., 
               data = kpct_AAA,
               sigdf = sigdf_kpct_AAA,
               sigquant = sigquant_kpct_AAA,
               k = k_kpct_AAA,
               power = power_kpct_AAA,
               base = base_kpct_AAA,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(kpct_AAA$K_pct_AAA - residuals(kpct_AAA_mod))

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

#finding the optimal number of CVs
bart_kpct_mlb_cvs <- function(cv){
  set.seed(101);kpct_mlb$fold <- sample(1:cv, nrow(kpct_mlb), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- kpct_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpct_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_kpct_mlb_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_kpct_mlb_cvs))

bart_kpct_mlb_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #4


hyperparam_bart_kpct_mlb <- function(sigdf_kpct_mlb, sigquant_kpct_mlb, k_kpct_mlb, power_kpct_mlb, base_kpct_mlb,row_num = 1){
  set.seed(101);kpct_mlb$fold <- sample(1:4, nrow(kpct_mlb), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_kpct_mlb))
  print(paste('Sigquant: ', sigquant_kpct_mlb))
  print(paste('K: ', k_kpct_mlb))
  print(paste('Power: ', power_kpct_mlb))
  print(paste('Base: ', base_kpct_mlb))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:4){
    print(paste('Iteration: ', i))
    train_data <- kpct_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpct_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_kpct_mlb,
                   sigquant = sigquant_kpct_mlb,
                   k = k_kpct_mlb,
                   power = power_kpct_mlb,
                   base = base_kpct_mlb,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_kpct_mlb_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_kpct_mlb_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_kpct_mlb_0.75_df$sigdf,
                                                          hyperparam_bart_kpct_mlb_0.75_df$sigquant, 
                                                          hyperparam_bart_kpct_mlb_0.75_df$k, 
                                                          hyperparam_bart_kpct_mlb_0.75_df$power,
                                                          hyperparam_bart_kpct_mlb_0.75_df$base,
                                                          hyperparam_bart_kpct_mlb_0.75_df$row_num), hyperparam_bart_kpct_mlb, .progress = TRUE)

hyperparam_bart_kpct_mlb_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_kpct_mlb_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_kpct_mlb_0.9_df$sigdf,
                                                         hyperparam_bart_kpct_mlb_0.9_df$sigquant, 
                                                         hyperparam_bart_kpct_mlb_0.9_df$k, 
                                                         hyperparam_bart_kpct_mlb_0.9_df$power,
                                                         hyperparam_bart_kpct_mlb_0.9_df$base,
                                                         hyperparam_bart_kpct_mlb_0.9_df$row_num), hyperparam_bart_kpct_mlb, .progress = TRUE)

hyperparam_bart_kpct_mlb_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_kpct_mlb_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_kpct_mlb_0.99_df$sigdf,
                                                          hyperparam_bart_kpct_mlb_0.99_df$sigquant, 
                                                          hyperparam_bart_kpct_mlb_0.99_df$k, 
                                                          hyperparam_bart_kpct_mlb_0.99_df$power,
                                                          hyperparam_bart_kpct_mlb_0.99_df$base,
                                                          hyperparam_bart_kpct_mlb_0.99_df$row_num), hyperparam_bart_kpct_mlb, .progress = TRUE)



hyperparam_bart_kpct_mlb_df <- bind_rows(hyperparam_bart_kpct_mlb_0.75_df, hyperparam_bart_kpct_mlb_0.9_df, hyperparam_bart_kpct_mlb_0.99_df)

hyperparam_bart_kpct_mlb_df <- hyperparam_bart_kpct_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_kpct_mlb <- hyperparam_bart_kpct_mlb_df$sigdf) #3
(sigquant_kpct_mlb <- hyperparam_bart_kpct_mlb_df$sigquant) #0.99
(k_kpct_mlb <- hyperparam_bart_kpct_mlb_df$k) #3
(power_kpct_mlb <- hyperparam_bart_kpct_mlb_df$power) #1
(base_kpct_mlb <- hyperparam_bart_kpct_mlb_df$base) #0.75


hyperparam_bart_kpct_mlb_df$rmse/sd(kpct_AAA$K_pct_AAA) #0.638 #BART Better

hyperparam_bart_kpct_mlb_trees <- function(trees_kpct_mlb){
  set.seed(101);kpct_mlb$fold <- sample(1:4, nrow(kpct_mlb), replace = TRUE)
  
  print(paste('Trees: ', trees_kpct_mlb))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:4){
    print(paste('Iteration: ', i))
    train_data <- kpct_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpct_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_kpct_mlb,
                   sigquant = sigquant_kpct_mlb,
                   k = k_kpct_mlb,
                   power = power_kpct_mlb,
                   base = base_kpct_mlb,
                   n.trees = trees_kpct_mlb,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_kpct_mlb_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_kpct_mlb_trees, .progress = TRUE))


bart_kpct_mlb_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() 

(kpct_mlb_error_ratio <- (bart_kpct_mlb_trees_df %>% filter(trees == 1000) %>% pull(rmse))/sd(kpct_mlb$K_pct_MLB)) #0.665 #BART Better


### Final Model K% MLB ####


kpct_mlb_mod <- bart2(K_pct_MLB ~ ., 
               data = kpct_mlb,
               sigdf = sigdf_kpct_mlb,
               sigquant = sigquant_kpct_mlb,
               k = k_kpct_mlb,
               power = power_kpct_mlb,
               base = base_kpct_mlb,
               n.trees = 1000,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)


summary(kpct_mlb$K_pct_MLB - residuals(kpct_mlb_mod))

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

#finding the optimal number of CVs
bart_bbpct_aplus_cvs <- function(cv){
  set.seed(101);bbpct_aplus$fold <- sample(1:cv, nrow(bbpct_aplus), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- bbpct_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpct_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_bbpct_aplus_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_bbpct_aplus_cvs))

bart_bbpct_aplus_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.043, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #8


hyperparam_bart_bbpct_aplus <- function(sigdf_bbpct_aplus, sigquant_bbpct_aplus, k_bbpct_aplus, power_bbpct_aplus, base_bbpct_aplus, row_num = 1){
  set.seed(101);bbpct_aplus$fold <- sample(1:8, nrow(bbpct_aplus), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_bbpct_aplus))
  print(paste('Sigquant: ', sigquant_bbpct_aplus))
  print(paste('K: ', k_bbpct_aplus))
  print(paste('Power: ', power_bbpct_aplus))
  print(paste('Base: ', base_bbpct_aplus))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:8){
    print(paste('Iteration: ', i))
    train_data <- bbpct_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpct_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_bbpct_aplus,
                   sigquant = sigquant_bbpct_aplus,
                   k = k_bbpct_aplus,
                   power = power_bbpct_aplus,
                   base = base_bbpct_aplus,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_bbpct_aplus_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bbpct_aplus_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpct_aplus_0.75_df$sigdf,
                                                         hyperparam_bart_bbpct_aplus_0.75_df$sigquant, 
                                                         hyperparam_bart_bbpct_aplus_0.75_df$k, 
                                                         hyperparam_bart_bbpct_aplus_0.75_df$power,
                                                         hyperparam_bart_bbpct_aplus_0.75_df$base,
                                                         hyperparam_bart_bbpct_aplus_0.75_df$row_num), hyperparam_bart_bbpct_aplus, .progress = TRUE)

hyperparam_bart_bbpct_aplus_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bbpct_aplus_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpct_aplus_0.9_df$sigdf,
                                                        hyperparam_bart_bbpct_aplus_0.9_df$sigquant, 
                                                        hyperparam_bart_bbpct_aplus_0.9_df$k, 
                                                        hyperparam_bart_bbpct_aplus_0.9_df$power,
                                                        hyperparam_bart_bbpct_aplus_0.9_df$base,
                                                        hyperparam_bart_bbpct_aplus_0.9_df$row_num), hyperparam_bart_bbpct_aplus, .progress = TRUE)

hyperparam_bart_bbpct_aplus_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_bbpct_aplus_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpct_aplus_0.99_df$sigdf,
                                                         hyperparam_bart_bbpct_aplus_0.99_df$sigquant, 
                                                         hyperparam_bart_bbpct_aplus_0.99_df$k, 
                                                         hyperparam_bart_bbpct_aplus_0.99_df$power,
                                                         hyperparam_bart_bbpct_aplus_0.99_df$base,
                                                         hyperparam_bart_bbpct_aplus_0.99_df$row_num), hyperparam_bart_bbpct_aplus, .progress = TRUE)



hyperparam_bart_bbpct_aplus_df <- bind_rows(hyperparam_bart_bbpct_aplus_0.75_df, hyperparam_bart_bbpct_aplus_0.9_df, hyperparam_bart_bbpct_aplus_0.99_df)

hyperparam_bart_bbpct_aplus_df <- hyperparam_bart_bbpct_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_bbpct_aplus <- hyperparam_bart_bbpct_aplus_df$sigdf) #3
(sigquant_bbpct_aplus <- hyperparam_bart_bbpct_aplus_df$sigquant) #0.9
(k_bbpct_aplus <- hyperparam_bart_bbpct_aplus_df$k) # 4
(power_bbpct_aplus <- hyperparam_bart_bbpct_aplus_df$power) #1
(base_bbpct_aplus <- hyperparam_bart_bbpct_aplus_df$base) # 0.75

hyperparam_bart_bbpct_aplus_df$rmse/sd(bbpct_aplus$BB_pct_Aplus) #0.893 #BART Better

hyperparam_bart_bbpct_aplus_trees <- function(trees_bbpct_aplus){
  set.seed(101);bbpct_aplus$fold <- sample(1:8, nrow(bbpct_aplus), replace = TRUE)
  
  print(paste('Trees: ', trees_bbpct_aplus))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:8){
    print(paste('Iteration: ', i))
    train_data <- bbpct_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpct_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_bbpct_aplus,
                   sigquant = sigquant_bbpct_aplus,
                   k = k_bbpct_aplus,
                   power = power_bbpct_aplus,
                   base = base_bbpct_aplus,
                   n.trees = trees_bbpct_aplus,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_bbpct_aplus_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_bbpct_aplus_trees, .progress = TRUE))


bart_bbpct_aplus_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #800

(bbpct_aplus_error_ratio <- (bart_bbpct_aplus_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(bbpct_aplus$BB_pct_Aplus)) #0.893


### Final Model BB% A+ ####
bbpct_aplus_mod <- bart2(BB_pct_Aplus ~ ., 
               data = bbpct_aplus,
               sigdf = sigdf_bbpct_aplus,
               sigquant = sigquant_bbpct_aplus,
               k = k_bbpct_aplus,
               power = power_bbpct_aplus,
               base = base_bbpct_aplus,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(bbpct_aplus$BB_pct_Aplus - residuals(bbpct_aplus_mod))


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

#finding the optimal number of CVs
bart_bbpct_AA_cvs <- function(cv){
  set.seed(101);bbpct_AA$fold <- sample(1:cv, nrow(bbpct_AA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- bbpct_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpct_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_bbpct_AA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_bbpct_AA_cvs))

bart_bbpct_AA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #20


hyperparam_bart_bbpct_AA <- function(sigdf_bbpct_AA, sigquant_bbpct_AA, k_bbpct_AA, power_bbpct_AA, base_bbpct_AA,row_num = 1){
  set.seed(101);bbpct_AA$fold <- sample(1:20, nrow(bbpct_AA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_bbpct_AA))
  print(paste('Sigquant: ', sigquant_bbpct_AA))
  print(paste('K: ', k_bbpct_AA))
  print(paste('Power: ', power_bbpct_AA))
  print(paste('Base: ', base_bbpct_AA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:20){
    print(paste('Iteration: ', i))
    train_data <- bbpct_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpct_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_bbpct_AA,
                   sigquant = sigquant_bbpct_AA,
                   k = k_bbpct_AA,
                   power = power_bbpct_AA,
                   base = base_bbpct_AA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_bbpct_AA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bbpct_AA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpct_AA_0.75_df$sigdf,
                                                      hyperparam_bart_bbpct_AA_0.75_df$sigquant, 
                                                      hyperparam_bart_bbpct_AA_0.75_df$k, 
                                                      hyperparam_bart_bbpct_AA_0.75_df$power,
                                                      hyperparam_bart_bbpct_AA_0.75_df$base,
                                                      hyperparam_bart_bbpct_AA_0.75_df$row_num), hyperparam_bart_bbpct_AA, .progress = TRUE)

hyperparam_bart_bbpct_AA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bbpct_AA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpct_AA_0.9_df$sigdf,
                                                     hyperparam_bart_bbpct_AA_0.9_df$sigquant, 
                                                     hyperparam_bart_bbpct_AA_0.9_df$k, 
                                                     hyperparam_bart_bbpct_AA_0.9_df$power,
                                                     hyperparam_bart_bbpct_AA_0.9_df$base,
                                                     hyperparam_bart_bbpct_AA_0.9_df$row_num), hyperparam_bart_bbpct_AA, .progress = TRUE)

hyperparam_bart_bbpct_AA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_bbpct_AA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpct_AA_0.99_df$sigdf,
                                                      hyperparam_bart_bbpct_AA_0.99_df$sigquant, 
                                                      hyperparam_bart_bbpct_AA_0.99_df$k, 
                                                      hyperparam_bart_bbpct_AA_0.99_df$power,
                                                      hyperparam_bart_bbpct_AA_0.99_df$base,
                                                      hyperparam_bart_bbpct_AA_0.99_df$row_num), hyperparam_bart_bbpct_AA, .progress = TRUE)



hyperparam_bart_bbpct_AA_df <- bind_rows(hyperparam_bart_bbpct_AA_0.75_df, hyperparam_bart_bbpct_AA_0.9_df, hyperparam_bart_bbpct_AA_0.99_df)

hyperparam_bart_bbpct_AA_df <- hyperparam_bart_bbpct_AA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_bbpct_AA <- hyperparam_bart_bbpct_AA_df$sigdf) #10
(sigquant_bbpct_AA <- hyperparam_bart_bbpct_AA_df$sigquant) #0.75
(k_bbpct_AA <- hyperparam_bart_bbpct_AA_df$k) #6
(power_bbpct_AA <- hyperparam_bart_bbpct_AA_df$power) #3
(base_bbpct_AA <- hyperparam_bart_bbpct_AA_df$base) #0.95

hyperparam_bart_bbpct_AA_df$rmse/sd(bbpct_AA$BB_pct_AA) #0.864 #BART Better

hyperparam_bart_bbpct_AA_trees <- function(trees_bbpct_AA){
  set.seed(101);bbpct_AA$fold <- sample(1:20, nrow(bbpct_AA), replace = TRUE)
  
  print(paste('Trees: ', trees_bbpct_AA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:20){
    print(paste('Iteration: ', i))
    train_data <- bbpct_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpct_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_bbpct_AA,
                   sigquant = sigquant_bbpct_AA,
                   k = k_bbpct_AA,
                   power = power_bbpct_AA,
                   base = base_bbpct_AA,
                   n.trees = trees_bbpct_AA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_bbpct_AA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_bbpct_AA_trees, .progress = TRUE))


bart_bbpct_AA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #600

(bbpct_AA_error_ratio <- (bart_bbpct_AA_trees_df %>% filter(trees == 600) %>% pull(rmse))/sd(bbpct_AA$BB_pct_AA)) #0.863 #BART better

### Final Model BB% AA ####

bbpct_AA_mod <- bart2(BB_pct_AA ~ ., 
               data = bbpct_AA,
               sigdf = sigdf_bbpct_AA,
               sigquant = sigquant_bbpct_AA,
               k = k_bbpct_AA,
               power = power_bbpct_AA,
               base = base_bbpct_AA,
               n.trees = 600,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(bbpct_AA$BB_pct_AA - residuals(bbpct_AA_mod))

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

#finding the optimal number of CVs
bart_bbpct_AAA_cvs <- function(cv){
  set.seed(101);bbpct_AAA$fold <- sample(1:cv, nrow(bbpct_AAA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- bbpct_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpct_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_bbpct_AAA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_bbpct_AAA_cvs))

bart_bbpct_AAA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.042, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #8


hyperparam_bart_bbpct_AAA <- function(sigdf_bbpct_AAA, sigquant_bbpct_AAA, k_bbpct_AAA, power_bbpct_AAA, base_bbpct_AAA, row_num = 1){
  set.seed(101);bbpct_AAA$fold <- sample(1:8, nrow(bbpct_AAA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_bbpct_AAA))
  print(paste('Sigquant: ', sigquant_bbpct_AAA))
  print(paste('K: ', k_bbpct_AAA))
  print(paste('Power: ', power_bbpct_AAA))
  print(paste('Base: ', base_bbpct_AAA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:8){
    print(paste('Iteration: ', i))
    train_data <- bbpct_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpct_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_bbpct_AAA,
                   sigquant = sigquant_bbpct_AAA,
                   k = k_bbpct_AAA,
                   base = base_bbpct_AAA,
                   power = power_bbpct_AAA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_bbpct_AAA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bbpct_AAA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpct_AAA_0.75_df$sigdf,
                                                       hyperparam_bart_bbpct_AAA_0.75_df$sigquant, 
                                                       hyperparam_bart_bbpct_AAA_0.75_df$k, 
                                                       hyperparam_bart_bbpct_AAA_0.75_df$power,
                                                       hyperparam_bart_bbpct_AAA_0.75_df$base, 
                                                       hyperparam_bart_bbpct_AAA_0.75_df$row_num), hyperparam_bart_bbpct_AAA, .progress = TRUE)

hyperparam_bart_bbpct_AAA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bbpct_AAA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpct_AAA_0.9_df$sigdf,
                                                      hyperparam_bart_bbpct_AAA_0.9_df$sigquant, 
                                                      hyperparam_bart_bbpct_AAA_0.9_df$k, 
                                                      hyperparam_bart_bbpct_AAA_0.9_df$power,
                                                      hyperparam_bart_bbpct_AAA_0.9_df$base,
                                                      hyperparam_bart_bbpct_AAA_0.9_df$row_num), hyperparam_bart_bbpct_AAA, .progress = TRUE)

hyperparam_bart_bbpct_AAA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_bbpct_AAA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpct_AAA_0.99_df$sigdf,
                                                       hyperparam_bart_bbpct_AAA_0.99_df$sigquant, 
                                                       hyperparam_bart_bbpct_AAA_0.99_df$k, 
                                                       hyperparam_bart_bbpct_AAA_0.99_df$power,
                                                       hyperparam_bart_bbpct_AAA_0.99_df$base,
                                                       hyperparam_bart_bbpct_AAA_0.99_df$row_num), hyperparam_bart_bbpct_AAA, .progress = TRUE)



hyperparam_bart_bbpct_AAA_df <- bind_rows(hyperparam_bart_bbpct_AAA_0.75_df, hyperparam_bart_bbpct_AAA_0.9_df, hyperparam_bart_bbpct_AAA_0.99_df)

hyperparam_bart_bbpct_AAA_df <- hyperparam_bart_bbpct_AAA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_bbpct_AAA <- hyperparam_bart_bbpct_AAA_df$sigdf) #10
(sigquant_bbpct_AAA <- hyperparam_bart_bbpct_AAA_df$sigquant) #0.75
(k_bbpct_AAA <- hyperparam_bart_bbpct_AAA_df$k) #5
(power_bbpct_AAA <- hyperparam_bart_bbpct_AAA_df$power) #6
(base_bbpct_AAA <- hyperparam_bart_bbpct_AAA_df$base) #0.95

hyperparam_bart_bbpct_AAA_df$rmse/sd(bbpct_AAA$BB_pct_AAA) #0.852 #BART Better

hyperparam_bart_bbpct_AAA_trees <- function(trees_bbpct_AAA){
  set.seed(101);bbpct_AAA$fold <- sample(1:8, nrow(bbpct_AAA), replace = TRUE)
  
  print(paste('Trees: ', trees_bbpct_AAA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:8){
    print(paste('Iteration: ', i))
    train_data <- bbpct_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpct_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_bbpct_AAA,
                   sigquant = sigquant_bbpct_AAA,
                   k = k_bbpct_AAA,
                   power = power_bbpct_AAA,
                   base = base_bbpct_AAA,
                   n.trees = trees_bbpct_AAA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_bbpct_AAA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_bbpct_AAA_trees, .progress = TRUE))


bart_bbpct_AAA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #600

(bbpct_AAA_error_ratio <- (bart_bbpct_AAA_trees_df %>% filter(trees == 600) %>% pull(rmse))/sd(bbpct_AAA$BB_pct_AAA)) #0.852

### Final Model BB% AAA ####
bbpct_AAA_mod <- bart2(BB_pct_AAA ~ ., 
               data = bbpct_AAA,
               sigdf = sigdf_bbpct_AAA,
               sigquant = sigquant_bbpct_AAA,
               k = k_bbpct_AAA,
               power = power_bbpct_AAA,
               base = base_bbpct_AAA,
               n.trees = 600,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(bbpct_AAA$BB_pct_AAA - residuals(bbpct_AAA_mod))


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

#finding the optimal number of CVs
bart_bbpct_mlb_cvs <- function(cv){
  set.seed(101);bbpct_mlb$fold <- sample(1:cv, nrow(bbpct_mlb), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- bbpct_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpct_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_bbpct_mlb_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_bbpct_mlb_cvs))

bart_bbpct_mlb_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #10


hyperparam_bart_bbpct_mlb <- function(sigdf_bbpct_mlb, sigquant_bbpct_mlb, k_bbpct_mlb, power_bbpct_mlb, base_bbpct_mlb,row_num = 1){
  set.seed(101);bbpct_mlb$fold <- sample(1:10, nrow(bbpct_mlb), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_bbpct_mlb))
  print(paste('Sigquant: ', sigquant_bbpct_mlb))
  print(paste('K: ', k_bbpct_mlb))
  print(paste('Power: ', power_bbpct_mlb))
  print(paste('Base: ', base_bbpct_mlb))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- bbpct_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpct_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_bbpct_mlb,
                   sigquant = sigquant_bbpct_mlb,
                   k = k_bbpct_mlb,
                   power = power_bbpct_mlb,
                   base = base_bbpct_mlb,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_bbpct_mlb_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bbpct_mlb_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpct_mlb_0.75_df$sigdf,
                                                       hyperparam_bart_bbpct_mlb_0.75_df$sigquant, 
                                                       hyperparam_bart_bbpct_mlb_0.75_df$k, 
                                                       hyperparam_bart_bbpct_mlb_0.75_df$power,
                                                       hyperparam_bart_bbpct_mlb_0.75_df$base,
                                                       hyperparam_bart_bbpct_mlb_0.75_df$row_num), hyperparam_bart_bbpct_mlb, .progress = TRUE)

hyperparam_bart_bbpct_mlb_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bbpct_mlb_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpct_mlb_0.9_df$sigdf,
                                                      hyperparam_bart_bbpct_mlb_0.9_df$sigquant, 
                                                      hyperparam_bart_bbpct_mlb_0.9_df$k, 
                                                      hyperparam_bart_bbpct_mlb_0.9_df$power,
                                                      hyperparam_bart_bbpct_mlb_0.9_df$base,
                                                      hyperparam_bart_bbpct_mlb_0.9_df$row_num), hyperparam_bart_bbpct_mlb, .progress = TRUE)

hyperparam_bart_bbpct_mlb_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_bbpct_mlb_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpct_mlb_0.99_df$sigdf,
                                                       hyperparam_bart_bbpct_mlb_0.99_df$sigquant, 
                                                       hyperparam_bart_bbpct_mlb_0.99_df$k, 
                                                       hyperparam_bart_bbpct_mlb_0.99_df$power,
                                                       hyperparam_bart_bbpct_mlb_0.99_df$base,
                                                       hyperparam_bart_bbpct_mlb_0.99_df$row_num), hyperparam_bart_bbpct_mlb, .progress = TRUE)



hyperparam_bart_bbpct_mlb_df <- bind_rows(hyperparam_bart_bbpct_mlb_0.75_df, hyperparam_bart_bbpct_mlb_0.9_df, hyperparam_bart_bbpct_mlb_0.99_df)

hyperparam_bart_bbpct_mlb_df <- hyperparam_bart_bbpct_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_bbpct_mlb <- hyperparam_bart_bbpct_mlb_df$sigdf) #3
(sigquant_bbpct_mlb <- hyperparam_bart_bbpct_mlb_df$sigquant) #0.9
(k_bbpct_mlb <- hyperparam_bart_bbpct_mlb_df$k) #4
(power_bbpct_mlb <- hyperparam_bart_bbpct_mlb_df$power) #3
(base_bbpct_mlb <- hyperparam_bart_bbpct_mlb_df$base) #0.95

hyperparam_bart_bbpct_mlb_df$rmse/sd(bbpct_mlb$BB_pct_MLB) #0.863 #BART Better

hyperparam_bart_bbpct_mlb_trees <- function(trees_bbpct_mlb){
  set.seed(101);bbpct_mlb$fold <- sample(1:10, nrow(bbpct_mlb), replace = TRUE)
  
  print(paste('Trees: ', trees_bbpct_mlb))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- bbpct_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpct_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_bbpct_mlb,
                   sigquant = sigquant_bbpct_mlb,
                   k = k_bbpct_mlb,
                   power = power_bbpct_mlb,
                   base = base_bbpct_mlb,
                   n.trees = trees_bbpct_mlb,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_bbpct_mlb_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_bbpct_mlb_trees, .progress = TRUE))


bart_bbpct_mlb_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() # 1000

(bbpct_mlb_error_ratio <- (bart_bbpct_mlb_trees_df %>% filter(trees == 1000) %>% pull(rmse))/sd(bbpct_mlb$BB_pct_MLB)) #0.863 #BART Better

### Final Model BB% MLB ####

bbpct_mlb_mod <- bart2(BB_pct_MLB ~ ., 
               data = bbpct_mlb,
               sigdf = sigdf_bbpct_mlb,
               sigquant = sigquant_bbpct_mlb,
               k = k_bbpct_mlb,
               power = power_bbpct_mlb,
               base = base_bbpct_mlb,
               n.trees = 1000,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(bbpct_mlb$BB_pct_MLB - residuals(bbpct_mlb_mod))

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

#finding the optimal number of CVs
bart_swstrpct_aplus_cvs <- function(cv){
  set.seed(101);swstrpct_aplus$fold <- sample(1:cv, nrow(swstrpct_aplus), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- swstrpct_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpct_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_swstrpct_aplus_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_swstrpct_aplus_cvs))

bart_swstrpct_aplus_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.043, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #12


hyperparam_bart_swstrpct_aplus <- function(sigdf_swstrpct_aplus, sigquant_swstrpct_aplus, k_swstrpct_aplus, power_swstrpct_aplus, base_swstrpct_aplus, row_num = 1){
  set.seed(101);swstrpct_aplus$fold <- sample(1:12, nrow(swstrpct_aplus), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_swstrpct_aplus))
  print(paste('Sigquant: ', sigquant_swstrpct_aplus))
  print(paste('K: ', k_swstrpct_aplus))
  print(paste('Power: ', power_swstrpct_aplus))
  print(paste('Base: ', base_swstrpct_aplus))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:12){
    print(paste('Iteration: ', i))
    train_data <- swstrpct_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpct_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_swstrpct_aplus,
                   sigquant = sigquant_swstrpct_aplus,
                   k = k_swstrpct_aplus,
                   power = power_swstrpct_aplus,
                   base = base_swstrpct_aplus,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_swstrpct_aplus_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_swstrpct_aplus_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpct_aplus_0.75_df$sigdf,
                                                          hyperparam_bart_swstrpct_aplus_0.75_df$sigquant, 
                                                          hyperparam_bart_swstrpct_aplus_0.75_df$k, 
                                                          hyperparam_bart_swstrpct_aplus_0.75_df$power,
                                                          hyperparam_bart_swstrpct_aplus_0.75_df$base,
                                                          hyperparam_bart_swstrpct_aplus_0.75_df$row_num), hyperparam_bart_swstrpct_aplus, .progress = TRUE)

hyperparam_bart_swstrpct_aplus_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_swstrpct_aplus_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpct_aplus_0.9_df$sigdf,
                                                         hyperparam_bart_swstrpct_aplus_0.9_df$sigquant, 
                                                         hyperparam_bart_swstrpct_aplus_0.9_df$k, 
                                                         hyperparam_bart_swstrpct_aplus_0.9_df$power,
                                                         hyperparam_bart_swstrpct_aplus_0.9_df$base,
                                                         hyperparam_bart_swstrpct_aplus_0.9_df$row_num), hyperparam_bart_swstrpct_aplus, .progress = TRUE)

hyperparam_bart_swstrpct_aplus_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_swstrpct_aplus_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpct_aplus_0.99_df$sigdf,
                                                          hyperparam_bart_swstrpct_aplus_0.99_df$sigquant, 
                                                          hyperparam_bart_swstrpct_aplus_0.99_df$k, 
                                                          hyperparam_bart_swstrpct_aplus_0.99_df$power,
                                                          hyperparam_bart_swstrpct_aplus_0.99_df$base,
                                                          hyperparam_bart_swstrpct_aplus_0.99_df$row_num), hyperparam_bart_swstrpct_aplus, .progress = TRUE)



hyperparam_bart_swstrpct_aplus_df <- bind_rows(hyperparam_bart_swstrpct_aplus_0.75_df, hyperparam_bart_swstrpct_aplus_0.9_df, hyperparam_bart_swstrpct_aplus_0.99_df)

hyperparam_bart_swstrpct_aplus_df <- hyperparam_bart_swstrpct_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_swstrpct_aplus <- hyperparam_bart_swstrpct_aplus_df$sigdf) #3
(sigquant_swstrpct_aplus <- hyperparam_bart_swstrpct_aplus_df$sigquant) #0.90
(k_swstrpct_aplus <- hyperparam_bart_swstrpct_aplus_df$k) # 3
(power_swstrpct_aplus <- hyperparam_bart_swstrpct_aplus_df$power) # 4
(base_swstrpct_aplus <- hyperparam_bart_swstrpct_aplus_df$base) # 0.95

hyperparam_bart_swstrpct_aplus_df$rmse/sd(swstrpct_aplus$SwStr_pct_Aplus) #0.747 #BART Better

hyperparam_bart_swstrpct_aplus_trees <- function(trees_swstrpct_aplus){
  set.seed(101);swstrpct_aplus$fold <- sample(1:12, nrow(swstrpct_aplus), replace = TRUE)
  
  print(paste('Trees: ', trees_swstrpct_aplus))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:12){
    print(paste('Iteration: ', i))
    train_data <- swstrpct_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpct_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_swstrpct_aplus,
                   sigquant = sigquant_swstrpct_aplus,
                   k = k_swstrpct_aplus,
                   power = power_swstrpct_aplus,
                   base = base_swstrpct_aplus,
                   n.trees = trees_swstrpct_aplus,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_swstrpct_aplus_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_swstrpct_aplus_trees,.progress = TRUE))


bart_swstrpct_aplus_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #200

(swstrpct_aplus_error_ratio <- (bart_swstrpct_aplus_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(swstrpct_aplus$SwStr_pct_Aplus)) #0.747 #BART Better

### Final Model SwStr% A+ ####

swstrpct_aplus_mod <- bart2(SwStr_pct_Aplus ~ ., 
               data = swstrpct_aplus,
               sigdf = sigdf_swstrpct_aplus,
               sigquant = sigquant_swstrpct_aplus,
               k = k_swstrpct_aplus,
               power = power_swstrpct_aplus,
               base = base_swstrpct_aplus,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(swstrpct_aplus$SwStr_pct_Aplus - residuals(swstrpct_aplus_mod))


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

#finding the optimal number of CVs
bart_swstrpct_AA_cvs <- function(cv){
  set.seed(101);swstrpct_AA$fold <- sample(1:cv, nrow(swstrpct_AA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- swstrpct_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpct_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_swstrpct_AA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_swstrpct_AA_cvs))

bart_swstrpct_AA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #6


hyperparam_bart_swstrpct_AA <- function(sigdf_swstrpct_AA, sigquant_swstrpct_AA, k_swstrpct_AA, power_swstrpct_AA, base_swstrpct_AA,row_num = 1){
  set.seed(101);swstrpct_AA$fold <- sample(1:6, nrow(swstrpct_AA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_swstrpct_AA))
  print(paste('Sigquant: ', sigquant_swstrpct_AA))
  print(paste('K: ', k_swstrpct_AA))
  print(paste('Power: ', power_swstrpct_AA))
  print(paste('Base: ', base_swstrpct_AA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:6){
    print(paste('Iteration: ', i))
    train_data <- swstrpct_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpct_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_swstrpct_AA,
                   sigquant = sigquant_swstrpct_AA,
                   k = k_swstrpct_AA,
                   power = power_swstrpct_AA,
                   base = base_swstrpct_AA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_swstrpct_AA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_swstrpct_AA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpct_AA_0.75_df$sigdf,
                                                       hyperparam_bart_swstrpct_AA_0.75_df$sigquant, 
                                                       hyperparam_bart_swstrpct_AA_0.75_df$k, 
                                                       hyperparam_bart_swstrpct_AA_0.75_df$power,
                                                       hyperparam_bart_swstrpct_AA_0.75_df$base,
                                                       hyperparam_bart_swstrpct_AA_0.75_df$row_num), hyperparam_bart_swstrpct_AA, .progress = TRUE)

hyperparam_bart_swstrpct_AA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_swstrpct_AA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpct_AA_0.9_df$sigdf,
                                                      hyperparam_bart_swstrpct_AA_0.9_df$sigquant, 
                                                      hyperparam_bart_swstrpct_AA_0.9_df$k, 
                                                      hyperparam_bart_swstrpct_AA_0.9_df$power,
                                                      hyperparam_bart_swstrpct_AA_0.9_df$base,
                                                      hyperparam_bart_swstrpct_AA_0.9_df$row_num), hyperparam_bart_swstrpct_AA, .progress = TRUE)

hyperparam_bart_swstrpct_AA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_swstrpct_AA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpct_AA_0.99_df$sigdf,
                                                       hyperparam_bart_swstrpct_AA_0.99_df$sigquant, 
                                                       hyperparam_bart_swstrpct_AA_0.99_df$k, 
                                                       hyperparam_bart_swstrpct_AA_0.99_df$power,
                                                       hyperparam_bart_swstrpct_AA_0.99_df$base,
                                                       hyperparam_bart_swstrpct_AA_0.99_df$row_num), hyperparam_bart_swstrpct_AA, .progress = TRUE)



hyperparam_bart_swstrpct_AA_df <- bind_rows(hyperparam_bart_swstrpct_AA_0.75_df, hyperparam_bart_swstrpct_AA_0.9_df, hyperparam_bart_swstrpct_AA_0.99_df)

hyperparam_bart_swstrpct_AA_df <- hyperparam_bart_swstrpct_AA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_swstrpct_AA <- hyperparam_bart_swstrpct_AA_df$sigdf) #10
(sigquant_swstrpct_AA <- hyperparam_bart_swstrpct_AA_df$sigquant) #0.75
(k_swstrpct_AA <- hyperparam_bart_swstrpct_AA_df$k) # 3
(power_swstrpct_AA <- hyperparam_bart_swstrpct_AA_df$power) # 1
(base_swstrpct_AA <- hyperparam_bart_swstrpct_AA_df$base) #0.95

hyperparam_bart_swstrpct_AA_df$rmse/sd(swstrpct_AA$SwStr_pct_AA) #0.682 #BART Better

hyperparam_bart_swstrpct_AA_trees <- function(trees_swstrpct_AA){
  set.seed(101);swstrpct_AA$fold <- sample(1:6, nrow(swstrpct_AA), replace = TRUE)
  
  print(paste('Trees: ', trees_swstrpct_AA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:6){
    print(paste('Iteration: ', i))
    train_data <- swstrpct_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpct_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_swstrpct_AA,
                   sigquant = sigquant_swstrpct_AA,
                   k = k_swstrpct_AA,
                   power = power_swstrpct_AA,
                   base = base_swstrpct_AA,
                   n.trees = trees_swstrpct_AA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_swstrpct_AA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_swstrpct_AA_trees, .progress = TRUE))


bart_swstrpct_AA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #200

(swstrpct_AA_error_ratio <- (bart_swstrpct_AA_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(swstrpct_AA$SwStr_pct_AA)) #0.682 #BART Better


### Final Model SwStr% AA ####

swstrpct_AA_mod <- bart2(SwStr_pct_AA ~ ., 
               data = swstrpct_AA,
               sigdf = sigdf_swstrpct_AA,
               sigquant = sigquant_swstrpct_AA,
               k = k_swstrpct_AA,
               power = power_swstrpct_AA,
               base = base_swstrpct_AA,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(swstrpct_AA$SwStr_pct_AA - residuals(swstrpct_AA_mod))

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

#finding the optimal number of CVs
bart_swstrpct_AAA_cvs <- function(cv){
  set.seed(101);swstrpct_AAA$fold <- sample(1:cv, nrow(swstrpct_AAA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- swstrpct_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpct_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_swstrpct_AAA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_swstrpct_AAA_cvs))

bart_swstrpct_AAA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.042, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #12


hyperparam_bart_swstrpct_AAA <- function(sigdf_swstrpct_AAA, sigquant_swstrpct_AAA, k_swstrpct_AAA, power_swstrpct_AAA, base_swstrpct_AAA, row_num = 1){
  set.seed(101);swstrpct_AAA$fold <- sample(1:12, nrow(swstrpct_AAA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_swstrpct_AAA))
  print(paste('Sigquant: ', sigquant_swstrpct_AAA))
  print(paste('K: ', k_swstrpct_AAA))
  print(paste('Power: ', power_swstrpct_AAA))
  print(paste('Base: ', base_swstrpct_AAA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:12){
    print(paste('Iteration: ', i))
    train_data <- swstrpct_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpct_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_swstrpct_AAA,
                   sigquant = sigquant_swstrpct_AAA,
                   k = k_swstrpct_AAA,
                   base = base_swstrpct_AAA,
                   power = power_swstrpct_AAA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_swstrpct_AAA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_swstrpct_AAA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpct_AAA_0.75_df$sigdf,
                                                        hyperparam_bart_swstrpct_AAA_0.75_df$sigquant, 
                                                        hyperparam_bart_swstrpct_AAA_0.75_df$k, 
                                                        hyperparam_bart_swstrpct_AAA_0.75_df$power,
                                                        hyperparam_bart_swstrpct_AAA_0.75_df$base, 
                                                        hyperparam_bart_swstrpct_AAA_0.75_df$row_num), hyperparam_bart_swstrpct_AAA, .progress = TRUE)

hyperparam_bart_swstrpct_AAA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_swstrpct_AAA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpct_AAA_0.9_df$sigdf,
                                                       hyperparam_bart_swstrpct_AAA_0.9_df$sigquant, 
                                                       hyperparam_bart_swstrpct_AAA_0.9_df$k, 
                                                       hyperparam_bart_swstrpct_AAA_0.9_df$power,
                                                       hyperparam_bart_swstrpct_AAA_0.9_df$base,
                                                       hyperparam_bart_swstrpct_AAA_0.9_df$row_num), hyperparam_bart_swstrpct_AAA, .progress = TRUE)

hyperparam_bart_swstrpct_AAA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_swstrpct_AAA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpct_AAA_0.99_df$sigdf,
                                                        hyperparam_bart_swstrpct_AAA_0.99_df$sigquant, 
                                                        hyperparam_bart_swstrpct_AAA_0.99_df$k, 
                                                        hyperparam_bart_swstrpct_AAA_0.99_df$power,
                                                        hyperparam_bart_swstrpct_AAA_0.99_df$base,
                                                        hyperparam_bart_swstrpct_AAA_0.99_df$row_num), hyperparam_bart_swstrpct_AAA, .progress = TRUE)



hyperparam_bart_swstrpct_AAA_df <- bind_rows(hyperparam_bart_swstrpct_AAA_0.75_df, hyperparam_bart_swstrpct_AAA_0.9_df, hyperparam_bart_swstrpct_AAA_0.99_df)

hyperparam_bart_swstrpct_AAA_df <- hyperparam_bart_swstrpct_AAA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_swstrpct_AAA <- hyperparam_bart_swstrpct_AAA_df$sigdf) #3
(sigquant_swstrpct_AAA <- hyperparam_bart_swstrpct_AAA_df$sigquant) #0.99
(k_swstrpct_AAA <- hyperparam_bart_swstrpct_AAA_df$k) #3
(power_swstrpct_AAA <- hyperparam_bart_swstrpct_AAA_df$power) #6
(base_swstrpct_AAA <- hyperparam_bart_swstrpct_AAA_df$base) #0.5

hyperparam_bart_swstrpct_AAA_df$rmse/sd(swstrpct_AAA$SwStr_pct_AAA) #0.628 #BART Better

hyperparam_bart_swstrpct_AAA_trees <- function(trees_swstrpct_AAA){
  set.seed(101);swstrpct_AAA$fold <- sample(1:12, nrow(swstrpct_AAA), replace = TRUE)
  
  print(paste('Trees: ', trees_swstrpct_AAA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:12){
    print(paste('Iteration: ', i))
    train_data <- swstrpct_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpct_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_swstrpct_AAA,
                   sigquant = sigquant_swstrpct_AAA,
                   k = k_swstrpct_AAA,
                   power = power_swstrpct_AAA,
                   base = base_swstrpct_AAA,
                   n.trees = trees_swstrpct_AAA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_swstrpct_AAA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_swstrpct_AAA_trees, .progress = TRUE))


bart_swstrpct_AAA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  # scale_y_continuous(limits = c(0.038, 0.0383)) +
  theme_bw() #200

(swstrpct_AAA_error_ratio <- (bart_swstrpct_AAA_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(swstrpct_AAA$SwStr_pct_AAA)) #0.628 #BART Better

### Final Model SwStr% AAA ####

swstrpct_AAA_mod <- bart2(SwStr_pct_AAA ~ ., 
               data = swstrpct_AAA,
               sigdf = sigdf_swstrpct_AAA,
               sigquant = sigquant_swstrpct_AAA,
               k = k_swstrpct_AAA,
               power = power_swstrpct_AAA,
               base = base_swstrpct_AAA,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(swstrpct_AAA$SwStr_pct_AAA - residuals(swstrpct_AAA_mod))

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

#finding the optimal number of CVs
bart_swstrpct_mlb_cvs <- function(cv){
  set.seed(101);swstrpct_mlb$fold <- sample(1:cv, nrow(swstrpct_mlb), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- swstrpct_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpct_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_swstrpct_mlb_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_swstrpct_mlb_cvs))

bart_swstrpct_mlb_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #2


hyperparam_bart_swstrpct_mlb <- function(sigdf_swstrpct_mlb, sigquant_swstrpct_mlb, k_swstrpct_mlb, power_swstrpct_mlb, base_swstrpct_mlb,row_num = 1){
  set.seed(101);swstrpct_mlb$fold <- sample(1:2, nrow(swstrpct_mlb), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_swstrpct_mlb))
  print(paste('Sigquant: ', sigquant_swstrpct_mlb))
  print(paste('K: ', k_swstrpct_mlb))
  print(paste('Power: ', power_swstrpct_mlb))
  print(paste('Base: ', base_swstrpct_mlb))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:2){
    print(paste('Iteration: ', i))
    train_data <- swstrpct_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpct_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_swstrpct_mlb,
                   sigquant = sigquant_swstrpct_mlb,
                   k = k_swstrpct_mlb,
                   power = power_swstrpct_mlb,
                   base = base_swstrpct_mlb,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_swstrpct_mlb_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_swstrpct_mlb_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpct_mlb_0.75_df$sigdf,
                                                        hyperparam_bart_swstrpct_mlb_0.75_df$sigquant, 
                                                        hyperparam_bart_swstrpct_mlb_0.75_df$k, 
                                                        hyperparam_bart_swstrpct_mlb_0.75_df$power,
                                                        hyperparam_bart_swstrpct_mlb_0.75_df$base,
                                                        hyperparam_bart_swstrpct_mlb_0.75_df$row_num), hyperparam_bart_swstrpct_mlb, .progress = TRUE)

hyperparam_bart_swstrpct_mlb_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_swstrpct_mlb_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpct_mlb_0.9_df$sigdf,
                                                       hyperparam_bart_swstrpct_mlb_0.9_df$sigquant, 
                                                       hyperparam_bart_swstrpct_mlb_0.9_df$k, 
                                                       hyperparam_bart_swstrpct_mlb_0.9_df$power,
                                                       hyperparam_bart_swstrpct_mlb_0.9_df$base,
                                                       hyperparam_bart_swstrpct_mlb_0.9_df$row_num), hyperparam_bart_swstrpct_mlb, .progress = TRUE)

hyperparam_bart_swstrpct_mlb_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_swstrpct_mlb_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpct_mlb_0.99_df$sigdf,
                                                        hyperparam_bart_swstrpct_mlb_0.99_df$sigquant, 
                                                        hyperparam_bart_swstrpct_mlb_0.99_df$k, 
                                                        hyperparam_bart_swstrpct_mlb_0.99_df$power,
                                                        hyperparam_bart_swstrpct_mlb_0.99_df$base,
                                                        hyperparam_bart_swstrpct_mlb_0.99_df$row_num), hyperparam_bart_swstrpct_mlb, .progress = TRUE)



hyperparam_bart_swstrpct_mlb_df <- bind_rows(hyperparam_bart_swstrpct_mlb_0.75_df, hyperparam_bart_swstrpct_mlb_0.9_df, hyperparam_bart_swstrpct_mlb_0.99_df)

hyperparam_bart_swstrpct_mlb_df <- hyperparam_bart_swstrpct_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_swstrpct_mlb <- hyperparam_bart_swstrpct_mlb_df$sigdf) #3
(sigquant_swstrpct_mlb <- hyperparam_bart_swstrpct_mlb_df$sigquant) #0.99
(k_swstrpct_mlb <- hyperparam_bart_swstrpct_mlb_df$k) #3
(power_swstrpct_mlb <- hyperparam_bart_swstrpct_mlb_df$power) #4
(base_swstrpct_mlb <- hyperparam_bart_swstrpct_mlb_df$base) #0.95

hyperparam_bart_swstrpct_mlb_df$rmse/sd(swstrpct_mlb$SwStr_pct_MLB) #0.586 #BART Better

hyperparam_bart_swstrpct_mlb_trees <- function(trees_swstrpct_mlb){
  set.seed(101);swstrpct_mlb$fold <- sample(1:2, nrow(swstrpct_mlb), replace = TRUE)
  
  print(paste('Trees: ', trees_swstrpct_mlb))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:2){
    print(paste('Iteration: ', i))
    train_data <- swstrpct_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpct_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_swstrpct_mlb,
                   sigquant = sigquant_swstrpct_mlb,
                   k = k_swstrpct_mlb,
                   power = power_swstrpct_mlb,
                   base = base_swstrpct_mlb,
                   n.trees = trees_swstrpct_mlb,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_swstrpct_mlb_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_swstrpct_mlb_trees, .progress = TRUE))


bart_swstrpct_mlb_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() 

(swstrpct_mlb_error_ratio <- (bart_swstrpct_mlb_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(swstrpct_mlb$SwStr_pct_MLB)) #0.586 #BART Better


### Final Model SwStr% MLB ####

swstrpct_mlb_mod <- bart2(SwStr_pct_MLB ~ ., 
               data = swstrpct_mlb,
               sigdf = sigdf_swstrpct_mlb,
               sigquant = sigquant_swstrpct_mlb,
               k = k_swstrpct_mlb,
               power = power_swstrpct_mlb,
               base = base_swstrpct_mlb,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(swstrpct_mlb$SwStr_pct_MLB - residuals(swstrpct_mlb_mod))

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

#finding the optimal number of CVs
bart_era_aplus_cvs <- function(cv){
  set.seed(101);era_aplus$fold <- sample(1:cv, nrow(era_aplus), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- era_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- era_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ERA_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ERA_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ERA_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_era_aplus_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_era_aplus_cvs, .progress = TRUE))

bart_era_aplus_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.043, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #16


hyperparam_bart_era_aplus <- function(sigdf_era_aplus, sigquant_era_aplus, k_era_aplus, power_era_aplus, base_era_aplus, row_num = 1){
  set.seed(101);era_aplus$fold <- sample(1:16, nrow(era_aplus), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_era_aplus))
  print(paste('Sigquant: ', sigquant_era_aplus))
  print(paste('K: ', k_era_aplus))
  print(paste('Power: ', power_era_aplus))
  print(paste('Base: ', base_era_aplus))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:16){
    print(paste('Iteration: ', i))
    train_data <- era_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- era_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ERA_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ERA_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_era_aplus,
                   sigquant = sigquant_era_aplus,
                   k = k_era_aplus,
                   power = power_era_aplus,
                   base = base_era_aplus,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ERA_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_era_aplus_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_era_aplus_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_era_aplus_0.75_df$sigdf,
                                                             hyperparam_bart_era_aplus_0.75_df$sigquant, 
                                                             hyperparam_bart_era_aplus_0.75_df$k, 
                                                             hyperparam_bart_era_aplus_0.75_df$power,
                                                             hyperparam_bart_era_aplus_0.75_df$base,
                                                             hyperparam_bart_era_aplus_0.75_df$row_num), hyperparam_bart_era_aplus, .progress = TRUE)

hyperparam_bart_era_aplus_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_era_aplus_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_era_aplus_0.9_df$sigdf,
                                                            hyperparam_bart_era_aplus_0.9_df$sigquant, 
                                                            hyperparam_bart_era_aplus_0.9_df$k, 
                                                            hyperparam_bart_era_aplus_0.9_df$power,
                                                            hyperparam_bart_era_aplus_0.9_df$base,
                                                            hyperparam_bart_era_aplus_0.9_df$row_num), hyperparam_bart_era_aplus, .progress = TRUE)

hyperparam_bart_era_aplus_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_era_aplus_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_era_aplus_0.99_df$sigdf,
                                                             hyperparam_bart_era_aplus_0.99_df$sigquant, 
                                                             hyperparam_bart_era_aplus_0.99_df$k, 
                                                             hyperparam_bart_era_aplus_0.99_df$power,
                                                             hyperparam_bart_era_aplus_0.99_df$base,
                                                             hyperparam_bart_era_aplus_0.99_df$row_num), hyperparam_bart_era_aplus, .progress = TRUE)



hyperparam_bart_era_aplus_df <- bind_rows(hyperparam_bart_era_aplus_0.75_df, hyperparam_bart_era_aplus_0.9_df, hyperparam_bart_era_aplus_0.99_df)

hyperparam_bart_era_aplus_df <- hyperparam_bart_era_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_era_aplus <- hyperparam_bart_era_aplus_df$sigdf) #3
(sigquant_era_aplus <- hyperparam_bart_era_aplus_df$sigquant) #0.9
(k_era_aplus <- hyperparam_bart_era_aplus_df$k) # 6
(power_era_aplus <- hyperparam_bart_era_aplus_df$power) # 3
(base_era_aplus <- hyperparam_bart_era_aplus_df$base) # 0.5

hyperparam_bart_era_aplus_df$rmse/sd(era_aplus$ERA_Aplus) #0.963 #BART Better


hyperparam_bart_era_aplus_trees <- function(trees_era_aplus){
  set.seed(101);era_aplus$fold <- sample(1:16, nrow(era_aplus), replace = TRUE)
  
  print(paste('Trees: ', trees_era_aplus))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:16){
    print(paste('Iteration: ', i))
    train_data <- era_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- era_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ERA_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ERA_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_era_aplus,
                   sigquant = sigquant_era_aplus,
                   k = k_era_aplus,
                   power = power_era_aplus,
                   base = base_era_aplus,
                   n.trees = trees_era_aplus,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ERA_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_era_aplus_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_era_aplus_trees,.progress = TRUE))


bart_era_aplus_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #1000

(era_aplus_error_ratio <- (bart_era_aplus_trees_df %>% filter(trees == 1000) %>% pull(rmse))/sd(era_aplus$ERA_Aplus)) #0.962

### Final Model ERA A+ ####
era_aplus_mod <- bart2(ERA_Aplus ~ ., 
               data = era_aplus,
               sigdf = sigdf_era_aplus,
               sigquant = sigquant_era_aplus,
               k = k_era_aplus,
               power = power_era_aplus,
               base = base_era_aplus,
               n.trees = 1000,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(era_aplus$ERA_Aplus - residuals(era_aplus_mod))

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

#finding the optimal number of CVs
bart_era_AA_cvs <- function(cv){
  set.seed(101);era_AA$fold <- sample(1:cv, nrow(era_AA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- era_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- era_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ERA_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ERA_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ERA_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_era_AA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_era_AA_cvs, .progress = TRUE))

bart_era_AA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2))  #14


hyperparam_bart_era_AA <- function(sigdf_era_AA, sigquant_era_AA, k_era_AA, power_era_AA, base_era_AA,row_num){
  set.seed(101);era_AA$fold <- sample(1:14, nrow(era_AA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_era_AA))
  print(paste('Sigquant: ', sigquant_era_AA))
  print(paste('K: ', k_era_AA))
  print(paste('Power: ', power_era_AA))
  print(paste('Base: ', base_era_AA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:14){
    print(paste('Iteration: ', i))
    train_data <- era_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- era_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ERA_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ERA_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_era_AA,
                   sigquant = sigquant_era_AA,
                   k = k_era_AA,
                   power = power_era_AA,
                   base = base_era_AA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ERA_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_era_AA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_era_AA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_era_AA_0.75_df$sigdf,
                                                          hyperparam_bart_era_AA_0.75_df$sigquant, 
                                                          hyperparam_bart_era_AA_0.75_df$k, 
                                                          hyperparam_bart_era_AA_0.75_df$power,
                                                          hyperparam_bart_era_AA_0.75_df$base,
                                                          hyperparam_bart_era_AA_0.75_df$row_num), hyperparam_bart_era_AA, .progress = TRUE)

hyperparam_bart_era_AA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_era_AA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_era_AA_0.9_df$sigdf,
                                                         hyperparam_bart_era_AA_0.9_df$sigquant, 
                                                         hyperparam_bart_era_AA_0.9_df$k, 
                                                         hyperparam_bart_era_AA_0.9_df$power,
                                                         hyperparam_bart_era_AA_0.9_df$base,
                                                         hyperparam_bart_era_AA_0.9_df$row_num), hyperparam_bart_era_AA, .progress = TRUE)

hyperparam_bart_era_AA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_era_AA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_era_AA_0.99_df$sigdf,
                                                          hyperparam_bart_era_AA_0.99_df$sigquant, 
                                                          hyperparam_bart_era_AA_0.99_df$k, 
                                                          hyperparam_bart_era_AA_0.99_df$power,
                                                          hyperparam_bart_era_AA_0.99_df$base,
                                                          hyperparam_bart_era_AA_0.99_df$row_num), hyperparam_bart_era_AA, .progress = TRUE)



hyperparam_bart_era_AA_df <- bind_rows(hyperparam_bart_era_AA_0.75_df, hyperparam_bart_era_AA_0.9_df, hyperparam_bart_era_AA_0.99_df)

hyperparam_bart_era_AA_df <- hyperparam_bart_era_AA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_era_AA <- hyperparam_bart_era_AA_df$sigdf) #3
(sigquant_era_AA <- hyperparam_bart_era_AA_df$sigquant) #0.9
(k_era_AA <- hyperparam_bart_era_AA_df$k) # 6
(power_era_AA <- hyperparam_bart_era_AA_df$power) # 3
(base_era_AA <- hyperparam_bart_era_AA_df$base) # 0.75

hyperparam_bart_era_AA_df$rmse/sd(era_AA$ERA_AA) #0.965 #BART Better

hyperparam_bart_era_AA_trees <- function(trees_era_AA){
  set.seed(101);era_AA$fold <- sample(1:14, nrow(era_AA), replace = TRUE)
  
  print(paste('Trees: ', trees_era_AA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:14){
    print(paste('Iteration: ', i))
    train_data <- era_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- era_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ERA_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ERA_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_era_AA,
                   sigquant = sigquant_era_AA,
                   k = k_era_AA,
                   power = power_era_AA,
                   base = base_era_AA,
                   n.trees = trees_era_AA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ERA_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_era_AA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_era_AA_trees, .progress = TRUE))


bart_era_AA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  #scale_y_continuous(limits = c(0.0416, 0.0418)) +
  theme_bw() #800

(era_AA_error_ratio <- (bart_era_AA_trees_df %>% filter(trees == 800) %>% pull(rmse))/sd(era_AA$ERA_AA)) #0.965 #BART Better

### Final Model ERA AA ####

era_AA_mod <- bart2(ERA_AA ~ ., 
               data = era_AA,
               sigdf = sigdf_era_AA,
               sigquant = sigquant_era_AA,
               k = k_era_AA,
               power = power_era_AA,
               base = base_era_AA,
               n.trees = 800,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)


summary(era_AA$ERA_AA - residuals(era_AA_mod))


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

#finding the optimal number of CVs
bart_era_AAA_cvs <- function(cv){
  set.seed(101);era_AAA$fold <- sample(1:cv, nrow(era_AAA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- era_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- era_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ERA_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ERA_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ERA_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_era_AAA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_era_AAA_cvs))

bart_era_AAA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.042, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #10


hyperparam_bart_era_AAA <- function(sigdf_era_AAA, sigquant_era_AAA, k_era_AAA, power_era_AAA, base_era_AAA, row_num = 1){
  set.seed(101);era_AAA$fold <- sample(1:10, nrow(era_AAA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_era_AAA))
  print(paste('Sigquant: ', sigquant_era_AAA))
  print(paste('K: ', k_era_AAA))
  print(paste('Power: ', power_era_AAA))
  print(paste('Base: ', base_era_AAA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- era_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- era_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ERA_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ERA_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_era_AAA,
                   sigquant = sigquant_era_AAA,
                   k = k_era_AAA,
                   base = base_era_AAA,
                   power = power_era_AAA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ERA_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_era_AAA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_era_AAA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_era_AAA_0.75_df$sigdf,
                                                           hyperparam_bart_era_AAA_0.75_df$sigquant, 
                                                           hyperparam_bart_era_AAA_0.75_df$k, 
                                                           hyperparam_bart_era_AAA_0.75_df$power,
                                                           hyperparam_bart_era_AAA_0.75_df$base, 
                                                           hyperparam_bart_era_AAA_0.75_df$row_num), hyperparam_bart_era_AAA, .progress = TRUE)

hyperparam_bart_era_AAA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_era_AAA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_era_AAA_0.9_df$sigdf,
                                                          hyperparam_bart_era_AAA_0.9_df$sigquant, 
                                                          hyperparam_bart_era_AAA_0.9_df$k, 
                                                          hyperparam_bart_era_AAA_0.9_df$power,
                                                          hyperparam_bart_era_AAA_0.9_df$base,
                                                          hyperparam_bart_era_AAA_0.9_df$row_num), hyperparam_bart_era_AAA, .progress = TRUE)

hyperparam_bart_era_AAA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_era_AAA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_era_AAA_0.99_df$sigdf,
                                                           hyperparam_bart_era_AAA_0.99_df$sigquant, 
                                                           hyperparam_bart_era_AAA_0.99_df$k, 
                                                           hyperparam_bart_era_AAA_0.99_df$power,
                                                           hyperparam_bart_era_AAA_0.99_df$base,
                                                           hyperparam_bart_era_AAA_0.99_df$row_num), hyperparam_bart_era_AAA, .progress = TRUE)



hyperparam_bart_era_AAA_df <- bind_rows(hyperparam_bart_era_AAA_0.75_df, hyperparam_bart_era_AAA_0.9_df, hyperparam_bart_era_AAA_0.99_df)

hyperparam_bart_era_AAA_df <- hyperparam_bart_era_AAA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_era_AAA <- hyperparam_bart_era_AAA_df$sigdf) #10
(sigquant_era_AAA <- hyperparam_bart_era_AAA_df$sigquant) #0.75
(k_era_AAA <- hyperparam_bart_era_AAA_df$k) #5
(power_era_AAA <- hyperparam_bart_era_AAA_df$power) #4
(base_era_AAA <- hyperparam_bart_era_AAA_df$base) #0.75

hyperparam_bart_era_AAA_df$rmse/sd(era_AAA$ERA_AAA) #0.973 #BART Better

hyperparam_bart_era_AAA_trees <- function(trees_era_AAA){
  set.seed(101);era_AAA$fold <- sample(1:10, nrow(era_AAA), replace = TRUE)
  
  print(paste('Trees: ', trees_era_AAA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- era_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- era_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ERA_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ERA_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_era_AAA,
                   sigquant = sigquant_era_AAA,
                   k = k_era_AAA,
                   power = power_era_AAA,
                   base = base_era_AAA,
                   n.trees = trees_era_AAA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ERA_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_era_AAA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_era_AAA_trees, .progress = TRUE))


bart_era_AAA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  # scale_y_continuous(limits = c(0.038, 0.0383)) +
  theme_bw() #200

(era_AAA_error_ratio <- (bart_era_AAA_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(era_AAA$ERA_AAA)) #0.973 #BART Better

### Final Model ERA AAA ####

era_AAA_mod <- bart2(ERA_AAA ~ ., 
               data = era_AAA,
               sigdf = sigdf_era_AAA,
               sigquant = sigquant_era_AAA,
               k = k_era_AAA,
               power = power_era_AAA,
               base = base_era_AAA,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(era_AAA$ERA_AAA - residuals(era_AAA_mod))

summary(era_AAA$ERA_AAA)

data.frame(actual = era_AAA$ERA_AAA,
           prediction = era_AAA$ERA_AAA - residuals(era_AAA_mod)) %>% 
  ggplot(aes(actual)) +
  geom_density(fill = 'blue', alpha = 0.7) +
  geom_density(aes(prediction), fill = 'red', alpha = 0.7)

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

#finding the optimal number of CVs
bart_era_mlb_cvs <- function(cv){
  set.seed(101);era_mlb$fold <- sample(1:cv, nrow(era_mlb), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- era_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- era_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ERA_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ERA_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ERA_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_era_mlb_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_era_mlb_cvs))

bart_era_mlb_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #14


hyperparam_bart_era_mlb <- function(sigdf_era_mlb, sigquant_era_mlb, k_era_mlb, power_era_mlb, base_era_mlb,row_num = 1){
  set.seed(101);era_mlb$fold <- sample(1:14, nrow(era_mlb), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_era_mlb))
  print(paste('Sigquant: ', sigquant_era_mlb))
  print(paste('K: ', k_era_mlb))
  print(paste('Power: ', power_era_mlb))
  print(paste('Base: ', base_era_mlb))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:14){
    print(paste('Iteration: ', i))
    train_data <- era_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- era_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ERA_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ERA_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_era_mlb,
                   sigquant = sigquant_era_mlb,
                   k = k_era_mlb,
                   power = power_era_mlb,
                   base = base_era_mlb,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ERA_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_era_mlb_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_era_mlb_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_era_mlb_0.75_df$sigdf,
                                                           hyperparam_bart_era_mlb_0.75_df$sigquant, 
                                                           hyperparam_bart_era_mlb_0.75_df$k, 
                                                           hyperparam_bart_era_mlb_0.75_df$power,
                                                           hyperparam_bart_era_mlb_0.75_df$base,
                                                           hyperparam_bart_era_mlb_0.75_df$row_num), hyperparam_bart_era_mlb, .progress = TRUE)

hyperparam_bart_era_mlb_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_era_mlb_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_era_mlb_0.9_df$sigdf,
                                                          hyperparam_bart_era_mlb_0.9_df$sigquant, 
                                                          hyperparam_bart_era_mlb_0.9_df$k, 
                                                          hyperparam_bart_era_mlb_0.9_df$power,
                                                          hyperparam_bart_era_mlb_0.9_df$base,
                                                          hyperparam_bart_era_mlb_0.9_df$row_num), hyperparam_bart_era_mlb, .progress = TRUE)

hyperparam_bart_era_mlb_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_era_mlb_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_era_mlb_0.99_df$sigdf,
                                                           hyperparam_bart_era_mlb_0.99_df$sigquant, 
                                                           hyperparam_bart_era_mlb_0.99_df$k, 
                                                           hyperparam_bart_era_mlb_0.99_df$power,
                                                           hyperparam_bart_era_mlb_0.99_df$base,
                                                           hyperparam_bart_era_mlb_0.99_df$row_num), hyperparam_bart_era_mlb, .progress = TRUE)



hyperparam_bart_era_mlb_df <- bind_rows(hyperparam_bart_era_mlb_0.75_df, hyperparam_bart_era_mlb_0.9_df, hyperparam_bart_era_mlb_0.99_df)

hyperparam_bart_era_mlb_df <- hyperparam_bart_era_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_era_mlb <- hyperparam_bart_era_mlb_df$sigdf) #3
(sigquant_era_mlb <- hyperparam_bart_era_mlb_df$sigquant) #0.99
(k_era_mlb <- hyperparam_bart_era_mlb_df$k) #4
(power_era_mlb <- hyperparam_bart_era_mlb_df$power) #3
(base_era_mlb <- hyperparam_bart_era_mlb_df$base) #0.5

hyperparam_bart_era_mlb_df$rmse/sd(era_mlb$ERA_MLB) #0.982 #BART

hyperparam_bart_era_mlb_trees <- function(trees_era_mlb){
  set.seed(101);era_mlb$fold <- sample(1:14, nrow(era_mlb), replace = TRUE)
  
  print(paste('Trees: ', trees_era_mlb))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:14){
    print(paste('Iteration: ', i))
    train_data <- era_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- era_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-ERA_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(ERA_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_era_mlb,
                   sigquant = sigquant_era_mlb,
                   k = k_era_mlb,
                   power = power_era_mlb,
                   base = base_era_mlb,
                   n.trees = trees_era_mlb,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$ERA_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_era_mlb_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_era_mlb_trees, .progress = TRUE))


bart_era_mlb_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() 

(era_mlb_error_ratio <- (bart_era_mlb_trees_df %>% filter(trees == 1000) %>% pull(rmse))/sd(era_mlb$ERA_MLB)) #0.981


### Final Model ERA MLB ####

era_mlb_mod <- bart2(ERA_MLB ~ ., 
               data = era_mlb,
               sigdf = sigdf_era_mlb,
               sigquant = sigquant_era_mlb,
               k = k_era_mlb,
               power = power_era_mlb,
               base = base_era_mlb,
               n.trees = 1000,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(era_mlb$ERA_MLB - residuals(era_mlb_mod))
summary(era_mlb$ERA_MLB)

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


#finding the optimal number of CVs
bart_fip_aplus_cvs <- function(cv){
  set.seed(101);fip_aplus$fold <- sample(1:cv, nrow(fip_aplus), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- fip_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- fip_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-FIP_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(FIP_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$FIP_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_fip_aplus_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_fip_aplus_cvs))

bart_fip_aplus_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.043, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #10


hyperparam_bart_fip_aplus <- function(sigdf_fip_aplus, sigquant_fip_aplus, k_fip_aplus, power_fip_aplus, base_fip_aplus, row_num){
  set.seed(101);fip_aplus$fold <- sample(1:10, nrow(fip_aplus), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_fip_aplus))
  print(paste('Sigquant: ', sigquant_fip_aplus))
  print(paste('K: ', k_fip_aplus))
  print(paste('Power: ', power_fip_aplus))
  print(paste('Base: ', base_fip_aplus))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- fip_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- fip_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-FIP_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(FIP_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_fip_aplus,
                   sigquant = sigquant_fip_aplus,
                   k = k_fip_aplus,
                   power = power_fip_aplus,
                   base = base_fip_aplus,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$FIP_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_fip_aplus_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_fip_aplus_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_fip_aplus_0.75_df$sigdf,
                                                        hyperparam_bart_fip_aplus_0.75_df$sigquant, 
                                                        hyperparam_bart_fip_aplus_0.75_df$k, 
                                                        hyperparam_bart_fip_aplus_0.75_df$power,
                                                        hyperparam_bart_fip_aplus_0.75_df$base,
                                                        hyperparam_bart_fip_aplus_0.75_df$row_num), hyperparam_bart_fip_aplus, .progress = TRUE)

hyperparam_bart_fip_aplus_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_fip_aplus_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_fip_aplus_0.9_df$sigdf,
                                                       hyperparam_bart_fip_aplus_0.9_df$sigquant, 
                                                       hyperparam_bart_fip_aplus_0.9_df$k, 
                                                       hyperparam_bart_fip_aplus_0.9_df$power,
                                                       hyperparam_bart_fip_aplus_0.9_df$base,
                                                       hyperparam_bart_fip_aplus_0.9_df$row_num), hyperparam_bart_fip_aplus, .progress = TRUE)

hyperparam_bart_fip_aplus_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_fip_aplus_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_fip_aplus_0.99_df$sigdf,
                                                        hyperparam_bart_fip_aplus_0.99_df$sigquant, 
                                                        hyperparam_bart_fip_aplus_0.99_df$k, 
                                                        hyperparam_bart_fip_aplus_0.99_df$power,
                                                        hyperparam_bart_fip_aplus_0.99_df$base,
                                                        hyperparam_bart_fip_aplus_0.99_df$row_num), hyperparam_bart_fip_aplus, .progress = TRUE)



hyperparam_bart_fip_aplus_df <- bind_rows(hyperparam_bart_fip_aplus_0.75_df, hyperparam_bart_fip_aplus_0.9_df, hyperparam_bart_fip_aplus_0.99_df)

hyperparam_bart_fip_aplus_df <- hyperparam_bart_fip_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_fip_aplus <- hyperparam_bart_fip_aplus_df$sigdf) #10
(sigquant_fip_aplus <- hyperparam_bart_fip_aplus_df$sigquant) #0.75
(k_fip_aplus <- hyperparam_bart_fip_aplus_df$k) # 4
(power_fip_aplus <- hyperparam_bart_fip_aplus_df$power) # 1
(base_fip_aplus <- hyperparam_bart_fip_aplus_df$base) # 0.5


hyperparam_bart_fip_aplus_trees <- function(trees_fip_aplus){
  set.seed(101);fip_aplus$fold <- sample(1:10, nrow(fip_aplus), replace = TRUE)
  
  print(paste('Trees: ', trees_fip_aplus))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- fip_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- fip_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-FIP_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(FIP_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_fip_aplus,
                   sigquant = sigquant_fip_aplus,
                   k = k_fip_aplus,
                   power = power_fip_aplus,
                   base = base_fip_aplus,
                   n.trees = trees_fip_aplus,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$FIP_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_fip_aplus_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_fip_aplus_trees, .progress = TRUE))


bart_fip_aplus_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #200



(fip_aplus_error_ratio <- (bart_fip_aplus_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(fip_aplus$FIP_Aplus)) #0.943

### Final Model FIP A+ ####

fip_aplus_mod <- bart2(FIP_Aplus ~ ., 
               data = fip_aplus,
               sigdf = sigdf_fip_aplus,
               sigquant = sigquant_fip_aplus,
               k = k_fip_aplus,
               power = power_fip_aplus,
               base = base_fip_aplus,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(fip_aplus$FIP_Aplus - residuals(fip_aplus_mod))


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

#finding the optimal number of CVs
bart_fip_AA_cvs <- function(cv){
  set.seed(101);fip_AA$fold <- sample(1:cv, nrow(fip_AA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- fip_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- fip_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-FIP_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(FIP_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$FIP_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_fip_AA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_fip_AA_cvs))

bart_fip_AA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2))  #8


hyperparam_bart_fip_AA <- function(sigdf_fip_AA, sigquant_fip_AA, k_fip_AA, power_fip_AA, base_fip_AA,row_num){
  set.seed(101);fip_AA$fold <- sample(1:8, nrow(fip_AA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_fip_AA))
  print(paste('Sigquant: ', sigquant_fip_AA))
  print(paste('K: ', k_fip_AA))
  print(paste('Power: ', power_fip_AA))
  print(paste('Base: ', base_fip_AA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:8){
    print(paste('Iteration: ', i))
    train_data <- fip_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- fip_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-FIP_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(FIP_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_fip_AA,
                   sigquant = sigquant_fip_AA,
                   k = k_fip_AA,
                   power = power_fip_AA,
                   base = base_fip_AA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$FIP_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_fip_AA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_fip_AA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_fip_AA_0.75_df$sigdf,
                                                     hyperparam_bart_fip_AA_0.75_df$sigquant, 
                                                     hyperparam_bart_fip_AA_0.75_df$k, 
                                                     hyperparam_bart_fip_AA_0.75_df$power,
                                                     hyperparam_bart_fip_AA_0.75_df$base,
                                                     hyperparam_bart_fip_AA_0.75_df$row_num), hyperparam_bart_fip_AA, .progress = TRUE)

hyperparam_bart_fip_AA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_fip_AA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_fip_AA_0.9_df$sigdf,
                                                    hyperparam_bart_fip_AA_0.9_df$sigquant, 
                                                    hyperparam_bart_fip_AA_0.9_df$k, 
                                                    hyperparam_bart_fip_AA_0.9_df$power,
                                                    hyperparam_bart_fip_AA_0.9_df$base,
                                                    hyperparam_bart_fip_AA_0.9_df$row_num), hyperparam_bart_fip_AA, .progress = TRUE)

hyperparam_bart_fip_AA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_fip_AA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_fip_AA_0.99_df$sigdf,
                                                     hyperparam_bart_fip_AA_0.99_df$sigquant, 
                                                     hyperparam_bart_fip_AA_0.99_df$k, 
                                                     hyperparam_bart_fip_AA_0.99_df$power,
                                                     hyperparam_bart_fip_AA_0.99_df$base,
                                                     hyperparam_bart_fip_AA_0.99_df$row_num), hyperparam_bart_fip_AA, .progress = TRUE)



hyperparam_bart_fip_AA_df <- bind_rows(hyperparam_bart_fip_AA_0.75_df, hyperparam_bart_fip_AA_0.9_df, hyperparam_bart_fip_AA_0.99_df)

hyperparam_bart_fip_AA_df <- hyperparam_bart_fip_AA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_fip_AA <- hyperparam_bart_fip_AA_df$sigdf) #3
(sigquant_fip_AA <- hyperparam_bart_fip_AA_df$sigquant) #0.9
(k_fip_AA <- hyperparam_bart_fip_AA_df$k) # 4
(power_fip_AA <- hyperparam_bart_fip_AA_df$power) # 4
(base_fip_AA <- hyperparam_bart_fip_AA_df$base) # 0.5


hyperparam_bart_fip_AA_trees <- function(trees_fip_AA){
  set.seed(101);fip_AA$fold <- sample(1:8, nrow(fip_AA), replace = TRUE)
  
  print(paste('Trees: ', trees_fip_AA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:8){
    print(paste('Iteration: ', i))
    train_data <- fip_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- fip_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-FIP_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(FIP_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_fip_AA,
                   sigquant = sigquant_fip_AA,
                   k = k_fip_AA,
                   power = power_fip_AA,
                   base = base_fip_AA,
                   n.trees = trees_fip_AA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$FIP_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_fip_AA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_fip_AA_trees, .progress = TRUE))


bart_fip_AA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #1000



(fip_AA_error_ratio <- (bart_fip_AA_trees_df %>% filter(trees == 1000) %>% pull(rmse))/sd(fip_AA$FIP_AA)) #0.953


### Final Model FIP AA ####

fip_AA_mod <- bart2(FIP_AA ~ ., 
               data = fip_AA,
               sigdf = sigdf_fip_AA,
               sigquant = sigquant_fip_AA,
               k = k_fip_AA,
               power = power_fip_AA,
               base = base_fip_AA,
               n.trees = 1000,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(fip_AA$FIP_AA - residuals(fip_AA_mod))

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

#finding the optimal number of CVs
bart_fip_AAA_cvs <- function(cv){
  set.seed(101);fip_AAA$fold <- sample(1:cv, nrow(fip_AAA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- fip_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- fip_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-FIP_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(FIP_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$FIP_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_fip_AAA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_fip_AAA_cvs))

bart_fip_AAA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.042, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #10


hyperparam_bart_fip_AAA <- function(sigdf_fip_AAA, sigquant_fip_AAA, k_fip_AAA, power_fip_AAA, base_fip_AAA, row_num){
  set.seed(101);fip_AAA$fold <- sample(1:10, nrow(fip_AAA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_fip_AAA))
  print(paste('Sigquant: ', sigquant_fip_AAA))
  print(paste('K: ', k_fip_AAA))
  print(paste('Power: ', power_fip_AAA))
  print(paste('Base: ', base_fip_AAA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- fip_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- fip_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-FIP_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(FIP_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_fip_AAA,
                   sigquant = sigquant_fip_AAA,
                   k = k_fip_AAA,
                   base = base_fip_AAA,
                   power = power_fip_AAA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$FIP_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_fip_AAA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_fip_AAA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_fip_AAA_0.75_df$sigdf,
                                                      hyperparam_bart_fip_AAA_0.75_df$sigquant, 
                                                      hyperparam_bart_fip_AAA_0.75_df$k, 
                                                      hyperparam_bart_fip_AAA_0.75_df$power,
                                                      hyperparam_bart_fip_AAA_0.75_df$base, 
                                                      hyperparam_bart_fip_AAA_0.75_df$row_num), hyperparam_bart_fip_AAA, .progress = TRUE)

hyperparam_bart_fip_AAA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_fip_AAA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_fip_AAA_0.9_df$sigdf,
                                                     hyperparam_bart_fip_AAA_0.9_df$sigquant, 
                                                     hyperparam_bart_fip_AAA_0.9_df$k, 
                                                     hyperparam_bart_fip_AAA_0.9_df$power,
                                                     hyperparam_bart_fip_AAA_0.9_df$base,
                                                     hyperparam_bart_fip_AAA_0.9_df$row_num), hyperparam_bart_fip_AAA, .progress = TRUE)

hyperparam_bart_fip_AAA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(3,4,5,6),
  power = c(3,4,5,6),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_fip_AAA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_fip_AAA_0.99_df$sigdf,
                                                      hyperparam_bart_fip_AAA_0.99_df$sigquant, 
                                                      hyperparam_bart_fip_AAA_0.99_df$k, 
                                                      hyperparam_bart_fip_AAA_0.99_df$power,
                                                      hyperparam_bart_fip_AAA_0.99_df$base,
                                                      hyperparam_bart_fip_AAA_0.99_df$row_num), hyperparam_bart_fip_AAA, .progress = TRUE)



hyperparam_bart_fip_AAA_df <- bind_rows(hyperparam_bart_fip_AAA_0.75_df, hyperparam_bart_fip_AAA_0.9_df, hyperparam_bart_fip_AAA_0.99_df)

hyperparam_bart_fip_AAA_df <- hyperparam_bart_fip_AAA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_fip_AAA <- hyperparam_bart_fip_AAA_df$sigdf) #3
(sigquant_fip_AAA <- hyperparam_bart_fip_AAA_df$sigquant) #0.9
(k_fip_AAA <- hyperparam_bart_fip_AAA_df$k) #4
(power_fip_AAA <- hyperparam_bart_fip_AAA_df$power) #5
(base_fip_AAA <- hyperparam_bart_fip_AAA_df$base) #0.75


hyperparam_bart_fip_AAA_trees <- function(trees_fip_AAA){
  set.seed(101);fip_AAA$fold <- sample(1:10, nrow(fip_AAA), replace = TRUE)
  
  print(paste('Trees: ', trees_fip_AAA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- fip_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- fip_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-FIP_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(FIP_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_fip_AAA,
                   sigquant = sigquant_fip_AAA,
                   k = k_fip_AAA,
                   power = power_fip_AAA,
                   base = base_fip_AAA,
                   n.trees = trees_fip_AAA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$FIP_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_fip_AAA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_fip_AAA_trees, .progress = TRUE))


bart_fip_AAA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #200

(fip_AAA_error_ratio <- (bart_fip_AAA_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(fip_AAA$FIP_AAA)) #0.946

### Final Model FIP AAA ####

fip_AAA_mod <- bart2(FIP_AAA ~ ., 
                     data = fip_AAA,
                     sigdf = sigdf_fip_AAA,
                     sigquant = sigquant_fip_AAA,
                     k = k_fip_AAA,
                     power = power_fip_AAA,
                     base = base_fip_AAA,
                     n.trees = 200,
                     n.samples = 300,
                     n.burn = 300,
                     n.threads = parallel::detectCores()-1, 
                     n.chains = parallel::detectCores() -1,
                     seed = 101, # ensures reproducability
                     keepTrees = TRUE, # needed for prediction,
                     printEvery = 1000,
                     verbose = FALSE # give more information about model building process
)


summary(fip_AAA$FIP_AAA - residuals(fip_AAA_mod))


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

#finding the optimal number of CVs
bart_fip_mlb_cvs <- function(cv){
  set.seed(101);fip_mlb$fold <- sample(1:cv, nrow(fip_mlb), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- fip_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- fip_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-FIP_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(FIP_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$FIP_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_fip_mlb_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_fip_mlb_cvs))

bart_fip_mlb_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #10


hyperparam_bart_fip_mlb <- function(sigdf_fip_mlb, sigquant_fip_mlb, k_fip_mlb, power_fip_mlb, base_fip_mlb,row_num){
  set.seed(101);fip_mlb$fold <- sample(1:10, nrow(fip_mlb), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_fip_mlb))
  print(paste('Sigquant: ', sigquant_fip_mlb))
  print(paste('K: ', k_fip_mlb))
  print(paste('Power: ', power_fip_mlb))
  print(paste('Base: ', base_fip_mlb))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- fip_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- fip_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-FIP_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(FIP_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_fip_mlb,
                   sigquant = sigquant_fip_mlb,
                   k = k_fip_mlb,
                   power = power_fip_mlb,
                   base = base_fip_mlb,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$FIP_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_fip_mlb_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_fip_mlb_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_fip_mlb_0.75_df$sigdf,
                                                      hyperparam_bart_fip_mlb_0.75_df$sigquant, 
                                                      hyperparam_bart_fip_mlb_0.75_df$k, 
                                                      hyperparam_bart_fip_mlb_0.75_df$power,
                                                      hyperparam_bart_fip_mlb_0.75_df$base,
                                                      hyperparam_bart_fip_mlb_0.75_df$row_num), hyperparam_bart_fip_mlb, .progress = TRUE)

hyperparam_bart_fip_mlb_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_fip_mlb_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_fip_mlb_0.9_df$sigdf,
                                                     hyperparam_bart_fip_mlb_0.9_df$sigquant, 
                                                     hyperparam_bart_fip_mlb_0.9_df$k, 
                                                     hyperparam_bart_fip_mlb_0.9_df$power,
                                                     hyperparam_bart_fip_mlb_0.9_df$base,
                                                     hyperparam_bart_fip_mlb_0.9_df$row_num), hyperparam_bart_fip_mlb, .progress = TRUE)

hyperparam_bart_fip_mlb_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_fip_mlb_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_fip_mlb_0.99_df$sigdf,
                                                      hyperparam_bart_fip_mlb_0.99_df$sigquant, 
                                                      hyperparam_bart_fip_mlb_0.99_df$k, 
                                                      hyperparam_bart_fip_mlb_0.99_df$power,
                                                      hyperparam_bart_fip_mlb_0.99_df$base,
                                                      hyperparam_bart_fip_mlb_0.99_df$row_num), hyperparam_bart_fip_mlb, .progress = TRUE)



hyperparam_bart_fip_mlb_df <- bind_rows(hyperparam_bart_fip_mlb_0.75_df, hyperparam_bart_fip_mlb_0.9_df, hyperparam_bart_fip_mlb_0.99_df)

hyperparam_bart_fip_mlb_df <- hyperparam_bart_fip_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_fip_mlb <- hyperparam_bart_fip_mlb_df$sigdf) #3
(sigquant_fip_mlb <- hyperparam_bart_fip_mlb_df$sigquant) #0.9
(k_fip_mlb <- hyperparam_bart_fip_mlb_df$k) #3
(power_fip_mlb <- hyperparam_bart_fip_mlb_df$power) #4
(base_fip_mlb <- hyperparam_bart_fip_mlb_df$base) #0.75


hyperparam_bart_fip_mlb_trees <- function(trees_fip_mlb){
  set.seed(101);fip_mlb$fold <- sample(1:10, nrow(fip_mlb), replace = TRUE)
  
  print(paste('Trees: ', trees_fip_mlb))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- fip_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- fip_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-FIP_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(FIP_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_fip_mlb,
                   sigquant = sigquant_fip_mlb,
                   k = k_fip_mlb,
                   power = power_fip_mlb,
                   base = base_fip_mlb,
                   n.trees = trees_fip_mlb,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$FIP_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)/sd(fip_mlb$FIP_MLB)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_fip_mlb_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_fip_mlb_trees, .progress = TRUE))


bart_fip_mlb_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() 

(fip_mlb_error_ratio <- (bart_fip_mlb_trees_df %>% filter(trees == 800) %>% pull(rmse))/sd(fip_mlb$FIP_MLB)) #0.967


### Final Model FIP MLB ####

fip_mlb_mod <- bart2(FIP_MLB ~ ., 
               data = fip_mlb,
               sigdf = sigdf_fip_mlb,
               sigquant = sigquant_fip_mlb,
               k = k_fip_mlb,
               power = power_fip_mlb,
               base = base_fip_mlb,
               n.trees = 800,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(fip_mlb$FIP_MLB - residuals(fip_mlb_mod))

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


#finding the optimal number of CVs
bart_kpctpit_aplus_cvs <- function(cv){
  set.seed(101);kpctpit_aplus$fold <- sample(1:cv, nrow(kpctpit_aplus), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- kpctpit_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpctpit_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_kpctpit_aplus_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_kpctpit_aplus_cvs))

bart_kpctpit_aplus_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.043, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #6


hyperparam_bart_kpctpit_aplus <- function(sigdf_kpctpit_aplus, sigquant_kpctpit_aplus, k_kpctpit_aplus, power_kpctpit_aplus, base_kpctpit_aplus, row_num){
  set.seed(101);kpctpit_aplus$fold <- sample(1:6, nrow(kpctpit_aplus), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_kpctpit_aplus))
  print(paste('Sigquant: ', sigquant_kpctpit_aplus))
  print(paste('K: ', k_kpctpit_aplus))
  print(paste('Power: ', power_kpctpit_aplus))
  print(paste('Base: ', base_kpctpit_aplus))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:6){
    print(paste('Iteration: ', i))
    train_data <- kpctpit_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpctpit_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_kpctpit_aplus,
                   sigquant = sigquant_kpctpit_aplus,
                   k = k_kpctpit_aplus,
                   power = power_kpctpit_aplus,
                   base = base_kpctpit_aplus,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_kpctpit_aplus_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_kpctpit_aplus_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_kpctpit_aplus_0.75_df$sigdf,
                                                        hyperparam_bart_kpctpit_aplus_0.75_df$sigquant, 
                                                        hyperparam_bart_kpctpit_aplus_0.75_df$k, 
                                                        hyperparam_bart_kpctpit_aplus_0.75_df$power,
                                                        hyperparam_bart_kpctpit_aplus_0.75_df$base,
                                                        hyperparam_bart_kpctpit_aplus_0.75_df$row_num), hyperparam_bart_kpctpit_aplus, .progress = TRUE)

hyperparam_bart_kpctpit_aplus_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_kpctpit_aplus_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_kpctpit_aplus_0.9_df$sigdf,
                                                       hyperparam_bart_kpctpit_aplus_0.9_df$sigquant, 
                                                       hyperparam_bart_kpctpit_aplus_0.9_df$k, 
                                                       hyperparam_bart_kpctpit_aplus_0.9_df$power,
                                                       hyperparam_bart_kpctpit_aplus_0.9_df$base,
                                                       hyperparam_bart_kpctpit_aplus_0.9_df$row_num), hyperparam_bart_kpctpit_aplus, .progress = TRUE)

hyperparam_bart_kpctpit_aplus_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_kpctpit_aplus_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_kpctpit_aplus_0.99_df$sigdf,
                                                        hyperparam_bart_kpctpit_aplus_0.99_df$sigquant, 
                                                        hyperparam_bart_kpctpit_aplus_0.99_df$k, 
                                                        hyperparam_bart_kpctpit_aplus_0.99_df$power,
                                                        hyperparam_bart_kpctpit_aplus_0.99_df$base,
                                                        hyperparam_bart_kpctpit_aplus_0.99_df$row_num), hyperparam_bart_kpctpit_aplus, .progress = TRUE)



hyperparam_bart_kpctpit_aplus_df <- bind_rows(hyperparam_bart_kpctpit_aplus_0.75_df, hyperparam_bart_kpctpit_aplus_0.9_df, hyperparam_bart_kpctpit_aplus_0.99_df)

hyperparam_bart_kpctpit_aplus_df <- hyperparam_bart_kpctpit_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_kpctpit_aplus <- hyperparam_bart_kpctpit_aplus_df$sigdf) #10
(sigquant_kpctpit_aplus <- hyperparam_bart_kpctpit_aplus_df$sigquant) #0.75
(k_kpctpit_aplus <- hyperparam_bart_kpctpit_aplus_df$k) # 4
(power_kpctpit_aplus <- hyperparam_bart_kpctpit_aplus_df$power) # 1
(base_kpctpit_aplus <- hyperparam_bart_kpctpit_aplus_df$base) # 0.75


hyperparam_bart_kpctpit_aplus_trees <- function(trees_kpctpit_aplus){
  set.seed(101);kpctpit_aplus$fold <- sample(1:6, nrow(kpctpit_aplus), replace = TRUE)
  
  print(paste('Trees: ', trees_kpctpit_aplus))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:6){
    print(paste('Iteration: ', i))
    train_data <- kpctpit_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpctpit_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_kpctpit_aplus,
                   sigquant = sigquant_kpctpit_aplus,
                   k = k_kpctpit_aplus,
                   power = power_kpctpit_aplus,
                   base = base_kpctpit_aplus,
                   n.trees = trees_kpctpit_aplus,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)/sd(kpctpit_aplus$K_pct_Aplus)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_kpctpit_aplus_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_kpctpit_aplus_trees, .progress = TRUE))


bart_kpctpit_aplus_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #200

(kpctpit_aplus_error_ratio <- (bart_kpctpit_aplus_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(kpctpit_aplus$K_pct_Aplus)) #0.885 #BART Better

### Final Model K% A+ Pit ####

kpctpit_aplus_mod <- bart2(K_pct_Aplus ~ ., 
               data = kpctpit_aplus,
               sigdf = sigdf_kpctpit_aplus,
               sigquant = sigquant_kpctpit_aplus,
               k = k_kpctpit_aplus,
               power = power_kpctpit_aplus,
               base = base_kpctpit_aplus,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(kpctpit_aplus$K_pct_Aplus - residuals(kpctpit_aplus_mod))

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

#finding the optimal number of CVs
bart_kpctpit_AA_cvs <- function(cv){
  set.seed(101);kpctpit_AA$fold <- sample(1:cv, nrow(kpctpit_AA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- kpctpit_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpctpit_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_kpctpit_AA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_kpctpit_AA_cvs))

bart_kpctpit_AA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2))  #10


hyperparam_bart_kpctpit_AA <- function(sigdf_kpctpit_AA, sigquant_kpctpit_AA, k_kpctpit_AA, power_kpctpit_AA, base_kpctpit_AA,row_num){
  set.seed(101);kpctpit_AA$fold <- sample(1:10, nrow(kpctpit_AA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_kpctpit_AA))
  print(paste('Sigquant: ', sigquant_kpctpit_AA))
  print(paste('K: ', k_kpctpit_AA))
  print(paste('Power: ', power_kpctpit_AA))
  print(paste('Base: ', base_kpctpit_AA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- kpctpit_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpctpit_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_kpctpit_AA,
                   sigquant = sigquant_kpctpit_AA,
                   k = k_kpctpit_AA,
                   power = power_kpctpit_AA,
                   base = base_kpctpit_AA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_kpctpit_AA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_kpctpit_AA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_kpctpit_AA_0.75_df$sigdf,
                                                     hyperparam_bart_kpctpit_AA_0.75_df$sigquant, 
                                                     hyperparam_bart_kpctpit_AA_0.75_df$k, 
                                                     hyperparam_bart_kpctpit_AA_0.75_df$power,
                                                     hyperparam_bart_kpctpit_AA_0.75_df$base,
                                                     hyperparam_bart_kpctpit_AA_0.75_df$row_num), hyperparam_bart_kpctpit_AA, .progress = TRUE)

hyperparam_bart_kpctpit_AA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_kpctpit_AA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_kpctpit_AA_0.9_df$sigdf,
                                                    hyperparam_bart_kpctpit_AA_0.9_df$sigquant, 
                                                    hyperparam_bart_kpctpit_AA_0.9_df$k, 
                                                    hyperparam_bart_kpctpit_AA_0.9_df$power,
                                                    hyperparam_bart_kpctpit_AA_0.9_df$base,
                                                    hyperparam_bart_kpctpit_AA_0.9_df$row_num), hyperparam_bart_kpctpit_AA, .progress = TRUE)

hyperparam_bart_kpctpit_AA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_kpctpit_AA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_kpctpit_AA_0.99_df$sigdf,
                                                     hyperparam_bart_kpctpit_AA_0.99_df$sigquant, 
                                                     hyperparam_bart_kpctpit_AA_0.99_df$k, 
                                                     hyperparam_bart_kpctpit_AA_0.99_df$power,
                                                     hyperparam_bart_kpctpit_AA_0.99_df$base,
                                                     hyperparam_bart_kpctpit_AA_0.99_df$row_num), hyperparam_bart_kpctpit_AA, .progress = TRUE)



hyperparam_bart_kpctpit_AA_df <- bind_rows(hyperparam_bart_kpctpit_AA_0.75_df, hyperparam_bart_kpctpit_AA_0.9_df, hyperparam_bart_kpctpit_AA_0.99_df)

hyperparam_bart_kpctpit_AA_df <- hyperparam_bart_kpctpit_AA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_kpctpit_AA <- hyperparam_bart_kpctpit_AA_df$sigdf) #10
(sigquant_kpctpit_AA <- hyperparam_bart_kpctpit_AA_df$sigquant) #0.75
(k_kpctpit_AA <- hyperparam_bart_kpctpit_AA_df$k) # 4
(power_kpctpit_AA <- hyperparam_bart_kpctpit_AA_df$power) # 1
(base_kpctpit_AA <- hyperparam_bart_kpctpit_AA_df$base) #0.75 


hyperparam_bart_kpctpit_AA_trees <- function(trees_kpctpit_AA){
  set.seed(101);kpctpit_AA$fold <- sample(1:10, nrow(kpctpit_AA), replace = TRUE)
  
  print(paste('Trees: ', trees_kpctpit_AA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- kpctpit_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpctpit_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_kpctpit_AA,
                   sigquant = sigquant_kpctpit_AA,
                   k = k_kpctpit_AA,
                   power = power_kpctpit_AA,
                   base = base_kpctpit_AA,
                   n.trees = trees_kpctpit_AA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)/sd(kpctpit_AA$K_pct_AA)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_kpctpit_AA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_kpctpit_AA_trees, .progress = TRUE))


bart_kpctpit_AA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #200


(kpctpit_AA_error_ratio <- (bart_kpctpit_AA_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(kpctpit_AA$K_pct_AA)) #0.878 #BART Better

### Final Model K% AA Pit ####

kpctpit_AA_mod <- bart2(K_pct_AA ~ ., 
               data = kpctpit_AA,
               sigdf = sigdf_kpctpit_AA,
               sigquant = sigquant_kpctpit_AA,
               k = k_kpctpit_AA,
               power = power_kpctpit_AA,
               base = base_kpctpit_AA,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(kpctpit_AA$K_pct_AA - residuals(kpctpit_AA_mode))


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

#finding the optimal number of CVs
bart_kpctpit_AAA_cvs <- function(cv){
  set.seed(101);kpctpit_AAA$fold <- sample(1:cv, nrow(kpctpit_AAA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- kpctpit_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpctpit_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_kpctpit_AAA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_kpctpit_AAA_cvs))

bart_kpctpit_AAA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.042, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #8


hyperparam_bart_kpctpit_AAA <- function(sigdf_kpctpit_AAA, sigquant_kpctpit_AAA, k_kpctpit_AAA, power_kpctpit_AAA, base_kpctpit_AAA, row_num){
  set.seed(101);kpctpit_AAA$fold <- sample(1:8, nrow(kpctpit_AAA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_kpctpit_AAA))
  print(paste('Sigquant: ', sigquant_kpctpit_AAA))
  print(paste('K: ', k_kpctpit_AAA))
  print(paste('Power: ', power_kpctpit_AAA))
  print(paste('Base: ', base_kpctpit_AAA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:8){
    print(paste('Iteration: ', i))
    train_data <- kpctpit_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpctpit_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_kpctpit_AAA,
                   sigquant = sigquant_kpctpit_AAA,
                   k = k_kpctpit_AAA,
                   base = base_kpctpit_AAA,
                   power = power_kpctpit_AAA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_kpctpit_AAA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_kpctpit_AAA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_kpctpit_AAA_0.75_df$sigdf,
                                                      hyperparam_bart_kpctpit_AAA_0.75_df$sigquant, 
                                                      hyperparam_bart_kpctpit_AAA_0.75_df$k, 
                                                      hyperparam_bart_kpctpit_AAA_0.75_df$power,
                                                      hyperparam_bart_kpctpit_AAA_0.75_df$base, 
                                                      hyperparam_bart_kpctpit_AAA_0.75_df$row_num), hyperparam_bart_kpctpit_AAA, .progress = TRUE)

hyperparam_bart_kpctpit_AAA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_kpctpit_AAA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_kpctpit_AAA_0.9_df$sigdf,
                                                     hyperparam_bart_kpctpit_AAA_0.9_df$sigquant, 
                                                     hyperparam_bart_kpctpit_AAA_0.9_df$k, 
                                                     hyperparam_bart_kpctpit_AAA_0.9_df$power,
                                                     hyperparam_bart_kpctpit_AAA_0.9_df$base,
                                                     hyperparam_bart_kpctpit_AAA_0.9_df$row_num), hyperparam_bart_kpctpit_AAA, .progress = TRUE)

hyperparam_bart_kpctpit_AAA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_kpctpit_AAA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_kpctpit_AAA_0.99_df$sigdf,
                                                      hyperparam_bart_kpctpit_AAA_0.99_df$sigquant, 
                                                      hyperparam_bart_kpctpit_AAA_0.99_df$k, 
                                                      hyperparam_bart_kpctpit_AAA_0.99_df$power,
                                                      hyperparam_bart_kpctpit_AAA_0.99_df$base,
                                                      hyperparam_bart_kpctpit_AAA_0.99_df$row_num), hyperparam_bart_kpctpit_AAA, .progress = TRUE)



hyperparam_bart_kpctpit_AAA_df <- bind_rows(hyperparam_bart_kpctpit_AAA_0.75_df, hyperparam_bart_kpctpit_AAA_0.9_df, hyperparam_bart_kpctpit_AAA_0.99_df)

hyperparam_bart_kpctpit_AAA_df <- hyperparam_bart_kpctpit_AAA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_kpctpit_AAA <- hyperparam_bart_kpctpit_AAA_df$sigdf) #3
(sigquant_kpctpit_AAA <- hyperparam_bart_kpctpit_AAA_df$sigquant) #0.9
(k_kpctpit_AAA <- hyperparam_bart_kpctpit_AAA_df$k) #4
(power_kpctpit_AAA <- hyperparam_bart_kpctpit_AAA_df$power) #2
(base_kpctpit_AAA <- hyperparam_bart_kpctpit_AAA_df$base) #0.5

hyperparam_bart_kpctpit_AAA_df$rmse/sd(kpctpit_AAA$K_pct_AAA) #0.54 #BART Better

hyperparam_bart_kpctpit_AAA_trees <- function(trees_kpctpit_AAA){
  set.seed(101);kpctpit_AAA$fold <- sample(1:8, nrow(kpctpit_AAA), replace = TRUE)
  
  print(paste('Trees: ', trees_kpctpit_AAA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:8){
    print(paste('Iteration: ', i))
    train_data <- kpctpit_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpctpit_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_kpctpit_AAA,
                   sigquant = sigquant_kpctpit_AAA,
                   k = k_kpctpit_AAA,
                   power = power_kpctpit_AAA,
                   base = base_kpctpit_AAA,
                   n.trees = trees_kpctpit_AAA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)/sd(kpctpit_AAA$K_pct_AAA)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_kpctpit_AAA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_kpctpit_AAA_trees, .progress = TRUE))


bart_kpctpit_AAA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #1200

(kpctpit_AAA_error_ratio <- (bart_kpctpit_AAA_trees_df %>% filter(trees == 1200) %>% pull(rmse))/sd(kpctpit_AAA$K_pct_AAA)) #0.811 #BART Better

### Final Model K% AAA Pit ####
kpctpit_AAA_mod <- bart2(K_pct_AAA ~ ., 
               data = kpctpit_AAA,
               sigdf = sigdf_kpctpit_AAA,
               sigquant = sigquant_kpctpit_AAA,
               k = k_kpctpit_AAA,
               power = power_kpctpit_AAA,
               base = base_kpctpit_AAA,
               n.trees = 1200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(kpctpit_AAA$K_pct_AAA - residuals(kpctpit_AAA_mod))


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

#finding the optimal number of CVs
bart_kpctpit_mlb_cvs <- function(cv){
  set.seed(101);kpctpit_mlb$fold <- sample(1:cv, nrow(kpctpit_mlb), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- kpctpit_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpctpit_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_kpctpit_mlb_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_kpctpit_mlb_cvs))

bart_kpctpit_mlb_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #10


hyperparam_bart_kpctpit_mlb <- function(sigdf_kpctpit_mlb, sigquant_kpctpit_mlb, k_kpctpit_mlb, power_kpctpit_mlb, base_kpctpit_mlb,row_num){
  set.seed(101);kpctpit_mlb$fold <- sample(1:10, nrow(kpctpit_mlb), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_kpctpit_mlb))
  print(paste('Sigquant: ', sigquant_kpctpit_mlb))
  print(paste('K: ', k_kpctpit_mlb))
  print(paste('Power: ', power_kpctpit_mlb))
  print(paste('Base: ', base_kpctpit_mlb))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- kpctpit_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpctpit_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_kpctpit_mlb,
                   sigquant = sigquant_kpctpit_mlb,
                   k = k_kpctpit_mlb,
                   power = power_kpctpit_mlb,
                   base = base_kpctpit_mlb,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_kpctpit_mlb_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_kpctpit_mlb_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_kpctpit_mlb_0.75_df$sigdf,
                                                      hyperparam_bart_kpctpit_mlb_0.75_df$sigquant, 
                                                      hyperparam_bart_kpctpit_mlb_0.75_df$k, 
                                                      hyperparam_bart_kpctpit_mlb_0.75_df$power,
                                                      hyperparam_bart_kpctpit_mlb_0.75_df$base,
                                                      hyperparam_bart_kpctpit_mlb_0.75_df$row_num), hyperparam_bart_kpctpit_mlb, .progress = TRUE)

hyperparam_bart_kpctpit_mlb_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_kpctpit_mlb_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_kpctpit_mlb_0.9_df$sigdf,
                                                     hyperparam_bart_kpctpit_mlb_0.9_df$sigquant, 
                                                     hyperparam_bart_kpctpit_mlb_0.9_df$k, 
                                                     hyperparam_bart_kpctpit_mlb_0.9_df$power,
                                                     hyperparam_bart_kpctpit_mlb_0.9_df$base,
                                                     hyperparam_bart_kpctpit_mlb_0.9_df$row_num), hyperparam_bart_kpctpit_mlb, .progress = TRUE)

hyperparam_bart_kpctpit_mlb_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_kpctpit_mlb_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_kpctpit_mlb_0.99_df$sigdf,
                                                      hyperparam_bart_kpctpit_mlb_0.99_df$sigquant, 
                                                      hyperparam_bart_kpctpit_mlb_0.99_df$k, 
                                                      hyperparam_bart_kpctpit_mlb_0.99_df$power,
                                                      hyperparam_bart_kpctpit_mlb_0.99_df$base,
                                                      hyperparam_bart_kpctpit_mlb_0.99_df$row_num), hyperparam_bart_kpctpit_mlb, .progress = TRUE)



hyperparam_bart_kpctpit_mlb_df <- bind_rows(hyperparam_bart_kpctpit_mlb_0.75_df, hyperparam_bart_kpctpit_mlb_0.9_df, hyperparam_bart_kpctpit_mlb_0.99_df)

hyperparam_bart_kpctpit_mlb_df <- hyperparam_bart_kpctpit_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_kpctpit_mlb <- hyperparam_bart_kpctpit_mlb_df$sigdf) #3
(sigquant_kpctpit_mlb <- hyperparam_bart_kpctpit_mlb_df$sigquant) #0.99
(k_kpctpit_mlb <- hyperparam_bart_kpctpit_mlb_df$k) #4
(power_kpctpit_mlb <- hyperparam_bart_kpctpit_mlb_df$power) #1
(base_kpctpit_mlb <- hyperparam_bart_kpctpit_mlb_df$base) #0.5



hyperparam_bart_kpctpit_mlb_trees <- function(trees_kpctpit_mlb){
  set.seed(101);kpctpit_mlb$fold <- sample(1:10, nrow(kpctpit_mlb), replace = TRUE)
  
  print(paste('Trees: ', trees_kpctpit_mlb))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- kpctpit_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- kpctpit_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-K_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(K_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_kpctpit_mlb,
                   sigquant = sigquant_kpctpit_mlb,
                   k = k_kpctpit_mlb,
                   power = power_kpctpit_mlb,
                   base = base_kpctpit_mlb,
                   n.trees = trees_kpctpit_mlb,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$K_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_kpctpit_mlb_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_kpctpit_mlb_trees, .progress = TRUE))


bart_kpctpit_mlb_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() 

(kpctpit_mlb_error_ratio <- (bart_kpctpit_mlb_trees_df %>% filter(trees == 800) %>% pull(rmse))/sd(kpctpit_mlb$K_pct_MLB)) # 0.897 #BART Better

### Final Model K% MLB Pit ####

kpctpit_mlb_mod <- bart2(K_pct_MLB ~ ., 
               data = kpctpit_mlb,
               sigdf = sigdf_kpctpit_mlb,
               sigquant = sigquant_kpctpit_mlb,
               k = k_kpctpit_mlb,
               power = power_kpctpit_mlb,
               base = base_kpctpit_mlb,
               n.trees = 800,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(kpctpit_mlb$K_pct_MLB - residuals(kpctpit_mlb_mod))


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


#finding the optimal number of CVs
bart_bbpctpit_aplus_cvs <- function(cv){
  set.seed(101);bbpctpit_aplus$fold <- sample(1:cv, nrow(bbpctpit_aplus), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- bbpctpit_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpctpit_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_bbpctpit_aplus_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_bbpctpit_aplus_cvs))

bart_bbpctpit_aplus_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #10


hyperparam_bart_bbpctpit_aplus <- function(sigdf_bbpctpit_aplus, sigquant_bbpctpit_aplus, k_bbpctpit_aplus, power_bbpctpit_aplus, base_bbpctpit_aplus, row_num){
  set.seed(101);bbpctpit_aplus$fold <- sample(1:10, nrow(bbpctpit_aplus), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_bbpctpit_aplus))
  print(paste('Sigquant: ', sigquant_bbpctpit_aplus))
  print(paste('K: ', k_bbpctpit_aplus))
  print(paste('Power: ', power_bbpctpit_aplus))
  print(paste('Base: ', base_bbpctpit_aplus))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- bbpctpit_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpctpit_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_bbpctpit_aplus,
                   sigquant = sigquant_bbpctpit_aplus,
                   k = k_bbpctpit_aplus,
                   power = power_bbpctpit_aplus,
                   base = base_bbpctpit_aplus,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_bbpctpit_aplus_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bbpctpit_aplus_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpctpit_aplus_0.75_df$sigdf,
                                                            hyperparam_bart_bbpctpit_aplus_0.75_df$sigquant, 
                                                            hyperparam_bart_bbpctpit_aplus_0.75_df$k, 
                                                            hyperparam_bart_bbpctpit_aplus_0.75_df$power,
                                                            hyperparam_bart_bbpctpit_aplus_0.75_df$base,
                                                            hyperparam_bart_bbpctpit_aplus_0.75_df$row_num), hyperparam_bart_bbpctpit_aplus, .progress = TRUE)

hyperparam_bart_bbpctpit_aplus_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bbpctpit_aplus_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpctpit_aplus_0.9_df$sigdf,
                                                           hyperparam_bart_bbpctpit_aplus_0.9_df$sigquant, 
                                                           hyperparam_bart_bbpctpit_aplus_0.9_df$k, 
                                                           hyperparam_bart_bbpctpit_aplus_0.9_df$power,
                                                           hyperparam_bart_bbpctpit_aplus_0.9_df$base,
                                                           hyperparam_bart_bbpctpit_aplus_0.9_df$row_num), hyperparam_bart_bbpctpit_aplus, .progress = TRUE)

hyperparam_bart_bbpctpit_aplus_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_bbpctpit_aplus_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpctpit_aplus_0.99_df$sigdf,
                                                            hyperparam_bart_bbpctpit_aplus_0.99_df$sigquant, 
                                                            hyperparam_bart_bbpctpit_aplus_0.99_df$k, 
                                                            hyperparam_bart_bbpctpit_aplus_0.99_df$power,
                                                            hyperparam_bart_bbpctpit_aplus_0.99_df$base,
                                                            hyperparam_bart_bbpctpit_aplus_0.99_df$row_num), hyperparam_bart_bbpctpit_aplus, .progress = TRUE)



hyperparam_bart_bbpctpit_aplus_df <- bind_rows(hyperparam_bart_bbpctpit_aplus_0.75_df, hyperparam_bart_bbpctpit_aplus_0.9_df, hyperparam_bart_bbpctpit_aplus_0.99_df)

hyperparam_bart_bbpctpit_aplus_df <- hyperparam_bart_bbpctpit_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_bbpctpit_aplus <- hyperparam_bart_bbpctpit_aplus_df$sigdf) #3
(sigquant_bbpctpit_aplus <- hyperparam_bart_bbpctpit_aplus_df$sigquant) #0.99
(k_bbpctpit_aplus <- hyperparam_bart_bbpctpit_aplus_df$k) # 4
(power_bbpctpit_aplus <- hyperparam_bart_bbpctpit_aplus_df$power) # 1
(base_bbpctpit_aplus <- hyperparam_bart_bbpctpit_aplus_df$base) # 0.5


hyperparam_bart_bbpctpit_aplus_trees <- function(trees_bbpctpit_aplus){
  set.seed(101);bbpctpit_aplus$fold <- sample(1:10, nrow(bbpctpit_aplus), replace = TRUE)
  
  print(paste('Trees: ', trees_bbpctpit_aplus))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- bbpctpit_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpctpit_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_bbpctpit_aplus,
                   sigquant = sigquant_bbpctpit_aplus,
                   k = k_bbpctpit_aplus,
                   power = power_bbpctpit_aplus,
                   base = base_bbpctpit_aplus,
                   n.trees = trees_bbpctpit_aplus,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)/sd(bbpctpit_aplus$BB_pct_Aplus)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_bbpctpit_aplus_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_bbpctpit_aplus_trees, .progress = TRUE))


bart_bbpctpit_aplus_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #1200

(bbpctpit_aplus_error_ratio <- (bart_bbpctpit_aplus_trees_df %>% filter(trees == 1200) %>% pull(rmse))/sd(bbpctpit_aplus$BB_pct_Aplus)) #0.820 #BART Better

### Final Model BB% A+ Pit ####

bbpctpit_aplus_mod <- bart2(BB_pct_Aplus ~ ., 
               data = bbpctpit_aplus,
               sigdf = sigdf_bbpctpit_aplus,
               sigquant = sigquant_bbpctpit_aplus,
               k = k_bbpctpit_aplus,
               power = power_bbpctpit_aplus,
               base = base_bbpctpit_aplus,
               n.trees = 1200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(bbpctpit_aplus$BB_pct_Aplus - residuals(bbpctpit_aplus_mod))

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

#finding the optimal number of CVs
bart_bbpctpit_AA_cvs <- function(cv){
  set.seed(101);bbpctpit_AA$fold <- sample(1:cv, nrow(bbpctpit_AA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- bbpctpit_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpctpit_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_bbpctpit_AA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_bbpctpit_AA_cvs))

bart_bbpctpit_AA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2))  #8


hyperparam_bart_bbpctpit_AA <- function(sigdf_bbpctpit_AA, sigquant_bbpctpit_AA, k_bbpctpit_AA, power_bbpctpit_AA, base_bbpctpit_AA,row_num){
  set.seed(101);bbpctpit_AA$fold <- sample(1:8, nrow(bbpctpit_AA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_bbpctpit_AA))
  print(paste('Sigquant: ', sigquant_bbpctpit_AA))
  print(paste('K: ', k_bbpctpit_AA))
  print(paste('Power: ', power_bbpctpit_AA))
  print(paste('Base: ', base_bbpctpit_AA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:8){
    print(paste('Iteration: ', i))
    train_data <- bbpctpit_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpctpit_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_bbpctpit_AA,
                   sigquant = sigquant_bbpctpit_AA,
                   k = k_bbpctpit_AA,
                   power = power_bbpctpit_AA,
                   base = base_bbpctpit_AA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_bbpctpit_AA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bbpctpit_AA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpctpit_AA_0.75_df$sigdf,
                                                         hyperparam_bart_bbpctpit_AA_0.75_df$sigquant, 
                                                         hyperparam_bart_bbpctpit_AA_0.75_df$k, 
                                                         hyperparam_bart_bbpctpit_AA_0.75_df$power,
                                                         hyperparam_bart_bbpctpit_AA_0.75_df$base,
                                                         hyperparam_bart_bbpctpit_AA_0.75_df$row_num), hyperparam_bart_bbpctpit_AA, .progress = TRUE)

hyperparam_bart_bbpctpit_AA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bbpctpit_AA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpctpit_AA_0.9_df$sigdf,
                                                        hyperparam_bart_bbpctpit_AA_0.9_df$sigquant, 
                                                        hyperparam_bart_bbpctpit_AA_0.9_df$k, 
                                                        hyperparam_bart_bbpctpit_AA_0.9_df$power,
                                                        hyperparam_bart_bbpctpit_AA_0.9_df$base,
                                                        hyperparam_bart_bbpctpit_AA_0.9_df$row_num), hyperparam_bart_bbpctpit_AA, .progress = TRUE)

hyperparam_bart_bbpctpit_AA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_bbpctpit_AA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpctpit_AA_0.99_df$sigdf,
                                                         hyperparam_bart_bbpctpit_AA_0.99_df$sigquant, 
                                                         hyperparam_bart_bbpctpit_AA_0.99_df$k, 
                                                         hyperparam_bart_bbpctpit_AA_0.99_df$power,
                                                         hyperparam_bart_bbpctpit_AA_0.99_df$base,
                                                         hyperparam_bart_bbpctpit_AA_0.99_df$row_num), hyperparam_bart_bbpctpit_AA, .progress = TRUE)



hyperparam_bart_bbpctpit_AA_df <- bind_rows(hyperparam_bart_bbpctpit_AA_0.75_df, hyperparam_bart_bbpctpit_AA_0.9_df, hyperparam_bart_bbpctpit_AA_0.99_df)

hyperparam_bart_bbpctpit_AA_df <- hyperparam_bart_bbpctpit_AA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_bbpctpit_AA <- hyperparam_bart_bbpctpit_AA_df$sigdf) #3
(sigquant_bbpctpit_AA <- hyperparam_bart_bbpctpit_AA_df$sigquant) #0.9
(k_bbpctpit_AA <- hyperparam_bart_bbpctpit_AA_df$k) # 4
(power_bbpctpit_AA <- hyperparam_bart_bbpctpit_AA_df$power) # 1
(base_bbpctpit_AA <- hyperparam_bart_bbpctpit_AA_df$base) # 0.5


hyperparam_bart_bbpctpit_AA_trees <- function(trees_bbpctpit_AA){
  set.seed(101);bbpctpit_AA$fold <- sample(1:8, nrow(bbpctpit_AA), replace = TRUE)
  
  print(paste('Trees: ', trees_bbpctpit_AA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:8){
    print(paste('Iteration: ', i))
    train_data <- bbpctpit_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpctpit_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_bbpctpit_AA,
                   sigquant = sigquant_bbpctpit_AA,
                   k = k_bbpctpit_AA,
                   power = power_bbpctpit_AA,
                   base = base_bbpctpit_AA,
                   n.trees = trees_bbpctpit_AA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)/sd(bbpctpit_AA$BB_pct_AA)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_bbpctpit_AA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_bbpctpit_AA_trees, .progress = TRUE))


bart_bbpctpit_AA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #200

(bbpctpit_AA_error_ratio <- (bart_bbpctpit_AA_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(bbpctpit_AA$BB_pct_AA)) #0.832 #BART Better


### Final Model BB% AA Pit ####
bbpctpit_AA_mod <- bart2(BB_pct_AA ~ ., 
               data = bbpctpit_AA,
               sigdf = sigdf_bbpctpit_AA,
               sigquant = sigquant_bbpctpit_AA,
               k = k_bbpctpit_AA,
               power = power_bbpctpit_AA,
               base = base_bbpctpit_AA,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(bbpctpit_AA$BB_pct_AA - residuals(bbpctpit_AA_mod))

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

#finding the optimal number of CVs
bart_bbpctpit_AAA_cvs <- function(cv){
  set.seed(101);bbpctpit_AAA$fold <- sample(1:cv, nrow(bbpctpit_AAA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- bbpctpit_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpctpit_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_bbpctpit_AAA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_bbpctpit_AAA_cvs))

bart_bbpctpit_AAA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.042, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #8


hyperparam_bart_bbpctpit_AAA <- function(sigdf_bbpctpit_AAA, sigquant_bbpctpit_AAA, k_bbpctpit_AAA, power_bbpctpit_AAA, base_bbpctpit_AAA, row_num){
  set.seed(101);bbpctpit_AAA$fold <- sample(1:8, nrow(bbpctpit_AAA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_bbpctpit_AAA))
  print(paste('Sigquant: ', sigquant_bbpctpit_AAA))
  print(paste('K: ', k_bbpctpit_AAA))
  print(paste('Power: ', power_bbpctpit_AAA))
  print(paste('Base: ', base_bbpctpit_AAA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:8){
    print(paste('Iteration: ', i))
    train_data <- bbpctpit_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpctpit_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_bbpctpit_AAA,
                   sigquant = sigquant_bbpctpit_AAA,
                   k = k_bbpctpit_AAA,
                   base = base_bbpctpit_AAA,
                   power = power_bbpctpit_AAA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_bbpctpit_AAA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bbpctpit_AAA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpctpit_AAA_0.75_df$sigdf,
                                                          hyperparam_bart_bbpctpit_AAA_0.75_df$sigquant, 
                                                          hyperparam_bart_bbpctpit_AAA_0.75_df$k, 
                                                          hyperparam_bart_bbpctpit_AAA_0.75_df$power,
                                                          hyperparam_bart_bbpctpit_AAA_0.75_df$base, 
                                                          hyperparam_bart_bbpctpit_AAA_0.75_df$row_num), hyperparam_bart_bbpctpit_AAA, .progress = TRUE)

hyperparam_bart_bbpctpit_AAA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bbpctpit_AAA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpctpit_AAA_0.9_df$sigdf,
                                                         hyperparam_bart_bbpctpit_AAA_0.9_df$sigquant, 
                                                         hyperparam_bart_bbpctpit_AAA_0.9_df$k, 
                                                         hyperparam_bart_bbpctpit_AAA_0.9_df$power,
                                                         hyperparam_bart_bbpctpit_AAA_0.9_df$base,
                                                         hyperparam_bart_bbpctpit_AAA_0.9_df$row_num), hyperparam_bart_bbpctpit_AAA, .progress = TRUE)

hyperparam_bart_bbpctpit_AAA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_bbpctpit_AAA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpctpit_AAA_0.99_df$sigdf,
                                                          hyperparam_bart_bbpctpit_AAA_0.99_df$sigquant, 
                                                          hyperparam_bart_bbpctpit_AAA_0.99_df$k, 
                                                          hyperparam_bart_bbpctpit_AAA_0.99_df$power,
                                                          hyperparam_bart_bbpctpit_AAA_0.99_df$base,
                                                          hyperparam_bart_bbpctpit_AAA_0.99_df$row_num), hyperparam_bart_bbpctpit_AAA, .progress = TRUE)



hyperparam_bart_bbpctpit_AAA_df <- bind_rows(hyperparam_bart_bbpctpit_AAA_0.75_df, hyperparam_bart_bbpctpit_AAA_0.9_df, hyperparam_bart_bbpctpit_AAA_0.99_df)

hyperparam_bart_bbpctpit_AAA_df <- hyperparam_bart_bbpctpit_AAA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_bbpctpit_AAA <- hyperparam_bart_bbpctpit_AAA_df$sigdf) #3
(sigquant_bbpctpit_AAA <- hyperparam_bart_bbpctpit_AAA_df$sigquant) #0.99
(k_bbpctpit_AAA <- hyperparam_bart_bbpctpit_AAA_df$k) #4
(power_bbpctpit_AAA <- hyperparam_bart_bbpctpit_AAA_df$power) #2
(base_bbpctpit_AAA <- hyperparam_bart_bbpctpit_AAA_df$base) #0.75


hyperparam_bart_bbpctpit_AAA_trees <- function(trees_bbpctpit_AAA){
  set.seed(101);bbpctpit_AAA$fold <- sample(1:8, nrow(bbpctpit_AAA), replace = TRUE)
  
  print(paste('Trees: ', trees_bbpctpit_AAA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:8){
    print(paste('Iteration: ', i))
    train_data <- bbpctpit_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpctpit_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_bbpctpit_AAA,
                   sigquant = sigquant_bbpctpit_AAA,
                   k = k_bbpctpit_AAA,
                   power = power_bbpctpit_AAA,
                   base = base_bbpctpit_AAA,
                   n.trees = trees_bbpctpit_AAA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)/sd(bbpctpit_AAA$BB_pct_AAA)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_bbpctpit_AAA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_bbpctpit_AAA_trees, .progress = TRUE))


bart_bbpctpit_AAA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  # scale_y_continuous(limits = c(0.038, 0.0383)) +
  theme_bw() #1200

(bbpctpit_AAA_error_ratio <- (bart_bbpctpit_AAA_trees_df %>% filter(trees == 1200) %>% pull(rmse))/sd(bbpctpit_AAA$BB_pct_AAA)) #0.807

### Final Model BB% AAA Pit ####

bbpctpit_AAA_mod <- bart2(BB_pct_AAA ~ ., 
               data = bbpctpit_AAA,
               sigdf = sigdf_bbpctpit_AAA,
               sigquant = sigquant_bbpctpit_AAA,
               k = k_bbpctpit_AAA,
               power = power_bbpctpit_AAA,
               base = base_bbpctpit_AAA,
               n.trees = 1200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(bbpctpit_AAA$BB_pct_AAA - residuals(bbpctpit_AAA_mod))

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

#finding the optimal number of CVs
bart_bbpctpit_mlb_cvs <- function(cv){
  set.seed(101);bbpctpit_mlb$fold <- sample(1:cv, nrow(bbpctpit_mlb), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- bbpctpit_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpctpit_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_bbpctpit_mlb_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_bbpctpit_mlb_cvs))

bart_bbpctpit_mlb_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_classic() +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #10


hyperparam_bart_bbpctpit_mlb <- function(sigdf_bbpctpit_mlb, sigquant_bbpctpit_mlb, k_bbpctpit_mlb, power_bbpctpit_mlb, base_bbpctpit_mlb,row_num){
  set.seed(101);bbpctpit_mlb$fold <- sample(1:10, nrow(bbpctpit_mlb), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_bbpctpit_mlb))
  print(paste('Sigquant: ', sigquant_bbpctpit_mlb))
  print(paste('K: ', k_bbpctpit_mlb))
  print(paste('Power: ', power_bbpctpit_mlb))
  print(paste('Base: ', base_bbpctpit_mlb))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- bbpctpit_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpctpit_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_bbpctpit_mlb,
                   sigquant = sigquant_bbpctpit_mlb,
                   k = k_bbpctpit_mlb,
                   power = power_bbpctpit_mlb,
                   base = base_bbpctpit_mlb,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_bbpctpit_mlb_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bbpctpit_mlb_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpctpit_mlb_0.75_df$sigdf,
                                                          hyperparam_bart_bbpctpit_mlb_0.75_df$sigquant, 
                                                          hyperparam_bart_bbpctpit_mlb_0.75_df$k, 
                                                          hyperparam_bart_bbpctpit_mlb_0.75_df$power,
                                                          hyperparam_bart_bbpctpit_mlb_0.75_df$base,
                                                          hyperparam_bart_bbpctpit_mlb_0.75_df$row_num), hyperparam_bart_bbpctpit_mlb, .progress = TRUE)

hyperparam_bart_bbpctpit_mlb_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_bbpctpit_mlb_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpctpit_mlb_0.9_df$sigdf,
                                                         hyperparam_bart_bbpctpit_mlb_0.9_df$sigquant, 
                                                         hyperparam_bart_bbpctpit_mlb_0.9_df$k, 
                                                         hyperparam_bart_bbpctpit_mlb_0.9_df$power,
                                                         hyperparam_bart_bbpctpit_mlb_0.9_df$base,
                                                         hyperparam_bart_bbpctpit_mlb_0.9_df$row_num), hyperparam_bart_bbpctpit_mlb, .progress = TRUE)

hyperparam_bart_bbpctpit_mlb_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_bbpctpit_mlb_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_bbpctpit_mlb_0.99_df$sigdf,
                                                          hyperparam_bart_bbpctpit_mlb_0.99_df$sigquant, 
                                                          hyperparam_bart_bbpctpit_mlb_0.99_df$k, 
                                                          hyperparam_bart_bbpctpit_mlb_0.99_df$power,
                                                          hyperparam_bart_bbpctpit_mlb_0.99_df$base,
                                                          hyperparam_bart_bbpctpit_mlb_0.99_df$row_num), hyperparam_bart_bbpctpit_mlb, .progress = TRUE)



hyperparam_bart_bbpctpit_mlb_df <- bind_rows(hyperparam_bart_bbpctpit_mlb_0.75_df, hyperparam_bart_bbpctpit_mlb_0.9_df, hyperparam_bart_bbpctpit_mlb_0.99_df)

hyperparam_bart_bbpctpit_mlb_df <- hyperparam_bart_bbpctpit_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_bbpctpit_mlb <- hyperparam_bart_bbpctpit_mlb_df$sigdf) #3
(sigquant_bbpctpit_mlb <- hyperparam_bart_bbpctpit_mlb_df$sigquant) #0.9
(k_bbpctpit_mlb <- hyperparam_bart_bbpctpit_mlb_df$k) #4
(power_bbpctpit_mlb <- hyperparam_bart_bbpctpit_mlb_df$power) #2
(base_bbpctpit_mlb <- hyperparam_bart_bbpctpit_mlb_df$base) #0.5


hyperparam_bart_bbpctpit_mlb_trees <- function(trees_bbpctpit_mlb){
  set.seed(101);bbpctpit_mlb$fold <- sample(1:10, nrow(bbpctpit_mlb), replace = TRUE)
  
  print(paste('Trees: ', trees_bbpctpit_mlb))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- bbpctpit_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- bbpctpit_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-BB_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(BB_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_bbpctpit_mlb,
                   sigquant = sigquant_bbpctpit_mlb,
                   k = k_bbpctpit_mlb,
                   power = power_bbpctpit_mlb,
                   base = base_bbpctpit_mlb,
                   n.trees = trees_bbpctpit_mlb,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$BB_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)/sd(bbpctpit_mlb$BB_pct_MLB)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_bbpctpit_mlb_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_bbpctpit_mlb_trees, .progress = TRUE))


bart_bbpctpit_mlb_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #1000

(bbpctpit_mlb_error_ratio <- (bart_bbpctpit_mlb_trees_df %>% filter(trees == 1000) %>% pull(rmse))/sd(bbpctpit_mlb$BB_pct_MLB)) #0.901 #BART Better

### Final Model BB% MLB Pit ####
bbpctpit_mlb_mod <- bart2(BB_pct_MLB ~ ., 
               data = bbpctpit_mlb,
               sigdf = sigdf_bbpctpit_mlb,
               sigquant = sigquant_bbpctpit_mlb,
               k = k_bbpctpit_mlb,
               power = power_bbpctpit_mlb,
               base = base_bbpctpit_mlb,
               n.trees = 1000,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(bbpctpit_mlb$BB_pct_MLB - residuals(bbpctpit_mlb_mod))

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


#finding the optimal number of CVs
bart_swstrpctpit_aplus_cvs <- function(cv){
  set.seed(101);swstrpctpit_aplus$fold <- sample(1:cv, nrow(swstrpctpit_aplus), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- swstrpctpit_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpctpit_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_swstrpctpit_aplus_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_swstrpctpit_aplus_cvs))

bart_swstrpctpit_aplus_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.043, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #12


hyperparam_bart_swstrpctpit_aplus <- function(sigdf_swstrpctpit_aplus, sigquant_swstrpctpit_aplus, k_swstrpctpit_aplus, power_swstrpctpit_aplus, base_swstrpctpit_aplus, row_num){
  set.seed(101);swstrpctpit_aplus$fold <- sample(1:12, nrow(swstrpctpit_aplus), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_swstrpctpit_aplus))
  print(paste('Sigquant: ', sigquant_swstrpctpit_aplus))
  print(paste('K: ', k_swstrpctpit_aplus))
  print(paste('Power: ', power_swstrpctpit_aplus))
  print(paste('Base: ', base_swstrpctpit_aplus))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:12){
    print(paste('Iteration: ', i))
    train_data <- swstrpctpit_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpctpit_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_swstrpctpit_aplus,
                   sigquant = sigquant_swstrpctpit_aplus,
                   k = k_swstrpctpit_aplus,
                   power = power_swstrpctpit_aplus,
                   base = base_swstrpctpit_aplus,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_swstrpctpit_aplus_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_swstrpctpit_aplus_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpctpit_aplus_0.75_df$sigdf,
                                                             hyperparam_bart_swstrpctpit_aplus_0.75_df$sigquant, 
                                                             hyperparam_bart_swstrpctpit_aplus_0.75_df$k, 
                                                             hyperparam_bart_swstrpctpit_aplus_0.75_df$power,
                                                             hyperparam_bart_swstrpctpit_aplus_0.75_df$base,
                                                             hyperparam_bart_swstrpctpit_aplus_0.75_df$row_num), hyperparam_bart_swstrpctpit_aplus, .progress = TRUE)

hyperparam_bart_swstrpctpit_aplus_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_swstrpctpit_aplus_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpctpit_aplus_0.9_df$sigdf,
                                                            hyperparam_bart_swstrpctpit_aplus_0.9_df$sigquant, 
                                                            hyperparam_bart_swstrpctpit_aplus_0.9_df$k, 
                                                            hyperparam_bart_swstrpctpit_aplus_0.9_df$power,
                                                            hyperparam_bart_swstrpctpit_aplus_0.9_df$base,
                                                            hyperparam_bart_swstrpctpit_aplus_0.9_df$row_num), hyperparam_bart_swstrpctpit_aplus, .progress = TRUE)

hyperparam_bart_swstrpctpit_aplus_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_swstrpctpit_aplus_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpctpit_aplus_0.99_df$sigdf,
                                                             hyperparam_bart_swstrpctpit_aplus_0.99_df$sigquant, 
                                                             hyperparam_bart_swstrpctpit_aplus_0.99_df$k, 
                                                             hyperparam_bart_swstrpctpit_aplus_0.99_df$power,
                                                             hyperparam_bart_swstrpctpit_aplus_0.99_df$base,
                                                             hyperparam_bart_swstrpctpit_aplus_0.99_df$row_num), hyperparam_bart_swstrpctpit_aplus, .progress = TRUE)



hyperparam_bart_swstrpctpit_aplus_df <- bind_rows(hyperparam_bart_swstrpctpit_aplus_0.75_df, hyperparam_bart_swstrpctpit_aplus_0.9_df, hyperparam_bart_swstrpctpit_aplus_0.99_df)

hyperparam_bart_swstrpctpit_aplus_df <- hyperparam_bart_swstrpctpit_aplus_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_swstrpctpit_aplus <- hyperparam_bart_swstrpctpit_aplus_df$sigdf) #3
(sigquant_swstrpctpit_aplus <- hyperparam_bart_swstrpctpit_aplus_df$sigquant) #0.99
(k_swstrpctpit_aplus <- hyperparam_bart_swstrpctpit_aplus_df$k) # 1
(power_swstrpctpit_aplus <- hyperparam_bart_swstrpctpit_aplus_df$power) # 1
(base_swstrpctpit_aplus <- hyperparam_bart_swstrpctpit_aplus_df$base) # 0.75


hyperparam_bart_swstrpctpit_aplus_df$rmse/sd(swstrpctpit_aplus$SwStr_pct_Aplus) #0.735 #BART Better

hyperparam_bart_swstrpctpit_aplus_trees <- function(trees_swstrpctpit_aplus){
  set.seed(101);swstrpctpit_aplus$fold <- sample(1:12, nrow(swstrpctpit_aplus), replace = TRUE)
  
  print(paste('Trees: ', trees_swstrpctpit_aplus))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:12){
    print(paste('Iteration: ', i))
    train_data <- swstrpctpit_aplus %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpctpit_aplus %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_Aplus,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_Aplus ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_swstrpctpit_aplus,
                   sigquant = sigquant_swstrpctpit_aplus,
                   k = k_swstrpctpit_aplus,
                   power = power_swstrpctpit_aplus,
                   base = base_swstrpctpit_aplus,
                   n.trees = trees_swstrpctpit_aplus,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_Aplus, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)/sd(swstrpctpit_aplus$SwStr_pct_Aplus)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_swstrpctpit_aplus_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_swstrpctpit_aplus_trees, .progress = TRUE))


bart_swstrpctpit_aplus_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #200


(swstrpctpit_aplus_error_ratio <- (bart_swstrpctpit_aplus_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(swstrpct_aplus$SwStr_pct_Aplus)) #0.742

### Final Model SwStr% A+ Pit ####

swstrpctpit_aplus_mod <- bart2(SwStr_pct_Aplus ~ ., 
               data = swstrpctpit_aplus,
               sigdf = sigdf_swstrpctpit_aplus,
               sigquant = sigquant_swstrpctpit_aplus,
               k = k_swstrpctpit_aplus,
               power = power_swstrpctpit_aplus,
               base = base_swstrpctpit_aplus,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(swstrpctpit_aplus$SwStr_pct_Aplus - residuals(swstrpctpit_aplus_mod))

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

#finding the optimal number of CVs
bart_swstrpctpit_AA_cvs <- function(cv){
  set.seed(101);swstrpctpit_AA$fold <- sample(1:cv, nrow(swstrpctpit_AA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- swstrpctpit_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpctpit_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  
  time_elapsed <- time_elapsed$toc[[1]] - time_elapsed$tic[[1]]
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_swstrpctpit_AA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_swstrpctpit_AA_cvs))

bart_swstrpctpit_AA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.040, 0.042)) +
  scale_x_continuous(breaks = seq(2,20, by = 2))  #12


hyperparam_bart_swstrpctpit_AA <- function(sigdf_swstrpctpit_AA, sigquant_swstrpctpit_AA, k_swstrpctpit_AA, power_swstrpctpit_AA, base_swstrpctpit_AA,row_num){
  set.seed(101);swstrpctpit_AA$fold <- sample(1:12, nrow(swstrpctpit_AA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_swstrpctpit_AA))
  print(paste('Sigquant: ', sigquant_swstrpctpit_AA))
  print(paste('K: ', k_swstrpctpit_AA))
  print(paste('Power: ', power_swstrpctpit_AA))
  print(paste('Base: ', base_swstrpctpit_AA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:12){
    print(paste('Iteration: ', i))
    train_data <- swstrpctpit_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpctpit_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_swstrpctpit_AA,
                   sigquant = sigquant_swstrpctpit_AA,
                   k = k_swstrpctpit_AA,
                   power = power_swstrpctpit_AA,
                   base = base_swstrpctpit_AA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_swstrpctpit_AA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_swstrpctpit_AA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpctpit_AA_0.75_df$sigdf,
                                                          hyperparam_bart_swstrpctpit_AA_0.75_df$sigquant, 
                                                          hyperparam_bart_swstrpctpit_AA_0.75_df$k, 
                                                          hyperparam_bart_swstrpctpit_AA_0.75_df$power,
                                                          hyperparam_bart_swstrpctpit_AA_0.75_df$base,
                                                          hyperparam_bart_swstrpctpit_AA_0.75_df$row_num), hyperparam_bart_swstrpctpit_AA, .progress = TRUE)

hyperparam_bart_swstrpctpit_AA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_swstrpctpit_AA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpctpit_AA_0.9_df$sigdf,
                                                         hyperparam_bart_swstrpctpit_AA_0.9_df$sigquant, 
                                                         hyperparam_bart_swstrpctpit_AA_0.9_df$k, 
                                                         hyperparam_bart_swstrpctpit_AA_0.9_df$power,
                                                         hyperparam_bart_swstrpctpit_AA_0.9_df$base,
                                                         hyperparam_bart_swstrpctpit_AA_0.9_df$row_num), hyperparam_bart_swstrpctpit_AA, .progress = TRUE)

hyperparam_bart_swstrpctpit_AA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_swstrpctpit_AA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpctpit_AA_0.99_df$sigdf,
                                                          hyperparam_bart_swstrpctpit_AA_0.99_df$sigquant, 
                                                          hyperparam_bart_swstrpctpit_AA_0.99_df$k, 
                                                          hyperparam_bart_swstrpctpit_AA_0.99_df$power,
                                                          hyperparam_bart_swstrpctpit_AA_0.99_df$base,
                                                          hyperparam_bart_swstrpctpit_AA_0.99_df$row_num), hyperparam_bart_swstrpctpit_AA, .progress = TRUE)



hyperparam_bart_swstrpctpit_AA_df <- bind_rows(hyperparam_bart_swstrpctpit_AA_0.75_df, hyperparam_bart_swstrpctpit_AA_0.9_df, hyperparam_bart_swstrpctpit_AA_0.99_df)

hyperparam_bart_swstrpctpit_AA_df <- hyperparam_bart_swstrpctpit_AA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_swstrpctpit_AA <- hyperparam_bart_swstrpctpit_AA_df$sigdf) #3
(sigquant_swstrpctpit_AA <- hyperparam_bart_swstrpctpit_AA_df$sigquant) #0.9
(k_swstrpctpit_AA <- hyperparam_bart_swstrpctpit_AA_df$k) # 1
(power_swstrpctpit_AA <- hyperparam_bart_swstrpctpit_AA_df$power) # 1
(base_swstrpctpit_AA <- hyperparam_bart_swstrpctpit_AA_df$base) # 0.95

hyperparam_bart_swstrpctpit_AA_df$rmse/sd(swstrpctpit_AA$SwStr_pct_AA) #0.723 #BART Better

hyperparam_bart_swstrpctpit_AA_trees <- function(trees_swstrpctpit_AA){
  set.seed(101);swstrpctpit_AA$fold <- sample(1:12, nrow(swstrpctpit_AA), replace = TRUE)
  
  print(paste('Trees: ', trees_swstrpctpit_AA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:12){
    print(paste('Iteration: ', i))
    train_data <- swstrpctpit_AA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpctpit_AA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_AA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_AA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_swstrpctpit_AA,
                   sigquant = sigquant_swstrpctpit_AA,
                   k = k_swstrpctpit_AA,
                   power = power_swstrpctpit_AA,
                   base = base_swstrpctpit_AA,
                   n.trees = trees_swstrpctpit_AA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_AA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)/sd(swstrpctpit_AA$SwStr_pct_AA)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_swstrpctpit_AA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_swstrpctpit_AA_trees, .progress = TRUE))


bart_swstrpctpit_AA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #1200

(swstrpctpit_AA_error_ratio <- (bart_swstrpctpit_AA_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(swstrpctpit_AA$SwStr_pct_AA)) #0.880


### Final Model SwStr% AA Pit ####

swstrpctpit_AA_mod <- bart2(SwStr_pct_AA ~ ., 
               data = swstrpctpit_AA,
               sigdf = sigdf_swstrpctpit_AA,
               sigquant = sigquant_swstrpctpit_AA,
               k = k_swstrpctpit_AA,
               power = power_swstrpctpit_AA,
               base = base_swstrpctpit_AA,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(swstrpctpit_AA$SwStr_pct_AA - residuals(swstrpctpit_AA_mod))


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

#finding the optimal number of CVs
bart_swstrpctpit_AAA_cvs <- function(cv){
  set.seed(101);swstrpctpit_AAA$fold <- sample(1:cv, nrow(swstrpctpit_AAA), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  tic()
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- swstrpctpit_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpctpit_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   n.burn = 300,
                   n.samples = 300,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }; time_elapsed <- toc()
  print(mean(rmse_val)) 
  
  return(mean(rmse_val))
  
  
}

bart_swstrpctpit_AAA_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_swstrpctpit_AAA_cvs))

bart_swstrpctpit_AAA_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0.042, 0.045)) +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #8


hyperparam_bart_swstrpctpit_AAA <- function(sigdf_swstrpctpit_AAA, sigquant_swstrpctpit_AAA, k_swstrpctpit_AAA, power_swstrpctpit_AAA, base_swstrpctpit_AAA, row_num){
  set.seed(101);swstrpctpit_AAA$fold <- sample(1:8, nrow(swstrpctpit_AAA), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_swstrpctpit_AAA))
  print(paste('Sigquant: ', sigquant_swstrpctpit_AAA))
  print(paste('K: ', k_swstrpctpit_AAA))
  print(paste('Power: ', power_swstrpctpit_AAA))
  print(paste('Base: ', base_swstrpctpit_AAA))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:8){
    print(paste('Iteration: ', i))
    train_data <- swstrpctpit_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpctpit_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_swstrpctpit_AAA,
                   sigquant = sigquant_swstrpctpit_AAA,
                   k = k_swstrpctpit_AAA,
                   base = base_swstrpctpit_AAA,
                   power = power_swstrpctpit_AAA,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_swstrpctpit_AAA_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_swstrpctpit_AAA_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpctpit_AAA_0.75_df$sigdf,
                                                           hyperparam_bart_swstrpctpit_AAA_0.75_df$sigquant, 
                                                           hyperparam_bart_swstrpctpit_AAA_0.75_df$k, 
                                                           hyperparam_bart_swstrpctpit_AAA_0.75_df$power,
                                                           hyperparam_bart_swstrpctpit_AAA_0.75_df$base, 
                                                           hyperparam_bart_swstrpctpit_AAA_0.75_df$row_num), hyperparam_bart_swstrpctpit_AAA, .progress = TRUE)

hyperparam_bart_swstrpctpit_AAA_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_swstrpctpit_AAA_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpctpit_AAA_0.9_df$sigdf,
                                                          hyperparam_bart_swstrpctpit_AAA_0.9_df$sigquant, 
                                                          hyperparam_bart_swstrpctpit_AAA_0.9_df$k, 
                                                          hyperparam_bart_swstrpctpit_AAA_0.9_df$power,
                                                          hyperparam_bart_swstrpctpit_AAA_0.9_df$base,
                                                          hyperparam_bart_swstrpctpit_AAA_0.9_df$row_num), hyperparam_bart_swstrpctpit_AAA, .progress = TRUE)

hyperparam_bart_swstrpctpit_AAA_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_swstrpctpit_AAA_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpctpit_AAA_0.99_df$sigdf,
                                                           hyperparam_bart_swstrpctpit_AAA_0.99_df$sigquant, 
                                                           hyperparam_bart_swstrpctpit_AAA_0.99_df$k, 
                                                           hyperparam_bart_swstrpctpit_AAA_0.99_df$power,
                                                           hyperparam_bart_swstrpctpit_AAA_0.99_df$base,
                                                           hyperparam_bart_swstrpctpit_AAA_0.99_df$row_num), hyperparam_bart_swstrpctpit_AAA, .progress = TRUE)



hyperparam_bart_swstrpctpit_AAA_df <- bind_rows(hyperparam_bart_swstrpctpit_AAA_0.75_df, hyperparam_bart_swstrpctpit_AAA_0.9_df, hyperparam_bart_swstrpctpit_AAA_0.99_df)

hyperparam_bart_swstrpctpit_AAA_df <- hyperparam_bart_swstrpctpit_AAA_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_swstrpctpit_AAA <- hyperparam_bart_swstrpctpit_AAA_df$sigdf) #3
(sigquant_swstrpctpit_AAA <- hyperparam_bart_swstrpctpit_AAA_df$sigquant) #0.90
(k_swstrpctpit_AAA <- hyperparam_bart_swstrpctpit_AAA_df$k) #1
(power_swstrpctpit_AAA <- hyperparam_bart_swstrpctpit_AAA_df$power) #1
(base_swstrpctpit_AAA <- hyperparam_bart_swstrpctpit_AAA_df$base) #0.95

hyperparam_bart_swstrpctpit_AAA_df$rmse/sd(swstrpctpit_AAA$SwStr_pct_AAA) #0.501 #BART Better

hyperparam_bart_swstrpctpit_AAA_trees <- function(trees_swstrpctpit_AAA){
  set.seed(101);swstrpctpit_AAA$fold <- sample(1:8, nrow(swstrpctpit_AAA), replace = TRUE)
  
  print(paste('Trees: ', trees_swstrpctpit_AAA))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:8){
    print(paste('Iteration: ', i))
    train_data <- swstrpctpit_AAA %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpctpit_AAA %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_AAA,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_AAA ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_swstrpctpit_AAA,
                   sigquant = sigquant_swstrpctpit_AAA,
                   k = k_swstrpctpit_AAA,
                   power = power_swstrpctpit_AAA,
                   base = base_swstrpctpit_AAA,
                   n.trees = trees_swstrpctpit_AAA,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )

    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_AAA, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)/sd(swstrpctpit_AAA$SwStr_pct_AAA)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_swstrpctpit_AAA_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_swstrpctpit_AAA_trees, .progress = TRUE))


bart_swstrpctpit_AAA_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #200

(swstrpctpit_AAA_error_ratio <- (bart_swstrpctpit_AAA_trees_df %>% filter(trees == 200) %>% pull(rmse))/sd(swstrpctpit_AAA$SwStr_pct_AAA)) #0.816 #BART Better

### Final Model SwStr% AAA Pit ####

swstrpctpit_AAA_mod <- bart2(SwStr_pct_AAA ~ ., 
               data = swstrpctpit_AAA,
               sigdf = sigdf_swstrpctpit_AAA,
               sigquant = sigquant_swstrpctpit_AAA,
               k = k_swstrpctpit_AAA,
               power = power_swstrpctpit_AAA,
               base = base_swstrpctpit_AAA,
               n.trees = 200,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)

summary(swstrpctpit_AAA$SwStr_pct_AAA - residuals(swstrpctpit_AAA_mod))

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

#finding the optimal number of CVs
bart_swstrpctpit_mlb_cvs <- function(cv){
  set.seed(101);swstrpctpit_mlb$fold <- sample(1:cv, nrow(swstrpctpit_mlb), replace = TRUE)
  
  print(paste('CV: ', cv))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:cv){
    print(paste('Iteration: ', i))
    train_data <- swstrpctpit_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpctpit_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   n.trees = 200,
                   n.threads = parallel::detectCores()-1,
                   n.chains = parallel::detectCores()-1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_swstrpctpit_mlb_cv_df <- tibble(
  cv = seq(2,20,by = 2),
) %>% 
  rowwise() %>% 
  mutate(rmse = map_dbl(cv, bart_swstrpctpit_mlb_cvs))

bart_swstrpctpit_mlb_cv_df %>% 
  ggplot(aes(cv, rmse)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme_classic() +
  scale_x_continuous(breaks = seq(2,20, by = 2)) #10


hyperparam_bart_swstrpctpit_mlb <- function(sigdf_swstrpctpit_mlb, sigquant_swstrpctpit_mlb, k_swstrpctpit_mlb, power_swstrpctpit_mlb, base_swstrpctpit_mlb,row_num){
  set.seed(101);swstrpctpit_mlb$fold <- sample(1:10, nrow(swstrpctpit_mlb), replace = TRUE)
  
  print(paste('Sigdf: ', sigdf_swstrpctpit_mlb))
  print(paste('Sigquant: ', sigquant_swstrpctpit_mlb))
  print(paste('K: ', k_swstrpctpit_mlb))
  print(paste('Power: ', power_swstrpctpit_mlb))
  print(paste('Base: ', base_swstrpctpit_mlb))
  print(paste('Row Number: ', row_num))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- swstrpctpit_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpctpit_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_swstrpctpit_mlb,
                   sigquant = sigquant_swstrpctpit_mlb,
                   k = k_swstrpctpit_mlb,
                   power = power_swstrpctpit_mlb,
                   base = base_swstrpctpit_mlb,
                   n.trees = 200,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, # don't want nthreads to be too high as it could affect predictions 
                   # according to the function help page
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

#running hyperparameters separately. The console crashses otherwise
hyperparam_bart_swstrpctpit_mlb_0.75_df <- expand_grid(
  sigdf = c(10),
  sigquant = c(0.75),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_swstrpctpit_mlb_0.75_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpctpit_mlb_0.75_df$sigdf,
                                                           hyperparam_bart_swstrpctpit_mlb_0.75_df$sigquant, 
                                                           hyperparam_bart_swstrpctpit_mlb_0.75_df$k, 
                                                           hyperparam_bart_swstrpctpit_mlb_0.75_df$power,
                                                           hyperparam_bart_swstrpctpit_mlb_0.75_df$base,
                                                           hyperparam_bart_swstrpctpit_mlb_0.75_df$row_num), hyperparam_bart_swstrpctpit_mlb, .progress = TRUE)

hyperparam_bart_swstrpctpit_mlb_0.9_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.9),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())

hyperparam_bart_swstrpctpit_mlb_0.9_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpctpit_mlb_0.9_df$sigdf,
                                                          hyperparam_bart_swstrpctpit_mlb_0.9_df$sigquant, 
                                                          hyperparam_bart_swstrpctpit_mlb_0.9_df$k, 
                                                          hyperparam_bart_swstrpctpit_mlb_0.9_df$power,
                                                          hyperparam_bart_swstrpctpit_mlb_0.9_df$base,
                                                          hyperparam_bart_swstrpctpit_mlb_0.9_df$row_num), hyperparam_bart_swstrpctpit_mlb, .progress = TRUE)

hyperparam_bart_swstrpctpit_mlb_0.99_df <- expand_grid(
  sigdf = c(3),
  sigquant = c(0.99),
  k = c(1,2,3,4),
  power = c(1,2,3,4),
  base = c(0.5,0.75,0.95)
) %>% 
  mutate(row_num = row_number())


hyperparam_bart_swstrpctpit_mlb_0.99_df$rmse <- pmap_dbl(list(hyperparam_bart_swstrpctpit_mlb_0.99_df$sigdf,
                                                           hyperparam_bart_swstrpctpit_mlb_0.99_df$sigquant, 
                                                           hyperparam_bart_swstrpctpit_mlb_0.99_df$k, 
                                                           hyperparam_bart_swstrpctpit_mlb_0.99_df$power,
                                                           hyperparam_bart_swstrpctpit_mlb_0.99_df$base,
                                                           hyperparam_bart_swstrpctpit_mlb_0.99_df$row_num), hyperparam_bart_swstrpctpit_mlb, .progress = TRUE)



hyperparam_bart_swstrpctpit_mlb_df <- bind_rows(hyperparam_bart_swstrpctpit_mlb_0.75_df, hyperparam_bart_swstrpctpit_mlb_0.9_df, hyperparam_bart_swstrpctpit_mlb_0.99_df)

hyperparam_bart_swstrpctpit_mlb_df <- hyperparam_bart_swstrpctpit_mlb_df %>% 
  slice_min(rmse, n = 1) %>% 
  slice(1)

(sigdf_swstrpctpit_mlb <- hyperparam_bart_swstrpctpit_mlb_df$sigdf) #10
(sigquant_swstrpctpit_mlb <- hyperparam_bart_swstrpctpit_mlb_df$sigquant) #0.75
(k_swstrpctpit_mlb <- hyperparam_bart_swstrpctpit_mlb_df$k) #1
(power_swstrpctpit_mlb <- hyperparam_bart_swstrpctpit_mlb_df$power) #1
(base_swstrpctpit_mlb <- hyperparam_bart_swstrpctpit_mlb_df$base) #0.95

hyperparam_bart_swstrpctpit_mlb(10,0.75,1,1,0.95,1)/sd(swstrpctpit_mlb$SwStr_pct_MLB) #0.680 #BART Better


hyperparam_bart_swstrpctpit_mlb_trees <- function(trees_swstrpctpit_mlb){
  set.seed(101);swstrpctpit_mlb$fold <- sample(1:10, nrow(swstrpctpit_mlb), replace = TRUE)
  
  print(paste('Trees: ', trees_swstrpctpit_mlb))
  
  rmse_val <- numeric();set.seed(101);for (i in 1:10){
    print(paste('Iteration: ', i))
    train_data <- swstrpctpit_mlb %>% 
      filter(fold != i) %>% 
      select(-fold)
    
    
    
    test_data <- swstrpctpit_mlb %>%
      filter(fold == i)
    
    test_x <- test_data %>% select(-SwStr_pct_MLB,-fold)
    
    test_matrix <- as.matrix(test_x)
    
    model <- bart2(SwStr_pct_MLB ~ ., 
                   data = train_data, 
                   test = test_matrix,
                   sigdf = sigdf_swstrpctpit_mlb,
                   sigquant = sigquant_swstrpctpit_mlb,
                   k = k_swstrpctpit_mlb,
                   power = power_swstrpctpit_mlb,
                   base = base_swstrpctpit_mlb,
                   n.trees = trees_swstrpctpit_mlb,
                   n.samples = 300,
                   n.burn = 300,
                   n.threads = parallel::detectCores()-1, 
                   n.chains = parallel::detectCores() -1,
                   seed = 101, # ensures reproducability
                   keepTrees = TRUE, # needed for prediction,
                   printEvery = 1000,
                   verbose = FALSE # give more information about model building process
    )
    # outputs predictions which includes 100 samples from the posterior distribution for each 
    #observation
    preds <- suppressWarnings(predict(model, test_matrix))
    predictions <- numeric()
    # for loop finds the mean of the posterior samples for each observation 
    # and saves it to the predictions vector.
    for (j in 1:nrow(test_x)){
      predictions <- c(predictions, mean(preds[,j]))
    }
    #print(predictions)
    new_rmse <- yardstick::rmse_vec(test_data$SwStr_pct_MLB, predictions)
    rmse_val <- c(rmse_val, new_rmse)
  }
  print(mean(rmse_val)) 
  print(sd(rmse_val)) 
  return(mean(rmse_val))
  
  
}

bart_swstrpctpit_mlb_trees_df <- tibble(
  trees = seq(1200, 200, by = -200)
) %>% 
  mutate(rmse = map_dbl(trees, hyperparam_bart_swstrpctpit_mlb_trees, .progress = TRUE))


bart_swstrpctpit_mlb_trees_df %>% 
  ggplot(aes(trees, rmse)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme_bw() #400

(swstrpctpit_mlb_error_ratio <- (bart_swstrpctpit_mlb_trees_df %>% filter(trees == 400) %>% pull(rmse))/sd(swstrpctpit_mlb$SwStr_pct_MLB)) #0.837


### Final Model SwStr% MLB Pit ####

swstrpctpit_mlb_mod <- bart2(SwStr_pct_MLB ~ ., 
               data = swstrpctpit_mlb,
               sigdf = sigdf_swstrpctpit_mlb,
               sigquant = sigquant_swstrpctpit_mlb,
               k = k_swstrpctpit_mlb,
               power = power_swstrpctpit_mlb,
               base = base_swstrpctpit_mlb,
               n.trees = 400,
               n.samples = 300,
               n.burn = 300,
               n.threads = parallel::detectCores()-1, 
               n.chains = parallel::detectCores() -1,
               seed = 101, # ensures reproducability
               keepTrees = TRUE, # needed for prediction,
               printEvery = 1000,
               verbose = FALSE # give more information about model building process
)