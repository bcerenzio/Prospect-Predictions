library(dbarts)
library(tidyverse)
library(parallel)
library(tictoc)
library(corrr)
library(progress)
library(pdp)

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


summary(swstrpctpit_mlb$SwStr_pct_MLB - residuals(swstrpctpit_mlb_mod))
