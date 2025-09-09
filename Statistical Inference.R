library(tidyverse)
library(car)
library(mgcv)
library(mgcViz)
library(tidygam)
library(gridExtra)


#### Made MLB Hitting Logit & Probit Level A ####

#finding each player's first minor league season (In A Ball)
first_minor_league_season_hitting_A <- minor_league_hitting %>% 
  filter(Level == 'A') %>%
  group_by(PlayerId, Name) %>% 
  reframe(PlayerId, Name, first_milb_season = min(Season)) %>% 
  distinct()

#finding median age they make the MLB
minor_league_hitting_A_median <- median(minor_league_hitting_A %>% 
                                          filter(made_mlb == 1) %>% 
                                          pull(years_to_mlb))

# logit
made_mlb_A_batter_mod <- glm(
  made_mlb ~ avg_100_rank + team_rank_ba + PA + Age + wRC_plus + 
    BB_pct + K_pct + SwStr_pct + GB_per_FB, data = minor_league_hitting_A %>% 
    mutate(avg_100_rank = (rank_ba + rank_mlb)/2) %>% 
    left_join(first_minor_league_season_hitting_A, by = c('PlayerId', 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_hitting_A_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_A_batter_mod)# best
vif(made_mlb_A_batter_mod)

minor_league_hitting_A %>% 
  mutate(avg_100_rank = (rank_ba + rank_mlb)/2) %>% 
  left_join(first_minor_league_season_hitting_A, by = c('PlayerId', 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_hitting_A_median) %>% 
  select(avg_100_rank, team_rank_ba, PA, Age, wRC_plus, BB_pct, K_pct, SwStr_pct, 
         GB_per_FB) %>%
  mutate(log_odds = log(made_mlb_A_batter_mod$fitted.values / (1 - made_mlb_A_batter_mod$fitted.values))) %>% 
  pivot_longer(-log_odds) %>%
  ggplot(aes(value, log_odds)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~name, scales = "free_x") # Age possibly needs smoothing

made_mlb_A_batter_gam <- gam(
  made_mlb ~ avg_100_rank + team_rank_ba + PA + s(Age, bs = 'tp') + wRC_plus + 
    BB_pct + K_pct + SwStr_pct + GB_per_FB, data = minor_league_hitting_A %>% 
    mutate(avg_100_rank = (rank_ba + rank_mlb)/2) %>% 
    left_join(first_minor_league_season_hitting_A, by = c('PlayerId', 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_hitting_A_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_A_batter_gam)
gam.check(made_mlb_A_batter_gam)

age_smoothing <- tidygam::predict_gam(made_mlb_A_batter_gam,
                                      values = list(
                                        avg_100_rank = mean(minor_league_hitting_A %>% 
                                                              mutate(avg_100_rank = (rank_ba + rank_mlb)/2) %>% 
                                                              pull(avg_100_rank)),
                                        team_rank_ba = mean(minor_league_hitting_A$team_rank_ba),
                                        PA = mean(minor_league_hitting_A$PA),
                                        Age = seq(18,40, by = 1),
                                        wRC_plus = mean(minor_league_hitting_A$wRC_plus),
                                        BB_pct = mean(minor_league_hitting_A$BB_pct),
                                        K_pct = mean(minor_league_hitting_A$K_pct),
                                        SwStr_pct = mean(minor_league_hitting_A$SwStr_pct),
                                        GB_per_FB = mean(minor_league_hitting_A$GB_per_FB)
                                      ))




age_smoothing %>% 
  mutate(mlb_prob = exp(made_mlb)/(1+exp(made_mlb))) %>% 
  ggplot(aes(Age, mlb_prob)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "tp")) +
  scale_x_continuous(breaks = seq(18, 40, by = 2)) +
  labs(title = 'MLB Probability By Age In Single-A (Hitters)',
       y = 'MLB Probability',
       x = 'Age') +
  theme_classic()


#### Made MLB Hitting Logit & Probit Level A+ ####
#finding each player's first minor league season (In A+ Ball)
first_minor_league_season_hitting_Aplus <- minor_league_hitting %>% 
  filter(Level == 'A+') %>% 
  group_by(PlayerId, Name) %>% 
  reframe(PlayerId, Name, first_milb_season = min(Season)) %>% 
  distinct()

#finding median age they make the MLB (A+)
minor_league_hitting_Aplus_median <- median(minor_league_hitting_Aplus %>% 
                                          filter(made_mlb == 1) %>% 
                                          pull(years_to_mlb))

# logit
made_mlb_Aplus_batter_mod <- glm(
  made_mlb ~ avg_100_rank + team_rank_ba_Aplus + Age_Aplus + wRC_plus_Aplus + PA_Aplus +
    BB_pct_Aplus + K_pct_Aplus + SwStr_pct_Aplus + GB_per_FB_Aplus, data = minor_league_hitting_Aplus %>% 
    mutate(avg_100_rank = (rank_ba_Aplus + rank_mlb_Aplus)/2) %>% 
    left_join(first_minor_league_season_hitting_Aplus, by = c('PlayerId', 'Name_Aplus' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_hitting_Aplus_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_Aplus_batter_mod)

vif(made_mlb_Aplus_batter_mod)

minor_league_hitting_Aplus %>%
  mutate(avg_100_rank = (rank_ba_Aplus + rank_mlb_Aplus)/2) %>% 
  left_join(first_minor_league_season_hitting_Aplus, by = c('PlayerId', 'Name_Aplus' = 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_hitting_Aplus_median) %>% 
  select(avg_100_rank, team_rank_ba_Aplus, PA_Aplus, Age_Aplus, wRC_plus_Aplus, BB_pct_Aplus, K_pct_Aplus, SwStr_pct_Aplus, 
         LD_pct_Aplus, GB_per_FB_Aplus) %>%
  mutate(log_odds = log(made_mlb_Aplus_batter_mod$fitted.values / (1 - made_mlb_Aplus_batter_mod$fitted.values))) %>% 
  pivot_longer(-log_odds) %>%
  ggplot(aes(value, log_odds)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~name, scales = "free_x")

made_mlb_Aplus_batter_gam <- gam(
  made_mlb ~ avg_100_rank + team_rank_ba_Aplus + s(Age_Aplus, bs = 'tp') + wRC_plus_Aplus + PA_Aplus +
    BB_pct_Aplus + K_pct_Aplus + SwStr_pct_Aplus, data = minor_league_hitting_Aplus %>% 
    mutate(avg_100_rank = (rank_ba_Aplus + rank_mlb_Aplus)/2) %>% 
    left_join(first_minor_league_season_hitting_Aplus, by = c('PlayerId', 'Name_Aplus' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_hitting_Aplus_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_Aplus_batter_gam)

age_smoothing_Aplus <- tidygam::predict_gam(made_mlb_Aplus_batter_gam,
                                      values = list(
                                        avg_100_rank = mean(minor_league_hitting_Aplus %>% 
                                                              mutate(avg_100_rank = (rank_ba_Aplus + rank_mlb_Aplus)/2) %>% 
                                                              pull(avg_100_rank)),
                                        team_rank_ba_Aplus = mean(minor_league_hitting_Aplus$team_rank_ba_Aplus),
                                        PA = mean(minor_league_hitting_Aplus$PA_Aplus),
                                        Age_Aplus = seq(18,40, by = 1),
                                        wRC_plus_Aplus = mean(minor_league_hitting_Aplus$wRC_plus_Aplus),
                                        BB_pct_Aplus = mean(minor_league_hitting_Aplus$BB_pct_Aplus),
                                        K_pct_Aplus = mean(minor_league_hitting_Aplus$K_pct_Aplus),
                                        SwStr_pct_Aplus = mean(minor_league_hitting_Aplus$SwStr_pct_Aplus),
                                        GB_per_FB_Aplus = mean(minor_league_hitting_Aplus$GB_per_FB_Aplus)
                                      ))




age_smoothing_Aplus %>% 
  mutate(mlb_prob = exp(made_mlb)/(1+exp(made_mlb))) %>% 
  ggplot(aes(Age_Aplus, mlb_prob)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "tp")) +
  scale_x_continuous(breaks = seq(18, 40, by = 2)) +
  labs(title = 'MLB Probability By Age in High-A (Hitters)',
       y = 'MLB Probability',
       x = 'Age') +
  theme_classic()



#### Made MLB Hitting Logit & Probit Level AA ####
#finding each player's first minor league season (In AA Ball)
first_minor_league_season_hitting_AA <- minor_league_hitting %>% 
  filter(Level == 'AA') %>% 
  group_by(PlayerId, Name) %>% 
  reframe(PlayerId, Name, first_milb_season = min(Season)) %>% 
  distinct()

#finding median age they make the MLB
minor_league_hitting_AA_median <- median(minor_league_hitting_AA %>% 
                                          filter(made_mlb == 1) %>% 
                                          pull(years_to_mlb))

# logit
made_mlb_AA_batter_mod <- glm(
  made_mlb ~ avg_100_rank + team_rank_ba_AA + Age_AA + wRC_plus_AA + PA_AA +
    BB_pct_AA + K_pct_AA + SwStr_pct_AA + + GB_per_FB_AA, 
  data = minor_league_hitting_AA %>% 
    mutate(avg_100_rank = (rank_ba_AA + rank_mlb_AA)/2) %>% 
    left_join(first_minor_league_season_hitting_AA, by = c('PlayerId', 'Name_AA' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_hitting_AA_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_AA_batter_mod)
vif(made_mlb_AA_batter_mod)

minor_league_hitting_AA %>% 
  mutate(avg_100_rank = (rank_ba_AA + rank_mlb_AA)/2) %>% 
  left_join(first_minor_league_season_hitting_AA, by = c('PlayerId', 'Name_AA' = 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_hitting_AA_median) %>% 
  select(avg_100_rank, team_rank_ba_AA, PA_AA, Age_AA, wRC_plus_AA, BB_pct_AA, K_pct_AA, SwStr_pct_AA, 
         GB_per_FB_AA) %>%
  mutate(log_odds = log(made_mlb_AA_batter_mod$fitted.values / (1 - made_mlb_AA_batter_mod$fitted.values))) %>% 
  pivot_longer(-log_odds) %>%
  ggplot(aes(value, log_odds)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~name, scales = "free_x") # Age possibly needs smoothing

made_mlb_AA_batter_gam <- gam(
  made_mlb ~ avg_100_rank + team_rank_ba_AA + PA_AA + s(Age_AA, bs = 'tp') + wRC_plus_AA + 
    BB_pct_AA + K_pct_AA + SwStr_pct_AA + GB_per_FB_AA, data = minor_league_hitting_AA %>% 
    mutate(avg_100_rank = (rank_ba_AA + rank_mlb_AA)/2) %>% 
    left_join(first_minor_league_season_hitting_AA, by = c('PlayerId', 'Name_AA' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_hitting_AA_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_AA_batter_gam)
gam.check(made_mlb_A_batter_gam)

age_smoothing_AA <- tidygam::predict_gam(made_mlb_AA_batter_gam,
                                      values = list(
                                        avg_100_rank = mean(minor_league_hitting_AA %>% 
                                                              mutate(avg_100_rank = (rank_ba_AA + rank_mlb_AA)/2) %>% 
                                                              pull(avg_100_rank)),
                                        team_rank_ba_AA = mean(minor_league_hitting_AA$team_rank_ba_AA),
                                        PA_AA = mean(minor_league_hitting_AA$PA_AA),
                                        Age_AA = 18:40,
                                        wRC_plus_AA = mean(minor_league_hitting_AA$wRC_plus_AA),
                                        BB_pct_AA = mean(minor_league_hitting_AA$BB_pct_AA),
                                        K_pct_AA = mean(minor_league_hitting_AA$K_pct_AA),
                                        SwStr_pct_AA = mean(minor_league_hitting_AA$SwStr_pct_AA),
                                        GB_per_FB_AA = mean(minor_league_hitting_AA$GB_per_FB_AA)
                                      ))

age_smoothing_AA %>% 
  mutate(mlb_prob = exp(made_mlb)/(1+exp(made_mlb))) %>% 
  ggplot(aes(Age_AA, mlb_prob)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "tp")) +
  scale_x_continuous(breaks = seq(18, 40, by = 2)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(title = 'MLB Probability By Age In AA (Hitters)',
       y = 'MLB Probability',
       x = 'Age') +
  theme_classic()


#### Made MLB Hitting Logit & Probit Level AAA ####

first_minor_league_season_hitting_AAA <- minor_league_hitting %>% 
  filter(Level == 'AAA') %>% 
  group_by(PlayerId, Name) %>% 
  reframe(PlayerId, Name, first_milb_season = min(Season)) %>% 
  distinct()

#finding median age they make the MLB
minor_league_hitting_AAA_median <- median(minor_league_hitting_AAA %>% 
                                           filter(made_mlb == 1) %>% 
                                           pull(years_to_mlb))

# logit
made_mlb_AAA_batter_mod <- glm(
  made_mlb ~ avg_100_rank + team_rank_ba_AAA + Age_AAA + wRC_plus_AAA + PA_AAA +
    BB_pct_AAA + K_pct_AAA + SwStr_pct_AAA + GB_per_FB_AAA, 
  data = minor_league_hitting_AAA %>% 
    mutate(avg_100_rank = (rank_ba_AAA + rank_mlb_AAA)/2)%>% 
    left_join(first_minor_league_season_hitting_AAA, by = c('PlayerId', 'Name_AAA' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_hitting_AAA_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_AAA_batter_mod)
vif(made_mlb_AAA_batter_mod)

minor_league_hitting_AAA %>% 
  mutate(avg_100_rank = (rank_ba_AAA + rank_mlb_AAA)/2) %>% 
  left_join(first_minor_league_season_hitting_AAA, by = c('PlayerId', 'Name_AAA' = 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_hitting_AAA_median) %>% 
  select(avg_100_rank, team_rank_ba_AAA, PA_AAA, Age_AAA, wRC_plus_AAA, BB_pct_AAA, K_pct_AAA, SwStr_pct_AAA, 
         GB_per_FB_AAA) %>%
  mutate(log_odds = log(made_mlb_AAA_batter_mod$fitted.values / (1 - made_mlb_AAA_batter_mod$fitted.values))) %>% 
  pivot_longer(-log_odds) %>%
  ggplot(aes(value, log_odds)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~name, scales = "free_x") # Age possibly needs smoothing

made_mlb_AAA_batter_gam <- gam(
  made_mlb ~ avg_100_rank + team_rank_ba_AAA + PA_AAA + s(Age_AAA, bs = 'tp') + wRC_plus_AAA + 
    BB_pct_AAA + K_pct_AAA + SwStr_pct_AAA + poly(GB_per_FB_AAA,2), data = minor_league_hitting_AAA %>% 
    mutate(avg_100_rank = (rank_ba_AAA + rank_mlb_AAA)/2) %>% 
    left_join(first_minor_league_season_hitting_AAA, by = c('PlayerId', 'Name_AAA' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_hitting_AAA_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_AA_batter_gam)
gam.check(made_mlb_A_batter_gam)

age_smoothing_AAA <- tidygam::predict_gam(made_mlb_AAA_batter_gam,
                                         values = list(
                                           avg_100_rank = mean(minor_league_hitting_AAA %>% 
                                                                 mutate(avg_100_rank = (rank_ba_AAA + rank_mlb_AAA)/2) %>% 
                                                                 pull(avg_100_rank)),
                                           team_rank_ba_AAA = mean(minor_league_hitting_AAA$team_rank_ba_AAA),
                                           PA_AAA = mean(minor_league_hitting_AAA$PA_AAA),
                                           Age_AAA = 18:40,
                                           wRC_plus_AAA = mean(minor_league_hitting_AAA$wRC_plus_AAA),
                                           BB_pct_AAA = mean(minor_league_hitting_AAA$BB_pct_AAA),
                                           K_pct_AAA = mean(minor_league_hitting_AAA$K_pct_AAA),
                                           SwStr_pct_AAA = mean(minor_league_hitting_AAA$SwStr_pct_AAA),
                                           GB_per_FB_AAA = mean(minor_league_hitting_AAA$GB_per_FB_AAA)
                                         ))

age_smoothing_AAA %>% 
  mutate(mlb_prob = exp(made_mlb)/(1+exp(made_mlb))) %>% 
  ggplot(aes(Age_AAA, mlb_prob)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "tp")) +
  scale_x_continuous(breaks = seq(18, 40, by = 2)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(title = 'MLB Probability By Age In AAA (Hitters)',
       y = 'MLB Probability',
       x = 'Age') +
  theme_classic()

### GAM Graphic ####
age_smoothing_hit <- bind_rows(
  age_smoothing %>% mutate(Level = 'A'),
  age_smoothing_Aplus %>% mutate(Level = 'A+') %>% 
    rename_with(~str_remove(.,'_Aplus$'), ends_with('_Aplus')),
  age_smoothing_AA %>% mutate(Level = 'AA') %>% 
    rename_with(~str_remove(.,'_AA$'), ends_with('_AA')),
  age_smoothing_AAA %>% mutate(Level = 'AAA') %>% 
    rename_with(~str_remove(.,'_AAA$'), ends_with('_AAA'))
)

(aging_curve_hit <- age_smoothing_hit %>% 
  mutate(mlb_prob = exp(made_mlb)/(1+exp(made_mlb))) %>% 
  ggplot(aes(Age, mlb_prob, color = Level, group = Level)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "tp")) +
  scale_x_continuous(breaks = seq(18, 40, by = 2)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(title = 'MLB Probability By Age & League (Hitters)',
       y = 'MLB Probability',
       x = 'Age') +
  theme_classic() +
  theme(legend.position = c(0.9,0.7),
        legend.key.size = unit(1,'cm'),
        legend.background = element_rect(color = 'white', fill = 'white')))

#### Made MLB SP Logit & Probit Level A ####

#finding each player's first minor league season (In A Ball)
first_minor_league_season_pit_A <- minor_league_pitching %>% 
  filter(Level == 'A') %>%
  group_by(PlayerId, Name) %>% 
  reframe(PlayerId, Name, first_milb_season = min(Season)) %>% 
  distinct()

#finding median age they make the MLB
minor_league_sp_A_median <- median(minor_league_pitching_A %>% 
                                          filter(made_mlb_sp == 1) %>% 
                                          pull(years_to_mlb))

# logit
made_mlb_A_sp_mod <- glm(
  made_mlb_sp ~ avg_100_rank + team_rank_ba + Age + GS + ERA + 
    BB_pct + K_pct + GB_pct + FIP, data = minor_league_pitching_A %>% 
    mutate(avg_100_rank = (rank_ba + rank_mlb)/2) %>% 
    left_join(first_minor_league_season_pit_A, by = c('PlayerId', 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_sp_A_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_A_sp_mod)
vif(made_mlb_A_sp_mod)

minor_league_pitching_A %>% 
  mutate(avg_100_rank = (rank_ba + rank_mlb)/2) %>% 
  left_join(first_minor_league_season_pit_A, by = c('PlayerId', 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_sp_A_median) %>%  
  select(avg_100_rank, team_rank_ba, GS, Age, ERA, BB_pct, K_pct, GB_pct, 
         FIP) %>%
  mutate(log_odds = log(made_mlb_A_sp_mod$fitted.values / (1 - made_mlb_A_sp_mod$fitted.values))) %>% 
  pivot_longer(-log_odds) %>%
  ggplot(aes(value, log_odds)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~name, scales = "free_x") 

made_mlb_A_sp_gam <- gam(
  made_mlb_sp ~ avg_100_rank + team_rank_ba + s(Age, bs = 'tp') + GS + ERA + 
    BB_pct + K_pct + GB_pct + FIP, data = minor_league_pitching_A %>% 
    mutate(avg_100_rank = (rank_ba + rank_mlb)/2) %>% 
    left_join(first_minor_league_season_pit_A, by = c('PlayerId', 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_sp_A_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_A_sp_gam)
gam.check(made_mlb_A_sp_gam)

age_smoothing_sp_A <- tidygam::predict_gam(made_mlb_A_sp_gam,
                                      values = list(
                                        avg_100_rank = mean(minor_league_pitching_A %>% 
                                                              mutate(avg_100_rank = (rank_ba + rank_mlb)/2) %>% 
                                                              pull(avg_100_rank)),
                                        team_rank_ba = mean(minor_league_pitching_A$team_rank_ba),
                                        GS = mean(minor_league_pitching_A$GS),
                                        Age = 18:40,
                                        ERA = mean(minor_league_pitching_A$ERA),
                                        BB_pct = mean(minor_league_pitching_A$BB_pct),
                                        K_pct = mean(minor_league_pitching_A$K_pct),
                                        GB_pct = mean(minor_league_pitching_A$GB_pct),
                                        FIP = mean(minor_league_pitching_A$FIP)
                                      ))




age_smoothing_sp_A %>% 
  mutate(mlb_prob = exp(made_mlb_sp)/(1+exp(made_mlb_sp))) %>% 
  ggplot(aes(Age, mlb_prob)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "tp")) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  scale_x_continuous(breaks = seq(18, 40, by = 2)) +
  labs(title = 'MLB Probability By Age In Single-A (SP)',
       y = 'MLB Probability',
       x = 'Age') +
  theme_classic()

#### Made MLB SP Logit & Probit Level A+ ####
first_minor_league_season_pit_Aplus <- minor_league_pitching %>% 
  filter(Level == 'A+') %>%
  group_by(PlayerId, Name) %>% 
  reframe(PlayerId, Name, first_milb_season = min(Season)) %>% 
  distinct()

#finding median age they make the MLB
minor_league_sp_Aplus_median <- median(minor_league_pitching_Aplus %>% 
                                     filter(made_mlb_sp == 1) %>% 
                                     pull(years_to_mlb))


# logit
made_mlb_Aplus_sp_mod <- glm(
  made_mlb_sp ~ avg_100_rank + team_rank_ba_Aplus + Age_Aplus + GS_Aplus +
    ERA_Aplus + BB_pct_Aplus + K_pct_Aplus + GB_pct_Aplus +
    FIP_Aplus, data = minor_league_pitching_Aplus %>% 
    mutate(avg_100_rank = (rank_ba_Aplus + rank_mlb_Aplus)/2) %>% 
    left_join(first_minor_league_season_pit_Aplus, by = c('PlayerId', 'Name_Aplus' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_sp_Aplus_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_Aplus_sp_mod)
vif(made_mlb_Aplus_sp_mod)

minor_league_pitching_Aplus %>% 
  mutate(avg_100_rank = (rank_ba_Aplus + rank_mlb_Aplus)/2) %>% 
  left_join(first_minor_league_season_pit_Aplus, by = c('PlayerId', 'Name_Aplus' = 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_sp_Aplus_median) %>%  
  reframe(avg_100_rank, team_rank_ba_Aplus, GS_Aplus, Age_Aplus, ERA_Aplus, BB_pct_Aplus, K_pct_Aplus, GB_pct_Aplus, 
         FIP_Aplus) %>%
  mutate(log_odds = log(made_mlb_Aplus_sp_mod$fitted.values / (1 - made_mlb_Aplus_sp_mod$fitted.values))) %>% 
  pivot_longer(-log_odds) %>%
  ggplot(aes(value, log_odds)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~name, scales = "free_x") #K% logorithmic relationship

made_mlb_Aplus_sp_gam <- gam(
  made_mlb_sp ~ avg_100_rank + team_rank_ba_Aplus + s(Age_Aplus, bs = 'tp', k = 10) + GS_Aplus +
    ERA_Aplus + BB_pct_Aplus + K_pct_Aplus + GB_pct_Aplus +
    FIP_Aplus, data = minor_league_pitching_Aplus %>% 
    mutate(avg_100_rank = (rank_ba_Aplus + rank_mlb_Aplus)/2) %>% 
    left_join(first_minor_league_season_pit_Aplus, by = c('PlayerId', 'Name_Aplus' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_sp_Aplus_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_Aplus_sp_gam)
gam.check(made_mlb_Aplus_sp_gam)

age_smoothing_sp_Aplus <- tidygam::predict_gam(made_mlb_Aplus_sp_gam,
                                           values = list(
                                             avg_100_rank = mean(minor_league_pitching_Aplus %>% 
                                                                   mutate(avg_100_rank = (rank_ba_Aplus + rank_mlb_Aplus)/2) %>% 
                                                                   pull(avg_100_rank)),
                                             team_rank_ba_Aplus = mean(minor_league_pitching_Aplus$team_rank_ba_Aplus),
                                             GS_Aplus = mean(minor_league_pitching_Aplus$GS_Aplus),
                                             Age_Aplus = 18:40,
                                             ERA_Aplus = mean(minor_league_pitching_Aplus$ERA_Aplus),
                                             BB_pct_Aplus = mean(minor_league_pitching_Aplus$BB_pct_Aplus),
                                             K_pct_Aplus = mean(minor_league_pitching_Aplus$K_pct_Aplus),
                                             GB_pct_Aplus = mean(minor_league_pitching_Aplus$GB_pct_Aplus),
                                             FIP_Aplus = mean(minor_league_pitching_Aplus$FIP_Aplus)
                                           ))




age_smoothing_sp_Aplus %>% 
  mutate(mlb_prob = exp(made_mlb_sp)/(1+exp(made_mlb_sp))) %>% 
  ggplot(aes(Age_Aplus, mlb_prob)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "tp")) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  scale_x_continuous(breaks = seq(18, 40, by = 2)) +
  labs(title = 'MLB Probability By Age In A+ (SP)',
       y = 'MLB Probability',
       x = 'Age') +
  theme_classic()



#### Made MLB SP Logit & Probit Level AA ####
#finding each player's first minor league season (In A Ball)
first_minor_league_season_pit_AA <- minor_league_pitching %>% 
  filter(Level == 'AA') %>%
  group_by(PlayerId, Name) %>% 
  reframe(PlayerId, Name, first_milb_season = min(Season)) %>% 
  distinct()

#finding median age they make the MLB
minor_league_sp_AA_median <- median(minor_league_pitching_AA %>% 
                                     filter(made_mlb_sp == 1) %>% 
                                     pull(years_to_mlb))


# logit
made_mlb_AA_sp_mod <- glm(
  made_mlb_sp ~ avg_100_rank + team_rank_ba_AA + Age_AA + GS_AA +
    ERA_AA + BB_pct_AA + K_pct_AA + GB_pct_AA + 
    FIP_AA, data = minor_league_pitching_AA %>% 
    mutate(avg_100_rank = (rank_ba_AA + rank_mlb_AA)/2) %>% 
    left_join(first_minor_league_season_pit_AA, by = c('PlayerId', 'Name_AA' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_sp_AA_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_AA_sp_mod)
vif(made_mlb_AA_sp_mod)

minor_league_pitching_AA %>% 
  mutate(avg_100_rank = (rank_ba_AA + rank_mlb_AA)/2) %>% 
  left_join(first_minor_league_season_pit_AA, by = c('PlayerId', 'Name_AA' = 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_sp_AA_median) %>%  
  reframe(avg_100_rank, team_rank_ba_AA, GS_AA, Age_AA, ERA_AA, BB_pct_AA, K_pct_AA, GB_pct_AA, 
          FIP_AA) %>%
  mutate(log_odds = log(made_mlb_AA_sp_mod$fitted.values / (1 - made_mlb_AA_sp_mod$fitted.values))) %>% 
  pivot_longer(-log_odds) %>%
  ggplot(aes(value, log_odds)) +
  geom_point() +
  geom_smooth(formula = y ~ s(x, bs = 'tp', k = 5)) +
  facet_wrap(~name, scales = "free_x") 

made_mlb_AA_sp_gam <- gam(
  made_mlb_sp ~ avg_100_rank + team_rank_ba_AA + s(Age_AA, bs = 'tp') + GS_AA +
    ERA_AA + BB_pct_AA + K_pct_AA + GB_pct_AA + 
    FIP_AA, data = minor_league_pitching_AA %>% 
    mutate(avg_100_rank = (rank_ba_AA + rank_mlb_AA)/2) %>% 
    left_join(first_minor_league_season_pit_AA, by = c('PlayerId', 'Name_AA' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_sp_AA_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_AA_sp_gam)
gam.check(made_mlb_AA_sp_gam)

age_smoothing_sp_AA <- tidygam::predict_gam(made_mlb_AA_sp_gam,
                                               values = list(
                                                 avg_100_rank = mean(minor_league_pitching_AA %>% 
                                                                       mutate(avg_100_rank = (rank_ba_AA + rank_mlb_AA)/2) %>% 
                                                                       pull(avg_100_rank)),
                                                 team_rank_ba_AA = mean(minor_league_pitching_AA$team_rank_ba_AA),
                                                 GS_AA = mean(minor_league_pitching_AA$GS_AA),
                                                 Age_AA = 18:40,
                                                 ERA_AA = mean(minor_league_pitching_AA$ERA_AA),
                                                 BB_pct_AA = mean(minor_league_pitching_AA$BB_pct_AA),
                                                 K_pct_AA = mean(minor_league_pitching_AA$K_pct_AA),
                                                 GB_pct_AA = mean(minor_league_pitching_AA$GB_pct_AA),
                                                 FIP_AA = mean(minor_league_pitching_AA$FIP_AA)
                                               ))


age_smoothing_sp_AA %>% 
  mutate(mlb_prob = exp(made_mlb_sp)/(1+exp(made_mlb_sp))) %>% 
  ggplot(aes(Age_AA, mlb_prob)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "tp")) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  scale_x_continuous(breaks = seq(18, 40, by = 2)) +
  labs(title = 'MLB Probability By Age In AA (SP)',
       y = 'MLB Probability',
       x = 'Age') +
  theme_classic()

#### Made MLB SP Logit & Probit Level AAA ####
first_minor_league_season_pit_AAA <- minor_league_pitching %>% 
  filter(Level == 'AAA') %>%
  group_by(PlayerId, Name) %>% 
  reframe(PlayerId, Name, first_milb_season = min(Season)) %>% 
  distinct()

#finding median age they make the MLB
minor_league_sp_AAA_median <- median(minor_league_pitching_AAA %>% 
                                      filter(made_mlb_sp == 1) %>% 
                                      pull(years_to_mlb))

# logit
made_mlb_AAA_sp_mod <- glm(
  made_mlb_sp ~ avg_100_rank + team_rank_ba_AAA + Age_AAA + GS_AAA +
    ERA_AAA + BB_pct_AAA + K_pct_AAA + GB_pct_AAA + 
    FIP_AAA, data = minor_league_pitching_AAA %>% 
    mutate(avg_100_rank = (rank_ba_AAA + rank_mlb_AAA)/2) %>% 
    left_join(first_minor_league_season_pit_AAA, by = c('PlayerId', 'Name_AAA' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_sp_AAA_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_AAA_sp_mod) #better model
vif(made_mlb_AAA_sp_mod)

minor_league_pitching_AAA %>% 
  mutate(avg_100_rank = (rank_ba_AAA + rank_mlb_AAA)/2) %>% 
  left_join(first_minor_league_season_pit_AAA, by = c('PlayerId', 'Name_AAA' = 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_sp_AAA_median) %>%  
  reframe(avg_100_rank, team_rank_ba_AAA, GS_AAA, Age_AAA, ERA_AAA, BB_pct_AAA, K_pct_AAA, GB_pct_AAA, 
          FIP_AAA) %>%
  mutate(log_odds = log(made_mlb_AAA_sp_mod$fitted.values / (1 - made_mlb_AAA_sp_mod$fitted.values))) %>% 
  pivot_longer(-log_odds) %>%
  ggplot(aes(value, log_odds)) +
  geom_point() +
  geom_smooth(formula = y ~ s(x, bs = 'tp', k = 5)) +
  facet_wrap(~name, scales = "free_x") 

made_mlb_AAA_sp_gam <- gam(
  made_mlb_sp ~ avg_100_rank + team_rank_ba_AAA + s(Age_AAA, bs = 'tp') + GS_AAA +
    ERA_AAA + BB_pct_AAA + K_pct_AAA + GB_pct_AAA + 
    FIP_AAA, data = minor_league_pitching_AAA %>% 
    mutate(avg_100_rank = (rank_ba_AAA + rank_mlb_AAA)/2) %>% 
    left_join(first_minor_league_season_pit_AAA, by = c('PlayerId', 'Name_AAA' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_sp_AAA_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_AAA_sp_gam)
gam.check(made_mlb_AAA_sp_gam)

age_smoothing_sp_AAA <- tidygam::predict_gam(made_mlb_AAA_sp_gam,
                                            values = list(
                                              avg_100_rank = mean(minor_league_pitching_AAA %>% 
                                                                    mutate(avg_100_rank = (rank_ba_AAA + rank_mlb_AAA)/2) %>% 
                                                                    pull(avg_100_rank)),
                                              team_rank_ba_AAA = mean(minor_league_pitching_AAA$team_rank_ba_AAA),
                                              GS_AAA = mean(minor_league_pitching_AAA$GS_AAA),
                                              Age_AAA = 20:40,
                                              ERA_AAA = mean(minor_league_pitching_AAA$ERA_AAA),
                                              BB_pct_AAA = mean(minor_league_pitching_AAA$BB_pct_AAA),
                                              K_pct_AAA = mean(minor_league_pitching_AAA$K_pct_AAA),
                                              GB_pct_AAA = mean(minor_league_pitching_AAA$GB_pct_AAA),
                                              FIP_AAA = mean(minor_league_pitching_AAA$FIP_AAA)
                                            ))




age_smoothing_sp_AAA %>% 
  mutate(mlb_prob = exp(made_mlb_sp)/(1+exp(made_mlb_sp))) %>% 
  ggplot(aes(Age_AAA, mlb_prob)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "tp")) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  scale_x_continuous(breaks = seq(18, 40, by = 2)) +
  labs(title = 'MLB Probability By Age In AAA (SP)',
       y = 'MLB Probability',
       x = 'Age') +
  theme_classic()

### GAM Graphic ####
age_smoothing_sp <- bind_rows(
  age_smoothing_sp_A %>% mutate(Level = 'A'),
  age_smoothing_sp_Aplus %>% mutate(Level = 'A+') %>% 
    rename_with(~str_remove(.,'_Aplus$'), ends_with('_Aplus')),
  age_smoothing_sp_AA %>% mutate(Level = 'AA') %>% 
    rename_with(~str_remove(.,'_AA$'), ends_with('_AA')),
  age_smoothing_sp_AAA %>% mutate(Level = 'AAA') %>% 
    rename_with(~str_remove(.,'_AAA$'), ends_with('_AAA'))
)

(sp_aging_curve <- age_smoothing_sp %>% 
  mutate(mlb_prob = exp(made_mlb_sp)/(1+exp(made_mlb_sp))) %>% 
  ggplot(aes(Age, mlb_prob, color = Level, group = Level)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "tp")) +
  scale_x_continuous(breaks = seq(18, 40, by = 2)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(title = 'MLB Probability By Age & League (SP)',
       y = 'MLB Probability',
       x = 'Age') +
  theme_classic() +
  theme(legend.position = c(0.9,0.7),
        legend.key.size = unit(1,'cm'),
        legend.background = element_rect(color = 'white', fill = 'white')))

#### Made MLB RP Logit & Probit Level A ####
#finding median age they make the MLB
minor_league_rp_A_median <- median(minor_league_pitching_A %>% 
                                     filter(made_mlb_rp == 1) %>% 
                                     pull(years_to_mlb_rp))

# logit
made_mlb_A_rp_mod <- glm(
  made_mlb_rp ~ avg_100_rank + team_rank_ba + TBF + Age + ERA + 
    BB_pct + K_pct + GB_pct +
    FIP , data = minor_league_pitching_A %>% 
    mutate(avg_100_rank = (rank_ba + rank_mlb)/2,
           made_mlb_rp = ifelse(made_mlb_rp == 1 & made_mlb_sp == 1, 0, made_mlb_rp)) %>% 
    left_join(first_minor_league_season_pit_A, by = c('PlayerId', 'Name')) %>% 
    filter(first_milb_season <= 2024-minor_league_rp_A_median),
  family = binomial(link = 'logit')
)

minor_league_pitching_A %>% 
  mutate(avg_100_rank = (rank_ba + rank_mlb)/2,
         made_mlb_rp = ifelse(made_mlb_rp == 1 & made_mlb_sp == 1, 0, made_mlb_rp)) %>% 
  left_join(first_minor_league_season_pit_A, by = c('PlayerId', 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_rp_A_median) %>%  
  select(avg_100_rank, team_rank_ba, TBF, Age, ERA, BB_pct, K_pct, GB_pct, 
         FIP) %>%
  mutate(log_odds = log(made_mlb_A_rp_mod$fitted.values / (1 - made_mlb_A_rp_mod$fitted.values))) %>% 
  pivot_longer(-log_odds) %>%
  ggplot(aes(value, log_odds)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~name, scales = "free_x") #no non-linear terms


made_mlb_A_rp_gam <- gam(
  made_mlb_rp ~ avg_100_rank + team_rank_ba + TBF + s(Age, bs = 'tp') + ERA + 
    BB_pct + K_pct + GB_pct +
    FIP , data = minor_league_pitching_A %>% 
    mutate(avg_100_rank = (rank_ba + rank_mlb)/2,
           made_mlb_rp = ifelse(made_mlb_rp == 1 & made_mlb_sp == 1, 0, made_mlb_rp)) %>% 
    left_join(first_minor_league_season_pit_A, by = c('PlayerId', 'Name')) %>% 
    filter(first_milb_season <= 2024-minor_league_rp_A_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_A_rp_gam)
gam.check(made_mlb_A_rp_gam)

age_smoothing_rp_A <- tidygam::predict_gam(made_mlb_A_rp_gam,
                                           values = list(
                                             avg_100_rank = mean(minor_league_pitching_A %>% 
                                                                   mutate(avg_100_rank = (rank_ba + rank_mlb)/2) %>% 
                                                                   pull(avg_100_rank)),
                                             team_rank_ba = mean(minor_league_pitching_A$team_rank_ba),
                                             TBF = mean(minor_league_pitching_A$TBF),
                                             Age = 18:40,
                                             ERA = mean(minor_league_pitching_A$ERA),
                                             BB_pct = mean(minor_league_pitching_A$BB_pct),
                                             K_pct = mean(minor_league_pitching_A$K_pct),
                                             GB_pct = mean(minor_league_pitching_A$GB_pct),
                                             FIP = mean(minor_league_pitching_A$FIP)
                                           ))




age_smoothing_rp_A %>% 
  mutate(mlb_prob = exp(made_mlb_rp)/(1+exp(made_mlb_rp))) %>% 
  ggplot(aes(Age, mlb_prob)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "tp")) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  scale_x_continuous(breaks = seq(18, 40, by = 2)) +
  labs(title = 'MLB Probability By Age In Single-A (RP)',
       y = 'MLB Probability',
       x = 'Age') +
  theme_classic()


#### Made MLB RP Logit & Probit Level A+ ####
#finding median age they make the MLB
minor_league_rp_Aplus_median <- median(minor_league_pitching_Aplus %>% 
                                         filter(made_mlb_rp == 1) %>% 
                                         pull(years_to_mlb_rp))
# logit
made_mlb_Aplus_rp_mod <- glm(
  made_mlb_rp ~ avg_100_rank + team_rank_ba_Aplus + TBF_Aplus + Age_Aplus +
    ERA_Aplus + BB_pct_Aplus + K_pct_Aplus + GB_pct_Aplus +
    FIP_Aplus, data = minor_league_pitching_Aplus %>% 
    mutate(avg_100_rank = (rank_ba_Aplus + rank_mlb_Aplus)/2,
           made_mlb_rp = ifelse(made_mlb_rp == 1 & made_mlb_sp == 1, 0, made_mlb_rp)) %>% 
    left_join(first_minor_league_season_pit_Aplus, by = c('PlayerId', 'Name_Aplus' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_rp_Aplus_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_Aplus_rp_mod)
vif(made_mlb_Aplus_rp_mod)

minor_league_pitching_Aplus %>% 
  mutate(avg_100_rank = (rank_ba_Aplus + rank_mlb_Aplus)/2,
         made_mlb_rp = ifelse(made_mlb_rp == 1 & made_mlb_sp == 1, 0, made_mlb_rp)) %>% 
  left_join(first_minor_league_season_pit_Aplus, by = c('PlayerId', 'Name_Aplus' = 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_sp_Aplus_median) %>%  
  reframe(avg_100_rank, team_rank_ba_Aplus, TBF_Aplus, Age_Aplus, ERA_Aplus, BB_pct_Aplus, K_pct_Aplus, GB_pct_Aplus, 
          FIP_Aplus) %>%
  mutate(log_odds = log(made_mlb_Aplus_sp_mod$fitted.values / (1 - made_mlb_Aplus_sp_mod$fitted.values))) %>% 
  pivot_longer(-log_odds) %>%
  ggplot(aes(value, log_odds)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~name, scales = "free_x") 

made_mlb_Aplus_rp_gam <- gam(
  made_mlb_rp ~ avg_100_rank + team_rank_ba_Aplus + TBF_Aplus + s(Age_Aplus, bs = 'tp') +
    ERA_Aplus + BB_pct_Aplus + K_pct_Aplus + GB_pct_Aplus +
    FIP_Aplus, data = minor_league_pitching_Aplus %>% 
    mutate(avg_100_rank = (rank_ba_Aplus + rank_mlb_Aplus)/2,
           made_mlb_rp = ifelse(made_mlb_rp == 1 & made_mlb_sp == 1, 0, made_mlb_rp)) %>% 
    left_join(first_minor_league_season_pit_Aplus, by = c('PlayerId', 'Name_Aplus' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_rp_Aplus_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_Aplus_rp_gam)
gam.check(made_mlb_Aplus_rp_gam)

age_smoothing_rp_Aplus <- tidygam::predict_gam(made_mlb_Aplus_rp_gam,
                                               values = list(
                                                 avg_100_rank = mean(minor_league_pitching_Aplus %>% 
                                                                       mutate(avg_100_rank = (rank_ba_Aplus + rank_mlb_Aplus)/2) %>% 
                                                                       pull(avg_100_rank)),
                                                 team_rank_ba_Aplus = mean(minor_league_pitching_Aplus$team_rank_ba_Aplus),
                                                 TBF_Aplus = mean(minor_league_pitching_Aplus$TBF_Aplus),
                                                 Age_Aplus = 18:40,
                                                 ERA_Aplus = mean(minor_league_pitching_Aplus$ERA_Aplus),
                                                 BB_pct_Aplus = mean(minor_league_pitching_Aplus$BB_pct_Aplus),
                                                 K_pct_Aplus = mean(minor_league_pitching_Aplus$K_pct_Aplus),
                                                 GB_pct_Aplus = mean(minor_league_pitching_Aplus$GB_pct_Aplus),
                                                 FIP_Aplus = mean(minor_league_pitching_Aplus$FIP_Aplus)
                                               ))




age_smoothing_rp_Aplus %>% 
  mutate(mlb_prob = exp(made_mlb_rp)/(1+exp(made_mlb_rp))) %>% 
  ggplot(aes(Age_Aplus, mlb_prob)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "tp")) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  scale_x_continuous(breaks = seq(18, 40, by = 2)) +
  labs(title = 'MLB Probability By Age In A+ (RP)',
       y = 'MLB Probability',
       x = 'Age') +
  theme_classic()


#### Made MLB RP Logit & Probit Level AA ####

#finding median age they make the MLB
minor_league_rp_AA_median <- median(minor_league_pitching_AA %>% 
                                      filter(made_mlb_rp == 1) %>% 
                                      pull(years_to_mlb_rp))

# logit
made_mlb_AA_rp_mod <- glm(
  made_mlb_rp ~ avg_100_rank + team_rank_ba_AA + TBF_AA + Age_AA + 
    ERA_AA + BB_pct_AA + K_pct_AA + GB_pct_AA + 
    FIP_AA, data = minor_league_pitching_AA %>% 
    mutate(avg_100_rank = (rank_ba_AA + rank_mlb_AA)/2,
           made_mlb_rp = ifelse(made_mlb_rp == 1 & made_mlb_sp == 1, 0, made_mlb_rp)) %>% 
    left_join(first_minor_league_season_pit_AA, by = c('PlayerId', 'Name_AA' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_rp_AA_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_AA_rp_mod) 
vif(made_mlb_AA_rp_mod)

minor_league_pitching_AA %>% 
  mutate(avg_100_rank = (rank_ba_AA + rank_mlb_AA)/2,
         made_mlb_rp = ifelse(made_mlb_rp == 1 & made_mlb_sp == 1, 0, made_mlb_rp)) %>% 
  left_join(first_minor_league_season_pit_AA, by = c('PlayerId', 'Name_AA' = 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_rp_AA_median) %>%  
  reframe(avg_100_rank, team_rank_ba_AA, TBF_AA, Age_AA, ERA_AA, BB_pct_AA, K_pct_AA, GB_pct_AA, 
          FIP_AA) %>%
  mutate(log_odds = log(made_mlb_AA_rp_mod$fitted.values / (1 - made_mlb_AA_rp_mod$fitted.values))) %>% 
  pivot_longer(-log_odds) %>%
  ggplot(aes(value, log_odds)) +
  geom_point() +
  geom_smooth(formula = y ~ s(x, bs = 'tp', k = 5)) +
  facet_wrap(~name, scales = "free_x") 

made_mlb_AA_rp_gam <- gam(
  made_mlb_rp ~ avg_100_rank + team_rank_ba_AA + TBF_AA + s(Age_AA, bs = 'tp', k = 10) + 
    ERA_AA + BB_pct_AA + K_pct_AA + GB_pct_AA + 
    FIP_AA, data = minor_league_pitching_AA %>% 
    mutate(avg_100_rank = (rank_ba_AA + rank_mlb_AA)/2,
           made_mlb_rp = ifelse(made_mlb_rp == 1 & made_mlb_sp == 1, 0, made_mlb_rp)) %>% 
    left_join(first_minor_league_season_pit_AA, by = c('PlayerId', 'Name_AA' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_rp_AA_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_AA_rp_gam)
gam.check(made_mlb_AA_rp_gam)

age_smoothing_rp_AA <- tidygam::predict_gam(made_mlb_AA_rp_gam,
                                            values = list(
                                              avg_100_rank = mean(minor_league_pitching_AA %>% 
                                                                    mutate(avg_100_rank = (rank_ba_AA + rank_mlb_AA)/2) %>% 
                                                                    pull(avg_100_rank)),
                                              team_rank_ba_AA = mean(minor_league_pitching_AA$team_rank_ba_AA),
                                              TBF_AA = mean(minor_league_pitching_AA$TBF_AA),
                                              Age_AA = 18:40,
                                              ERA_AA = mean(minor_league_pitching_AA$ERA_AA),
                                              BB_pct_AA = mean(minor_league_pitching_AA$BB_pct_AA),
                                              K_pct_AA = mean(minor_league_pitching_AA$K_pct_AA),
                                              GB_pct_AA = mean(minor_league_pitching_AA$GB_pct_AA),
                                              FIP_AA = mean(minor_league_pitching_AA$FIP_AA)
                                            ))




age_smoothing_rp_AA %>% 
  mutate(mlb_prob = exp(made_mlb_rp)/(1+exp(made_mlb_rp))) %>% 
  ggplot(aes(Age_AA, mlb_prob)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "tp")) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  scale_x_continuous(breaks = seq(18, 40, by = 2)) +
  labs(title = 'MLB Probability By Age In AA (RP)',
       y = 'MLB Probability',
       x = 'Age') +
  theme_classic()


#### Made MLB RP Logit & Probit Level AAA ####
#finding median age they make the MLB
minor_league_rp_AAA_median <- median(minor_league_pitching_AAA %>% 
                                       filter(made_mlb_rp == 1) %>% 
                                       pull(years_to_mlb_rp))

# logit
made_mlb_AAA_rp_mod <- glm(
  made_mlb_rp ~ avg_100_rank + team_rank_ba_AAA + TBF_AAA + Age_AAA +
    ERA_AAA + BB_pct_AAA + K_pct_AAA + GB_pct_AAA + 
    FIP_AAA, data = minor_league_pitching_AAA %>% 
    mutate(avg_100_rank = (rank_ba_AAA + rank_mlb_AAA)/2,
           made_mlb_rp = ifelse(made_mlb_rp == 1 & made_mlb_sp == 1, 0, made_mlb_rp)) %>% 
    left_join(first_minor_league_season_pit_AAA, by = c('PlayerId', 'Name_AAA' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_rp_AAA_median),
  family = binomial(link = 'logit')
)

summary(made_mlb_AAA_rp_mod) #better
vif(made_mlb_AAA_rp_mod)

minor_league_pitching_AAA %>% 
  mutate(avg_100_rank = (rank_ba_AAA + rank_mlb_AAA)/2,
         made_mlb_rp = ifelse(made_mlb_sp == 1 & made_mlb_rp == 1, 0, made_mlb_rp)) %>% 
  left_join(first_minor_league_season_pit_AAA, by = c('PlayerId', 'Name_AAA' = 'Name')) %>% 
  filter(first_milb_season <= 2024 - minor_league_rp_AAA_median) %>%  
  reframe(avg_100_rank, team_rank_ba_AAA, TBF_AAA, Age_AAA, ERA_AAA, BB_pct_AAA, K_pct_AAA, GB_pct_AAA, 
          FIP_AAA) %>%
  mutate(log_odds = log(made_mlb_AAA_rp_mod$fitted.values / (1 - made_mlb_AAA_rp_mod$fitted.values))) %>% 
  pivot_longer(-log_odds) %>%
  ggplot(aes(value, log_odds)) +
  geom_point() +
  geom_smooth(formula = y ~ s(x, bs = 'tp', k = 5)) +
  facet_wrap(~name, scales = "free_x") # ERA% is non-linear

made_mlb_AAA_rp_gam <- gam(
  made_mlb_rp ~ avg_100_rank + team_rank_ba_AAA + TBF_AAA + s(Age_AAA, bs = 'tp') +
    poly(ERA_AAA,2) + BB_pct_AAA + K_pct_AAA + GB_pct_AAA + 
    FIP_AAA, data = minor_league_pitching_AAA %>% 
    mutate(avg_100_rank = (rank_ba_AAA + rank_mlb_AAA)/2,
           made_mlb_rp = ifelse(made_mlb_rp == 1 & made_mlb_sp == 1, 0, made_mlb_rp)) %>% 
    left_join(first_minor_league_season_pit_AAA, by = c('PlayerId', 'Name_AAA' = 'Name')) %>% 
    filter(first_milb_season <= 2024 - minor_league_rp_AAA_median),
  family = binomial(link = 'logit')
)

age_smoothing_rp_AAA <- tidygam::predict_gam(made_mlb_AAA_rp_gam,
                                             values = list(
                                               avg_100_rank = mean(minor_league_pitching_AAA %>% 
                                                                     mutate(avg_100_rank = (rank_ba_AAA + rank_mlb_AAA)/2) %>% 
                                                                     pull(avg_100_rank)),
                                               team_rank_ba_AAA = mean(minor_league_pitching_AAA$team_rank_ba_AAA),
                                               TBF_AAA = mean(minor_league_pitching_AAA$TBF_AAA),
                                               Age_AAA = 20:40,
                                               ERA_AAA = mean(minor_league_pitching_AAA$ERA_AAA),
                                               BB_pct_AAA = mean(minor_league_pitching_AAA$BB_pct_AAA),
                                               K_pct_AAA = mean(minor_league_pitching_AAA$K_pct_AAA),
                                               GB_pct_AAA = mean(minor_league_pitching_AAA$GB_pct_AAA),
                                               FIP_AAA = mean(minor_league_pitching_AAA$FIP_AAA)
                                             ))




age_smoothing_rp_AAA %>% 
  mutate(mlb_prob = exp(made_mlb_rp)/(1+exp(made_mlb_rp))) %>% 
  ggplot(aes(Age_AAA, mlb_prob)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "tp")) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  scale_x_continuous(breaks = seq(20, 40, by = 2)) +
  labs(title = 'MLB Probability By Age In AAA (RP)',
       y = 'MLB Probability',
       x = 'Age') +
  theme_classic()



### GAM Graphic ####
age_smoothing_rp <- bind_rows(
  age_smoothing_rp_A %>% mutate(Level = 'A'),
  age_smoothing_rp_Aplus %>% mutate(Level = 'A+') %>% 
    rename_with(~str_remove(.,'_Aplus$'), ends_with('_Aplus')),
  age_smoothing_rp_AA %>% mutate(Level = 'AA') %>% 
    rename_with(~str_remove(.,'_AA$'), ends_with('_AA')),
  age_smoothing_rp_AAA %>% mutate(Level = 'AAA') %>% 
    rename_with(~str_remove(.,'_AAA$'), ends_with('_AAA'))
)

(rp_age_curve <- age_smoothing_rp %>% 
  mutate(mlb_prob = exp(made_mlb_rp)/(1+exp(made_mlb_rp))) %>% 
  ggplot(aes(Age, mlb_prob, color = Level, group = Level)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "tp")) +
  scale_x_continuous(breaks = seq(18, 40, by = 2)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(title = 'MLB Probability By Age & League (RP)',
       y = 'MLB Probability',
       x = 'Age') +
  theme_classic() +
  theme(legend.position = c(0.9,0.7),
        legend.key.size = unit(1,'cm'),
        legend.background = element_rect(color = 'white', fill = 'white')))

### Final Aging Curve Graphic ####
(final_aging_curve <- grid.arrange(aging_curve_hit + guides(color = 'none'), sp_aging_curve + theme(legend.key.size = unit(0.75, 'cm'),
                                                                                                    legend.position = c(0.9,0.8)), rp_age_curve + guides(color = 'none')))

ggsave('aging curves making mlb.png', final_aging_curve, width = 2000, height = 2000, units = 'px')

## Beta Regression OBP from Level to Level ####
library(betareg)

# Gets Previous Level
prev_level_df <- bind_rows(minor_league_hitting_Aplus %>% rename_with(~paste0('prev_lvl_', str_remove(., '_A$'), recycle0 = TRUE), ends_with('_A')) %>% 
                                                          rename_with(~paste0('current_lvl_', str_remove(., '_Aplus$'), recycle0 = TRUE), ends_with('_Aplus')),
                           minor_league_hitting_AA %>% rename_with(~paste0('prev_lvl_', str_remove(., '_Aplus$'), recycle0 = TRUE), ends_with('_Aplus')) %>% 
                             rename_with(~paste0('current_lvl_', str_remove(., '_AA$'), recycle0 = TRUE), ends_with('_AA')),
                           minor_league_hitting_AAA %>% rename_with(~paste0('prev_lvl_', str_remove(., '_AA$'), recycle0 = TRUE), ends_with('_AA')) %>% 

                             rename_with(~paste0('current_lvl_', str_remove(., '_AAA$'), recycle0 = TRUE), ends_with('_AAA')),
                           asfs <- mlb_hitting %>% 
                             rename('Level_AAA' = 'Level',
                                    'rank_ba_AAA' = 'rank_ba',
                                    'rank_mlb_AAA' = 'rank_mlb',
                                    'team_rank_ba_AAA' = 'team_rank_ba') %>% 
                             rename_with(~paste0('prev_lvl_', str_remove(., '_AAA$'), recycle0 = TRUE), ends_with('_AAA')) %>% 
                             rename_with(~paste0('current_lvl_', str_remove(., '_MLB$'), recycle0 = TRUE), ends_with('_MLB')) %>% 
                                           rename('prev_lvl_rank_mlb' = 'current_lvl_prev_lvl_rank_mlb')
) %>% 
  drop_na(prev_lvl_Name)

obp_mod <- betareg(current_lvl_OBP ~ prev_lvl_OBP + avg_100_rank + prev_lvl_team_rank_ba + prev_lvl_PA +
                           prev_lvl_BB_pct +  prev_lvl_SwStr_pct + as.factor(prev_lvl_Level),
                          data = prev_level_df %>% 
                            mutate(avg_100_rank = (prev_lvl_rank_ba + prev_lvl_rank_mlb)/2),
                          link = 'logit')

summary(obp_mod)
vif(obp_mod)

## Fractional Logit AVG from A to A+ ####
avg_a_to_aplus <- glm(AVG_Aplus ~ AVG_A + avg_100_rank + avg_team_rank + PA_A +
                            BB_pct_A + SwStr_pct_A + poly(Age_A, 2),
                          data = minor_league_hitting_Aplus %>% 
                            drop_na(Name_A) %>% 
                            mutate(avg_100_rank = (rank_ba_A + rank_mlb_A)/2,
                                   avg_team_rank = (team_rank_ba_A + team_rank_mlb_A)/2) ,
                          family = quasibinomial(link = 'logit'))

summary(avg_a_to_aplus)
vif(avg_a_to_aplus)

## Fractional Logit BB% from A to A+ ####
bbpct_a_to_aplus <- glm(BB_pct_Aplus ~ avg_100_rank + avg_team_rank + PA_A +
                            BB_pct_A + wRC_plus_A + SwStr_pct_A,
                          data = minor_league_hitting_Aplus %>% 
                            drop_na(Name_A) %>% 
                            mutate(avg_100_rank = (rank_ba_A + rank_mlb_A)/2,
                                   avg_team_rank = (team_rank_ba_A + team_rank_mlb_A)/2),
                          family = quasibinomial(link = 'logit'))

summary(bbpct_a_to_aplus)
vif(bbpct_a_to_aplus)

## Beta Regression SwStr from A to A+ ####

swstrpct_a_to_aplus <- betareg(SwStr_pct_Aplus ~ K_pct_A + avg_100_rank + avg_team_rank + PA_A +
                            BB_pct_A + wRC_plus_A + SwStr_pct_A + poly(Age_A,2)| 
                            K_pct_A + avg_100_rank + avg_team_rank + PA_A + BB_pct_A + wRC_plus_A + 
                              SwStr_pct_A + poly(Age_A,2),
                          data = minor_league_hitting_Aplus %>% 
                            drop_na(Name_A) %>% 
                            mutate(avg_100_rank = (rank_ba_A + rank_mlb_A)/2,
                                   avg_team_rank = (team_rank_ba_A + team_rank_mlb_A)/2),
                          link = 'logit')

summary(swstrpct_a_to_aplus)
vif(swstrpct_a_to_aplus)


## Fractional Logit with K% from A to A+ ####
kpct_a_to_aplus <- glm(K_pct_Aplus ~ avg_100_rank + avg_team_rank + PA_A +
                          K_pct_A + wRC_plus_A + SwStr_pct_A,
                        data = minor_league_hitting_Aplus %>% 
                          drop_na(Name_A) %>% 
                          mutate(avg_100_rank = (rank_ba_A + rank_mlb_A)/2,
                                 avg_team_rank = (team_rank_ba_A + team_rank_mlb_A)/2),
                        family = quasibinomial(link = 'logit'))

summary(kpct_a_to_aplus)
vif(kpct_a_to_aplus)

### years to MLB lm (hitter) A ####

years_to_mlb_batterA_mod <- lm(
  years_to_mlb ~ avg_100_rank + team_rank_ba + PA + Age + 
    K_pct + BB_pct + poly(wRC_plus,2) + SwStr_pct + Pull_pct, 
 data = minor_league_hitting_A %>% 
    mutate(avg_100_rank = (rank_ba + rank_mlb)/2,
           avg_team_rank = (team_rank_ba  + team_rank_mlb)/2) %>% 
   filter(years_to_mlb != 25)
)

summary(years_to_mlb_batterA_mod)
AIC(years_to_mlb_batterA_mod)
vif(years_to_mlb_batterA_mod)

years_to_mlb_A_batter_gam <- gam(
  years_to_mlb ~ avg_100_rank + team_rank_ba + PA + s(Age, bs = 'bs') + wRC_plus + 
    BB_pct + K_pct + SwStr_pct + LD_pct + GB_per_FB, data = minor_league_hitting_A %>% 
    mutate(avg_100_rank = (rank_ba + rank_mlb)/2) %>% 
    filter(years_to_mlb != 25),
  family = 'gaussian'
)

age_smoothing_to_mlb <- predict.gam(years_to_mlb_A_batter_gam, minor_league_hitting_A %>% mutate(avg_100_rank = (rank_ba + rank_mlb)/2))

AIC(years_to_mlb_A_batter_gam)

minor_league_hitting_A %>% 
  mutate(years_to_mlb = age_smoothing_to_mlb) %>% 
  ggplot(aes(Age, years_to_mlb)) +
  geom_jitter(position = position_jitter(width = 0.3, height = 0, seed = 101)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'bs')) +
  scale_x_continuous(breaks = seq(18, 30, by = 2)) +
  theme_classic()


minor_league_hitting_A %>%
  mutate(avg_100_rank = (rank_ba + rank_mlb)/2) %>% 
  select(avg_100_rank, team_rank_ba, PA, Age, wRC_plus, BB_pct, K_pct, SwStr_pct, 
         LD_pct, GB_per_FB,years_to_mlb) %>%
  filter(years_to_mlb != 25) %>% 
  pivot_longer(-years_to_mlb) %>%
  ggplot(aes(value, years_to_mlb)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  facet_wrap(~name, scales = "free_x") #wRC+ needs to be polynomial


years_to_mlb_A_batter_gam <- gam(
  years_to_mlb ~ avg_100_rank + team_rank_ba + PA + Age + poly(wRC_plus,2) + 
    s(BB_pct, bs = 'bs') + K_pct + SwStr_pct + LD_pct + GB_per_FB, data = minor_league_hitting_A %>% 
    mutate(avg_100_rank = (rank_ba + rank_mlb)/2) %>% 
    filter(years_to_mlb != 25),
  family = 'gaussian'
)


age_smoothing_to_mlb <- predict.gam(years_to_mlb_A_batter_gam, minor_league_hitting_A %>% mutate(avg_100_rank = (rank_ba + rank_mlb)/2))

summary(years_to_mlb_A_batter_gam)
AIC(years_to_mlb_A_batter_gam)

minor_league_hitting_A %>% 
  mutate(years_to_mlb = age_smoothing_to_mlb) %>% 
  ggplot(aes(BB_pct, years_to_mlb)) +
  geom_point() +
  #geom_jitter(position = position_jitter(width = 0.3, height = 0, seed = 101)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'bs')) +
  #scale_x_continuous(breaks = seq(18, 30, by = 2)) +
  theme_classic()



# Gets Previous Level
minor_league_hitting_Aplus %>% rename_with(~paste0('prev_', str_remove(., '_A$'), recycle0 = TRUE), ends_with('_A'))