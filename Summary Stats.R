#install.packages('vtable')
library(tidyverse)
library(vtable)
#install.packages('flextable')
require(flextable)
require(summarytools)
require(gtsummary)
require(arsenal)


view(stby(minor_league_hitting %>% 
        reframe(
                Age,
                PA,
                AVG, 
                OBP, 
                SLG,
                K_pct,
                BB_pct,
                SwStr_pct,
                wRC_plus = round(wRC_plus,0), 
                rank_ba = ifelse(rank_ba > 100, NA, rank_ba), 
                rank_mlb = ifelse(rank_mlb > 100 , NA, rank_mlb), 
                team_rank_ba = ifelse(team_rank_ba > 30, NA, team_rank_ba), 
                #made_mlb,
                #years_to_mlb
                ),
      INDICES = minor_league_hitting$Level,
      FUN = descr,
      stats = c('min','q1','med','mean','q3','sd','max', 'n.valid'),
      order = 'p',
      round.digits = 3), file = 'asfs.html')




#AAA
st(minor_league_hitting_AAA %>%
     mutate(rank_ba_AAA = ifelse(rank_ba_AAA > 100, NA, rank_ba_AAA),
            rank_mlb_AAA = ifelse(rank_mlb_AAA > 100, NA, rank_mlb_AAA),
            years_to_mlb = ifelse(years_to_mlb == 25, NA, years_to_mlb)) %>% 
              mutate(across(c('BB_pct_AAA','K_pct_AAA','SwStr_pct_AAA', 'OBP_AAA','AVG_AAA','SLG_AAA'), ~round(., 3))), 
   vars = c('PA_AAA','AVG_AAA','OBP_AAA','SLG_AAA','BB_pct_AAA',
                                      'K_pct_AAA','wRC_plus_AAA', 'SwStr_pct_AAA','made_mlb', 'rank_ba_AAA','rank_mlb_AAA', 'years_to_mlb'),
                                        summ = c('notNA(x)', 'min(x)', 'pctile(x)[25]','median(x)','mean(x)','sd(x)',
                                                 'pctile(x)[75]', 'max(x)', 'propNA(x)'), skip.format = NA,digits = 3, factor.numeric = TRUE,
                                        summ.names = c('N', 'Min.', '25%','Median','Mean','SD','75%','Max.','Prop. NA'),
   title = 'AAA Batting Summary Stats'
   )

#AA
st(minor_league_hitting_AA %>%
     mutate(rank_ba_AA = ifelse(rank_ba_AA > 100, NA, rank_ba_AA),
            rank_mlb_AA = ifelse(rank_mlb_AA > 100, NA, rank_mlb_AA),
            years_to_mlb = ifelse(years_to_mlb == 25, NA, years_to_mlb)), vars = c('PA_AA','AVG_AA','OBP_AA','SLG_AA','BB_pct_AA',
                                                                                   'K_pct_AA','wRC_plus_AA', 'SwStr_pct_AA','made_mlb', 'rank_ba_AA','rank_mlb_AA', 'years_to_mlb'),
   summ = c('notNA(x)', 'min(x)', 'pctile(x)[25]','median(x)','mean(x)','sd(x)',
            'pctile(x)[75]', 'max(x)', 'propNA(x)'), skip.format = NA,digits = 3, factor.numeric = TRUE,
   summ.names = c('N', 'Min.', '25%','Median','Mean','SD','75%','Max.','Prop. NA'),
   title = 'AA Summary Stats'
)

#A+
st(minor_league_hitting_Aplus %>%
     mutate(rank_ba_Aplus = ifelse(rank_ba_Aplus > 100, NA, rank_ba_Aplus),
            rank_mlb_Aplus = ifelse(rank_mlb_Aplus > 100, NA, rank_mlb_Aplus),
            years_to_mlb = ifelse(years_to_mlb == 25, NA, years_to_mlb)), vars = c('PA_Aplus','AVG_Aplus','OBP_Aplus','SLG_Aplus','BB_pct_Aplus',
                                                                                   'K_pct_Aplus','wRC_plus_Aplus', 'SwStr_pct_Aplus','made_mlb', 'rank_ba_Aplus','rank_mlb_Aplus', 'years_to_mlb'),
   summ = c('notNA(x)', 'min(x)', 'pctile(x)[25]','median(x)','mean(x)','sd(x)',
            'pctile(x)[75]', 'max(x)', 'propNA(x)'), skip.format = NA,digits = 3, factor.numeric = TRUE,
   summ.names = c('N', 'Min.', '25%','Median','Mean','SD','75%','Max.','Prop. NA'),
   title = 'A+ Summary Stats'
)


#A
st(minor_league_hitting_A %>%
     mutate(rank_ba = ifelse(rank_ba > 100, NA, rank_ba),
            rank_mlb = ifelse(rank_mlb > 100, NA, rank_mlb),
            years_to_mlb = ifelse(years_to_mlb == 25, NA, years_to_mlb)), vars = c('PA','AVG','OBP','SLG','BB_pct',
                                                                                   'K_pct','wRC_plus', 'SwStr_pct','made_mlb', 'rank_ba','rank_mlb', 'years_to_mlb'),
   summ = c('notNA(x)', 'min(x)', 'pctile(x)[25]','median(x)','mean(x)','sd(x)',
            'pctile(x)[75]', 'max(x)', 'propNA(x)'), skip.format = NA,digits = 3, factor.numeric = TRUE,
   summ.names = c('N', 'Min.', '25%','Median','Mean','SD','75%','Max.','Prop. NA'),
   title = 'A Summary Stats'
)


#AAA Pitching
st(minor_league_pitching_AAA %>%
     mutate(rank_ba_AAA = ifelse(rank_ba_AAA > 100, NA, rank_ba_AAA),
            rank_mlb_AAA = ifelse(rank_mlb_AAA > 100, NA, rank_mlb_AAA),
            years_to_mlb = ifelse(years_to_mlb == 25, NA, years_to_mlb)) %>% 
     mutate(across(c('BB_pct_AAA','K_pct_AAA','SwStr_pct_AAA'), ~round(.,3))), vars = c('TBF_AAA','ERA_AAA','FIP_AAA','BB_pct_AAA',
                                'K_pct_AAA', 'SwStr_pct_AAA','made_mlb_sp','made_mlb_rp', 'rank_ba_AAA','rank_mlb_AAA', 'years_to_mlb'),
   summ = c('notNA(x)', 'min(x)', 'pctile(x)[25]','median(x)','mean(x)','sd(x)',
            'pctile(x)[75]', 'max(x)', 'propNA(x)'), skip.format = NA,digits = 3, factor.numeric = TRUE,
   summ.names = c('N', 'Min.', '25%','Median','Mean','SD','75%','Max.','Prop. NA'),
   title = 'AAA Pitching Summary Stats'
)

#AA
st(minor_league_pitching_AA %>%
     mutate(rank_ba_AA = ifelse(rank_ba_AA > 100, NA, rank_ba_AA),
            rank_mlb_AA = ifelse(rank_mlb_AA > 100, NA, rank_mlb_AA),
            years_to_mlb = ifelse(years_to_mlb == 25, NA, years_to_mlb)), vars = c('TBF_AA','ERA_AA','FIP_AA','BB_pct_AA',
                                                                                   'K_pct_AA', 'SwStr_pct_AA','made_mlb_sp','made_mlb_rp', 'rank_ba_AA','rank_mlb_AA', 'years_to_mlb'),
   summ = c('notNA(x)', 'min(x)', 'pctile(x)[25]','median(x)','mean(x)','sd(x)',
            'pctile(x)[75]', 'max(x)', 'propNA(x)'), skip.format = NA,digits = 3, factor.numeric = TRUE,
   summ.names = c('N', 'Min.', '25%','Median','Mean','SD','75%','Max.','Prop. NA'),
   title = 'AA Summary Stats'
)


#Aplus
st(minor_league_pitching_Aplus %>%
     mutate(rank_ba_Aplus = ifelse(rank_ba_Aplus > 100, NA, rank_ba_Aplus),
            rank_mlb_Aplus = ifelse(rank_mlb_Aplus > 100, NA, rank_mlb_Aplus),
            years_to_mlb = ifelse(years_to_mlb == 25, NA, years_to_mlb)), vars = c('TBF_Aplus','ERA_Aplus','FIP_Aplus','BB_pct_Aplus',
                                                                                   'K_pct_Aplus', 'SwStr_pct_Aplus','made_mlb_sp','made_mlb_rp', 'rank_ba_Aplus','rank_mlb_Aplus', 'years_to_mlb'),
   summ = c('notNA(x)', 'min(x)', 'pctile(x)[25]','median(x)','mean(x)','sd(x)',
            'pctile(x)[75]', 'max(x)', 'propNA(x)'), skip.format = NA,digits = 3, factor.numeric = TRUE,
   summ.names = c('N', 'Min.', '25%','Median','Mean','SD','75%','Max.','Prop. NA'),
   title = 'A+ Summary Stats'
)


#A
st(minor_league_pitching_A %>%
     mutate(rank_ba = ifelse(rank_ba > 100, NA, rank_ba),
            rank_mlb = ifelse(rank_mlb > 100, NA, rank_mlb),
            years_to_mlb = ifelse(years_to_mlb == 25, NA, years_to_mlb)), vars = c('TBF','ERA','FIP','BB_pct',
                                                                                   'K_pct', 'SwStr_pct','made_mlb_sp','made_mlb_rp', 'rank_ba','rank_mlb', 'years_to_mlb'),
   summ = c('notNA(x)', 'min(x)', 'pctile(x)[25]','median(x)','mean(x)','sd(x)',
            'pctile(x)[75]', 'max(x)', 'propNA(x)'), skip.format = NA,digits = 3, factor.numeric = TRUE,
   summ.names = c('N', 'Min.', '25%','Median','Mean','SD','75%','Max.','Prop. NA'),
   title = 'A Summary Stats'
)
