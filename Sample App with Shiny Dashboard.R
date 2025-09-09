library(DT)
library(shiny)
library(shinydashboard)
library(bslib)
library(tidyverse)
library(shinyWidgets)
library(readxl)



top_100_pitching_shiny <- read_csv('top_100_pitching_shiny.csv')
top_100_hitting_shiny <- read_csv('top_100_hitting_shiny.csv')
top_30_prospects <- read_excel('Data/MLB 2025 Prospect Rankings.xlsx', sheet = 'Top 30 Rankings') %>% 
  mutate(Position = case_when(
    Position == 'RHp' ~ 'RHP', 
    Position == '3N' ~ '3B',
    .default = Position))
shiny_pitching_top_30 <- read_csv('shiny_pitching_top_30.csv')
shiny_hitting_top_30 <- read_csv('shiny_hitting_top_30.csv')

player_pictures <- read_csv('player_pictures.csv')

shiny_pitching_top_30 <- shiny_pitching_top_30 %>% 
  left_join(player_pictures, by = c('Name' = 'player')) %>% 
  relocate(picture, .before = 'Name')

shiny_pitching_top_30$picture <- paste0('<img src="', shiny_pitching_top_30$picture, '" height = "60"></img>')

shiny_hitting_top_30 <- shiny_hitting_top_30 %>% 
  left_join(player_pictures, by = c('Name' = 'player')) %>% 
  relocate(picture, .before = 'Name')

shiny_hitting_top_30$picture <- paste0('<img src="', shiny_hitting_top_30$picture, '" height = "60"></img>')


# Define UI for application that draws a histogram
ui <- dashboardPage(
  theme = bs_theme(preset = 'sandstone'),
  dashboardHeader(title = '2025 Prospect Forecasting Predictions'),
  dashboardSidebar(),
  dashboardBody(
    #title = '2025 Prospect Forecasting Predictions',
    tabPanel(
      title = 'Top 100 Rankings',
      fluidPage(
        verticalLayout(
          tabsetPanel(
            tabPanel(title = 'Batters', DTOutput('batter_top_100')),
            tabPanel(title = 'Pitchers', DTOutput('pitcher_top_100'))
          )
        )
      )
    ),
    tabPanel(
      title = 'Top 30 Team Rankings',
      fluidPage(
        verticalLayout(
          selectInput('selected_team', label = 'Choose Team:', 
                      choices = unique(top_30_prospects$Team), selected = 'ARI'
          ),
          tabsetPanel(
            tabPanel(title = 'Batters', DTOutput('batter_top_30')),
            tabPanel(title = 'Pitchers', DTOutput('pitcher_top_30'))
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$batter_top_100 <- renderDT({
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
                    ), class = c('display', 'cell-border'),
                  filter = 'top') %>% 
      DT::formatRound(columns = c('AVG', 'OBP', 'SLG', 'ISO'), digits = 3) %>% 
      DT::formatPercentage(columns = c('K%','BB%','SwStr%'), digits = 1) %>% 
      DT::formatPercentage(columns = c('MLB Likelihood'), digits = 1)
    
  })
  
  output$pitcher_top_100 <- renderDT({
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
                    ), class = c('display', 'cell-border'),
                  filter = 'top') %>% 
      DT::formatRound(columns = c('ERA', 'FIP'), digits = 2) %>% 
      DT::formatPercentage(columns = c('K%','BB%','SwStr%'), digits = 1) %>% 
      DT::formatPercentage(columns = c('MLB RP Likelihood', 'MLB SP Likelihood'), digits = 1)
    
  })
  
  output$batter_top_30 <- renderDT({
    DT::datatable(shiny_hitting_top_30 %>%
                    filter(Team == !!input$selected_team) %>% 
                    mutate(mlb_prob = mlb_prob/100,
                           pred_Kpct = pred_Kpct/100,
                           pred_BBpct = pred_BBpct/100,
                           pred_SwStrpct = pred_SwStrpct/100) %>% 
                    rename(
                      ' ' = 'picture',
                      'AVG' = 'pred_AVG',
                      'OBP' = 'pred_OBP',
                      'SLG' = 'pred_SLG',
                      'ISO' = 'pred_ISO',
                      'wRC+' = 'pred_wrcplus',
                      'K%' = 'pred_Kpct',
                      'BB%' = 'pred_BBpct',
                      'SwStr%' = 'pred_SwStrpct',
                      'MLB Likelihood' = 'mlb_prob'
                    ),
                  options = list(pageLength = 30),
                  escape = FALSE,
                  class = c('display', 'cell-border'),
                  filter = 'top') %>% 
      DT::formatRound(columns = c('AVG', 'OBP', 'SLG', 'ISO'), digits = 3) %>% 
      DT::formatPercentage(columns = c('K%','BB%','SwStr%'), digits = 1) %>% 
      DT::formatPercentage(columns = c('MLB Likelihood'), digits = 1)
    
  })
  
  output$pitcher_top_30 <- renderDT({
    DT::datatable(shiny_pitching_top_30 %>% 
                    filter(Team == !!input$selected_team) %>% 
                    mutate(mlb_rp_prob = mlb_rp_prob/100,
                           mlb_sp_prob = mlb_sp_prob/100,
                           pred_Kpct = pred_Kpct/100,
                           pred_BBpct = pred_BBpct/100,
                           pred_SwStrpct = pred_SwStrpct/100) %>% 
                    rename(
                      ' ' = 'picture',
                      'ERA' = 'pred_ERA',
                      'FIP' = 'pred_FIP',
                      'K%' = 'pred_Kpct',
                      'BB%' = 'pred_BBpct',
                      'SwStr%' = 'pred_SwStrpct',
                      'MLB RP Likelihood' = 'mlb_rp_prob',
                      'MLB SP Likelihood' = 'mlb_sp_prob'
                    ),
                  options = list(pageLength = 30), 
                  escape = FALSE,
                  class = c('display', 'cell-border'),
                  filter = 'top') %>% 
      DT::formatRound(columns = c('ERA', 'FIP'), digits = 2) %>% 
      DT::formatPercentage(columns = c('K%','BB%','SwStr%'), digits = 1) %>% 
      DT::formatPercentage(columns = c('MLB RP Likelihood', 'MLB SP Likelihood'), digits = 1)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
