library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)

setwd('/Users/williampetti/baseball_tools/baseball_tools/minor_league_park_factors/')

milb_pf <- read_csv('data/minor_league_park_factors.csv')

ui <- 
    navbarPage(
        #theme = 'bootstrap.css',
        #theme = shinytheme('flatly'),
        
        title = ('baseball tools'),
        tabPanel("Minor League Park Factors", 
                 DTOutput('milb_pf_table'))
    )


server <- function(input, output) {
    
    output$milb_pf_table <- renderDT({
        
        df <- milb_pf %>%
            select(venue.id, venue.name, teams.home.team.id, 
                   teams.home.team.name, year, level, years, park_effect_halved) %>%
            mutate_at(vars(venue.id, venue.name, 
                           teams.home.team.id, teams.home.team.name, 
                           level, years, year), as.factor) %>%
            mutate(level = factor(level, levels = c('Triple-A', 
                                                    'Double-A', 
                                                    'Class A Advanced', 
                                                    'Class A',
                                                    'Class A Short Season', 
                                                    'Rookie Advanced', 
                                                    'Rookie', 
                                                    'Winter League'
            )))
        
        names(df) <- c('Park ID', 'Park', 'Team ID', 'Team', 'Year', 
                       'Level', '# of Years', 'Park Effect')
        DT::datatable(df,
                      filter = 'top',
                      options = list(dom = 'ltp',
                                     pageLength = 50, 
                                     searchCols = list(NULL, 
                                                       NULL, 
                                                       NULL, 
                                                       NULL, 
                                                       NULL,
                                                       list(search = '["2019"]'), 
                                                       list(search = '["Triple-A"]'), 
                                                       list(search = '["3"]'), 
                                                       NULL),
                                     lengthMenu = list(c(15,30,50,-1), c("15", "30", 
                                                                         "50", "All")),
                                     autoWidth = TRUE,
                                     columnDefs = list(list(width = '160px', targets = c(2)),
                                                       list(width = '150px', targets = c(4)),
                                                       list(width = '120px', targets = c(6)),
                                                       list(width = '10px', targets = c(1,3,5,7,8)), 
                                                       list(width = '5px', targets = c(0)), 
                                                       list(className = 'dt-center', targets = c(3,5,7,8))))
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
