library(shiny)
library(tidyverse)
library(kableExtra)
library(janitor)

#### Loop through team files to get data file paths ####
my_files <-
  list.files(
    path = ("../card_data/"),
    pattern = "*.csv",
    full.names = TRUE
  )

#### Read in files with the tidyverse, removing rows with all NA, and correcting a typo ####
cards <- read_csv(my_files, na = c("", "NA", "none"))

#### Select all cool cards for card showcase ####
cool_cards <- read_csv("cool_cards.csv")

# set ggplot theme
my_theme <-  theme(panel.grid.minor.y = element_blank(),
                   panel.background = element_rect(fill = 'grey90'),
                   axis.ticks.x = element_blank(),
                   axis.ticks.y = element_blank())

# Define UI ----
ui <- fluidPage(navbarPage(
  "Baseball Card Lookup",
  #### Home Page ####
  tabPanel(
    "Home",
    h1('About This App and Collection'),
    p('Welcome to my baseball card collection!'),
    p(
      "I've collected baseball cards for quite some time now (since at least the late 2000's. I also really enjoy statistics and thought it would be interesting to electronically record all of my cards.)"
    ),
    p(
      'The goals of this app are twofold: to share some general data about my collection and to allow myself and others to search the collection itself.'
    ),
    p(
      "The tabs at the top of the screen provide these functions. 'Curated Collections' consists of pre-made charts and plots summarizing my collection. See what comes up! 'Card Showcase' offers a glance at cards I personally find cool or interesting. Finally, the 'Search' tab leads to a search bar with multiple filters."
    ),
    p(
      "The last update to this app/the data was _____."
    )
  ),
  
  
  #### Plots and Charts ####
  tabPanel("Curated Summaries",
           mainPanel(width = 12,
             h1('Pre-made Summary Plots and Tables'),
             p('Here I demonstrate summary information about my collection. The plots and tables below may rotate and change over time as I experiment with new ways to share info.'),
             h2('Plots'),
             fluidRow(
               splitLayout(cellWidths = c("50%", "50%"), plotOutput("parallel_by_color"), plotOutput("n_by_position"))
             ),
             fluidRow(
               splitLayout(cellWidths = c("50%", "50%"), plotOutput("decade_rookie"), plotOutput("cards_by_team"))
             ),
             h2('Tables'),
             htmlOutput("total_counts"),
             htmlOutput("counts_by_team"),
             htmlOutput("counts_by_brand")
             )
             
           ),
  
  
  #### Card Showcase ####
  tabPanel("Card Showcase",
           mainPanel(width = 12,
             h1("Showing Off Cards I Find Cool"),
           p("The card shown here was randomly generated from a list of cards that I want to show off from my collection. Each time you press the refresh button, you'll get a different card!"),
           fluidRow(
             column(12, offset = 5, actionButton('showcase_refresh', 'New Card!'))
           ),
           h2(textOutput("showcase_player_name")),
           p(textOutput("showcase_team_position")),
           htmlOutput("showcase_info"),
           fluidRow(
             column(6, imageOutput("showcase_front", width = "100px"))
           )
           
           )),
  
  
  
  tabPanel("Search",
           h1('Searching the Card Collection'),
           p("Hey! This page doesn't work yet. Come back soon!"))
  
))

# Define server logic ----
server <- function(input, output) {
  
  #### static plots for plot summaries ####
  
  output$parallel_by_color <- renderPlot({
    cards %>% 
      filter(parallel != 0) %>% 
      select(parallel, refractor) %>% 
      ggplot(aes(x = fct_infreq(parallel))) +
      geom_bar(fill = "violetred4") +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = "parallel color",
           title = "Number of Parallel Cards by Color",
           subtitle = paste("Total Parallel Cards:", nrow(cards %>% filter(parallel != 0)))) +
      my_theme
  })
  
  output$n_by_position <- renderPlot({
    cards %>% 
      filter(!is.na(position)) %>% 
      mutate(app_pos = ifelse(nchar(position) > 2 & !(position %in% c("team", "split", "coach", "manager", "mascot", "stadium")), 'multiple', position)) %>% 
      select(position, app_pos) %>% 
      ggplot(aes(x = fct_infreq(app_pos))) +
      geom_bar(fill = 'violetred4') +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 2200)) +
      labs(x = "player position", title = "Number of Cards by Player Position",
           subtitle = "Cards with No Position: 6") +
      my_theme +
      theme(axis.text.x = element_text(angle = 20, vjust = 0.5),
            axis.ticks.x = element_line(color = 'black'))
  })
  
  output$decade_rookie <- renderPlot({
    cards %>% 
      mutate(decade = str_replace(year, '[1-9]$', '0')) %>% 
      ggplot(aes(x = factor(decade), fill = factor(rookie_card))) +
      geom_bar() +
      scale_y_continuous(expand = c(0, 0, 0, 50)) +
      scale_fill_brewer(palette = 'Accent', labels = c('No', 'Yes')) +
      labs(x = "decade", title = 'Number of Cards by Decade', fill = "Rookie Card") +
      my_theme
  })
  
  output$cards_by_team <- renderPlot({
    cards %>% 
      mutate(team_app = ifelse(team %in% c("Los Angeles Angels", "Los Angeles Angels of Anaheim", "California Angels"), "Angels (all versions)", team)) %>% 
      filter(team_app != "Split Card") %>% 
      ggplot(aes(x = fct_rev(fct_infreq(team_app)))) +
      geom_bar(fill = '#d95f02') +
      scale_y_continuous(expand = c(0, 0, 0, 50)) +
      # scale_fill_manual(values = c('#1c9099', '#a6bddb'), labels = c("No", "Yes")) +
      labs(x = "team name", title = "Number of Cards By Team", fill = "Rookie Card") +
      coord_flip() +
      my_theme
  })
  
  #### static tables for summaries ####
  
  output$total_counts <- renderText({
    cards %>% 
      summarise(total = sum(count),
                rc = sum(rookie_card),
                od = sum(opening_day),
                asg = sum(asg),
                chrome = sum(chrome),
                refractor = sum(refractor),
                parallel = length(which(parallel != 0)),
                heritage = sum(heritage),
                auto = sum(auto),
                num = length(which(numbered != 0))
      ) %>% 
      kbl("html", caption = "Number of Cards by Category",
          col.names = c("Total", "Rookie Cards", "Opening Day", "All Star Game", "Chrome", "Refractor", "Parallel", "Heritage", "Autograph", "Numbered")) %>% 
      kable_styling('bordered', full_width = F)
  })
  
  output$counts_by_team <- renderText({
    
    cards %>% 
      group_by(team) %>% 
      summarise(total = sum(count),
                rc = sum(rookie_card),
                od = sum(opening_day),
                asg = sum(asg),
                chrome = sum(chrome),
                refractor = sum(refractor),
                heritage = sum(heritage),
                auto = sum(auto),
                num = length(which(numbered != 0))
      ) %>% 
      kbl(caption = "Number of Cards by Team",
          col.names = c("Team", "Total", "Rookie Cards", "Opening Day", "All Star Game", "Chrome", "Refractor", "Heritage", "Autograph", "Numbered")) %>%
      kable_styling(c('bordered', 'condensed', 'striped'), full_width = F)
    
  })
  
  output$counts_by_brand <- renderText({
    
    cards %>% 
      group_by(brand) %>% 
      summarise(total = sum(count),
                rc = sum(rookie_card),
                od = sum(opening_day),
                asg = sum(asg),
                chrome = sum(chrome),
                refractor = sum(refractor),
                heritage = sum(heritage),
                auto = sum(auto),
                num = length(which(numbered != 0))
      ) %>% 
      kbl(caption = "Number of Cards by Brand",
          col.names = c("Card Brand", "Total", "Rookie Cards", "Opening Day", "All Star Game", "Chrome", "Refractor", "Heritage", "Autograph", "Numbered")) %>%
      kable_styling(c('bordered', 'condensed', 'striped'), full_width = F)
    
  })
  
  #### card showcase page pieces ####
  
  # random number to pick a showcase card
  generate_showcase_int <- reactive({
    if (input$showcase_refresh == 0) {
      # showcase_int <- sample(seq(1:nrow(cool_cards)), 1)
    }
   
    # showcase_int <- sample(seq(1:nrow(cool_cards)), 1)
    showcase_int <- 2
    
  })
  
  # player name
  output$showcase_player_name <- renderText({
    cool_cards[generate_showcase_int(),]$name
  })
  
  # player position and team
  output$showcase_team_position <- renderText({
    paste0(cool_cards[generate_showcase_int(),]$team, " | ", cool_cards[generate_showcase_int(),]$position)
  })
  
  # table of summary information
  output$showcase_info <- renderText({
    
    card <- cool_cards[generate_showcase_int(), ]
    
    tibble(
      year = card$year,
      brand = card$brand,
      number = card$card_number,
      rc = ifelse(card$rookie_card == 1, "Yes", "No"),
      chrome = ifelse(card$chrome == 1, "Yes", "No"),
      parallel = ifelse(card$parallel != 0, card$parallel, "None"),
      refractor = ifelse(card$refractor == 1, "Yes", "No"),
      auto = ifelse(card$auto == 1, "Yes", "No"),
      numbered = ifelse(card$numbered != 0, card$numbered, "No"),
      imitation = ifelse(card$imitation != 0, card$imitation, "No"),
      notes = ifelse(!is.na(card$notes), card$notes, "None")
    ) %>% 
      kbl(col.names = c("Year", "Brand", "Card Number", "Rookie Card", "Chrome", "Parallel Color", "Refractor", "Autographed", "Numbered", "Imitation Year", "Notes"), align = 'c') %>% 
      kable_styling(c('bordered', 'condensed'), full_width = FALSE)
  })
  
  output$showcase_front <- renderImage({
    
    card <- cool_cards[generate_showcase_int(), ]
    new_num <- str_remove(card$card_number, "-") %>% tolower()
    
    filename <- normalizePath(file.path("./imgs", paste(generate_showcase_int(),
                                                       new_num, "f.png", sep = "_")))
    
    list(src = filename)
    
  }, deleteFile = FALSE)
  
}

# Run the app ----
shinyApp(ui = ui, server = server)