
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(party)
library(shiny)
Teams <- c("DET", "ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", 
           "DEN", "GB", "HOU", "IND", "JAX", "KC", "LA", "LAC", "MIA", "MIN", "NE",
           "NO", "NYG", "NYJ", "OAK", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS")
#load("C://data/FootballPredictions.rda")
app <- read.csv("football_trimmed.csv")
#app <- read.csv("C://data/input$posteam.csv")
#app <- subset(app, posteam == input$posteam)
app$ColumnA <- NULL
app$X <- NULL

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Predicting NFL Plays"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Stringency of Predictive Model",
                  min = 0.6,
                  max = 1,
                  value = 0.8, step = 0.05),
      selectInput("posteam", "Offensive Team", choices = Teams)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("table1")
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #selectedData <- reactive({
  # app %>% filter_all(all_vars(!grepl(input$posteam,.)))
  #})
  
  output$table1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    # draw the histogram with the specified number of bins
    xplr<-ctree_control(mincriterion = input$bins, minbucket=1)
    tree2 <- ctree(play_type ~ ., data=subset(app, posteam==input$posteam), controls=xplr)
    plot(tree2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)