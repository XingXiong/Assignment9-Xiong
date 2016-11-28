library(shiny)
library(gapminder)
library(dplyr)
library(tidyverse)

ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(sliderInput("yearInput", "Year", min = 1952, max = 2007,
                             value = c(1960, 2000)),
                 textInput("nameInput", "Country")),
    
    
    mainPanel(plotOutput("main_plot"),
              tableOutput("results"))
  ),
  
  titlePanel("per capita GDP")
)



server <- function(input, output, session) {
  reduced_df <- reactive({
    gapminder$gdpPercap<-log10(gapminder$gdpPercap)
    
    filter(
      gapminder, 
      country == input$nameInput, 
      year >= input$yearInput[1] & year <= input$yearInput[2]
      
    )
  })
  
  output$main_plot <- renderPlot({
    ggplot(data = reduced_df(), aes(lifeExp, gdpPercap)) + geom_point() +geom_line() + ggtitle("per capita GDP")
  })
  
  output$results <- renderTable({ 
    reduced_df()
  })
}

shinyApp(ui = ui, server = server)