#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidytext)
library(widyr)
library(irlba)
library(broom)
library(lexiconPT)
library(highcharter)
require(forcats)
require(plotly)


load(file = "word_vectors.RData")

words <- c("moro", "dallagnol", "#vazajato", "intercept", "lula", "bolsonaro",
           "haddad", "eleição", "denúncia", "crime", "armação", "aúdios")

search_synonyms <- function(word_vectors, selected_vector) {
  
  similarities <- word_vectors %*% selected_vector %>%
    tidy() %>%
    as_tibble() %>%
    rename(token = .rownames,
           similarity = unrowname.x.)
  
  similarities %>%
    arrange(-similarity)    
}

# Define UI for application that draws a histogram

ui <- fluidPage(

    # Application title
    titlePanel("Operação Vaza Jato"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("word", "Palavra de Interesse:",
                      words, multiple = FALSE, selected = "#vazajato"),
          sliderInput("n", "Escolha quantas palavras desejar:",
                      min = 10, max = 100, value = 20)
        ),
      
        

        # Show a plot of the generated distribution
        mainPanel(
           highchartOutput("Plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Plot <- renderHighchart({
      
     p1 <- search_synonyms(word_vectors, word_vectors[input$word,]) %>% 
        top_n(input$n, abs(similarity))
     
     hchart(p1, "column", hcaes(x = reorder(token, similarity), 
                                  y = similarity)) %>% 
       hc_add_theme(
         hc_theme_flatdark(
           chart = list(
             backgroundColor = "transparent",
             divBackgroundImage = "https://static.poder360.com.br/2019/06/photo5184012972538832908-868x644.jpg")))
     

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
