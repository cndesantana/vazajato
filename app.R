library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Panorama geral", tabName = "geral", icon = icon("dashboard")),
      menuItem("Rede dos twites", tabName = "rede", icon = icon("twitter")),
      menuItem("Conteúdo dos twites", tabName = "texto", icon = icon("envelope-open"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "geral",
        fluidRow(
          box(
            plotOutput("graf_evo_tuites")
          ),
          box(
            plotOutput("graf_evo_sentimento")
          )
        ),
        fluidRow(
          box(
            plotOutput("graf_retuites")
          ),
          box(
            plotOutput("graf_pessoas")
          )
        )
      ),
      tabItem(
        tabName = "rede",
        fluidRow(
          box(
            width = 12, plotOutput("graf_rede_relacoes")
          )
        )
      ),
      tabItem(
        tabName = "texto",
        fluidRow(
          box(
            width = 6, plotOutput("graf_rede_texto")
          ),
          box(
            width = 6, plotOutput("graf_cont_texto")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  output$graf_evo_tuites <- renderPlot({
    plot(1, 1, "n")
    text(1, 1, "twittes")
  })
  
  output$graf_evo_sentimento <- renderPlot({
    plot(1, 1, "n")
    text(1, 1, "Sentimento")
  })
  
  output$graf_retuites <- renderPlot({
    plot(1, 1, "n")
    text(1, 1, "retuites")
  })
  
  output$graf_pessoas <- renderPlot({
    plot(1, 1, "n")
    text(1, 1, "pessoas")
  })
  
  output$graf_rede_relacoes <- renderPlot({
    plot(1, 1, "n")
    text(1, 1, "Rede de relacionamentos")
  })
  
  output$graf_rede_texto <- renderPlot({
    plot(1, 1, "n")
    text(1, 1, "Rede de palavras")
  })
  
  output$graf_cont_texto <- renderPlot({
    plot(1, 1, "n")
    text(1, 1, "Contagem de palavras")
  })
  
}

shinyApp(ui, server)
         
