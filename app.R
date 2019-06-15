library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidytext)
library(lexiconPT)

options(scipen = 9999)

theme_set(theme_classic())

ler_tuites <- function(path) {
  env <- new.env()
  load(path, env)
  env <- as.list(env)
  do.call(bind_rows, env) %>% 
    as_tibble()
}

tuites <- dir("data/", full.names = TRUE) %>% 
  str_subset("usuarios", TRUE) %>% 
  map_df(ler_tuites) %>% 
  distinct()

data("oplexicon_v3.0")

tidy_tuites <- tuites %>% 
  unnest_tokens(word, text) %>% 
  left_join(lexiconPT::oplexicon_v3.0, by = c("word" = "term"))

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
    tuites %>% 
      mutate(data = lubridate::round_date(created, "minute")) %>% 
      count(term, data) %>% 
      ggplot(aes(data, n, col = term)) +
      geom_line() +
      theme(legend.position = "top",  legend.direction = "horizontal", 
            legend.title = element_blank())
  })
  
  output$graf_evo_sentimento <- renderPlot({
    tidy_tuites %>% 
      mutate(data = lubridate::round_date(created, "minute")) %>% 
      group_by(term, data) %>% 
      summarise(sentimento = sum(polarity, na.rm = TRUE)) %>% 
      ggplot(aes(data, sentimento, col = term)) +
      geom_line() +
      theme(legend.position = "top",  legend.direction = "horizontal", 
            legend.title = element_blank())
  })
  
  output$graf_retuites <- renderPlot({
    tuites %>%
      group_by(term) %>%
      summarise(tot = sum(retweetCount)) %>%
      filter(tot >= 10 ^ 5) %>% 
      ggplot(aes(x = reorder(term, tot), tot)) +
      geom_bar(stat = "identity", fill = "forestgreen") +
      geom_text(aes(x = reorder(term,tot), y = 10 ^7, 
                    label = paste0(signif(tot/10 ^ 6,2)," Milhões") ),
                size = 2.8) +
      xlab("Termos") +
      ylab("Retweets") +
      coord_flip()
  })
  
  output$graf_pessoas <- renderPlot({
    tuites %>%
      group_by(usuario = screenName) %>%
      summarise(tot = sum(retweetCount)) %>%
      top_n(10) %>% 
      ggplot(aes(x = reorder(usuario, tot), tot)) +
      geom_bar(stat = "identity", fill = "forestgreen") +
      geom_text(aes(x = reorder(usuario, tot), y = 1.5 * (10 ^ 4), 
                    label = paste0(signif(tot/10 ^ 3,2)," Mil") ),
                size = 2.8) +
      xlab("Usuário") +
      ylab("Retweets") +
      coord_flip() 
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
         
