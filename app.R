library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidytext)
library(lexiconPT)
library(igraph)
library(threejs)

options(scipen = 9999)

theme_set(theme_classic())

ler_tuites <- function(path) {
  env <- new.env()
  load(path, env)
  env <- as.list(env)
  do.call(bind_rows, env) %>% 
    as_tibble()
}

sent2cat <- function(valor) {
  resp <- rep("neutro", length(valor))
  resp[valor < 0] <- "negativo"
  resp[valor > 0] <- "positivos"
  resp
}

tratar_rede <- function(grafo_, size = 0.2, arrow_width = 1, 
                        paleta = "Set1") {
  centralidade <- centr_degree(grafo_, "in")$res
  tamanhos <- floor((centralidade[centralidade != 0]) ^ (1/3))
  nomes_grupos <- vertex_attr(grafo_, "name", V(grafo_)[centralidade != 0])
  
  comunidades <- igraph::clusters(grafo_)
  n_comun <- length(unique(comunidades$membership))
  
  if (n_comun == 1) {
    comunidades <- grafo_ %>% 
      igraph::as_data_frame() %>% 
      select(from, to) %>% 
      graph_from_data_frame(FALSE) %>% 
      igraph::cluster_louvain()
    n_comun <- length(unique(comunidades$membership))
  }
  
  if (n_comun > 9) {
    cores <- colors()[seq(30, 600, length.out = n_comun)]
  } else {
    cores <- RColorBrewer::brewer.pal(n_comun, paleta)
  }
  
  vertex_attr(grafo_, "size") <- size
  vertex_attr(grafo_, "color") <- cores[comunidades$membership]
  vertex_attr(grafo_, "label") <- ""
  vertex_attr(grafo_, "size",  V(grafo_)[centralidade != 0]) <- tamanhos
  vertex_attr(grafo_, "label",  V(grafo_)[centralidade != 0]) <- nomes_grupos
  edge_attr(grafo_, "arrow.width") <- arrow_width
  grafo_
}

tuites <- dir("data/", full.names = TRUE) %>% 
  str_subset("usuarios", TRUE) %>% 
  map_df(ler_tuites) %>% 
  distinct() %>% 
  mutate(data = lubridate::round_date(created, "10 minutes"))

tidy_tuites <- tuites %>% 
  unnest_tokens(word, text) %>% 
  filter(! word %in% c(get_stopwords("pt")$word, 
                       "t.co", "https", "é", "rt", 
                       "ser", "pra", "sobre", "1")) %>% 
  left_join(lexiconPT::oplexicon_v3.0, by = c("word" = "term"))

rede <- ler_tuites("data/tweets_dos_usuarios.RData") %>% 
  select(screen_name, mentions_screen_name) %>% 
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name))
  
grafo <- rede %>% 
  graph_from_data_frame() %>% 
  tratar_rede()

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Panorama geral", tabName = "geral", icon = icon("dashboard")),
      menuItem("Rede dos usuários", tabName = "rede", icon = icon("twitter")),
      menuItem("Conteúdo dos tuítes", tabName = "texto", icon = icon("envelope-open"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "geral",
        fluidRow(
          box(
            title = "Evolução do uso das hashtags", 
            plotOutput("graf_evo_tuites")
          ),
          box(
            title = "Evolução do sentimento dos tuítes", 
            plotOutput("graf_evo_sentimento")
          )
        ),
        fluidRow(
          box(
            title = "Quantidade de mensagens retuitadas, por termo de busca",
            plotOutput("graf_retuites")
          ),
          box(
            title = "Quantidade de mensagens retuitadas, por termo de usuário",
            plotOutput("graf_pessoas")
          )
        )
      ),
      tabItem(
        tabName = "rede",
        fluidRow(
          box(
            width = 12, title = "Rede de menções de perfis (interativa)",
            scatterplotThreeOutput("graf_rede_relacoes")
          )
        )
      ),
      tabItem(
        tabName = "texto",
        fluidRow(
          box(
            width = 6, title = "Rede relacionamento entre as palavras",
            plotOutput("graf_rede_texto")
          ),
          box(
            width = 6, title = "Palavras mais usadas",
            plotOutput("graf_cont_texto")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  output$graf_evo_tuites <- renderPlot({
    tuites %>% 
      count(term, data) %>% 
      ggplot(aes(data, n, col = term)) +
      geom_line() +
      theme(legend.position = "top",  legend.direction = "horizontal", 
            legend.title = element_blank())
  })
  
  output$graf_evo_sentimento <- renderPlot({
    tidy_tuites %>% 
      group_by(term, id, data) %>% 
      summarise(sentimento = sum(polarity, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(sentimento = sent2cat(sentimento)) %>% 
      count(term, data, sentimento) %>% 
      ggplot(aes(data, n, col = sentimento)) +
      geom_line(alpha = 0.6) +
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
  
  output$graf_rede_relacoes <- renderScatterplotThree({
    threejs::graphjs(grafo)
  })
  
  output$graf_rede_texto <- renderPlot({
    plot(1, 1, "n")
    text(1, 1, "Rede de palavras")
  })
  
  output$graf_cont_texto <- renderPlot({
    tidy_tuites %>% 
      count(term, word, sort = TRUE) %>% 
      # group_by(term) %>% 
      top_n(15) %>% 
      mutate(word = fct_reorder(word, n)) %>% 
      ggplot(aes(word, n, fill = term)) +
      geom_col() +
      coord_flip()
  })
  
}

shinyApp(ui, server)
         
