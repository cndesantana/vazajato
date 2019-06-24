
library(stringr)
library(dplyr)
library(tidytext)
library(tidyr)
library(widyr)
library(shiny)
library(igraph)
library(ggraph)


load("data/todas_hashtags_repetindo.RData")

df_tweets<- df_todas

names(df_todas)

rm("df_todas")
texto_df <- dplyr::data_frame(classe = df_tweets$termo,texto =df_tweets$text)

analise_tweets <- texto_df %>%
  unnest_tokens(palavra,texto) %>%
  count(classe, palavra, sort = TRUE) %>%
  ungroup()

total_palavras <- analise_tweets %>%
  group_by(classe) %>%
  summarize(total=sum(n))

analise_tweets<- analise_tweets %>% left_join(total_palavras)


analise_tweets <- analise_tweets %>%
  bind_tf_idf(palavra, classe, n)



stop_words_pcasp <- unique(c(unique(analise_tweets$palavra[analise_tweets$idf==0]), stopwords::stopwords("pt")))

termos<- unique(df_tweets$termo)


analise_twitter_secoes <- dplyr::data_frame(classe = df_tweets$termo,texto =df_tweets$text)%>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% stop_words_pcasp)

save(list=c("analise_twitter_secoes"),  file="data/word_cors.RData")

# we need to filter for at least relatively common words first
word_cors <- analise_twitter_secoes %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)


save(list=c("analise_twitter_secoes"),  file="data/word_cors.RData")

set.seed(2016)

word_cors %>%
  filter(correlation > .65) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


  
  df_texto_url <- df_todas%>%
    select(text, status_url, description) %>%
    mutate(text = str_to_lower(text))
  
  
  save(list = "df_texto_url", file = "data/df_texto_url.RData")
  
  df_texto_url %>%
    str_subset(text,"^(?=.*\\bmoro\\b)(?=.*\\bdescuido\\b)(?=.*\\bdedinho\\b).*$")
  
  
  str_subset(texto,"^(?=.*\\bmoro\\b)(?=.*\\bdescuido\\b)(?=.*\\bdedinho\\b).*$")
  
  str_detect(string=df_todas$text, pattern = "tacla", negate = TRUE)

  
  select(text)
  
  str_detect(text, "tacla")



  select(distinct(text))
  
  group_by(select(distinct(text)))%>%
  summarise(
    n()
  )

