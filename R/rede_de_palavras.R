library(dplyr)
library(tidytext)
library(wordcloud)
library(tidyr)
library(scales)
library(ggplot2)
library(igraph)
library(ggraph)
library(widyr)


#dataframe com texto e classe

load("data/df_tweets_10000_vazajato.Rdat")
texto_df <- dplyr::data_frame(classe = df_tweets$term,texto =df_tweets$text)

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

termos<- unique(df_tweets$term)


analise_twitter_secoes <- dplyr::data_frame(classe = df_tweets$term,texto =df_tweets$text)%>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% stop_words_pcasp)


# we need to filter for at least relatively common words first
word_cors <- analise_twitter_secoes %>%
  group_by(word) %>%
  filter(n() >= 100) %>%
  pairwise_cor(word, section, sort = TRUE)


set.seed(2016)

p1 <- word_cors %>%
  filter(correlation > .65) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

png("figures/redes_de_palavras.png", width = 3200, height = 1800, res = 300)
print(p1)
dev.off()

  
